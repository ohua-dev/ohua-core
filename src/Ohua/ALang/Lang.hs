-- |
-- Module      : $Header$
-- Description : Definition of an abstract expression language as the first IR for the Ohua compiler.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.ALang.Lang where

import           Control.DeepSeq
import           Data.Function
import           Data.String
import           Ohua.Types
import           Ohua.Util

-- bindingType is the type of general bindings
-- Binding is the type for function arguments and `let`s etc which we know to always be local

-- I revised the proposal.
-- I got rid of `Fn`. Not because of cyclic dependencies but because I think that function application can be more general.

newtype HostExpr = HostExpr { unwrapHostExpr :: Int } deriving (Show, Eq, Ord)

-- Only exists to allow literal integers to be interpreted as host expressions
instance Num HostExpr where fromInteger = HostExpr . fromInteger

-- (Sebastian) can we treat opaque JVM objects here somehow?
-- (Justus) Several. Well have to discus what the best option is at some point,
--          because the concrete type of the host expression depends on the backend.


-- some types for `bindingType`

type RawSymbol = HostExpr
data ResolvedSymbol

       -- the basic symbols occuring in the algorithm language

    = Local Binding

        -- a variable/binding in the algorithm language

    | Sf FnName (Maybe FnId)

        -- reference to a stateful function

    | Algo FnName

        -- reference to an algo definition

    | Env HostExpr

        -- reference to an environment object. this maybe a var or any other term of the host language.
    deriving (Show, Eq)

instance IsString ResolvedSymbol where
    fromString = either error (either (`Sf` Nothing) Local) . symbolFromString

instance ExtractBindings ResolvedSymbol where
    extractBindings (Local l) = return l
    extractBindings _         = mempty

instance NFData ResolvedSymbol where
    rnf (Local b) = rnf b
    rnf (Algo a)  = rnf a
    rnf (Env _)   = ()
    rnf (Sf s i)  = rnf s `seq` rnf i

lrPrewalkExpr :: Monad m => (Expr b -> m (Expr b)) -> Expr b -> m (Expr b)
lrPrewalkExpr f e = f e >>= \case
    Let bnd val body -> Let bnd <$> lrPrewalkExpr f val <*> lrPrewalkExpr f body
    Apply fn arg -> Apply <$> lrPrewalkExpr f fn <*> lrPrewalkExpr f arg
    Lambda assign body -> Lambda assign <$> lrPrewalkExpr f body
    e' -> return e'


lrPostwalkExpr :: Monad m => (Expr b -> m (Expr b)) -> Expr b -> m (Expr b)
lrPostwalkExpr f (Let assign val body) = f =<< Let assign <$> lrPostwalkExpr f val <*> lrPostwalkExpr f body
lrPostwalkExpr f (Apply fn arg) = f =<< Apply <$> lrPostwalkExpr f fn <*> lrPostwalkExpr f arg
lrPostwalkExpr f (Lambda assign body) = f =<< lrPostwalkExpr f body
lrPostwalkExpr _ e = return e

-- (Sebastian) TODO: In Clojure, every variable/binding, even local ones, are namespaced. Should we go down this path too, i.e., change Binding to take FnReference/Reference?
-- (Justus)    AFAIK The local ones aren't namespaced ... Apart from that since we distinguish between local and
--             env bindings already it seems redundant to me to namespace them too.

-- IMPORTANT: we need this to be polymorphic over `bindingType` or at least I would very much
-- recommend that, because then we can separate the generation of the algorithms language
-- and the symbol resolution.
-- If we dont separate those we'll have to reimplement the complete symbol resolution pass for
-- each frontend

-- I changed this again so that application and let both do not use lists.
-- this makes generic transformations much simpler to write, such as SSA
--
--
-- For comparison, here is the GHC intermediate language `Core`
--
-- data Expr b
    -- much like us they are polymorphic over the
    -- binding type, however in their case its the for
    -- assigments, not variables
--
--   = Var   Id
--
--   | Lit   Literal
    -- we dont need this as this is encoded in `Var`
--
--   | App   (Expr b) (Arg b)
    -- Expr == Arg => App ~= Apply
--
--   | Lam   b (Expr b)
--   | Let   (Bind b) (Expr b)
    -- Bind == (b, Expr b) (plus recursive)
    -- I want to note that they too use single assigment
    -- let, not multiple assigment with lists
    -- same goes for lambdas and apply
--
--   | Case  (Expr b) b Type [Alt b]
    -- We have no case, hence we don't need this constructor
--
--   | Cast  (Expr b) Coercion
    -- This we might need if we get a type system

--   | Tick  (Tickish Id) (Expr b)
    -- No clue what this is used for yet
--
--   | Type  Type
    -- Types ... we might need this too at some point
--
--   | Coercion Coercion
    -- I've heard about this, this is what makes
    -- newtypes efficient now
--   deriving Data
--
data Expr bindingType
    = Var bindingType
    | Let Assignment (Expr bindingType) (Expr bindingType)
    | Apply (Expr bindingType) (Expr bindingType)
    | Lambda Assignment (Expr bindingType)
    deriving (Show, Eq)

instance IsString b => IsString (Expr b) where
    fromString = Var . fromString

instance ExtractBindings b => ExtractBindings (Expr b) where
    extractBindings (Var v) = extractBindings v
    extractBindings (Let assign val body) = extractBindings assign ++ extractBindings val ++ extractBindings body
    extractBindings (Apply function argument) = extractBindings function ++ extractBindings argument
    extractBindings (Lambda assign body) = extractBindings assign ++ extractBindings body


instance NFData a => NFData (Expr a) where
    rnf (Let a b c)  = rnf a `seq` rnf b `seq` rnf c
    rnf (Var a)      = rnf a
    rnf (Apply a b)  = rnf a `seq` rnf b
    rnf (Lambda a b) = rnf a `seq` rnf b

type Expression = Expr ResolvedSymbol


-- (Sebastian) TODO: I think there needs to be a clear distinction between stateful functions and algos.
--             Only lambdas for algos are allowed.
-- (Justus)    In my mind `Lambda` is always an algo. An inline clojure function is an env arg with the function expression.
-- (Sebastian) I think, if possible, we should make that explicit in the language. The point is that the language currently
--             does not say whether it is Apply Algo or Apply Sfn. I therefore added another symbol below for that.
--             We can discuss whether a mapping should already have to inline all algos but I believe not requiring
--             that makes adapting a new language easier. Plus: we are in control in case we ever want to change that.
-- (Justus)    The issue is that depending on our constraints on runtime algos it may not always be possible to determine
--             whether something is an algo or sfn application if the application target is dynamically provided at runtime.
-- (Sebastian) True. Should we add basic type info to the language then? Something like only 2 types: Sfn and Algo?
--             Hence, when a Clojure is called which returns something to be invoked then one has to provide type information
--             because the compiler can not infer it. Potentially inside the algorithm itself in order to support calling
--             functions define in a library that we can not edit. The compiler could request that information on demand.

-- example

-- (let [x (sfn 5 0)
--       [y z] (sfn4 "string")]
--   ((algo [a] (functionF a)) (functionA (functionB y) (functionC x z))))

-- with SimpleBinding
-- Object represented as quoted clojure expressions
-- Binding represented through strings

-- firstConversionPass :: Expression RawSymbol
-- firstConversionPass =
--     Let [ (Direct "x", Apply (Var "sfn") [Var 5, Var 0])
--         , (Destructure ["y", "z"], Apply (Var "sfn4") [Var "string"])
--         ]
--         (Apply
--             (Lambda ["a"] (Apply (Var "functionF") [Var "a"]))
--             (Apply
--                 (Var "functionA")
--                 [ Apply (Var "functionB") [Var "y"]
--                 , Apply (Var "functionC") [Var "x", Var "z"]
--                 ]))

-- with ResolvedBindings
-- resolvePass :: Expression ResolvedSymbol
-- resolvePass =
--     Let [ (Direct "x", Apply (Var "sfn") [Var (Env 5), Var (Env 0)])
--         , (Destructure ["y", "z"], Apply (Var "sfn4") [Var (Env "string")])
--         ]
--         (Apply
--             (Lambda [Direct "a"] (Apply (Var (Sfn "functionF")) [Var (Local "a")]))
--             (Apply
--                 (Var (Sfn "functionA"))
--                 [ Apply (Var (Sfn "functionB")) [Var (Local "y")]
--                 , Apply (Var (Sfn "functionC")) [Var (Local "x"), Var (Local "z")]
--                 ]))
