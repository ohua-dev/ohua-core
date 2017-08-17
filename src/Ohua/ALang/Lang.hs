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
import           Data.Functor.Identity
import           Data.String
import           Ohua.Types


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


-- | Traverse an ALang expression from left to right and top down, building a new expression.
lrPrewalkExprM :: Monad m => (Expr b -> m (Expr b)) -> Expr b -> m (Expr b)
lrPrewalkExprM f e = f e >>= \case
    Let bnd val body -> Let bnd <$> lrPrewalkExprM f val <*> lrPrewalkExprM f body
    Apply fn arg -> Apply <$> lrPrewalkExprM f fn <*> lrPrewalkExprM f arg
    Lambda assign body -> Lambda assign <$> lrPrewalkExprM f body
    e' -> return e'


-- | Traverse an ALang expression from right to left and top down.
rlPrewalkExprM :: Monad m => (Expr b -> m (Expr b)) -> Expr b -> m (Expr b)
rlPrewalkExprM f e = f e >>= \case
    Let bnd val body -> flip (Let bnd) <$> lrPrewalkExprM f body <*> lrPrewalkExprM f val
    Apply fn arg -> flip Apply <$> lrPrewalkExprM f arg <*> lrPrewalkExprM f fn
    Lambda assign body -> Lambda assign <$> lrPrewalkExprM f body
    e' -> return e'


-- | Same as 'lrPrewalkExprM' but does not carry a monaic value.
lrPrewalkExpr :: (Expr b -> (Expr b)) -> Expr b -> (Expr b)
lrPrewalkExpr f = runIdentity . lrPrewalkExprM (return . f)


-- | Same as 'rlPrewalkExprM' but does not carry a monaic value.
rlPrewalkExpr :: (Expr b -> (Expr b)) -> Expr b -> (Expr b)
rlPrewalkExpr f = runIdentity . rlPrewalkExprM (return . f)


-- | Traverse an ALang expression from left to right and from the bottom up.
lrPostwalkExprM :: Monad m => (Expr b -> m (Expr b)) -> Expr b -> m (Expr b)
lrPostwalkExprM f e = f =<< case e of
    Let assign val body -> Let assign <$> lrPostwalkExprM f val <*> lrPostwalkExprM f body
    Apply fn arg -> Apply <$> lrPostwalkExprM f fn <*> lrPostwalkExprM f arg
    Lambda assign body -> Lambda assign <$> lrPostwalkExprM f body
    _ -> return e


-- | Traverse an ALang expression from right to left and from the bottom up.
rlPostwalkExprM :: Monad m => (Expr b -> m (Expr b)) -> Expr b -> m (Expr b)
rlPostwalkExprM f e = f =<< case e of
    Let assign val body -> flip (Let assign) <$> lrPostwalkExprM f body <*> lrPostwalkExprM f val
    Apply fn arg -> flip Apply <$> lrPostwalkExprM f arg <*>  lrPostwalkExprM f fn
    Lambda assign body -> Lambda assign <$> lrPostwalkExprM f body
    _ -> return e


-- | Same as 'lrPostwalkExprM' but does not carry a monad.
lrPostwalkExpr :: (Expr b -> (Expr b)) -> Expr b -> (Expr b)
lrPostwalkExpr f = runIdentity . lrPostwalkExprM (return . f)


-- | Same as 'lrPostwalkExprM' bot does not carry a monad.
rlPostwalkExpr :: (Expr b -> (Expr b)) -> Expr b -> (Expr b)
rlPostwalkExpr f = runIdentity . rlPostwalkExprM (return . f)


-- | Generic fold over an ALang expression.
-- Folds from top down and from left to right.
foldlExprM :: Monad m => (Expr a -> b -> m b) -> b -> Expr a -> m b
foldlExprM f b e = do
    b' <- f e b
    case e of
        Apply e1 e2 -> do
            b1 <- foldlExprM f b' e1
            foldlExprM f b1 e2
        Let _ e1 e2 -> do
            b1 <- foldlExprM f b' e1
            foldlExprM f b1 e2
        Lambda _ e1 -> foldlExprM f b' e1
        _ -> return b'


-- | Generic fold over an ALang expression.
-- Folds from bottom up and from right to left.
foldrExprM :: Monad m => (a -> Expr b -> m a) -> Expr b -> a -> m a
foldrExprM f e a = do
    a' <- case e of
        Apply e1 e2 -> do
            a2 <- foldrExprM f e2 a
            foldrExprM f e1 a2
        Let _ e1 e2 -> do
            a2 <- foldrExprM f e2 a
            foldrExprM f e1 a2
        Lambda _ e1 -> foldrExprM f e1 a
        _ -> return a
    f a' e


-- | Same as 'foldlExprM' but does not carry a monad.
foldlExpr :: (Expr a -> b -> b) -> b -> Expr a -> b
foldlExpr f b e = runIdentity $ foldlExprM (\x y -> return $ f x y) b e


-- | Same as 'foldrExprM' but does not carry a monad.
foldrExpr :: (a -> Expr b -> a) -> Expr b -> a -> a
foldrExpr f e b = runIdentity $ foldrExprM (\x y -> return $ f x y) e b

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
