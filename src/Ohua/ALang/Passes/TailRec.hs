{-# LANGUAGE CPP #-}

module Ohua.ALang.Passes.TailRec where

import Protolude

import Control.Monad.Writer
import Data.Functor.Foldable
import qualified Data.HashSet as HS

import Ohua.ALang.Lang
import Ohua.Types



-- This must run before algos are being inlined!
--
-- TODO verify the following assumption:
--      the recursive call is the last expression on an if branch (lambda expression used in if).
--      if this turns out not to be true then we should throw an exception.
-- the call is (recur algoRef args)
findTailRecs :: Expression -> Expression
findTailRecs = snd . flip findRecCall HS.empty

markRecursiveBindings :: Expression -> Expression
markRecursiveBindings = fst . runWriter . cata go
  where
    go (LetF assign e b)
      -- We censor here as this binding would shadow bindings from outside
     =
        shadowAssign assign $ do
            (e', isUsed) <-
                listens
                    (not .
                     null . HS.intersection (HS.fromList (extractBindings assign)))
                    e
            if isUsed
                then case assign of
                         Direct bnd -> Let (Recursive bnd) e' <$> b
                         _ -> panic "Cannot use destrutured binding recursively"
                else Let assign e' <$> b
    go (VarF val@(Local bnd)) = tell (HS.singleton bnd) >> pure (Var val)
    go e@(LambdaF assign _) = shadowAssign assign $ embed <$> sequence e
    go e = embed <$> sequence e
    shadowAssign (Direct b) = censor (HS.delete b)
    shadowAssign (Destructure bnds) = censor (`HS.difference` HS.fromList bnds)
    shadowAssign (Recursive _) =
        panic "TODO implement `shadowAssign` for `Recursive`"

findRecCall ::
       Expression -> HS.HashSet Binding -> (HS.HashSet Binding, Expression)
findRecCall (Let (Direct a) expr inExpr) algosInScope
    | HS.member a found = (iFound, Let (Recursive a) e iExpr)
    | otherwise = (HS.union found iFound, Let (Direct a) e iExpr)
  where
    -- TODO: Why?
    (found, e) = findRecCall expr $ HS.insert a algosInScope
    (iFound, iExpr) = findRecCall inExpr algosInScope


findRecCall (Let a expr inExpr) algosInScope =
    let (iFound, iExpr) = findRecCall inExpr algosInScope
     in (iFound, Let a expr iExpr)
findRecCall (Apply (Var (Local binding)) a) algosInScope
    | HS.member binding algosInScope
     -- no recursion here because if the expression is correct then these can be only nested APPLY statements
     = (HS.insert binding HS.empty, Apply "ohua.lang/recur" a)
findRecCall (Apply a b) algosInScope =
    let (aFound, aExpr) = findRecCall a algosInScope
        (bFound, bExpr) = findRecCall b algosInScope
     in (HS.union aFound bFound, Apply aExpr bExpr)
findRecCall (Var b) _ = (HS.empty, Var b)
findRecCall (Lambda a e) algosInScope =
    let (eFound, eExpr) = findRecCall e algosInScope
     in if HS.size eFound == 0
            then (eFound, Lambda a eExpr)
                              -- TODO would I need to lift that lambda here?
            else (eFound, Lambda a eExpr)
