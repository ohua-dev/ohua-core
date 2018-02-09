-- |
-- Module      : $Header$
-- Description : Transform an algorithm language term into single static assignment form
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=8 #-}
module Ohua.ALang.Passes.SSA where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Error  as E
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import           Data.Functor.Foldable
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid
import           Ohua.ALang.Lang
import           Ohua.Monad
import           Ohua.Types                 as Ty
import qualified Ohua.Util.Str              as Str


type LocalScope = HM.HashMap Binding Binding

ssaResolve :: Member (Reader LocalScope) effs => Binding -> Eff effs Binding
ssaResolve bnd = asks $ fromMaybe bnd <$> HM.lookup bnd

-- | Generate a new name for the provided binding and run the inner computation with
-- that name in scope to replace the provided binding
-- Returns both the generated binding and the result of the inner computation
--
-- Passing in the computation which is to be executed in the modified environment
-- makes this function a bit harder to use (or rather the code using it less readable)
-- because it does a lot of passing functions as arguments, however it very nicely
-- encapsulates the scope changes which means they will never leak from where they are
-- supposed to be applied
ssaRename :: (Members '[GenBnd, Reader LocalScope] effs, Eff effs a ~ t) => Binding -> (Binding -> t) -> t
ssaRename oldBnd cont = do
    newBnd <- generateBindingWith oldBnd
    local (HM.insert oldBnd newBnd) $ cont newBnd

ssaRenameMany :: (Members '[GenBnd, Reader LocalScope] effs, Eff effs a ~ t) => [Binding] -> ([Binding] -> t) -> t
ssaRenameMany oldBnds cont = do
    newBnds <- traverse generateBindingWith oldBnds
    local (HM.union $ HM.fromList $ zip oldBnds newBnds) $ cont newBnds

performSSA :: Member GenBnd effs => Expression -> Eff effs Expression
performSSA = runReader (mempty :: LocalScope) . ssa

flattenTuple :: (a, ([a], b)) -> ([a], b)
flattenTuple (a, (as, r)) = (a:as, r)

ssa :: Members '[Reader LocalScope, GenBnd] effs => Expression -> Eff effs Expression
ssa = cata $ \case
  VarF (Local bnd) -> Var . Local <$> ssaResolve bnd
  LambdaF arguments body -> handleAssignment arguments $ \assign -> Lambda assign <$> body
  LetF assignment value body -> handleAssignment assignment $ \assign -> Let assign <$> value <*> body
  e -> embed <$> sequence e

-- As you can see the destructuring makes writing some stuff quite difficult.
-- I wonder if it might not be easier to represent destructuring with a builtin function
-- instead and collapse it down at the very end ...
handleAssignment :: (Members '[Reader LocalScope, GenBnd] effs, Eff effs a ~ t)
                 => Assignment -> (Assignment -> t) -> t
handleAssignment (Direct d)       = ssaRename d . (. Direct)
handleAssignment (Recursive d)    = ssaRename d . (. Recursive)
handleAssignment (Destructure ds) = ssaRenameMany ds . (. Destructure)


-- Check if an expression is in ssa form.
-- Returns @Nothing@ if it is SSA
-- Returns @Just aBinding@ where @aBinding@ is a binding which was defined (at least) twice
isSSA :: Expression -> Maybe Binding
isSSA = either Just (const Nothing :: () -> Maybe a)
        . run
        . evalState (mempty :: HS.HashSet Binding)
        . runError
        . cata go
  where
    failOrInsert bnd = do
      isDefined <- gets (HS.member bnd)
      when isDefined $ throwError bnd
      modify (HS.insert bnd)
    go e = do
      case e of
        LetF assign _ _  -> mapM_ failOrInsert $ flattenAssign assign
        LambdaF assign _ -> mapM_ failOrInsert $ flattenAssign assign
        _                -> pure ()
      sequence_ e


checkSSA :: Member (E.Error Ty.Error) effs => Expression -> Eff effs ()
checkSSA = maybe (return ()) (throwError . mkMsg) . isSSA
  where
    mkMsg bnd = "Redefinition of binding " <> Str.showS bnd :: Str.Str
