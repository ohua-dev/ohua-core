-- |
-- Module      : $Header$
-- Description : Transform an algorithm language term into single static assignment form
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
module Ohua.ALang.Passes.SSA where

import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Data.Maybe           (fromMaybe)
import           Data.Monoid
import           Ohua.ALang.Lang
import           Ohua.Monad
import           Ohua.Types
import           Ohua.Util


type LocalScope = HM.HashMap Binding Binding

ssaResolve :: MonadReader LocalScope m => Binding -> m Binding
ssaResolve bnd = reader $ fromMaybe bnd <$> HM.lookup bnd

-- | Generate a new name for the provided binding and run the inner computation with
-- that name in scope to replace the provided binding
-- Returns both the generated binding and the result of the inner computation
--
-- Passing in the computation which is to be executed in the modified environment
-- makes this function a bit harder to use (or rather the code using it less readable)
-- becuase it does a lot of passing functions as arguments, however it very nicely
-- encapsulates the scope changes which means they will never leak from where they are
-- supposed to be applied
ssaRename :: (MonadOhua envExpr m, MonadReader LocalScope m) => Binding -> m a -> m (Binding, a)
ssaRename oldBnd cont = do
    newBnd <- generateBindingWith oldBnd
    (newBnd,) <$> local (HM.insert oldBnd newBnd) cont

performSSA :: MonadOhua envExpr m => Expression -> m Expression
performSSA = flip runReaderT mempty . ssa

flattenTuple :: (a, ([a], b)) -> ([a], b)
flattenTuple (a, (as, r)) = (a:as, r)

ssa :: (MonadOhua envExpr m, MonadReader LocalScope m) => Expression -> m Expression
ssa (Var abstrBinding) = Var <$>
    case abstrBinding of
        Local bnd -> Local <$> ssaResolve bnd
        other     -> return other
ssa (Apply function argument) = Apply <$> ssa function <*> ssa argument
ssa (Lambda argument body) = uncurry Lambda <$> handleAssignment argument (ssa body)
ssa (Let assignment value body) = do
    (ssaAssignment, (ssaValue, ssaBody)) <- handleAssignment assignment $ (,) <$> ssa value <*> ssa body
    return $ Let ssaAssignment ssaValue ssaBody

-- As you can see the destructuring makes writing some stuff quite difficult.
-- I wonder if it might not be easier to represent destructuring with a builtin function
-- instead and collapse it down at the very end ...
handleAssignment :: (MonadOhua envExpr m, MonadReader LocalScope m) => Assignment -> m t -> m (Assignment, t)
handleAssignment (Direct d) = fmap (first Direct) . ssaRename d
handleAssignment (Destructure ds) = fmap (first Destructure) . foldl (\f bnd -> f . fmap flattenTuple . ssaRename bnd) id ds . fmap ([],)


-- Check if an expression is in ssa form.
-- Returns @Nothing@ if it is SSA
-- Returns @Just aBinding@ where @aBinding@ is a binding which was defined (at least) twice
isSSA :: Expression -> Maybe Binding
isSSA = either Just (const Nothing) . flip evalState mempty . runExceptT . go
  where
    failOrInsert bnd = do
        isDefined <- gets (HS.member bnd)
        when isDefined $ throwError bnd
        modify (HS.insert bnd)
    go (Let assignment expr1 expr2) = do
        mapM_ failOrInsert $ flattenAssign assignment
        go expr1
        go expr2
    go (Apply expr1 expr2) = go expr1 >> go expr2
    go (Lambda assignment body) = do
        mapM_ failOrInsert $ flattenAssign assignment
        go body
    go (Var _) = return ()


checkSSA :: MonadOhua envExpr m => Expression -> m ()
checkSSA = maybe (return ()) (failWith . mkMsg) . isSSA
  where
    mkMsg bnd = "Redefinition of binding " <> showT bnd
