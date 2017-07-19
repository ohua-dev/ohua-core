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
import           Ohua.ALang.Lang
import           Ohua.Monad
import           Ohua.Types


newtype LocalScope = LocalScope { unLocalScope :: HM.HashMap Binding Binding } deriving (Eq, Show)

newtype SSAM a = SSAM { runSSAM :: ReaderT LocalScope OhuaC a }
    deriving (Functor, Applicative, Monad)

instance MonadOhua SSAM where
    liftOhua = SSAM . lift

ssaResolve :: Binding -> SSAM Binding
ssaResolve bnd = SSAM $ reader $ fromMaybe bnd <$> HM.lookup bnd . unLocalScope

-- | Generate a new name for the provided binding and run the inner computation with
-- that name in scope to replace the provided binding
-- Returns both the generated binding and the result of the inner computation
--
-- Passing in the computation which is to be executed in the modified environment
-- makes this function a bit harder to use (or rather the code using it less readable)
-- becuase it does a lot of passing functions as arguments, however it very nicely
-- encapsulates the scope changes which means they will never leak from where they are
-- supposed to be applied
ssaRename :: Binding -> SSAM a -> SSAM (Binding, a)
ssaRename oldBnd cont = do
    newBnd <- generateBindingWith oldBnd
    SSAM $ (newBnd,) <$> local (LocalScope . HM.insert oldBnd newBnd . unLocalScope) (runSSAM cont)

mkSSA :: MonadOhua m => Expression -> m Expression
mkSSA = liftOhua . flip runReaderT (LocalScope mempty) . runSSAM . ssa

flattenTuple (a, (as, r)) = (a:as, r)

ssa :: Expression -> SSAM Expression
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
handleAssignment :: Assignment -> SSAM t -> SSAM (Assignment, t)
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
    go (Var a) = return ()
