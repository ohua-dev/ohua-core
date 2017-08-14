{-# LANGUAGE CPP #-}
module Ohua.Compile where


import           Control.Monad.Except
import           Data.Functor.Identity
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe
import           Lens.Micro
import           Ohua.ALang.Lang
import           Ohua.ALang.Optimizations
import           Ohua.ALang.Passes
import           Ohua.ALang.Passes.SSA
import           Ohua.DFGraph
import           Ohua.DFLang.Lang
import           Ohua.DFLang.Optimizations
import           Ohua.DFLang.Passes
import           Ohua.Monad
import           Ohua.Types


pipeline :: (MonadError String m, MonadOhua m) => Expression -> m OutGraph
pipeline e
    = performSSA e
    >>= normalize
    >>= \e -> checkHigherOrderFunctionSupport e >> return e
#ifdef DEBUG
    >>= \e -> Ohua.ALang.Passes.SSA.checkSSA e >> return e
#endif
    >>= Ohua.ALang.Optimizations.runOptimizations
#ifdef DEBUG
    >>= \e -> Ohua.ALang.Passes.SSA.checkSSA e >> return e
#endif
    >>= lowerALang
#ifdef DEBUG
    >>= \e -> Ohua.DFLang.Passes.checkSSA e >> return e
#endif
    >>= Ohua.DFLang.Optimizations.runOptimizations

#ifdef DEBUG
    >>= \e -> Ohua.DFLang.Passes.checkSSA e >> return e
#endif
    -- Comment: I use `<&>` (aka `fmap`) here because `toGraph` does not run in a monad
    <&> toGraph


compile :: Expression -> IO (Either String OutGraph)
compile e = flip runOhuaC e $ runExceptT . pipeline


checkHigherOrderFunctionSupport :: MonadError String m => Expression -> m ()
checkHigherOrderFunctionSupport (Let _ e rest) = do
    checkNestedExpr e
    checkHigherOrderFunctionSupport rest
  where
    checkNestedExpr (Apply f arg) = do
        supportsHOF <- checkNestedExpr f
        when (isLambda arg && not supportsHOF) $ throwError "Lambdas may only be input to higher order functions!"
        return True
    checkNestedExpr (Var (Sf n _)) = return $ HM.member n (passes :: HM.HashMap FnName (Pass (ExceptT String (OhuaT Identity))))
    checkNestedExpr (Var _) = return False
    checkNestedExpr _ = throwError "Expected var or apply expr"
    isLambda (Lambda _ _) = True
    isLambda _            = False
checkHigherOrderFunctionSupport (Var _) = return ()
checkHigherOrderFunctionSupport _ = throwError "Expected let or var"
