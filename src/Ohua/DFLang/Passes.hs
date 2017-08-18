-- |
-- Module      : $Header$
-- Description : Definition of an abstract expression language as the first IR for the Ohua compiler.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
--
-- Passes required to transform an expression in ALang into an expression in DFLang.
--
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeApplications #-}
#endif
module Ohua.DFLang.Passes where


import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Either
import           Data.Foldable
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import           Data.Maybe
import           Data.Proxy
import           Data.Sequence                   (Seq, (<|), (><), (|>))
import qualified Data.Sequence                   as S
import           Data.Traversable
import           Lens.Micro
import           Ohua.ALang.Lang
import           Ohua.DFLang.HigherOrderFunction as HOF
import           Ohua.DFLang.Lang                (DFExpr (..), DFFnRef (..),
                                                  DFVar (..), LetExpr (..))
import           Ohua.Monad
import           Ohua.Types

import           Debug.Trace


type Pass m = FnName -> FnId -> Assignment -> [Expression] -> m (Seq LetExpr)

passes :: (MonadOhua m, MonadError String m) => HM.HashMap FnName (Pass m)
passes =
    [ ("com.ohua.lang/smap", lowerSmap)
    , ("com.ohua.lang/if", lowerIf) -- TODO check if that "if" is actually the correct name
    , ("com.ohua.lang/seq", lowerSeq)
    ]


-- | Check that a sequence of let expressions does not redefine bindings.
checkSSA :: (Foldable f, MonadError String m) => f LetExpr -> m ()
checkSSA = flip evalStateT mempty . mapM_ go
  where
    go le = do
        defined <- get
        let produced = flattenAssign (returnAssignment le)
            f a | HS.member a defined = Just a
            f _ = Nothing
        case msum $ map f produced of
            Just b  -> throwError $ "Rebinding of " ++ show b
            Nothing -> return ()
        modify (addAll produced)

    addAll add set = foldr' HS.insert set add


-- | Check that a DFExpression is in SSA form.
checkSSAExpr :: MonadError String m => DFExpr -> m ()
checkSSAExpr (DFExpr l _) = checkSSA l


-- | Transform an ALang expression into a DFExpression.
-- This assumes a certain structure in the expression.
-- This can be achieved with the 'normalize' and 'performSSA' functions and tested with
-- 'checkProgramValidity'.
lowerALang :: (MonadOhua m, MonadError String m) => Expression -> m DFExpr
lowerALang expr = do
    (var, exprs) <- runWriterT (go expr)
#ifdef DEBUG
    checkSSA exprs
#endif
    return $ DFExpr exprs var
  where
    go  (Var (Local bnd)) = return bnd
    go  (Var _) = throwError "Non local return binding"
    go  (Let assign expr rest) = do
        (fn, fnId, args) <- handleApplyExpr expr
        tell =<< dispatchFnType fn fnId assign args
        go rest
    go  x = throwError "Expected `let` or binding"


lowerALang2 :: (MonadOhua m, MonadError String m) => Expression -> m DFExpr
lowerALang2 expr = do
    (var, exprs) <- runWriterT (go expr)
#ifdef DEBUG
    checkSSA exprs
#endif
    return $ DFExpr exprs var
  where
    go  (Var (Local bnd)) = return bnd
    go  (Var _) = throwError "Non local return binding"
    go  (Let assign expr rest) = do
        (fn, fnId, args) <- handleApplyExpr expr
        case HM.lookup fn hofNames of
#if __GLASGOW_HASKELL__ >= 800
            Just (WHOF (_ :: Proxy p)) -> lowerHOF (name @p) assign args
#else
            Just (WHOF (_ :: Proxy p)) -> lowerHOF (name :: TaggedFnName p) assign args
#endif
            Nothing       -> tell =<< lowerDefault fn fnId assign args
        go rest
    go  x = throwError "Expected `let` or binding"

    hofNames = HM.fromList $ map (extractName &&& id) hofs
#if __GLASGOW_HASKELL__ >= 800
    extractName (WHOF (_ :: Proxy p)) = unTagFnName $ name @p
#else
    extractName (WHOF (_ :: Proxy p)) = unTagFnName $ (name :: TaggedFnName p)
#endif


-- | Select a function for lowering a let expression absed on the type of the function called.
dispatchFnType :: (MonadOhua m, MonadError String m) => Pass m
dispatchFnType fn = fromMaybe lowerDefault (HM.lookup fn passes) $ fn


-- | Lowers an smap call.
lowerSmap :: (MonadOhua m, MonadError String m) => Pass m
lowerSmap _ fnId assign args = do
    -- TODO add the "one-to-n"s
    lowered <- lowerALang body
    identityId <- generateId
    coll <- expectVar collE
    pure
        $ (LetExpr fnId inVar (DFFunction "com.ohua.lang/smap-fun") (return coll) Nothing
            <| letExprs lowered)
        |> LetExpr identityId assign (EmbedSf "com.ohua.lang/collect") [DFVar (returnVar lowered)] Nothing
  where
    [Lambda inVar body, collE] = args


-- | Lowers an if call.
lowerIf :: (MonadOhua m, MonadError String m) => Pass m
lowerIf _ fnId assign args = do
    dfCond <- expectVar condition
    loweredThen <- lowerALang thenBody
    loweredElse <- lowerALang elseBody

    switchId <- generateId

    pure
        $ (LetExpr fnId (Destructure [thenVar, elseVar])
            (DFFunction "com.ohua.lang/ifThenElse") [dfCond] Nothing
            <| tieContext thenVar (letExprs loweredThen)
            >< tieContext elseVar (letExprs loweredElse))
        |> LetExpr switchId assign (DFFunction "com.ohua.lang/switch")
            [DFVar (returnVar loweredThen), DFVar (returnVar loweredElse)]
            Nothing
  where
    [condition, Lambda (Direct thenVar) thenBody, Lambda (Direct elseVar) elseBody] = args


-- | Lowers a seq call.
lowerSeq :: (MonadOhua m, MonadError String m) => Pass m
lowerSeq _ fnId assign args = do
    loweredExpr <- lowerALang toSeq
    return $ tieContext binding (letExprs loweredExpr) |> LetExpr fnId assign (EmbedSf "com.ohua.lang/id") [DFVar (returnVar loweredExpr)] Nothing
  where
    -- Is binding first the right way around?
    [Var (Local binding), Lambda _ toSeq] = args


-- | Lower any not specially treated function type.
lowerDefault :: (MonadOhua m, MonadError String m) => Pass m
lowerDefault fn fnId assign args = mapM expectVar args <&> \args' -> [LetExpr fnId assign (EmbedSf fn) args' Nothing]


-- | Tie all functions which do not depend on locally bound variables via context arc
-- to a source binding.
tieContext :: (Functor f, Foldable f) => Binding -> f LetExpr -> f LetExpr
tieContext ctxSource exprs = tieContext0 bounds ctxSource exprs
  where
    bounds = findBoundVars exprs


tieContext0 :: Functor f => HS.HashSet Binding -> Binding -> f LetExpr -> f LetExpr
tieContext0 bounds ctxSource = fmap go
  where
    go e | any isBoundArg (callArguments e) = e
    go e = e { contextArg = Just ctxSource }

    isBoundArg (DFVar v) = v `HS.member` bounds
    isBoundArg _         = False

-- | Find all locally bound variables.
findBoundVars :: (Functor f, Foldable f) => f LetExpr -> HS.HashSet Binding
findBoundVars = HS.fromList . fold . fmap (flattenAssign . returnAssignment)


findFreeVars0 :: Foldable f => HS.HashSet Binding -> f LetExpr -> HS.HashSet Binding
findFreeVars0 boundVars = HS.fromList . concatMap (mapMaybe f . callArguments)
  where
    f (DFVar b) | not (HS.member b boundVars) = Just b
    f _         = Nothing


-- | Insert a `one-to-n` node for each free variable to scope them.
replicateFreeVars :: (MonadOhua m, MonadError String m) => Binding -> [Binding] -> Seq LetExpr -> m (Seq LetExpr)
replicateFreeVars countSource_ initialBindings exprs = do
    (replicators, replications) <- unzip <$> mapM mkReplicator freeVars
    pure $ S.fromList replicators >< renameWith (HM.fromList $ zip freeVars replications) exprs
  where
    boundVars = findBoundVars exprs `mappend` HS.fromList initialBindings

    freeVars = HS.toList $ HS.fromList $ concatMap (mapMaybe f . callArguments) exprs

    f (DFVar b) | not (HS.member b boundVars) = Just b
    f _         = Nothing

    mkReplicator var = do
        id <- generateId
        newVar <- generateBindingWith var
        pure (LetExpr id (Direct newVar) (DFFunction "com.ohua.lang/one-to-n") [DFVar countSource_, DFVar var] Nothing, newVar)


renameWith :: Functor f => HM.HashMap Binding Binding -> f LetExpr -> f LetExpr
renameWith m = fmap go
  where
    go e = e { callArguments = map (\case v@(DFVar var) -> maybe v DFVar $ HM.lookup var m; v -> v;) (callArguments e) }


-- | Ananlyze an apply expression, extracting the inner stateful function and the nested arguments as a list.
-- Also generates a new function id for the inner function should it not have one yet.
handleApplyExpr :: (MonadOhua m, MonadError String m) => Expression -> m (FnName, FnId, [Expression])
handleApplyExpr (Apply fn arg) = go fn [arg]
  where
    go (Var (Sf fn id)) args = (fn, , args) <$> maybe generateId return id
            -- reject algos for now
    go (Var v) _             = throwError $ "Expected Var Sf but got: Var " ++ show v -- FIXME there should be a special type of error here that takes the string and a value
    go (Apply fn arg) args   = go fn (arg:args)
    go x _                   = throwError $ "Expected Apply or Var but got: " ++ show x
handleApplyExpr (Var (Sf fn id)) = (fn, , []) <$> maybe generateId return id
handleApplyExpr g = throwError $ "Expected apply but got: " ++ show g


-- | Inspect an expression expecting something which can be captured in a DFVar otherwies throws appropriate errors.
expectVar :: MonadError String m => Expression -> m DFVar
expectVar (Var (Local bnd)) = pure $ DFVar bnd
expectVar (Var (Env i))     = pure $ DFEnvVar i
expectVar (Var _)           = throwError "Var must be local or env"
expectVar _                 = throwError "Argument must be var"


lowerHOF :: forall f m . (MonadError String m, MonadOhua m, HigherOrderFunction f, MonadWriter (Seq LetExpr) m)
         => TaggedFnName f -> Assignment -> [Expression] -> m ()
lowerHOF name assign args = do
    simpleArgs <- mapM handleArg args
    let newFnArgs = map (either Variable LamArg . fmap fst) simpleArgs
    f <- HOF.init newFnArgs :: m f
    flip evalStateT f $ do
        begin >>= tell
        let lambdas = rights simpleArgs
        for_ lambdas $ \(lam, body) -> do
            let boundVars = findBoundVars body `mappend` HS.fromList (flattenAssign $ beginAssignment lam)
            let freeVars = HS.toList $ findFreeVars0 boundVars body
            (scopers, renaming) <- scopeFreeVariables lam freeVars
            tell scopers
            scopeUnbound <- scopeUnboundFunctions lam
            let tieContext1 = if scopeUnbound then tieContext0 boundVars (head $ flattenAssign $ beginAssignment lam) else id
            tell $ tieContext1 (renameWith (HM.fromList renaming) body)
        end assign >>= tell
  where
    handleArg (Var (Local v)) = return $ Left $ DFVar v
    handleArg (Var (Env e)) = return $ Left $ DFEnvVar e
    handleArg (Lambda assign body) = do
        DFExpr lets bnd <- lowerALang body
        return $ Right (Lam assign bnd, lets)
    handleArg _ = throwError "unexpected type of argument"


data SmapFn = SmapFn
    { collSource :: !DFVar
    , smapLambda :: !Lambda
    , sizeSource :: Binding
    }


instance HigherOrderFunction SmapFn where
    name = "com.ohua.lang/smap"

    init [Variable v, LamArg bnd] = return $ SmapFn v bnd (error "size uninitialized")
    init _ = throwError "Unexpected number/type of arguments to smap"

    begin = do
        f <- get
        sizeId <- generateId
        sizeRet <- generateBindingWith "size"
        otnId <- generateId
        otnRet <- generateBinding
        smapId <- generateId
        modify $ \s -> s { sizeSource = otnRet } -- do we need the size or otn of the size?
        return
            [ LetExpr sizeId (Direct sizeRet) (EmbedSf "com.ohua.lang/size") [collSource f] Nothing
            -- I am not sure about the order here ... is it size first or collection first?
            , LetExpr otnId (Direct otnRet) (DFFunction "com.ohua.lang/one-to-n") [DFVar sizeRet, collSource f] Nothing
            , LetExpr smapId (beginAssignment $ smapLambda f) (DFFunction "com.ohua.lang/smap-fun") [DFVar otnRet] Nothing
            ]

    end assignment = do
        collectId <- generateId
        f <- get
        return
            [ LetExpr collectId assignment (DFFunction "com.ohua.lang/collect") [DFVar $ sizeSource f, DFVar $ resultBinding $ smapLambda f] Nothing
            ]

    scopeFreeVariables _ freeVars = do
        SmapFn{sizeSource} <- get
        let mkReplicator var = do
                id <- generateId
                newVar <- generateBindingWith var
                pure (LetExpr id (Direct newVar) (DFFunction "com.ohua.lang/one-to-n") [DFVar sizeSource, DFVar var] Nothing, newVar)
        (replicators, replications) <- unzip <$> mapM mkReplicator freeVars
        return (S.fromList replicators, zip freeVars replications)

    scopeUnboundFunctions _ = return True


data IfFn = IfFn
    { conditionVariable :: !DFVar
    , thenBranch        :: !Lambda
    , elseBranch        :: !Lambda
    , ifRet             :: Binding
    }


instance HigherOrderFunction IfFn where
    name = "com.ohua.lang/if"

    init [Variable v, LamArg thenBr, LamArg elseBr] = return $ IfFn v thenBr elseBr (error "return uninitialized")

    begin = do
        f <- get
        ifId <- generateId
        ifRet <- generateBinding
        let Direct thenBnd = beginAssignment $ thenBranch f
            Direct elseBnd = beginAssignment $ elseBranch f
        modify $ \s -> s { ifRet = ifRet }
        return
            -- not sure this has to be a dffunction
            [ LetExpr ifId (Destructure [ifRet, thenBnd, elseBnd]) (DFFunction "com.ohua.lang/if") [conditionVariable f] Nothing
            ]

    end assignment = do
        switchId <- generateId
        IfFn {..} <- get
        return
            [ LetExpr switchId assignment (DFFunction "com.ohua.lang/switch") [DFVar ifRet, DFVar $ resultBinding thenBranch, DFVar $ resultBinding elseBranch] Nothing
            ]

    scopeFreeVariables lam freeVars = do
        selected <- mapM generateBindingWith freeVars
        selectorId <- generateId
        let Direct sourceVar = beginAssignment lam
        return   -- im just prepending the if return, this is probably not correct
            (   [ LetExpr selectorId (Destructure selected) (DFFunction "com.ohua.lang/scope") (map DFVar (sourceVar:freeVars)) Nothing
                ]
            ,   zip freeVars selected
            )

    scopeUnboundFunctions _ = return True


hofs :: [WHOF]
hofs =
    [ WHOF (Proxy :: Proxy IfFn)
    , WHOF (Proxy :: Proxy SmapFn)
    -- , WHOF (Proxy :: Proxy SeqFn)
    ]
