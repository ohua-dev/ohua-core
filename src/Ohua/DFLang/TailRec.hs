module Ohua.DFLang.TailRec where

import Protolude

import Control.Exception
import Control.Monad.Writer
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S

import Ohua.ALang.Lang
import Ohua.ALang.Refs as ALangRefs
import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs
import Ohua.DFLang.Util
import Ohua.Monad
import Ohua.Types
import Ohua.Util

-- the call in ALang is still (recur algoRef args).
-- it needs to become (recur conditionOutput algoInArgs recurArgs).
data RecursiveLambdaSpec = RecursiveLambdaSpec
    { formalInputs :: [Binding] -- as defined by the expression
    , lambdaFormalsToAlgoIn :: HM.HashMap Binding Binding
    , dfExpr :: DFExpr
    } deriving (Show)

-- | Executed to generate the recursive lambda expression.
recursionLowering ::
       (MonadError Error m, MonadGenId m, MonadGenBnd m)
    => [Binding]
    -> DFExpr
    -> m RecursiveLambdaSpec
--recursionLowering binding = (transformRecursiveTailCall
recursionLowering lambdaFormals dfExpr0 =
    transformRecursiveTailCall lambdaFormals dfExpr0

transformRecursiveTailCall ::
       (MonadError Error m, MonadGenId m, MonadGenBnd m)
    => [Binding]
    -> DFExpr
    -> m RecursiveLambdaSpec
transformRecursiveTailCall lambdaFormals exprs =
    handleRecursiveTailCall lambdaFormals exprs $
    findExpr Refs.recur $ letExprs exprs

handleRecursiveTailCall ::
       (MonadError Error m, MonadGenBnd m, MonadGenId m)
    => [Binding]
    -> DFExpr
    -> Maybe LetExpr
    -> m RecursiveLambdaSpec
handleRecursiveTailCall _ dfExpr0 Nothing =
    failWith $ "could not find recur in DFExpr:\n" <> show dfExpr0
handleRecursiveTailCall lambdaFormals dfExpr0 (Just recurFn) = do
    let dfExprs = letExprs dfExpr0
    -- helpers
    let unAssignment e x =
            case x of
                Direct b -> b
                _ ->
                    panicS $
                    "invariant broken: '" <> e <> "' always gives direct output"
    let unMaybe fid =
            fromMaybe
                (panicS $
                 "Invariant broken: should have found something but got Nothing! " <>
                 show fid)
    let unDFVar x =
            case x of
                DFVar b -> b
                _ ->
                    panicS $
                    "invariant broken: should be DFVar but was: " <> show x
    -- get the condition result
    let usages =
            findUsages (unAssignment "recur" (returnAssignment recurFn)) dfExprs
    let switchFn =
            unMaybe ("switch does not use recur output" :: Text) $
            find ((== Refs.select) . functionRef) usages
    -- let conditionOutput :: DFVar
    let conditionOutput =
            case switchFn of
                (LetExpr _ _ _ (c:_) _) -> c
                _ ->
                    panicS
                        ("invariant broken: recur is assumed to be the final call on a 'then' or 'else' branch." :: Text)
    -- the switch is not needed anymore because the only output comes
    -- from the termination branch
    let minusSwitchExprs = S.filter (/= switchFn) dfExprs
    let switchRet = returnAssignment switchFn
    let d = HS.difference $ HS.fromList $ callArguments switchFn
    let terminationBranchOut =
            d $
            HS.fromList
                [ conditionOutput
                , DFVar $ unAssignment "recur" $ returnAssignment recurFn
                ]
    let terminationBranch =
            case head $ HS.toList terminationBranchOut of
                Nothing ->
                    panicS "Invariant broken, empty termination branch out"
                Just outp -> outp
    let switchRetBnd = unAssignment "switch" switchRet
    let rewiredTOutExps =
            flip renameWith minusSwitchExprs $
            HM.singleton switchRetBnd (unDFVar terminationBranch)
    let newLambdaRetVar =
            unDFVar $
            flip assert terminationBranch $ returnVar dfExpr0 == switchRetBnd
    -- get the algo-in vars. note that for the recursive case this
    -- includes all free vars (even those accessed via the lexical
    -- scope).  FIXME this should not be necessary once we implemented
    -- lambda lifting on ALang!
    let lambdaInVars = HS.toList $ findFreeVars rewiredTOutExps
    lambdaInToRecurInVars <-
        foldM
            (\x y -> do
                 s <- generateBindingWith y
                 return $ HM.insert y s x)
            HM.empty
            lambdaInVars -- TODO beware to pertain the ordering here!
    -- The following expression is unused. What is its purpose?
    --let updatedRecurExpr = renameWith lambdaInToRecurInVars rewiredTOutExps
    let recurInVars = callArguments recurFn
    algoInVarsArrayId <- generateId
    algoInVarsArrayRet <- generateBindingWith "algo-in"
    recurInVarsArrayId <- generateId
    recurInVarsArrayRet <- generateBindingWith "recur-in"
    -- the args that are input to the algo-in-array function must come
    -- from the outside. hence the need to use the var that will later
    -- be bound to the actuals! (instead of the formal var)
    let algoInVarsArray =
            LetExpr
                algoInVarsArrayId
                (Direct algoInVarsArrayRet)
                Refs.array
                (map (DFVar .
                      unMaybe ("formal args" :: Text) .
                      flip HM.lookup lambdaInToRecurInVars)
                     lambdaInVars)
                Nothing
    let recurInVarsArray =
            LetExpr
                recurInVarsArrayId
                (Direct recurInVarsArrayRet)
                Refs.array
                recurInVars $
            contextArg recurFn
    -- remember: 'recur' produces the formals used inside the lambda expression!
    let updatedRecurExpr =
            LetExpr
                (callSiteId recurFn)
                (Destructure lambdaFormals)
                (functionRef recurFn)
                [ conditionOutput
                , DFVar algoInVarsArrayRet
                , DFVar recurInVarsArrayRet
                ]
                Nothing
    return $
        RecursiveLambdaSpec lambdaFormals lambdaInToRecurInVars $
        flip DFExpr newLambdaRetVar $
        algoInVarsArray <|
        (S.filter (/= recurFn) rewiredTOutExps |> recurInVarsArray |>
         updatedRecurExpr)

--    let recursionInVars = map (unMaybe "input into recur not found" . flip HM.lookup algoInToRecurInVars) $ trace ("algoInToRecurVars: " ++ show algoInToRecurInVars) algoInVars
-- | Executed whenever an initial call to a recursive algorithm is detected during the lowering.
lowerRecAlgoCall ::
       (MonadWriter (Seq LetExpr) m, MonadGenId m)
    => (QualifiedBinding -> FnId -> Assignment -> [Expression] -> m (Seq LetExpr))
    -> [Expression]
    -> RecursiveLambdaSpec
    -> m (QualifiedBinding, FnId, [Expression])
lowerRecAlgoCall lowerDefault actuals RecursiveLambdaSpec { lambdaFormalsToAlgoIn
                                                          , formalInputs
                                                          , dfExpr
                                                          }
        -- input side
 = do
    mapM_
        (\(x, y) -> do
             newId <- generateId
             tell =<< mkIdFn newId x y) $
        zip actuals $ map Direct algoInInputFormals
        -- recreate the body and 'tell' it to the writer
    tell $ letExprs dfExpr
        -- output side
    newId <- generateId
    return (ALangRefs.id, newId, [Var $ Local $ returnVar dfExpr])
  where
    mkIdFn fnId i o = lowerDefault ALangRefs.id fnId o [i]
    algoInInputFormals =
        map
            (fromMaybe (panic "Invariant broken") .
             flip HM.lookup lambdaFormalsToAlgoIn)
            formalInputs
