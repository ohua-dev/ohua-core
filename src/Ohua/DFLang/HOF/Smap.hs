{-# LANGUAGE NamedFieldPuns, OverloadedLists      #-}
module Ohua.DFLang.HOF.Smap where


import           Ohua.DFLang.HOF
import Ohua.Types
import Ohua.DFLang.Lang
import Ohua.Monad
import Control.Monad.Except
import qualified Data.Sequence                   as S
import Control.Monad.State


data SmapFn = SmapFn
    { collSource :: !DFVar
    , smapLambda :: !Lambda
    , sizeSource :: Binding
    }


instance HigherOrderFunction SmapFn where
    name = "com.ohua.lang/smap"

    parseCallAndInitState [LamArg lam, Variable v] = return $ SmapFn v lam (error "size uninitialized")
    parseCallAndInitState _ = throwError "Unexpected number/type of arguments to smap"

    createContextEntry = do
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

    createContextExit assignment = do
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

    contextifyUnboundFunctions _ = return True
