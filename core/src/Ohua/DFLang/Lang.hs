-- |
-- Module      : $Header$
-- Description : Definition of an abstract expression language as the first IR for the Ohua compiler.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
--
-- This module defines the dataflow IR. It introduces the notion of a Flow type
-- and defines a new stateful function execution that works based on flows
-- instead of variables. The ALang IR is transformed straight into the dataflow
-- IR. One important aspect of DFLang: it does not define any abstractions,
-- i.e., there are no function definitions.
--
{-# LANGUAGE CPP, DeriveLift #-}
#include "compat.h"
module Ohua.DFLang.Lang where

import Ohua.ALang.Lang (Lit(..))
import Ohua.Prelude

import Language.Haskell.TH.Syntax (Lift)

-- | A sequence of let statements with a terminating binding to be used as return value
data DFExpr = DFExpr
    { letExprs :: Seq LetExpr
    , returnVar :: !Binding
    } deriving (Eq, Show, Lift)

data LetExpr = LetExpr
    { callSiteId :: !FnId
    , output :: ![Binding]
    , functionRef :: !DFFnRef
    , callArguments :: ![DFVar]
    , contextArg :: !(Maybe Binding)
    } deriving (Eq, Show, Lift)

data DFFnRef
    = DFFunction !QualifiedBinding -- a built-in function of DFLang
    | EmbedSf !QualifiedBinding -- an generic dataflow function that wraps a stateful function call
    deriving (Eq, Show, Lift)

instance Hashable DFFnRef where
    hashWithSalt s =
        hashWithSalt s . \case
            DFFunction f -> (0 :: Int, f)
            EmbedSf f -> (1, f)

data DFVar
    = DFEnvVar !Lit
    | DFVar !Binding
    | DFVarList ![Binding]
    deriving (Eq, Show, Lift)

instance Hashable DFVar where
    hashWithSalt s (DFVar v) = hashWithSalt s (0 :: Int, v)
    hashWithSalt s (DFEnvVar e) = hashWithSalt s (1 :: Int, e)

instance IsString DFVar where
    fromString = DFVar . fromString

instance NFData DFExpr where
    rnf (DFExpr a b) = a `deepseq` rnf b

instance NFData LetExpr where
    rnf (LetExpr a b c d e) =
        a `deepseq` b `deepseq` c `deepseq` d `deepseq` rnf e

instance NFData DFFnRef where
    rnf (DFFunction bnd) = rnf bnd
    rnf (EmbedSf bnd) = rnf bnd

instance NFData DFVar where
    rnf (DFEnvVar e) = rnf e
    rnf (DFVar v) = rnf v

instance ExtractBindings DFVar where
    extractBindings (DFEnvVar _) = []
    extractBindings (DFVar b) = [b]
    extractBindings (DFVarList bs) = bs

dfEnvExpr :: HostExpr -> DFVar
dfEnvExpr = DFEnvVar . EnvRefLit
