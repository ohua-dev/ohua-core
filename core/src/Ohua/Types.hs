-- |
-- Module      : $Header$
-- Description : Basic types for the Ohua compiler
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
--
-- Fundamental types used in the ohua compilation process. For many
-- types this module exposes only the type, not its concrete
-- construction. This is intentional, as internal representations may
-- change. The type classes 'Make' and 'Unwrap' are provided to
-- convert to and from those types as needed.

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

#include "compat.h"

module Ohua.Types
    ( FnId
    , Binding
    , NSRef
    , QualifiedBinding(..)
    , SomeBinding(..)
    , symbolFromString
    , HostExpr
    , FunRef(..)
    , Lit(..)
    , Environment
    , options
    , Options
    , callEnvExpr
    , callLocalFunction
    , transformRecursiveFunctions
    , stageHandling
    , StageHandling
    , StageName
    , AbortCompilation
    , DumpCode(..)
    , Error
    , OhuaState
    , nameGenerator
    , idCounter
    , envExpressions
    , NameGenerator
    , simpleNameList
    , takenNames
    , Annotated(Annotated)
    , TyExpr(TyApp, TyRef)
    , TyExprF(..)
    , TyVar(..)
    , SomeTyVar
    , DefaultTyExpr

    -- * Creating and inspecting values
    , SourceType
    , Make(make)
    , makeThrow
    , Unwrap(unwrap)
    , unwrapped

    , Embed(embedE)
    -- ** Unsafely creating values
    , UnsafeMake(unsafeMake)
    ) where

import Ohua.Types.Annotated
import Ohua.Types.Base
import Ohua.Types.Environment
import Ohua.Types.Literal
import Ohua.Types.Make
import Ohua.Types.Reference
import Ohua.Types.Stage
import Ohua.Types.TyExpr
