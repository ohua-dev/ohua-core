-- |
-- Module      : $Header$
-- Description : Definition of an abstract expression language as the first IR for the Ohua compiler.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.DFLang.Lang where

--
-- This module defines the dataflow IR.
-- It introduces the notion of a Flow type and defines a new stateful function execution that
-- works based on flows instead of variables.
-- The ALang IR is transformed straight into the dataflow IR.
-- One important aspect of DFLang: it does not define any abstractions, i.e., there are no function definitions.
--

