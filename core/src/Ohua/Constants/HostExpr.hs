-- |
-- Module      : $Header$
-- Description : Definition of an abstract expression language as the first IR for the Ohua compiler.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2018. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
--
-- This module defines a set of constant values which each backend
-- must provide.
-- 
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.Constants.HostExpr (true, unit) where


import Ohua.Types



unit :: HostExpr
unit = unsafeMake (-1)

true :: HostExpr
true = unsafeMake (-2)
