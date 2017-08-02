-- |
-- Module      : $Header$
-- Description : Definition of an abstract expression language as the first IR for the Ohua compiler.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.DAGLang.Lang where

--
-- This module defines the dag lang which is inspired by the dag calculus:
-- "Umut A. Acar, Arthur Charguéraud, Mike Rainey, and Filip Sieczkowski. 2016. Dag-calculus: a calculus for parallel computation. In Proceedings of the 21st ACM SIGPLAN International Conference on Functional Programming (ICFP 2016)."
-- It contains concepts for spawning and joining tasks.
-- As such it represents the backend IR which contains parallel abstractions.
-- It allows to implement schedulers and realizes Ohua's section concept.
-- We transform the DFLang straight into the DAGLang.
--
