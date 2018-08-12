-- |
-- Module      : $Header$
-- Description : Show algorithm language terms as lambda terms
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file

-- Same here as in `DFGraph.Show`, type inference can't deal with my lists
{-# LANGUAGE NoOverloadedLists, CPP #-}

#include "compat.h"

module Ohua.ALang.Show where

import Data.Functor.Foldable
import Ohua.ALang.Lang
import Ohua.Types
import Ohua.Util.Str (toString)
import Text.PrettyPrint.Boxes

#if PRELUDE_EXPORTS_MAPPEND_OPERATOR
import Prelude hiding ((<>))
#endif

renderExpr :: Expression -> Box
renderExpr = fst . cata worker
  where
    parenthesize (e, True) = "(" <> e <> ")"
    parenthesize (e, _) = e
    needParens = (, True)
    noParens = (, False)
    worker (VarF b) = noParens $ showSym b
    worker (LetF assign val body) =
        needParens $
        vcat
            left
            ["let" <+> showAssign assign <+> "=" <+> fst val <+> "in", fst body]
    worker (ApplyF fun val) = needParens $ fst fun <+> parenthesize val
    worker (LambdaF assign body) =
        needParens $ "λ" <+> showAssign assign <+> "→" <+> fst body
    showSym (Local b) = showBnd b
    showSym (Sf b fnId) =
        "Sf[" <> showQualBnd b <> "]" <>
        maybe nullBox (\a -> "<" <> text (show a) <> ">") fnId
    showSym (Env e) = "Env[" <> text (show e) <> "]"
    showAssign (Direct b) = showBnd b
    showAssign (Destructure bs) =
        "[" <> punctuateH left ", " (map showBnd bs) <> "]"
    showAssign (Recursive _) = error "implement show for recursive bindings"
    showBnd = text . toString . unwrap
    showQualBnd = text . show
