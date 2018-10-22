module Ohua.Test where

import Ohua.Prelude hiding (lift)

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift, Exp, Q)
import qualified Data.ByteString.Lazy.Char8 as LB

import Ohua.ALang.Lang
import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Parser as ParseDFLang
import qualified Ohua.Compat.ML.Parser as ParseALang


embedALang :: QuasiQuoter
embedALang =
    QuasiQuoter
        { quoteExp = (lift :: Expression -> Q Exp) . fmap remapRefs . ParseALang.parseExp . LB.pack
        , quotePat = notDefined
        , quoteType = notDefined
        , quoteDec = notDefined
        }
  where
    notDefined = const $ fail "ALang can only be embedded as an expression"
    remapRefs (Qual q) = Sf q Nothing
    remapRefs (Unqual b) = Local b

embedDFLang :: QuasiQuoter
embedDFLang =
    QuasiQuoter
        { quoteExp = lift . ParseDFLang.parseExp . LB.pack
        , quotePat = notDefined
        , quoteType = notDefined
        , quoteDec = notDefined
        }
  where
    notDefined = const $ fail "DFLang can only be embedded as an expression"
