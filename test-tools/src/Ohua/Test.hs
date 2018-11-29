module Ohua.Test where

import Ohua.Prelude hiding (lift)

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift, Exp, Q)
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Lens.Plated (universeOn, cosmos)
import qualified Data.HashSet as HS

import Ohua.ALang.Lang
import Ohua.Frontend.Lang
import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Parser as ParseDFLang
import qualified Ohua.Compat.ML.Parser as ParseALang


embedALang :: QuasiQuoter
embedALang =
    QuasiQuoter
        { quoteExp =
              \e -> do
                  let olang = ParseALang.parseExp $ LB.pack e
                  let names =
                          HS.fromList $
                          [v | VarE v <- universe olang] <>
                          [v | VarP v <- universeOn (cosmos . patterns) olang]
                  alang <-
                      fmap (either error id) $
                      runExceptT $ runGenBndT names $ toAlang olang
                  lift alang
        , quotePat = notDefined
        , quoteType = notDefined
        , quoteDec = notDefined
        }
  where
    notDefined = const $ fail "ALang can only be embedded as an expression"

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
