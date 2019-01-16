module Ohua.Test (embedALang, embedDFLang, showWithPretty) where

import Ohua.Prelude hiding (lift)

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift)
import qualified Data.ByteString.Lazy.Char8 as LB

import Ohua.ALang.PPrint (Pretty, pretty)
import Ohua.Frontend.Lang
import qualified Ohua.DFLang.Parser as ParseDFLang
import qualified Ohua.Compat.ML.Parser as ParseALang

import qualified Prelude (show)

embedALang :: QuasiQuoter
embedALang =
    QuasiQuoter
        { quoteExp =
              \e -> do
                  let olang = ParseALang.parseExp $ LB.pack e
                  let names = definedBindings olang
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


newtype PrettyAsShow a = PrettyAsShow { unPrettyAsShow :: a } deriving (Eq)

instance Pretty a => Show (PrettyAsShow a) where
    show = show . pretty . unPrettyAsShow

-- | Helper function for working with `hspec` tests.
--
-- This wraps the data (@a@) into a 'PrettyAsShow' newtype which implements 'Eq'
-- simply as the 'Eq' defined on the @a@ but overrides 'Show' by calling
-- 'pretty'. This is useful in tests for ALang of DFLang etc where the default
-- 'Show' implementation is often hard to read.
showWithPretty :: a -> PrettyAsShow a
showWithPretty = PrettyAsShow
