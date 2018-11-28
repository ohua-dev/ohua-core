module Ohua.Stage where

import Ohua.Prelude

import qualified Data.Text.Lazy.IO as LT
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Ohua.ALang.PPrint

stage ::
       (MonadReadEnvironment m, MonadIO m, Pretty code)
    => StageName
    -> code
    -> m ()
stage stName code = do
    stageH <- fromEnv $ options . stageHandling
    let (dumpInstructions, shouldAbort) = stageH stName
    case dumpInstructions of
        DumpPretty ->
            liftIO $
            LT.writeFile (toString $ stName <> ".dump") $
            renderLazy $ layoutSmart ohuaDefaultLayoutOpts $ pretty code
        Don'tDump -> pure ()
    when shouldAbort exitSuccess

resolvedAlang :: StageName
resolvedAlang = "alang-resolved"

ssaAlang :: StageName
ssaAlang = "alang-ssa"

normalizedAlang :: StageName
normalizedAlang = "alang-normalized"

customAlangPasses :: StageName
customAlangPasses = "alang-custom"

coreAlang :: StageName
coreAlang = "alang-core"

initialDflang :: StageName
initialDflang = "dflang-initial"

customDflang :: StageName
customDflang = "dflang-custom"

coreDflang :: StageName
coreDflang = "dflang-core"

knownStages :: [StageName]
knownStages =
    [ resolvedAlang
    , ssaAlang
    , normalizedAlang
    , customAlangPasses
    , coreAlang
    , initialDflang
    , customDflang
    , coreDflang
    ]
