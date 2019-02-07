module Ohua.Feature.TailRec where


import Ohua.Prelude

import Ohua.Compile.Configuration
import Ohua.DFLang.Passes (collapseNth)

import Ohua.Feature.TailRec.Passes.ALang
import Ohua.Feature.TailRec.Passes.DFLang


loadTailRecPasses :: Bool -> CustomPasses env -> CustomPasses env
loadTailRecPasses enabled passes =
    passes
        { passBeforeNormalize =
              findTailRecs enabled >=> passBeforeNormalize passes
        , passAfterNormalize =
              (if enabled
                   then rewriteAll
                   else pure) >=>
              passAfterNormalize passes
        , passAfterDFLowering =
              passAfterDFLowering passes .
              if enabled
                  then recurLowering . collapseNth (== recurStartMarker)
                  else id
        }
