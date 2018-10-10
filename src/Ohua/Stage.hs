module Ohua.Stage (stage) where

import Ohua.Prelude

stage :: (MonadReadEnvironment m, MonadIO m, Show code, Pretty code) => StageName -> code -> m ()
stage stName code = do
  stageH <- fromEnv stageHandling
  let (dumpInstructions, shouldAbort) = stageH stName


  when shouldAbort exitSuccess
