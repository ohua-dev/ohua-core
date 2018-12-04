module Ohua.Types.Stage where

import Universum

type StageName = Text
type AbortCompilation = Bool
data DumpCode
    = Don'tDump
    | DumpPretty
type StageHandling = StageName -> (DumpCode, AbortCompilation)
