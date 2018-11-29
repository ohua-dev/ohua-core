module Ohua.DFLang.Refs where

import qualified Ohua.ALang.Passes.TailRec as TailRec
import qualified Ohua.ALang.Refs as Refs
import Ohua.DFLang.Lang
import Ohua.Types

import Data.HashMap.Lazy as HM
import Prelude hiding (id)

lowerBuiltinFunctions :: QualifiedBinding -> Maybe DFFnRef
lowerBuiltinFunctions = flip HM.lookup builtInFunctions
  where
    builtInFunctions =
        HM.fromList
            [ (Refs.collect, collect)
            , (Refs.smapFun, smapFun)
            , (Refs.ifFun, ifFun)
            , (Refs.select, select)
            , (Refs.ctrl, ctrl)
            , (TailRec.recurFun, recurFun)
            , (Refs.seqFun, seqFun)
            ]

collect :: DFFnRef
collect = DFFunction Refs.collect

smapFun :: DFFnRef
smapFun = DFFunction Refs.smapFun

ifFun :: DFFnRef
ifFun = DFFunction Refs.ifFun

select :: DFFnRef
select = DFFunction Refs.select

ctrl :: DFFnRef
ctrl = DFFunction Refs.ctrl

recurFun :: DFFnRef
recurFun = DFFunction TailRec.recurFun

seqFun :: DFFnRef
seqFun = EmbedSf Refs.seqFun
