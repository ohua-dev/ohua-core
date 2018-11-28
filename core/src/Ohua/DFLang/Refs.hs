module Ohua.DFLang.Refs where

import qualified Ohua.ALang.Passes.TailRec as TailRec
import qualified Ohua.ALang.Refs as Refs
import Ohua.DFLang.Lang
import Prelude hiding (id)

collect :: DFFnRef
collect = DFFunction Refs.collect

smapFun :: DFFnRef
smapFun = DFFunction Refs.smapFun

id :: DFFnRef
id = EmbedSf Refs.id

ifFun :: DFFnRef
ifFun = DFFunction Refs.ifFun

select :: DFFnRef
select = DFFunction Refs.select

ctrl :: DFFnRef
ctrl = EmbedSf Refs.ctrl

recurFun :: DFFnRef
recurFun = DFFunction TailRec.recurFun
