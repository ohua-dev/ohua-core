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

smapGFun :: DFFnRef
smapGFun = DFFunction "ohua.lang/smapGFun"

collectG :: DFFnRef
collectG = DFFunction "ohua.lang/collectG"

-- TODO Why do we need this?
repeat :: DFFnRef
repeat = DFFunction "ohua.lang/repeat"

-- TODO Why do we need this?
isJust :: DFFnRef
isJust = EmbedSf "ohua.lang/isJust"

-- FIXME I don't think we need this as of now.
ndMerge :: DFFnRef
ndMerge = DFFunction "ohua.lang/ndMerge"

toGen :: DFFnRef
toGen = DFFunction "ohua.lang/toGen"
