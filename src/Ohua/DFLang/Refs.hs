module Ohua.DFLang.Refs where


import qualified Ohua.ALang.Refs  as Refs
import           Ohua.DFLang.Lang
import           Prelude          hiding (id)


oneToN :: DFFnRef
oneToN = DFFunction "ohua.lang/oneToN"


size :: DFFnRef
size = EmbedSf Refs.size


collect :: DFFnRef
collect = DFFunction "ohua.lang/collect"


-- FIXME should be ohua.lang/smap
smapFun :: DFFnRef
smapFun = DFFunction "ohua.lang/smapFun"


id :: DFFnRef
id = EmbedSf Refs.id


bool :: DFFnRef
bool = EmbedSf Refs.bool

-- FIXME should be ohua.lang/if
ifThenElse :: DFFnRef
ifThenElse = bool
{-# DEPRECATED ifThenElse "use 'bool' instead" #-}


switch ::DFFnRef
switch = DFFunction "ohua.lang/select"


scope :: DFFnRef
scope = DFFunction Refs.scope


seq :: DFFnRef
seq = EmbedSf "ohua.lang/seq"


recur :: DFFnRef
recur = DFFunction Refs.recur


array :: DFFnRef
array = EmbedSf Refs.array
