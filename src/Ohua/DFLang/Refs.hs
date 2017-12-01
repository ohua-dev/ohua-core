module Ohua.DFLang.Refs where


import           Ohua.DFLang.Lang
import           Ohua.Types
import           Prelude          hiding (id)


oneToN :: DFFnRef
oneToN = DFFunction "ohua.lang/oneToN"


size :: DFFnRef
size = EmbedSf "ohua.lang/size"


collect :: DFFnRef
collect = DFFunction "ohua.lang/collect"


smapFun :: DFFnRef
smapFun = DFFunction "ohua.lang/smapFun"


id :: DFFnRef
id = EmbedSf "ohua.lang/id"


ifThenElse :: DFFnRef
ifThenElse = EmbedSf "ohua.lang/bool"


switch ::DFFnRef
switch = DFFunction "ohua.lang/select"


scope :: DFFnRef
scope = DFFunction "ohua.lang/scope"


seq :: DFFnRef
seq = EmbedSf "ohua.lang/seq"
