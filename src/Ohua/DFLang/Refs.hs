module Ohua.DFLang.Refs where


import           Ohua.DFLang.Lang
import           Ohua.Types
import           Prelude          hiding (id)


oneToN :: DFFnRef
oneToN = DFFunction "ohua.lang/one-to-n"


size :: DFFnRef
size = EmbedSf "ohua.lang/size"


collect :: DFFnRef
collect = DFFunction "ohua.lang/collect"


smapFun :: DFFnRef
smapFun = DFFunction "ohua.lang/smap-fun"


id :: DFFnRef
id = EmbedSf "ohua.lang/id"


ifThenElse :: DFFnRef
ifThenElse = EmbedSf "ohua.lang/ifThenElse"


switch ::DFFnRef
switch = DFFunction "ohua.lang/switch"


scope :: DFFnRef
scope = DFFunction "ohua.lang/scope"
