module Ohua.DFLang.Refs where


import           Ohua.DFLang.Lang
import           Ohua.Types
import           Prelude          hiding (id)


oneToN :: DFFnRef
oneToN = DFFunction "com.ohua.lang/one-to-n"


size :: DFFnRef
size = EmbedSf "com.ohua.lang/size"


collect :: DFFnRef
collect = DFFunction "com.ohua.lang/collect"


smapFun :: DFFnRef
smapFun = DFFunction "com.ohua.lang/smap-fun"


id :: DFFnRef
id = EmbedSf "com.ohua.lang/id"


ifThenElse :: DFFnRef
ifThenElse = EmbedSf "com.ohua.lang/ifThenElse"


switch ::DFFnRef
switch = DFFunction "com.ohua.lang/switch"


scope :: DFFnRef
scope = DFFunction "com.ohua.lang/scope"
