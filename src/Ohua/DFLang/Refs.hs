module Ohua.DFLang.Refs where


import           Ohua.DFLang.Lang
import qualified Ohua.ALang.Refs  as Refs
import           Ohua.Types
import           Prelude          hiding (id)


oneToN :: DFFnRef
oneToN = DFFunction "com.ohua.lang/one-to-n"


size :: DFFnRef
size = EmbedSf Refs.size


collect :: DFFnRef
collect = DFFunction "com.ohua.lang/collect"


-- FIXME should be ohua.lang/smap
smapFun :: DFFnRef
smapFun = DFFunction "com.ohua.lang/smap-fun"


id :: DFFnRef
id = EmbedSf Refs.id

-- FIXME should be ohua.lang/if
ifThenElse :: DFFnRef
ifThenElse = EmbedSf "com.ohua.lang/ifThenElse"


switch ::DFFnRef
switch = DFFunction "com.ohua.lang/switch"


scope :: DFFnRef
scope = DFFunction Refs.scope


recur :: DFFnRef
recur = DFFunction Refs.recur


array :: DFFnRef
array = EmbedSf Refs.array
