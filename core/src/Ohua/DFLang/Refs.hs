module Ohua.DFLang.Refs where


import qualified Ohua.ALang.Refs  as Refs
import           Ohua.DFLang.Lang
import qualified Ohua.ALang.Passes.TailRec as TailRec
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

true :: DFFnRef
true = EmbedSf Refs.true

false :: DFFnRef
false = EmbedSf Refs.false

bool :: DFFnRef
bool = EmbedSf Refs.bool

-- FIXME should be ohua.lang/if
ifThenElse :: DFFnRef
ifThenElse = bool
{-# DEPRECATED ifThenElse "use 'bool' instead" #-}


switch :: DFFnRef
switch = select
{-# DEPRECATED switch "use 'select' instead" #-}


select :: DFFnRef
select = DFFunction "ohua.lang/select"


scope :: DFFnRef
scope = DFFunction Refs.scope


seq :: DFFnRef
seq = EmbedSf "ohua.lang/seq"


recur :: DFFnRef
recur = DFFunction TailRec.recur


array :: DFFnRef
array = EmbedSf Refs.array


mkTuple :: DFFnRef
mkTuple = EmbedSf Refs.mkTuple


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

-- FIXME I don't think we needs this as of now.
ndMerge :: DFFnRef
ndMerge = DFFunction "ohua.lang/ndMerge"


toGen :: DFFnRef
toGen = DFFunction "ohua.lang/toGen"
