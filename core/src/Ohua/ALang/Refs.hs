module Ohua.ALang.Refs where

import Ohua.ParseTools.Refs
import Ohua.Types

id :: QualifiedBinding
id = "ohua.lang/id"

-- transforms into `ifFun` and `select`
ifThenElse :: QualifiedBinding
ifThenElse = "ohua.lang/if"

-- TODO: maybe these functions belong into the concrete passes
-- semantically a different function from `ifThenElse`
ifFun :: QualifiedBinding
ifFun = "ohua.lang/ifFun"

select :: QualifiedBinding
select = "ohua.lang/select"

-- transforms into `smapFun` and `collect`
smap :: QualifiedBinding
smap = "ohua.lang/smap"

-- TODO: maybe these functions belong into the concrete passes
-- semantically a different function from `smap`
smapFun :: QualifiedBinding
smapFun = "ohua.lang/smapFun"

collect :: QualifiedBinding
collect = "ohua.lang/collect"

-- transforms into `seqFun`
seq :: QualifiedBinding
seq = "ohua.lang/seq"

-- TODO: maybe these functions belong into the concrete passes
-- semantically a different function from `seq`
seqFun :: QualifiedBinding
seqFun = "ohua.lang/seqFun"

recur :: QualifiedBinding
recur = "ohua.lang/recur"

nth :: QualifiedBinding
nth = "ohua.lang/nth"

ctrl :: QualifiedBinding
ctrl = "ohua.lang/ctrl"
