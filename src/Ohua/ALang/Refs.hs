module Ohua.ALang.Refs where

import Ohua.ParseTools.Refs
import Ohua.Types

size :: QualifiedBinding
size = "ohua.lang/size"

id :: QualifiedBinding
id = "ohua.lang/id"

ifThenElse :: QualifiedBinding
ifThenElse = "ohua.lang/if"

scope :: QualifiedBinding
scope = "ohua.lang/scope"

recur :: QualifiedBinding
recur = "ohua.lang/recur"

array :: QualifiedBinding
array = "ohua.lang/array"

smap :: QualifiedBinding
smap = "ohua.lang/smap"

true :: DFFnQualifiedBindingRef
true = "ohua.lang/true"

false :: QualifiedBinding
false = "ohua.lang/false"

bool :: QualifiedBinding
bool = "ohua.lang/bool"

seq :: QualifiedBinding
seq = "ohua.lang/seq"

mkTuple :: QualifiedBinding
mkTuple = QualifiedBinding ohuaLangNS "(,)"

smapG :: QualifiedBinding
smapG = "ohua.lang/smapG"

generate :: QualifiedBinding
generate = "ohua.lang/generate"
