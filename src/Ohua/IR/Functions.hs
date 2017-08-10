{-# LANGUAGE OverloadedStrings #-}
module Ohua.IR.Functions where

import           Ohua.Types

packagerName, oneToNName, collectName, idName, pmapName, pCollectName, smapName, smapIOName, chunkName, flattenerName, sizeName, selectName, algoOutName, seqName :: FnName
packagerName = "com.ohua.lang/__package-args"
oneToNName = "com.ohua.lang/one-to-n"
collectName = "com.ohua.lang/collect"
idName = "com.ohua.lang/id"
pmapName = "com.ohua.lang/pmap"
pCollectName = "com.ohua.lang/pcollect"
smapName = "com.ohua.lang/smap-fun"
smapIOName = "com.ohua.lang/smap-io-fun"
chunkName = "com.ohua.lang/__chunk"
flattenerName = "com.ohua.lang/__flatten"
sizeName = "com.ohua.lang/size"
selectName = "com.ohua.lang/select"
algoOutName = "com.ohua.lang/algo-out"
seqName = "com.ohua.lang/seq"
