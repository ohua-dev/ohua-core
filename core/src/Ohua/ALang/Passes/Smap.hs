{-|
Module      : $Header$
Description : Implementation of ALang transformation for If.
Copyright   : (c) Sebastian Ertel, Justus Adam 2018. All Rights Reserved.
License     : EPL-1.0
Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

== Design:
As described in Ohua.ALang.Passes.Control, the resulting expression will look as follows:

@
let dataGen = ... in
 let (data :: Control a, ctrlVar :: Control (Maybe Int), size :: Int) = smapFun dataGen in
  let (a,b,c) = ctrl ctrlVar a b c in
   let result = body in -- lifted into control context
    let resultData = collect size result in
      resultData
@

Note that `smapFun` must be an operator because the first two outputs reside in the control
context while the last one doesn't.
-}
module Ohua.ALang.Passes.Smap where

import Ohua.ALang.Lang
import Ohua.ALang.Passes.Control
import qualified Ohua.ALang.Refs as Refs
import Ohua.ALang.Util (mkDestructured)
import Ohua.Prelude

smapSf :: Expression
smapSf = Lit $ FunRefLit $ FunRef Refs.smapFun Nothing

collectSf :: Expression
collectSf = Lit $ FunRefLit $ FunRef Refs.collect Nothing

smapRewrite :: (Monad m, MonadGenBnd m) => Expression -> m Expression
smapRewrite (Let v a b) = Let v <$> smapRewrite a <*> smapRewrite b
smapRewrite (Lambda v e) = Lambda v <$> smapRewrite e
smapRewrite (Apply (Apply smapSf body) dataGen) = do
    ctrlVar <- generateBindingWith "ctrl"
    body' <- liftIntoCtrlCtxt ctrlVar body
  --   [ohualang|
  --     let (d, $var:ctrlVar, size) = ohua.lang/smapFun $var:dataGen in
  --      let (a,b,c) = ctrl $var:ctrlVar a b c in
  --       let result = $expr:body' in -- lifted into control context
  --        let resultList = collect size result in
  --          resultList
  -- this breaks haddock |]
    d <- generateBindingWith "d"
    size <- generateBindingWith "size"
    ctrls <- generateBindingWith "ctrls"
    result <- generateBindingWith "result"
    resultList <- generateBindingWith "resultList"
    return $
        Let ctrls (Apply smapSf dataGen) $
        mkDestructured [d, ctrlVar, size] ctrls $
        Let result body' $
        Let resultList (Apply (Apply collectSf $ Var size) $ Var result) $
        Var resultList
