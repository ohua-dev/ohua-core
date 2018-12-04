{-|
Module      : $Header$
Description : Implementation for basic tail recrusion support.
Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
License     : EPL-1.0
Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

== Design:
It is the very same rewrite as for the true branch of the `ifRewrite`.
In fact, we could turn the `seq` into an if that is always true. We just would need a
new function `trueC :: a -> Bool` that always returns `True`.
Instead, we do a more performant version and introduce a `seqFun` operator that does this
and also creates the control signal (saving the `if` node).

Note that `seq` is about sequencing side-effects to I/O.
-}
module Ohua.ALang.Passes.Seq where

import Ohua.Prelude

import Ohua.ALang.Lang
import Ohua.ALang.Passes.Control
import qualified Ohua.ALang.Refs as Refs (seq, seqFun)
import Ohua.ALang.Util (lambdaArgsAndBody)

seqFunSf :: Expression
seqFunSf = Lit $ FunRefLit $ FunRef Refs.seqFun Nothing

seqRewrite :: (Monad m, MonadGenBnd m) => Expression -> m Expression
seqRewrite (Let v a b) = Let v <$> seqRewrite a <*> seqRewrite b
seqRewrite (Lambda v e) = Lambda v <$> seqRewrite e
seqRewrite (Apply (Apply (Lit (FunRefLit (FunRef "ohua.lang/seq" Nothing))) dep) expr) = do
    ctrl <- generateBindingWith "ctrl"
    expr' <- liftIntoCtrlCtxt ctrl expr
    -- TODO verify that this arg is actually ()
    let ((_:[]), expr'') = lambdaArgsAndBody expr'
    -- return $
    --     [ohualang|
    --       let $var:ctrl = ohua.lang/seqFun $var:dep in
    --         let result = $expr:expr' in
    --             result
    --                |]
    result <- generateBindingWith "result"
    return $ Let ctrl (Apply seqFunSf dep) $ Let result expr'' $ Var result
seqRewrite e = return e
