module Ohua.ParseTools.Refs where

import Ohua.Prelude

import Ohua.ALang.Lang


ohuaLangNS :: NSRef
ohuaLangNS = makeThrow ["ohua", "lang"]


mkQualVar :: NSRef -> Binding -> Expr SomeBinding
mkQualVar ns name0 = Var $ Qual $ QualifiedBinding ns name0


mkOhuaLangRef :: Binding -> Expr SomeBinding
mkOhuaLangRef = mkQualVar ohuaLangNS


ifBuiltin :: Expr SomeBinding
ifBuiltin = mkOhuaLangRef "if"


smapBuiltin :: Expr SomeBinding
smapBuiltin = mkOhuaLangRef "smap"


funcTyConRef :: QualifiedBinding
funcTyConRef = QualifiedBinding ohuaLangNS "->"


funcTyConSBind :: SomeBinding
funcTyConSBind = Qual funcTyConRef


funcTyCon :: SomeTyVar
funcTyCon = TyCon funcTyConSBind


mkFunc :: DefaultTyExpr -> DefaultTyExpr -> DefaultTyExpr
mkFunc a b = TyRef funcTyCon `TyApp` a `TyApp` b
