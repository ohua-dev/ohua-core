module Ohua.ParseTools.Refs where

import Ohua.Prelude

import Ohua.ALang.Lang


ohuaLangNS :: NSRef
ohuaLangNS = makeThrow ["ohua", "lang"]


mkQualVar :: NSRef -> Binding -> Expr
mkQualVar ns name0 = Sf (QualifiedBinding ns name0) Nothing


mkOhuaLangRef :: Binding -> Expr
mkOhuaLangRef = mkQualVar ohuaLangNS


ifBuiltin :: Expr
ifBuiltin = mkOhuaLangRef "if"


smapBuiltin :: Expr
smapBuiltin = mkOhuaLangRef "smap"


funcTyConRef :: QualifiedBinding
funcTyConRef = QualifiedBinding ohuaLangNS "->"


funcTyConSBind :: SomeBinding
funcTyConSBind = Qual funcTyConRef


funcTyCon :: SomeTyVar
funcTyCon = TyCon funcTyConSBind


mkFunc :: DefaultTyExpr -> DefaultTyExpr -> DefaultTyExpr
mkFunc a b = TyRef funcTyCon `TyApp` a `TyApp` b
