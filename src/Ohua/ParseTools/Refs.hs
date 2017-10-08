{-# LANGUAGE OverloadedLists #-}
module Ohua.ParseTools.Refs where


import qualified Data.Vector     as V
import           Ohua.ALang.Lang
import           Ohua.Types


ohuaLangNS :: NSRef
ohuaLangNS = nsRefFromList ["ohua", "lang"]


mkQualVar :: NSRef -> Binding -> Expr SomeBinding
mkQualVar ns name = Var $ Qual $ QualifiedBinding ns name


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
