{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Ohua.ALang.NS
  ( FunAnn(..)
  , Imports
  , Namespace(Namespace)
  , HasName(name), algoImports, sfImports, HasDecls(decls)
  ) where


import qualified Data.HashMap.Strict as HM
import           Lens.Micro
import           Ohua.ALang.Lang
import           Ohua.LensClasses
import           Ohua.Types

data FunAnn tyExpr = FunAnn
  { argTypes :: [tyExpr]
  , retType  :: tyExpr
  } deriving (Show, Eq, Functor, Foldable, Traversable)

type Imports = [(NSRef, [Binding])]

-- | A namespace as defined by the ohua API. It has a name, a list of dependencies and aliasings,
-- defined expressions (currently constrained to lambdas/algos) and optionally ends with an executable expression.
data Namespace decl =
  Namespace
    NSRef -- name
    Imports -- algo imports
    Imports -- sf imports
    (HM.HashMap Binding decl) -- declarations

instance HasName (Namespace decls) NSRef where
  name f (Namespace a b c d) = (\a' -> Namespace a' b c d) <$> f a

algoImports :: Lens' (Namespace decls) Imports
algoImports f (Namespace a b c d) = (\b' -> Namespace a b' c d) <$> f b

sfImports :: Lens' (Namespace decls) Imports
sfImports f (Namespace a b c d) = (\c' -> Namespace a b c' d) <$> f c

instance HasDecls (Namespace decls) (Namespace decls') (HM.HashMap Binding decls) (HM.HashMap Binding decls') where
  decls f (Namespace a b c d) = Namespace a b c <$> f d
