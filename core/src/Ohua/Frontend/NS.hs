-- |
-- Module      : $Header$
-- Description : Ohua namespaces
-- Copyright   : (c) Justus Adam 2018. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
--
--
{-# LANGUAGE DeriveLift, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ohua.Frontend.NS
    ( FunAnn(..)
    , Imports
    , Namespace
    , emptyNamespace
    , HasName(name)
    , algoImports
    , sfImports
    , pragmas
    , HasDecls(decls)
    , Pragma(..)
    , parsePragma
    , Feature
    ) where

import Ohua.Prelude

import qualified Data.HashMap.Strict as HM
import qualified Language.Haskell.TH as TH (Exp(VarE, AppE))
import qualified Language.Haskell.TH.Syntax as TH (Lift, lift)

data FunAnn tyExpr = FunAnn
    { argTypes :: [tyExpr]
    , retType :: tyExpr
    } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

type Imports = [(NSRef, [Binding])]

type Feature = Text
data Pragma =
    Feature Feature
    deriving (Generic, Show, Eq, Ord, TH.Lift)

parsePragma :: MonadError Error m => Text -> m Pragma
parsePragma t =
    case words t of
        ["feature", f] -> pure $ Feature f
        (x:_) -> throwErrorDebugS $ "Unknown Pragma \'" <> x <> "\'"
        [] -> throwErrorDebugS $ "Pragma cannot be empty"

instance (TH.Lift k, TH.Lift v) => TH.Lift (HM.HashMap k v) where
  lift m = do
    listE <- TH.lift (HM.toList m)
    pure $ TH.VarE 'HM.fromList `TH.AppE` listE

-- | A namespace as defined by the ohua API. It has a name, a list of
-- dependencies and aliasings and some defined expressions (currently
-- constrained to lambdas/algos)
--
-- Because the layout of the namespace itself is considered unstable use lenses
-- instead to interact with namespaces. To create new namespaces first create an
-- empty one with 'emptyNamespace' and then populate it using lenses.
--
-- Note that the "Eq" instance here is mainly meant for testing purposes and
-- should otherwise not be relied upon.
data Namespace decl =
    Namespace NSRef -- name
              [Pragma]
              Imports -- algo imports
              Imports -- sf imports
              (HM.HashMap Binding decl) -- declarations
    deriving (Generic, Show, Eq, TH.Lift)

emptyNamespace :: NSRef -> Namespace decl
emptyNamespace name0 = Namespace name0 [] [] [] mempty

instance HasName (Namespace decls) NSRef where
    name f (Namespace a b c d e) = (\a' -> Namespace a' b c d e) <$> f a

pragmas :: Lens' (Namespace decls) [Pragma]
pragmas f (Namespace a b c d e) = (\b' -> Namespace a b' c d e) <$> f b

algoImports :: Lens' (Namespace decls) Imports
algoImports f (Namespace a b c d e) = (\c' -> Namespace a b c' d e) <$> f c

sfImports :: Lens' (Namespace decls) Imports
sfImports f (Namespace a b c d e) = (\d' -> Namespace a b c d' e) <$> f d

instance HasDecls (Namespace decls) (Namespace decls') (HM.HashMap Binding decls) (HM.HashMap Binding decls') where
    decls f (Namespace a b c d e) = Namespace a b c d <$> f e