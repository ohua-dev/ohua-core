{-# LANGUAGE DeriveLift #-}
module Ohua.Types.Annotated where

import Universum

import Control.Lens.Plated
import Language.Haskell.TH.Syntax (Lift)
import Control.Comonad
import Data.Bitraversable
import Data.Bifoldable

import Ohua.LensClasses

-- | Generic way of attaching arbitrary, alterable data to some type.
--
-- This is used primarily to add information to AST nodes, such as with the
-- 'Ohua.ALang.Lang.AnnExpr' type, wich uses 'Annotated' internally.
--
-- It is recommended to use the lenses 'annotated' and 'value' to interact with
-- this type and other annotated values, as those are a more stable and clean
-- interface.
data Annotated annotation value =
    Annotated !annotation
              !value
    deriving (Eq, Show, Lift, Functor, Foldable, Traversable, Generic)

instance HasValue (Annotated annotation value) (Annotated annotation value') value value' where
    value f (Annotated ann val) = Annotated ann <$> f val

instance HasAnnotation (Annotated annotation value) (Annotated annotation' value) annotation annotation' where
    annotation f (Annotated ann val) = flip Annotated val <$> f ann

instance (NFData annotation, NFData value) =>
         NFData (Annotated annotation value)
instance (Hashable annotation, Hashable value) =>
         Hashable (Annotated annotation value)

instance Plated (Annotated annotation value) where plate = gplate

instance Bifunctor Annotated where
    bimap f g (Annotated ann val) = Annotated (f ann) (g val)

instance Bifoldable Annotated where
    bifoldr f g c (Annotated ann val) = f ann $ g val c

instance Bitraversable Annotated where
    bitraverse f g (Annotated ann val) = Annotated <$> f ann <*> g val

instance Comonad (Annotated ann) where
    extract (Annotated _ val) = val
    duplicate x@(Annotated ann _) = Annotated ann x
    extend f a@(Annotated ann _) = Annotated ann (f a)
