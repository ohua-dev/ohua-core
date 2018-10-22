{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Ohua.Types.Arbitrary where

import Ohua.Prelude

import qualified Data.Text as T
import Test.QuickCheck

import Ohua.ALang.Lang
import Ohua.DFGraph


genFromMake :: (HasCallStack, Make t, Arbitrary (SourceType t)) => Gen t
genFromMake =
    fmap (fromRight $ error "impossible") $
    (make <$> arbitrary) `suchThat` isRight

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary
instance Arbitrary Operator where
    arbitrary = Operator <$> arbitrary <*> arbitrary
instance Arbitrary Target where
    arbitrary = liftM2 Target arbitrary arbitrary
instance Arbitrary a => Arbitrary (Arc a) where
    arbitrary = liftM2 Arc arbitrary arbitrary
instance Arbitrary a => Arbitrary (Source a) where
    arbitrary = oneof [LocalSource <$> arbitrary, EnvSource <$> arbitrary]
instance Arbitrary a => Arbitrary (AbstractOutGraph a) where
    arbitrary = liftM3 OutGraph arbitrary arbitrary arbitrary


instance Arbitrary Binding where arbitrary = genFromMake
instance Arbitrary QualifiedBinding where arbitrary = QualifiedBinding <$> arbitrary <*> arbitrary
instance Arbitrary NSRef where arbitrary = genFromMake
instance Arbitrary FnId where arbitrary = genFromMake
instance Arbitrary HostExpr where arbitrary = genFromMake
instance Arbitrary a => Arbitrary (Symbol a) where
    arbitrary = oneof
        [ Local <$> arbitrary
        , Sf <$> arbitrary <*> arbitrary
        , Env <$> arbitrary
        ]
instance Arbitrary Assignment where
    arbitrary = oneof [Direct <$> arbitrary, Destructure <$> arbitrary]
instance Arbitrary a => Arbitrary (Expr a) where
    arbitrary = sized expr
      where
        expr :: Int -> Gen (Expr a)
        expr 0 = Var <$> arbitrary
        expr n = oneof
            [ liftM3 Let arbitrary nestExpr nestExpr
            , liftM2 Apply nestExpr nestExpr
            , liftM2 Lambda arbitrary nestExpr
            , Var <$> arbitrary
            ]
          where
            nestExpr = expr $ n `div` 2
    shrink (Let _ _ b)  = b: shrink b
    shrink (Apply _ a)  = a:shrink a
    shrink (Lambda _ a) = a:shrink a
    shrink _            = []