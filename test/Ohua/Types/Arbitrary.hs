module Ohua.Types.Arbitrary where


import           Control.Monad
import           Ohua.ALang.Lang
import           Ohua.IR.Functions
import           Ohua.Types
import           Test.QuickCheck
import Ohua.DFGraph
import qualified Data.Text as T

instance Arbitrary T.Text where arbitrary = T.pack <$> arbitrary
instance Arbitrary Operator where
    arbitrary = Operator <$> arbitrary <*> arbitrary
instance Arbitrary Target where arbitrary = liftM2 Target arbitrary arbitrary
instance Arbitrary Arc where arbitrary = liftM2 Arc arbitrary arbitrary
instance Arbitrary Source where arbitrary = oneof [LocalSource <$> arbitrary, EnvSource <$> arbitrary]
instance Arbitrary OutGraph where arbitrary = liftM2 OutGraph arbitrary arbitrary


instance Arbitrary Binding where arbitrary = Binding <$> arbitrary
instance Arbitrary FnName where arbitrary = FnName <$> arbitrary
instance Arbitrary FnId where arbitrary = FnId <$> arbitrary
instance Arbitrary HostExpr where arbitrary = HostExpr <$> arbitrary
instance Arbitrary ResolvedSymbol where
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
        expr 0 = Var <$> arbitrary
        expr n = oneof
            [ liftM3 Let arbitrary nestExpr nestExpr
            , liftM2 Apply nestExpr nestExpr
            , liftM2 Lambda arbitrary nestExpr
            , Var <$> arbitrary
            ]
          where
            nestExpr = expr $ n `div` 2
    shrink (Let _ _ b) = b: shrink b
    shrink (Apply _ a) = a:shrink a
    shrink (Lambda _ a) = a:shrink a
    shrink _ = []
    