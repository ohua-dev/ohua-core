module Ohua.Types.Arbitrary where


import           Control.Monad
import           Ohua.ALang.Lang
import           Ohua.IR.Functions
import           Ohua.Types
import           Test.QuickCheck


instance Arbitrary Binding where
    arbitrary = oneof $ map pure
        [ "x", "y", "z", "a", "b", "c"
        ]

instance Arbitrary FnName where
    arbitrary = oneof $ map pure [pmapName, pCollectName, smapIOName, flattenerName, seqName, idName]

instance Arbitrary FnId where
    arbitrary = FnId <$> arbitrary

instance Arbitrary HostExpr where
    arbitrary = HostExpr <$> arbitrary

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
