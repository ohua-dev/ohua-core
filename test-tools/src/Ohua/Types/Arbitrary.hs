{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Ohua.Types.Arbitrary where

import Ohua.Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Test.QuickCheck

import Ohua.ALang.Lang
import Ohua.DFGraph
import Ohua.DFLang.Lang
import Ohua.Frontend.NS

genFromMake :: (HasCallStack, Make t, Arbitrary (SourceType t)) => Gen t
genFromMake =
    fmap (fromRight $ error "impossible") $
    (make <$> arbitrary) `suchThat` isRight

reservedWords :: HS.HashSet Binding
reservedWords =
    HS.fromList
        [ "dataflow"
        , "let"
        , "if"
        , "then"
        , "else"
        , "in"
        , "module"
        , "import"
        , "sf"
        , "algo"
        ]

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary NodeType where
    arbitrary = oneof [pure OperatorNode, pure FunctionNode]

instance Arbitrary Operator where
    arbitrary = Operator <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Target where
    arbitrary = liftM2 Target arbitrary arbitrary

instance Arbitrary a => Arbitrary (Arcs a) where
    arbitrary = liftM3 Arcs arbitrary arbitrary arbitrary

instance ( Arbitrary a, Arbitrary b ) => Arbitrary (Arc a b) where
    arbitrary = Arc <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Source a) where
    arbitrary = oneof [LocalSource <$> arbitrary, EnvSource <$> arbitrary]

instance Arbitrary a => Arbitrary (AbstractOutGraph a) where
    arbitrary = liftM3 OutGraph arbitrary arbitrary arbitrary

isValidForBinding = not . flip HS.member reservedWords

instance Arbitrary Binding where
    arbitrary = do
        a <- oneof $ map pure idstartchars
        as <- listOf $ oneof $ map pure idchars
        pure (unsafeMake $ toText $ a : as) `suchThat` isValidForBinding
      where
        idstartchars = ['a' .. 'z'] <> ['A' .. 'Z']
        idchars = idstartchars <> ['0' .. '9'] <> ['_']
    shrink (unwrap -> b) =
        [ b'
        | b' <- unsafeMake . toText <$> shrink (toString b)
        , isValidForBinding b'
        ]

instance Arbitrary QualifiedBinding where
    arbitrary = QualifiedBinding <$> arbitrary <*> arbitrary

instance Arbitrary SomeBinding where
    arbitrary = oneof [Qual <$> arbitrary, Unqual <$> arbitrary]

instance Arbitrary NSRef where
    arbitrary = genFromMake `suchThat` (not . null . unwrap)

instance Arbitrary FnId where
    arbitrary = genFromMake

instance Arbitrary HostExpr where
    arbitrary = genFromMake

instance Arbitrary FunRef where
    arbitrary = liftM2 FunRef arbitrary arbitrary

instance Arbitrary Lit where
    arbitrary =
        oneof
            [ NumericLit <$> arbitrary
            , FunRefLit <$> arbitrary
            , EnvRefLit <$> arbitrary
            , pure UnitLit
            ]

instance Arbitrary Expr where
    arbitrary = sized expr
      where
        expr :: Int -> Gen Expr
        expr 0 = Var <$> arbitrary
        expr n =
            oneof
                [ liftM3 Let arbitrary nestExpr nestExpr
                , liftM2 Apply nestExpr nestExpr
                , liftM2 Lambda arbitrary nestExpr
                , Lit <$> arbitrary
                , Var <$> arbitrary
                ]
          where
            nestExpr = expr $ n `div` 2
    shrink (Let a b c) = b : c : [Let a b' c' | (b', c') <- shrink (b, c)]
    shrink (Apply a b) = a : b : [Apply a' b' | (a', b') <- shrink (a, b)]
    shrink (Lambda a b) = b : map (Lambda a) (shrink b)
    shrink _ = []

instance Arbitrary a => Arbitrary (Namespace a) where
    arbitrary = do
        n <- arbitrary
        algoImports0 <- arbitrary
        sfImports0 <- arbitrary
        decls0 <- arbitrary
        pure $
            (emptyNamespace n :: Namespace ()) & algoImports .~ algoImports0 &
            sfImports .~ sfImports0 &
            decls .~ HM.fromList decls0
    shrink ns = do
        (ai, si, de) <-
            shrink (ns ^. algoImports, ns ^. sfImports, HM.toList $ ns ^. decls)
        pure $
            ns & algoImports .~ ai & sfImports .~ si & decls .~ HM.fromList de

instance Arbitrary DFExpr where
    arbitrary = DFExpr <$> arbitrary <*> arbitrary
    shrink (DFExpr lets ret) = map (`DFExpr` ret) $ shrink lets

instance Arbitrary LetExpr where
    arbitrary = LetExpr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink (LetExpr id r fr sa ca) =
        [LetExpr id' r' fr' sa' ca' | (id', r', fr', ca', sa') <- shrink (id, r, fr, ca, sa)]

instance Arbitrary DFVar where
    arbitrary = oneof [DFEnvVar <$> arbitrary, DFVar <$> arbitrary]
    shrink (DFEnvVar e) = map DFEnvVar $ shrink e
    shrink (DFVar b) = map DFVar $ shrink b

instance Arbitrary DFFnRef where
    arbitrary = oneof [DFFunction <$> arbitrary, EmbedSf <$> arbitrary]
    shrink (DFFunction f) = map DFFunction $ shrink f
    shrink (EmbedSf f) = map EmbedSf $ shrink f
