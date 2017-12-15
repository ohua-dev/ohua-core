{-# LANGUAGE ExplicitForAll, ScopedTypeVariables #-}
module StrTest where


import           Control.Arrow
import           Data.Char
import           Ohua.Util.Str         as Str
import           Prelude               as P
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import qualified Data.List as L


strClassProps :: forall s . IsString s => (String -> s) -> Spec
strClassProps toStr = do
    prop "conversion" $ \str -> str == toString (toStr str)
    prop "singleton" $ \char -> singleton char == toString (singleton char :: Str)
    prop "null" $ \str -> P.null str == Str.null (toStr str)
    prop "concat" $ \strs -> Str.concat strs == toString (Str.concat $ P.map toStr strs)
    prop "intercalate" $ \str strs -> L.intercalate str strs == toString (Str.intercalate (toStr str) (P.map toStr strs))
    prop "cons" $ \char str -> char:str == toString (Str.cons char (toStr str))
    prop "snoc" $ \char str -> str ++ [char] == toString (Str.snoc (toStr str) char)
    prop "uncons" $ \str -> L.uncons str == fmap (second toString) (Str.uncons str)
    prop "unsnoc" $ fromPartial Str.unsnoc (\str -> (toStr $ P.init str, P.last str))
    prop "head" $ fromPartial Str.head P.head
    prop "tail" $ fromPartial Str.tail (toStr . P.tail)
    prop "last" $ fromPartial Str.last P.last
    prop "inits" $ fromPartial Str.init (toStr . P.init)
    prop "length" $ \str -> Str.length (toStr str) == P.length str
    prop "map" $ \str -> P.map toUpper str == toString (Str.map toUpper $ toStr str)
    prop "isPrefixOf" $ \s1 s2 -> Str.isPrefixOf s1 s2 == Str.isPrefixOf (toStr s1) (toStr s2)
    prop "isSuffixOf" $ \s1 s2 -> Str.isSuffixOf s1 s2 == Str.isSuffixOf (toStr s1) (toStr s2)
    prop "drop" $ \i str -> Str.drop i str == toString (Str.drop i (toStr str))
    prop "take" $ \i str -> Str.take i str == toString (Str.take i (toStr str))
    prop "splitAt" $ \i str -> Str.splitAt i str == (toString *** toString) (Str.splitAt i (toStr str))
    prop "dropWhile" $ \str -> Str.dropWhile isLetter str == toString (Str.dropWhile isLetter (toStr str))
    prop "takeWhile" $ \str -> Str.takeWhile isDigit str == toString (Str.takeWhile isDigit (toStr str))
    prop "dropWhileEnd" $ \str -> Str.dropWhileEnd isPrint str == toString (Str.dropWhileEnd isPrint (toStr str))
    prop "words" $ \str -> P.words str == P.map toString (Str.words (toStr str))
    -- TODO add "lines" test once semantics are decided
    -- prop "lines" $ \str -> P.lines str == P.map toString (Str.lines (toStr str))
    prop "unwords" $ \strs -> P.unwords strs == toString (Str.unwords (P.map toStr strs))
    -- TODO add "unlines" test once semantics are decided
    -- prop "unlines" $ \strs -> P.unlines strs == toString (Str.unlines (P.map toStr strs))
  where
    fromPartial f1 f2 [] = f1 (toStr "") == Nothing
    fromPartial f1 f2 str = f1 (toStr str) == Just (f2 str)


strTest = describe "Equivalence of Str and String" $ 
    strClassProps (fromString :: String -> Str)
