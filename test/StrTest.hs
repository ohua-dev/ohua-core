module StrTest where


import           Ohua.Util.Str as Str
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import Prelude as P
import Control.Arrow
import Data.Char

toStr :: String -> Str
toStr = fromString

strTest = describe "Equivalence of Str and String" $ do
    prop "conversion" $ \str -> str == toString (toStr str)
    prop "singleton" $ \char -> singleton char == toString (singleton char :: Str)
    prop "concat" $ \strs -> Str.concat strs == toString (Str.concat $ P.map toStr strs)
    prop "isPrefixOf" $ \s1 s2 -> Str.isPrefixOf s1 s2 == Str.isPrefixOf (toStr s1) (toStr s2)
    prop "isSuffixOf" $ \s1 s2 -> Str.isSuffixOf s1 s2 == Str.isSuffixOf (toStr s1) (toStr s2)
    prop "drop" $ \i str -> Str.drop i str == toString (Str.drop i (toStr str))
    prop "take" $ \i str -> Str.take i str == toString (Str.take i (toStr str))
    prop "splitAt" $ \i str -> Str.splitAt i str == (toString *** toString) (Str.splitAt i (toStr str))
    prop "dropWhile" $ \str -> Str.dropWhile isLetter str == toString (Str.dropWhile isLetter (toStr str))
    prop "takeWhile" $ \str -> Str.takeWhile isDigit str == toString (Str.takeWhile isDigit (toStr str))
    prop "dropWhileEnd" $ \str -> Str.dropWhileEnd isPrint str == toString (Str.dropWhileEnd isPrint (toStr str))
    prop "words" $ \str -> P.words str == P.map toString (Str.words (toStr str))
    prop "lines" $ \str -> P.lines str == P.map toString (Str.lines (toStr str))
    prop "unwords" $ \strs -> P.unwords strs == toString (Str.unwords (P.map toStr strs))
    prop "unlines" $ \strs -> P.unlines strs == toString (Str.unlines (P.map toStr strs))
