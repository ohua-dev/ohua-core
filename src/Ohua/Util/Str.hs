{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnboxedTuples              #-}
module Ohua.Util.Str
    ( IsString(fromString, toString, singleton, null, concat, intercalate, cons, uncons, snoc, unsnoc, head, tail, init, last, length, map, intersperse, reverse, splitAt, splitAtEnd, span, spanEnd, isPrefixOf, isSuffixOf, isInfixOf, filter, find, findEnd, index, split, elem)
    , strip, stripStart, stripEnd, take, takeEnd, drop, dropEnd, takeWhile, takeWhileEnd, dropWhile, dropWhileEnd, dropAround, break, lines, unlines, words, unwords, showS
    , (<>)
    , Str
    ) where

import           Control.Applicative    ((<|>))
import           Control.Arrow          (first)
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.Char              (isSpace)
import           Data.Foldable          (foldl')
import qualified Data.Foldable          as F
import           Data.Function          (on)
import           Data.Hashable
import qualified Data.List              as X
import           Data.Monoid
import qualified Data.String            as S
import           Prelude                (Bool, Char, Eq, Int,
                                         Maybe (Just, Nothing), Ord, String,
                                         fmap, fst, id, not, otherwise, pure,
                                         snd, ($), (-), (.), (==))
import qualified Prelude                as P
import           System.IO              (Handle, stderr, stdin, stdout)
import qualified System.IO              as SysIO


-- | This interface describes the basic capabilities of a string.
-- It is intentionally large so as to allow people to specify
-- more efficient versions of these functions.

-- All of these functions have default implementations, though you do have to
-- at least implement the minimal rewuired methods, otherwise you face non-termination.

-- Many of the functions however are imlpemented by converting to @String@ and then
-- using the string version of the function, which is often inefficient.
-- You therefore should provide an efficient implementation for the following functions
-- in addition to 'fromString', 'toString', 'singleton', 'uncons':
-- 'unsnoc', 'length', 'map', 'intersperse', 'reverse', 'splitAt', 'splitAtEnd',
-- 'span', 'spanEnd', 'isPrefixOf', 'isInfixOf', 'isSuffixOf', 'filter', 'find',
-- 'findEnd', 'index', 'elem', 'hPutStr', 'hGetLine'.
class (Monoid s, P.Eq s, P.Ord s, Hashable s, NFData s) => IsString s where

    {-# MINIMAL (singleton|fromString), (toString|uncons) #-}

    -- | Construct this string type from the `String` type from the base library
    fromString :: P.String -> s
    fromString = concat . P.map singleton

    -- | Produce the String type of the base library from this string type
    toString :: s -> P.String
    toString s | Just (x, xs) <- uncons s = x:toString xs
    toString _ = []

    -- | Produce a string which only contains a single character
    singleton :: P.Char -> s
    singleton = fromString . pure

    -- | Test if the String is empty
    null :: s -> P.Bool
    null = (== mempty)

    -- | Concatenate a collection of strings
    concat :: P.Foldable f => f s -> s
    concat = foldl' mappend mempty

    -- | Concatenate a collection of strings inserting a string as separator
    intercalate :: P.Foldable f => s -> f s -> s
    intercalate s = concat . X.intersperse s . F.toList

    -- | Append a character to the front of the string
    cons :: P.Char -> s -> s
    cons = mappend . singleton

    -- | Append a character to the back of the string
    snoc :: s -> P.Char -> s
    snoc str c = str `mappend` singleton c

    -- | Disassemble a string into the first character and a tail
    uncons :: s -> P.Maybe (P.Char, s)
    uncons s | (x:rest) <- toString s = P.Just (x, fromString rest)
    uncons _ = P.Nothing

    -- | Disassemble a string into an initial string and the last character
    unsnoc :: s -> Maybe (s, Char)
    unsnoc = P.fmap (first fromString) . f id . toString
      where
        f _ []       = P.Nothing
        f app [x]    = P.Just (app [], x)
        f app (x:xs) = f (app . (x:)) xs

    -- | Get the first character of the string
    head :: s -> Maybe Char
    head = fmap fst . uncons

    -- | Get the remainder of the string after removing the first character
    tail :: s -> Maybe s
    tail = fmap snd . uncons

    -- | Get the last character of a string
    last :: s -> Maybe Char
    last = fmap snd . unsnoc

    -- | Get the initial string with the last character removed
    init :: s -> Maybe s
    init = fmap fst . unsnoc

    -- | Get the length of the string
    length :: s -> Int
    length = X.length . toString

    -- | Apply a function to all characters in a string
    map :: (Char -> Char) -> s -> s
    map f = onString $ P.map f

    -- | insert a character between all characters of this string
    intersperse :: Char -> s -> s
    intersperse = onString . X.intersperse

    -- | Reverse the string
    reverse :: s -> s
    reverse = onString $ X.reverse

    -- replace :: s -> s -> s -> s
    -- replace =

    -- | Split the sstring into a string before and a string after a specified index
    splitAt :: Int -> s -> (s, s)
    splitAt = onStrTup . X.splitAt

    -- | Same as splitAt but counting from the end of the string
    splitAtEnd :: Int -> s -> (s, s)
    splitAtEnd i s = onStrTup (X.splitAt (length s - i)) s
    -- TODO test whether we need a (-1) here

    -- | Splits a string into a prefix in which all characters satisfy a predicate
    -- and a remaining string.
    span :: (Char -> Bool) -> s -> (s, s)
    span = onStrTup . X.span

    -- | Same as span but where the initial string does *not* satisfy the predicate
    break :: (Char -> Bool) -> s -> (s, s)
    break = span . (not .)

    -- | Splits a string into a suffix where all characters satisfy a predicate and a
    -- remaining initial string
    spanEnd :: (Char -> Bool) -> s -> (s, s)
    spanEnd = onStrTup . f id id
      where
        f appSat appNoSat _ xs@[] = (appSat xs, appNoSat xs)
        f appSat appNoSat p (x:xs)
            | p x = f (appSat . (x:)) appNoSat p xs
            | otherwise = f id (appNoSat . appSat . (x:)) p xs

    -- | Retains only the first @n@ characters of the string.
    -- Retains the whole string if @n >= length@.
    take :: Int -> s -> s
    take i = fst . splitAt i

    -- | Retains only the last @n@ characters of the string.
    -- Retains the whole string if @n >= length@.
    takeEnd ::Int -> s -> s
    takeEnd i = snd . splitAtEnd i

    -- | The string after removing the first @n@ characters.
    -- If @n >= length@ the result is the empty string.
    drop :: Int -> s -> s
    drop i = snd . splitAt i

    -- | The string after removing the last @n@ characters.
    -- If @n >= length@ the result is the empty string.
    dropEnd :: Int -> s -> s
    dropEnd i = snd . splitAtEnd i

    -- | Retain the prefix of the String which satisfies the predicate.
    takeWhile :: (Char -> Bool) -> s -> s
    takeWhile p = fst . span p

    -- | Retain the suffix of the string which satisfies the predicate.
    takeWhileEnd :: (Char -> Bool) -> s -> s
    takeWhileEnd p = fst . spanEnd p

    -- | Remove the prefix in which all characters satisfy the predicate.
    dropWhile :: (Char -> Bool) -> s -> s
    dropWhile p = snd . span p

    -- | Remove the suffix in which all characters satisfy the predicate.
    dropWhileEnd :: (Char -> Bool) -> s -> s
    dropWhileEnd p = snd . spanEnd p

    -- | Remove the prefix and suffix in which all characters satisfy the predicate.
    dropAround :: (Char -> Bool) -> s -> s
    dropAround f = dropWhile f . dropWhileEnd f

    -- | Test if the first string is the beginning of the second.
    isPrefixOf :: s -> s -> Bool
    isPrefixOf s1 s2 = s1 == take (length s1) s2

    -- | Test if the first string is the end of the second
    isSuffixOf :: s -> s -> Bool
    isSuffixOf s1 s2 = s1 == takeEnd (length s1) s2

    -- | Test if the first string is contained in the second
    isInfixOf :: s -> s -> Bool
    isInfixOf = X.isInfixOf `on` toString

    -- | Remove all characters which satisfy the predicate.
    filter :: (Char -> Bool) -> s -> s
    filter = onString . P.filter

    -- | Return the first character that satisfies the predicate.
    find :: (Char -> Bool) -> s -> Maybe Char
    find p s = X.find p (toString s)

    -- | Return the first character that satisfies the predicate starting from the end of the string.
    findEnd :: (Char -> Bool) -> s -> Maybe Char
    findEnd p s = f p (toString s)
      where
        f p [] = Nothing
        f p (x:xs) | p x = f p xs <|> Just x
                   | otherwise = f p xs

    -- | Return the index of the first character which satisfies the predicate.
    index :: (Char -> Bool) -> s -> Maybe Int
    index p s = X.findIndex p (toString s)

    -- | Split the string each time the predicate is satisfied.
    split :: (Char -> Bool) -> s -> [s]
    split p s = x : P.maybe [] (split p) (tail xs)
      where (x,xs) = span (not . p) s

    -- | Test if the character occurs in the string
    elem :: Char -> s -> Bool
    elem c = P.elem c . toString

    -- | Write the string to a handle.
    hPutStr :: MonadIO m => Handle -> s -> m ()
    hPutStr h = liftIO . SysIO.hPutStr h . toString

    -- | Read a line of this string type from the handle.
    hGetLine :: MonadIO m => Handle -> m s
    hGetLine = P.fmap fromString . liftIO . SysIO.hGetLine


-- | An opaque string type to make refactoring against different string libraries easier
newtype Str = Str { unStr :: P.String }
    deriving (Monoid, Eq, Ord, S.IsString, Hashable, NFData)

instance P.Show Str where
    show = P.show . unStr

instance IsString Str where
    fromString = Str
    toString = unStr

instance IsString P.String where
    fromString = id
    toString = id
    singleton = pure
    concat = P.concat
    isPrefixOf = X.isPrefixOf
    isInfixOf = X.isInfixOf
    isSuffixOf = X.isSuffixOf
    drop = X.drop
    take = X.take
    splitAt = X.splitAt
    dropWhile = X.dropWhile
    takeWhile = X.takeWhile
    dropWhileEnd = X.dropWhileEnd


onString :: IsString s => (String -> P.String) -> s -> s
onString f = fromString . f . toString

onStrTup :: IsString s => (String -> (String, P.String)) -> s -> (s, s)
onStrTup f s = (fromString s1, fromString s2)
  where (s1, s2) = f $ toString s

-- | Write the string to the handle and prepend a newline
hPutStrLn :: (IsString s, MonadIO m) => Handle -> s -> m ()
hPutStrLn h s = do
    hPutStr h s
    liftIO $ SysIO.hPutChar h '\n'


-- | Split the string on newlines.
lines :: IsString s => s -> [s]
lines = split (== '\n')

-- | Concatenate the strings with newlines
unlines :: (F.Foldable f, IsString s) => f s -> s
unlines = intercalate (singleton '\n')

-- | Split the string on whitespace.
-- Result sequence does not contain any whitespace characters or empty strings.
words :: IsString s => s -> [s]
words = P.filter (not . null) . split isSpace

-- | Concatenate the strings with spaces
unwords :: (F.Foldable f, IsString s) => f s -> s
unwords = intercalate (singleton ' ')

putStr :: (IsString s, MonadIO m) => s -> m ()
putStr = hPutStr stdout

putStrLn :: (IsString s, MonadIO m) => s -> m ()
putStrLn = hPutStrLn stdout

strip :: IsString s => s -> s
strip = dropAround isSpace

stripStart :: IsString s => s -> s
stripStart = dropWhile isSpace

stripEnd :: IsString s => s -> s
stripEnd = dropWhileEnd isSpace

showS :: (IsString s, P.Show a) => a -> s
showS = fromString . P.show
