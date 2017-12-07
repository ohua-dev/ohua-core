{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnboxedTuples              #-}
module Ohua.Util.Str
    ( IsString(fromString, toString, singleton, null, concat, intercalate, cons, uncons, snoc, unsnoc, length, map, intersperse, reverse, splitAt, splitAtEnd, span, spanEnd, isPrefixOf, isSuffixOf, isInfixOf, filter, find, findEnd, index, split, elem)
    , strip, stripStart, stripEnd, take, takeEnd, drop, dropEnd, takeWhile, takeWhileEnd, dropWhile, dropWhileEnd, dropAround, break, lines, unlines, showS
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
                                         fst, id, not, otherwise, pure, snd,
                                         ($), (-), (.), (==))
import qualified Prelude                as P
import           System.IO              (Handle, stderr, stdin, stdout)
import qualified System.IO              as SysIO


class (Monoid s, P.Eq s, P.Ord s, Hashable s, NFData s) => IsString s where

    {-# MINIMAL (singleton|fromString), (toString|uncons) #-}

    fromString :: P.String -> s
    fromString = concat . P.map singleton

    toString :: s -> P.String
    toString s | Just (x, xs) <- uncons s = x:toString xs
    toString _ = []

    singleton :: P.Char -> s
    singleton = fromString . pure

    null :: s -> P.Bool
    null = (== mempty)

    concat :: P.Foldable f => f s -> s
    concat = foldl' mappend mempty

    intercalate :: P.Foldable f => s -> f s -> s
    intercalate s = concat . X.intersperse s . F.toList

    cons :: P.Char -> s -> s
    cons = mappend . singleton

    snoc :: s -> P.Char -> s
    snoc str c = str `mappend` singleton c

    uncons :: s -> P.Maybe (P.Char, s)
    uncons s | (x:rest) <- toString s = P.Just (x, fromString rest)
    uncons _ = P.Nothing

    unsnoc :: s -> Maybe (s, Char)
    unsnoc = P.fmap (first fromString) . f id . toString
      where
        f _ []       = P.Nothing
        f app [x]    = P.Just (app [], x)
        f app (x:xs) = f (app . (x:)) xs

    length :: s -> Int
    length = X.length . toString

    map :: (Char -> Char) -> s -> s
    map f = onString $ P.map f

    intersperse :: Char -> s -> s
    intersperse = onString . X.intersperse

    reverse :: s -> s
    reverse = onString $ X.reverse

    -- replace :: s -> s -> s -> s
    -- replace =

    splitAt :: Int -> s -> (s, s)
    splitAt = onStrTup . X.splitAt

    splitAtEnd :: Int -> s -> (s, s)
    splitAtEnd i s = onStrTup (X.splitAt (length s - i)) s
    -- TODO test whether we need a (-1) here

    span :: (Char -> Bool) -> s -> (s, s)
    span = onStrTup . X.span

    spanEnd :: (Char -> Bool) -> s -> (s, s)
    spanEnd = onStrTup . f id id
      where
        f appSat appNoSat _ xs@[] = (appSat xs, appNoSat xs)
        f appSat appNoSat p (x:xs)
            | p x = f (appSat . (x:)) appNoSat p xs
            | otherwise = f id (appNoSat . appSat . (x:)) p xs

    isPrefixOf :: s -> s -> Bool
    isPrefixOf = X.isPrefixOf `on` toString

    isSuffixOf :: s -> s -> Bool
    isSuffixOf = X.isSuffixOf `on` toString

    isInfixOf :: s -> s -> Bool
    isInfixOf = X.isInfixOf `on` toString

    filter :: (Char -> Bool) -> s -> s
    filter = onString . P.filter

    find :: (Char -> Bool) -> s -> Maybe Char
    find p s = X.find p (toString s)

    findEnd :: (Char -> Bool) -> s -> Maybe Char
    findEnd p s = f p (toString s)
      where
        f p [] = Nothing
        f p (x:xs) | p x = f p xs <|> Just x
                   | otherwise = f p xs

    index :: (Char -> Bool) -> s -> Maybe Int
    index p s = X.findIndex p (toString s)

    split :: (Char -> Bool) -> s -> [s]
    split p s = x : split p xs
      where (x,xs) = span (not . p) s

    elem :: Char -> s -> Bool
    elem c = P.elem c . toString

    hPutStr :: MonadIO m => Handle -> s -> m ()
    hPutStr h = liftIO . SysIO.hPutStr h . toString

    hPutStrLn :: MonadIO m => Handle -> s -> m ()
    hPutStrLn h s = do
        hPutStr h s
        liftIO $ SysIO.hPutChar h '\n'


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


onString :: IsString s => (String -> P.String) -> s -> s
onString f = fromString . f . toString

onStrTup :: IsString s => (String -> (String, P.String)) -> s -> (s, s)
onStrTup f s = (fromString s1, fromString s2)
  where (s1, s2) = f $ toString s


putStr :: (IsString s, MonadIO m) => s -> m ()
putStr = hPutStr stdout

putStrLn :: (IsString s, MonadIO m) => s -> m ()
putStrLn = hPutStrLn stdout

break :: IsString s => (Char -> Bool) -> s -> (s, s)
break = span . (not .)

strip :: IsString s => s -> s
strip = dropAround isSpace

stripStart :: IsString s => s -> s
stripStart = dropWhile isSpace

stripEnd :: IsString s => s -> s
stripEnd = dropWhileEnd isSpace

take :: IsString s => Int -> s -> s
take i = fst . splitAt i

takeEnd :: IsString s => Int -> s -> s
takeEnd i = snd . splitAtEnd i

drop :: IsString s => Int -> s -> s
drop i = snd . splitAt i

dropEnd :: IsString s => Int -> s -> s
dropEnd i = snd . splitAtEnd i

takeWhile :: IsString s => (Char -> Bool) -> s -> s
takeWhile p = fst . span p

takeWhileEnd :: IsString s => (Char -> Bool) -> s -> s
takeWhileEnd p = fst . spanEnd p

dropWhile :: IsString s => (Char -> Bool) -> s -> s
dropWhile p = snd . span p

dropWhileEnd :: IsString s => (Char -> Bool) -> s -> s
dropWhileEnd p = snd . spanEnd p

dropAround :: IsString s => (Char -> Bool) -> s -> s
dropAround f = dropWhile f . dropWhileEnd f

lines :: IsString s => s -> [s]
lines = split (== '\n') . filter (not . (== '\r'))

unlines :: (IsString s, F.Foldable f) => f s -> s
unlines = intercalate (singleton '\n')

showS :: (IsString s, P.Show a) => a -> s
showS = fromString . P.show
