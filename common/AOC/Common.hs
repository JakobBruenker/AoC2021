{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module AOC.Common
  ( module AOC.Common
  , module Data.Functor
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Reader
  , module Control.Monad.ST
  , module Control.Monad.IO.Class
  , module Control.Monad.Except
  , module Control.Arrow
  , module Data.Monoid
  , module Data.Maybe
  , module Data.Either
  , module Data.Char
  , module Data.These
  , module Data.Foldable
  , module Data.Bifunctor
  , module Data.Bitraversable
  , module Data.Profunctor
  , module Data.Ord
  , module Data.Coerce
  , module Data.Data
  , module Data.Data.Lens
  , module Data.Generics.Labels
  , module Data.Kind
  , module Control.Category
  , module Control.Monad.Loops
  , module Data.Function
  , module Data.List
  , module Data.Word
  , module Data.Bool
  , module Data.Bits
  , module Data.Void
  , module Numeric.Lens
  , module Text.Read
  , module System.Random
  , module GHC.Generics
  , module GHC.Stack

  , module Prelude
  , module Data.Boolean.Overload
  
  , module Data.String.Interpolate

  , module Control.Lens
  , module Data.List.Split
  , module GHC.IO
  , module Debug.Trace
  ) where

import Data.Maybe
import Data.Either
import Data.Char
import Data.These
import Data.Functor
import Data.Profunctor hiding (WrappedArrow(..))
import Data.Ord
import Data.Coerce
import Data.Data
import Data.Data.Lens
import Data.Generics.Labels
import Data.Kind
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.Except
import Control.Arrow hiding (first, second)
import Data.Monoid
import Data.Foldable
import Data.Bifunctor
import Data.Bitraversable
import Control.Category hiding ((.), id)
import Control.Monad.Loops
import Data.Function
import Data.List hiding (uncons)
import Data.Word
import Data.Bool hiding ((&&), (||), not)
import Data.Bits
import Data.Void
import Numeric.Lens
import Text.Read hiding (lift, get, step, (+++))
import System.Random hiding (split)
import GHC.Generics (Generic)
import GHC.Stack

import Prelude hiding ((&&), (||), not)
import Data.Boolean.Overload ((&&), (||), not)

import Control.Lens hiding ((<.>))
import Data.List.Split

import Data.String.Interpolate

import Data.Text (Text)
import Data.Text qualified as T

import GHC.IO (unsafePerformIO, throwIO)

import Debug.Trace
import Control.Exception (ErrorCall(ErrorCall))

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True  f x = f x
applyWhen False _ x = x

applyN :: Int -> (a -> a) -> a -> a
applyN n f = applyWhen (n > 0) $ applyN (n - 1) f . f

infixr 9 .:
(.:) :: (b -> c) -> (a -> d -> b) -> a -> d -> c
f .: g = (f .) . g

readInput :: HasCallStack => Maybe Int -> IO String
readInput mday = do
  let extractDay :: CallStack -> Maybe Int
      extractDay = readMaybe . take 2 . drop 3 . srcLocFile . snd <=< listToMaybe . getCallStack
      can'tReadDay = ErrorCall $ "Couldn't get day from " <> prettyCallStack callStack
  autoDay <- justifyLeft 2 '0' . show <$> maybe (throwIO can'tReadDay) pure (mday <|> extractDay callStack)
  readFile $ "day" <> autoDay <> "/input"

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 1 <<&>>
(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) = flip (<<$>>)

ffmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
ffmap = fmap . fmap

fffmap :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fffmap = fmap . fmap . fmap

justifyLeft :: Int -> a -> [a] -> [a]
justifyLeft n x xs = replicate (n - length xs) x <> xs

justifyRight :: Int -> a -> [a] -> [a]
justifyRight n x xs = xs <> replicate (n - length xs) x

loeb :: Functor f => f (f a -> a) -> f a
loeb = moeb fmap

moeb :: (((a -> b) -> b) -> c -> a) -> c -> a
moeb f x = go where go = f ($ go) x

whenM :: Monad m => m Bool -> m () -> m ()
whenM mcond action = do
  cond <- mcond
  when cond action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mcond action = do
  cond <- mcond
  unless cond action

headMay :: [a] -> Maybe a
headMay = listToMaybe

count :: (a -> Bool) -> [a] -> Int
count = length .: filter

infixr 3 &=&
(&=&) :: Applicative p => (a -> p b) -> (a -> p c) -> a -> p (b, c)
(&=&) = (liftA2 . liftA2) (,)

infixr 3 *=*
(*=*) :: Applicative p => (a1 -> p b1) -> (a2 -> p b2) -> (a1, a2) -> p (b1, b2)
(f *=* g) (x, y) = liftA2 (,) (f x) (g y)

infixr 9 <.>
(<.>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
f <.> g = fmap f . g

class Display a where
  display :: a -> String
  
instance {-# OVERLAPPABLE #-} Show a => Display a where
  display = show

instance {-# OVERLAPPING #-} Display String where
  display = id
  
instance {-# OVERLAPPING #-} Display Text where
  display = T.unpack
