{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import System.Directory (listDirectory, copyFile, createDirectory, doesDirectoryExist, removeDirectoryRecursive)
import Text.Read (readMaybe, look, Read (readPrec))
import Data.Maybe (mapMaybe)
import Data.List (intercalate, stripPrefix, sort)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import Data.Foldable (traverse_)
import Control.Monad ((<=<), forM_, when)
import GHC.TypeLits (KnownNat, Nat, natVal')
import Data.Kind (Type)
import Data.Char (toLower)
import GHC.Prim (proxy#)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)

{-# ANN module "HLint: ignore Redundant multi-way if" #-}

type Between :: Nat -> Nat -> Type
newtype Between min max = MkBetween {unBetween :: Int}
  deriving (Num, Read, Show, Eq, Ord) via Int

instance (KnownNat min, KnownNat max) => Bounded (Between min max) where
  minBound = fromIntegral $ natVal' (proxy# @min)
  maxBound = fromIntegral $ natVal' (proxy# @max)

instance (KnownNat min, KnownNat max) => Enum (Between min max) where
  toEnum n | n < minBound = error "Between, toEnum: value too small"
           | n > maxBound = error "Between, toEnum: value too large"
           | otherwise = MkBetween n
  fromEnum = unBetween
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

newtype Day = MkDay {unDay :: Int}
  deriving (Bounded, Enum, Eq, Ord, Num) via Between 1 25

instance Show Day where
  show (MkDay (show -> intStr)) =
    replicate (length (show $ unDay maxBound) - length intStr) '0' <> intStr

instance Read Day where
  readPrec = [ day | str <- look
                   , length str == length (show $ maxBound @Day)
                   , day <- MkDay <$> readPrec @Int
                   , day >= minBound, day <= maxBound
                   ]

-- | Copy day 1 to cover all 25 days
main :: IO ()
main = do
  existing <- existingDays
  if | null existing -> createDirs
     | otherwise -> do
       let plural | [_] <- existing = False
                  | otherwise = True
       putStrLn $ "Day" <> ['s' | plural] <> " " <>
         intercalate ", " (show <$> existing) <> " already exist."
       putStr $ "Should I delete " <> (if plural then "them" else "it") <> "? yes/No "
       hFlush stdout
       response <- getLine
       if | map toLower response `elem` ["y", "yes"] -> createDirs
          | otherwise -> putStrLn "Directories left unchanged."
  where
    createDirs = do
      forM_ [minBound + 1 :: Day ..] \day -> do
        let path = "day" <> show day
        dayExists <- doesDirectoryExist path
        when dayExists $ removeDirectoryRecursive path
        copyDirectory "day01" path
      putStrLn "Created directories."

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory source target = do
  createDirectory target
  listDirectory source >>= traverse_ \sub -> do
    let subOf path = path <> "/" <> sub
    subExists <- doesDirectoryExist $ subOf source
    (if subExists then copyDirectory else copyFile) (subOf source) (subOf target)

existingDays :: IO [Day]
existingDays = filter inRange . sort . mapMaybe dayDir <$> listDirectory "."
  where
    dayDir = readMaybe <=< stripPrefix "day"
    inRange n = n > minBound && n <= maxBound
