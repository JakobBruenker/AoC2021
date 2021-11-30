{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import AOC.Common
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Map.Lazy qualified as ML
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.IORef
import Data.STRef
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable (IOVector, STVector)
import Data.Vector.Mutable qualified as V

import Data.Attoparsec.Text qualified as P

{-# ANN module ("HLint: ignore Redundant multi-way if" :: String) #-}

main :: IO ()
main = do
  input <- readInput Nothing
  putStrLn [i|
Part 1:
#{display $ part1 input}

#{replicate 80 '-'}

Part 2:
#{display $ part2 input}
|]

part1 = const @String "part1"

part2 = const @String "part2"
