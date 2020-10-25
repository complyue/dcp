
module Main where

import           Prelude

import qualified Data.Text                     as T

import           Parser


main :: IO ()
main = do
  !src <- getContents
  let moduDecl = parseModule $ T.pack src

  putStrLn $ " * Module parsed as: " <> show moduDecl

