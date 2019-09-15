module Main where

import Hex
import Namer
import System.Environment

main :: IO ()
main = do
  [id] <- getArgs
  let name = cp2name (hex2dec id)
  putStr(name ++ "\n")
