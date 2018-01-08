module Main where

import Parser
import Eval
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  line <- TIO.getLine
  print . eval $ readExpr line
