module Main where

import Parser
import Eval
import Control.Monad
import qualified Data.Text.IO as TIO

main :: IO ()
main = forever $ do
  line <- TIO.getLine
  print . eval $ readExpr line
