module Main where

import           System.Environment         (getArgs)
import SectionDetector.DetectSections

main :: IO ()
main = do
  (filename:_) <- getArgs
  finalState <- detectSections filename
  print finalState

