module Main where

import Examples as E
import RunOctopus (runOctopus)

main :: IO ()
main = runOctopus E.testScene
