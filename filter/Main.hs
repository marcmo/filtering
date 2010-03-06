{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import System.TimeIt
import FilterByRegex(processFile)

data Sample = Filter {
    filterPath :: FilePath,
    filterRegex :: String,
    inverse :: Bool
} deriving (Show, Data, Typeable)

filterFile ::  Mode Sample
filterFile = mode $ Filter {
  filterPath = def &= typDir & flag "P" & typ "DIR" & text "path of input file",
  filterRegex = def &= flag "R" & typ "REGEX" & text "regex to filter lines",
  inverse = def &= text "invert filtering (filter out everything that does NOT match)"
}

filterFiles path regex v = do
    (timeIt . (processFile path v regex)) 10

main ::  IO ()
main = do 
  actions <- cmdArgs "FilterTool 0.1.0, (c) Oliver Mueller 2010" modes
  execute actions

execute :: Sample -> IO ()
execute (Filter p r v) = do
  putStrLn $ "regex: " ++ show r ++ ", path: " ++ p ++ ", was inverse: " ++ show v
  filterFiles p r v

modes = [filterFile]
