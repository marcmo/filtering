module Main
    where 
import Char
import Text.Printf
import System.IO
import ChunkedFileProcessing
import qualified Data.Map as M
import System.Environment
import Data.Int (Int64)
import Control.Exception (bracket, finally)
import Control.Monad
import Maybe
import FilterByRegex
import Data.List (foldl', sortBy)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Text.Regex.PCRE.Light -- (compile, match)
import LineChunks (chunkedReadWith)
import MapReduce (mapReduce)
import System.TimeIt
import Control.Parallel.Strategies (NFData(..), rwhnf)

sampleRegex = toRegex "\\s*\\d*\\.\\d*\\s*3"

readBinaryFile :: FilePath -> IO String
readBinaryFile s = openBinaryFile s ReadMode >>= hGetContents

main2 = do
  hSetBuffering stdout NoBuffering
  putStr "Input file: "
  ifile <- getLine 
  putStr "Output file: "
  ofile <- getLine 
  s <- readBinaryFile ifile 
--   writeFile ofile (filter isAscii s)
  writeFile ofile (concat $ zipWith (printf "0x%02x,%s") s (cycle $ replicate 15 "" ++ ["\n"]))
  putStr "Filtering successful\n"

logFilePath = "/Users/oliver/tmp/output.txt"

instance NFData B.ByteString where
    rnf _ = ()    -- not built into Control.Parallel.Strategies

collectLines ::  [LB.ByteString] -> [B.ByteString]
collectLines = mapReduce rwhnf (filterLines sampleRegex) rwhnf concat



main = test4

-- naive straight forward solution...read in all at once
test = do
    args <- getArgs
    let regex = compile pattern []
    forM_ args $ \name -> do
        outH <- openFile (name ++ ".out1") WriteMode
        content <- LB.readFile name
        let filtered = filterLines sampleRegex content
        B.hPut outH (B.unlines filtered)
        hClose outH

-- tryout using the map reduce framework
test2 = do
  args <- getArgs
  forM_ args $ \path -> do
    outH <- openFile (path ++ ".out") WriteMode
    m <- chunkedReadWith collectLines path
    print $ length m
    B.hPut outH (B.unlines m)

-- tryout solution with chunked processing...only for estimation
test3 = do
    [path] <- getArgs
    let chunkSize = 50*1024*1024
    h <- openFile path ReadMode
    totalSize <- fromIntegral `liftM` hFileSize h
    print totalSize
    let nrOfChunks = totalSize `div` fromIntegral chunkSize
    print nrOfChunks
    outH <- openFile (path ++ ".out") WriteMode
    replicateM nrOfChunks $ do
        print "writing..."
        chunk <- LB.take chunkSize `liftM` LB.hGetContents h
        let filtered = filterLines sampleRegex chunk
        B.hPut outH (B.unlines filtered)
    hClose outH


test4 = do
    [path,regex] <- getArgs
    (timeIt . (processFile path regex)) 10
    -- processFile path (read numChunks)

   -- 0.458685 3  A			Rx   d 4 39 01 39 00 
createHugeSample :: Int -> IO ()
createHugeSample n = do
  contents <- B.readFile "trace2.asc"
  wh <- openFile logFilePath AppendMode 
  replicateM_ n $
    B.hPut wh contents
  


