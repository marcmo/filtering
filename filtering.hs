module Main
    where 
import Char
import Text.Printf
import System.IO

readBinaryFile :: FilePath -> IO String
readBinaryFile s = openBinaryFile s ReadMode >>= hGetContents

main = do
  hSetBuffering stdout NoBuffering
  putStr "Input file: "
  ifile <- getLine 
  putStr "Output file: "
  ofile <- getLine 
  s <- readBinaryFile ifile 
--   writeFile ofile (filter isAscii s)
  writeFile ofile (concat $ zipWith (printf "0x%02x,%s") s (cycle $ replicate 15 "" ++ ["\n"]))
  putStr "Filtering successful\n"
