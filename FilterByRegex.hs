module FilterByRegex
where

import ChunkedFileProcessing
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')
import Text.Regex.PCRE.Light (compile, match, Regex)
import Control.Monad (forM)
import Control.Parallel.Strategies (NFData(..), rwhnf)
import Control.Exception (finally)
import System.IO 


regex = compile pattern []
pattern = B.pack "\\s*\\d*\\.\\d*\\s*3"
toRegex s = compile (B.pack s) []
strict  = B.concat . LB.toChunks

filterLines :: Regex -> LB.ByteString -> [B.ByteString]
filterLines reg chunk = reverse $ foldl' augment [] (LB.lines chunk)
    where augment accum line =
            case match reg (strict line) [] of
              Just _ -> accum
              _ -> (strict line):accum

processFile ::  FilePath -> String -> Int -> IO ()
processFile path r numChunks = do
  print $ "processFile with chunks: " ++ show numChunks
  print $ "regex was :" ++ r
  let regex = toRegex r
  (chunks, handles) <- chunkedRead path numChunks
  outH <- openFile (path ++ ".out") WriteMode
  r <- forM chunks $ \chunk -> do
            print $ "processing...:" ++ (show $ LB.length chunk)
            B.hPut outH (B.unlines $ filterLines regex chunk)
  (rnf r `seq` return r) `finally` mapM_ hClose handles
  hClose outH
