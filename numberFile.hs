module TextUtilsOli
    where

import Monad

--usage: numberFile "monads01.hs"
numberFile :: FilePath -> IO()
numberFile fp = do
  text <- readFile fp
  let textLines = lines text
  let n = zipWith (\x y -> show x ++ ' ' : y)[1..]textLines
  mapM_ putStrLn n

numberFile2 :: FilePath -> IO ()
numberFile2 fp = do
  l <- lines `liftM` readFile fp
  let n = zipWith (\n t -> show n ++ ' ' : t) [1..] l
  mapM_ putStrLn n
