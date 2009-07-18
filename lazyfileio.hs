import Data.Char(toUpper)

main = do 
       inpStr <- readFile "input.txt"
       let xs = lines inpStr
       writeFile "output.txt" (processData $ head $ xs)

processData :: String -> String
processData = map toUpper
