import System.Environment
import Data.Char

toInt :: String -> Int
toInt = read

convert :: Int -> Int
convert x = max 0 $ (x `div` 3) - 2

aggregate :: [Int] -> Int
aggregate [] = 0
aggregate (x:xs) = convert x + aggregate xs

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let numbers = map toInt $ lines input
    putStr $ show $ aggregate numbers
