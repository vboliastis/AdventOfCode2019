import System.Environment
import Data.Char

toInt :: String -> Int
toInt = read

convert :: Int -> Int
convert x
    | x <= 0 = 0
    | otherwise = max 0 $ (x `div` 3) - 2

continuousConvert :: Int -> Int
continuousConvert 0 = 0
continuousConvert x = let y = convert x in y + (continuousConvert y)

aggregate :: [Int] -> Int
aggregate [] = 0 
aggregate (x:xs) = continuousConvert x + aggregate xs

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let numbers = map toInt $ lines input
    putStr $ show $ aggregate numbers
