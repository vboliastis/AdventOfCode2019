#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

import System.Environment
import Data.Char
import Data.List.Split
import Data.Set

toInt :: String -> Int
toInt = read

unwrap :: String -> ([(Int, Int)] -> [(Int, Int)])
unwrap (orientation:dist) = \ps -> let
    (a, b) = last ps
    (pointsA, pointsB) = case orientation of
        'L' -> (repeat a,[b-1,b-2..])
        'R' -> (repeat a,[b+1..])
        'U' -> ([a+1..],repeat b)
        'D' -> ([a-1,a-2..],repeat b)
    in ps ++ (Prelude.take (read dist) $ zip pointsA pointsB)

aggregate :: [a -> a] -> (a -> a)
aggregate (f:[]) = f
aggregate (f:fs) = aggregate fs . f

unwrapInputPath :: [String] -> Set (Int, Int)
unwrapInputPath path = Data.Set.fromList $ tail $ aggregate (Prelude.map unwrap path) [(0,0)]

distanceToMiddle :: (Int, Int) -> Double
distanceToMiddle (a, b) = let
    fa = fromIntegral a :: Double
    fb = fromIntegral b :: Double
    in sqrt $ fa*fa + fb*fb

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let [inputPath1, inputPath2] = Prelude.map (splitOn ",") $ lines input
    let crosses = Data.Set.intersection (unwrapInputPath inputPath1) (unwrapInputPath inputPath2)
    putStr $ show $ findMin $ Data.Set.map distanceToMiddle crosses
    
