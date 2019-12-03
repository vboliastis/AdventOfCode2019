#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

import System.Environment
import Data.Char
import Data.List.Split

toInt :: String -> Int
toInt = read

opFunc 1 = (+)
opFunc 2 = (*)

replace :: Int -> Int -> [Int] -> [Int]
replace _ _ [] = []
replace index value list = (take index list) ++ value : (drop (index + 1) list)

compute :: [Int] -> ([Int] -> [Int])
compute (99:_) = \l -> l
compute (op:a:b:wr:[]) = \l -> let 
    operate = opFunc op
    lA = l !! a
    lB = l !! b
    in replace wr (operate lA lB) l

opGroup :: [Int] -> [[Int] -> [Int]]
opGroup (a:b:c:d:r) = compute [a,b,c,d] : opGroup r
opGroup _ = []

aggregate :: [[Int] -> [Int]] -> [Int] -> [Int]
aggregate comp = foldl1 (.) (reverse comp)

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let line = (head . lines) input
    let numbers = map toInt $ splitOn "," line
    let presets = (replace 1 12) . (replace 2 2) $ numbers
    putStr $ show $ head $ aggregate (opGroup presets) presets
