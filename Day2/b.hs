#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

import System.Environment
import Data.Char
import Data.List.Split

toInt :: String -> Int
toInt = read

replace :: Int -> Int -> [Int] -> [Int]
replace _ _ [] = []
replace index value list = (take index list) ++ value : (drop (index + 1) list)

opFunc 1 = (+)
opFunc 2 = (*)

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

programm :: Int -> Int -> [Int] -> Int
programm noun verb numbers = head $ aggregate (opGroup presets) presets
    where presets = (replace 1 noun) . (replace 2 verb) $ numbers

bruteForce :: Int -> [Int] -> Int
bruteForce target numbers = head [100 * noun + verb | noun <- [1..99], verb <- [1..99], (programm noun verb numbers) == target]

main = do
    [inputFile, target] <- getArgs
    input <- readFile inputFile
    let line = (head . lines) input
    let numbers = map toInt $ splitOn "," line
    putStr $ show $ bruteForce (read target) numbers
