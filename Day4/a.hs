#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

import System.Environment
import Data.Char
import Data.List.Split
import Data.List

inc :: [String] -> String -> [String]
inc (x:xs) m =
    if x > m then xs
    else
        let plus1 = (show ((read x) + 1))
        in case (checkSorted x) && (checkAtLeastOnceEqual x) of
            True -> inc (plus1:x:xs) m
            False -> inc (plus1:xs) m

checkSorted :: String -> Bool
checkSorted s = (s == sort s)

checkAtLeastOnceEqual :: String -> Bool
checkAtLeastOnceEqual [] = False
checkAtLeastOnceEqual (_:[]) = False
checkAtLeastOnceEqual (x:xx:xs) = (x == xx) || checkAtLeastOnceEqual (xx:xs)

main = do
    [input] <- getArgs
    let [from, to] = splitOn "-" input
    putStr $ show $ length $ inc [from] to
