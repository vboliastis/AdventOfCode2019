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
        in case (checkSorted x) && (checkDuplicate x) of
            True -> inc (plus1:x:xs) m
            False -> inc (plus1:xs) m

checkSorted :: String -> Bool
checkSorted s = (s == sort s)

cmp :: String -> String -> String
cmp sl@(s:_) c = if s == cc then (s:sl) else [cc]
    where cc = head c

aggs :: String -> [String]
aggs s = scanl1 cmp $ map (\c -> [c]) s

checkDuplicate :: String -> Bool
checkDuplicate ls = any (\s -> (length s) == 2) $ scanl1 normalize lls
    where
        lls = reverse $ aggs ls
        normalize a b = if head a == head b then a else b

main = do
    [input] <- getArgs
    let [from, to] = splitOn "-" input
    putStr $ show $ length $ inc [from] to
