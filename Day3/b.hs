#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

import System.Environment
import Data.Char
import Data.List.Split
import Data.List
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

unwrapInputPath :: [String] -> [(Int, Int)]
unwrapInputPath path = tail $ aggregate (Prelude.map unwrap path) [(0,0)]

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [1..]

crosses :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Maybe Int)]
crosses xs ys = [(fst a, (snd a) `Data.List.elemIndex` ys) | a <- zipWithIndex xs, (snd a) `member` (fromList ys)]

found :: Maybe Int -> Bool
found (Just _) = True
found _ = False

crosses' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Maybe Int)]
crosses' xs ys = Data.List.filter (found . snd) $ Prelude.map (\x -> (fst x, (snd x) `Data.List.elemIndex` ys)) (zipWithIndex xs)

sumIndices :: (Int, Maybe Int) -> Int
sumIndices (x, Just y) = x + y + 1

main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let [inputPathA, inputPathB] = Prelude.map (splitOn ",") $ lines input
    let pointsA = unwrapInputPath inputPathA
    let pointsB = unwrapInputPath inputPathB
    putStr $ show $ minimum $ Prelude.map sumIndices $ Prelude.take 5 $ crosses' pointsA pointsB
    

