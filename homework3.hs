module Golf where

import Data.List
import Debug.Trace

-- Exercise 1

skips :: [a] -> [[a]]
skips [] = []
skips xs = skips' 1 (length xs) xs
  where
    skips' :: Int -> Int -> [a] -> [[a]]
    skips' n len xs
      | n > len = []
      | otherwise = nNth n xs : skips' (n + 1) len xs

nNth :: Int -> [a] -> [a]
nNth 0 _ = []
nNth 1 xs = xs
nNth n xs = nNth' 1 n xs
  where
    nNth' :: Int -> Int -> [a] -> [a]
    nNth' _ _ [] = []
    nNth' cur n (x:xs)
      | cur == n = x : nNth' 1 n xs
      | otherwise = nNth' (cur + 1) n xs

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (l:c:r:xs)
  | c > l && c > r = c : localMaxima (c:r:xs)
  | otherwise = localMaxima (c:r:xs)

-- Exercise 3

histogram :: [Integer] -> String
histogram [] = []
histogram xs = unlines $ asterisk $ fillEmpty $ group $ sort xs


asterisk :: [[Integer]] -> [String]
asterisk xs = asterisk' xs
  where
    asterisk' :: [[Integer]] -> [String]
    asterisk' (x:xs)
      | concat xs == [] = ["==========\n0123456789"]
      | otherwise = column (x:xs) : asterisk' (getOne (x:xs))

column :: [[Integer]] -> String
column [] = ""
column (x:xs) = intStar x ++ column (xs)

intStar :: [Integer] -> String
intStar [] = " "
intStar xs = "*"

getOne :: [[Integer]] -> [[Integer]]
getOne [] = []
getOne (x:xs) = getOne' x : getOne xs
  where
    getOne' :: [Integer] -> [Integer]
    getOne' [] = []
    getOne' (x:xs) = xs

fillEmpty :: [[Integer]] -> [[Integer]]
fillEmpty xs = fillEmpty' xs 0
  where
    fillEmpty' :: [[Integer]] -> Integer -> [[Integer]]
    fillEmpty' [] n = [[] | _ <- [n..9]]
    fillEmpty' (x:xs) n
      | n == 9 = []
      | x' < [n] = [] : fillEmpty' (x:xs) (n + 1)
      | x' == [n] = x : fillEmpty' xs (n + 1)
      | otherwise = []:fillEmpty' (x:xs) (n + 1)
        where
          x' = take 1 x
