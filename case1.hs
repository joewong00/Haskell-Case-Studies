-- Case Study 1 - Code Cracker

import Data.Char

let2nat :: Char -> Int
let2nat c = ord c - ord 'a'

nat2let :: Int -> Char
nat2let n = chr (ord 'a' + n)
 
shift :: Int -> Char -> Char
shift n c | isLower c = nat2let ((let2nat c + n) `mod` 26)
          | otherwise = c
 
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n xs = [shift (-n) x | x <- xs]

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]
 
lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

percent :: Int -> Int -> Float
percent n m  = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) xss | x <- ['a'..'z']]
           where xss = lowers xs

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o,e) <- zip os es]
 
 
position :: Eq a => a -> [a] -> Int
position x xs = head [i | (x',i) <- zip xs [0..], x == x']
 
crack :: String -> String
crack xs = decode factor xs
           where
             factor = position (minimum chitab) chitab
             chitab = [chisqr (rotate n xss) table | n <- [0..25]]
             xss = freqs xs
