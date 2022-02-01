-- Case Study 2 - String Transmitter

import Data.Char

type Bit = Int

tobin :: Int -> [Bit]
tobin n = if n < 2 then [n] -- base case
          else [n `mod` 2] ++ tobin (n `div` 2) -- inductive step

make8 :: [Bit] -> [Bit]
make8 [] = [] -- base case
make8 xs = if (length xs) < 8 then make8 (xs ++ [0]) -- inductive step
           else take 8 xs

encode :: String -> [Bit]
encode "" = []
encode (x:xs) = make8 (tobin (ord x)) ++ encode xs

frombin :: [Bit] -> Int
frombin [] = 0
frombin xs = last xs * 2^(n-1) + frombin (init xs)
             where n = length xs

chop8 :: [Bit] -> [[Bit]]
chop8 [] = [[]]
chop8 xs = if (length xs) <= 8 then [make8 xs]
           else [take 8 xs] ++ chop8 (drop 8 xs)

decode :: [Bit] -> String
decode [] = ""
decode xs = if (length xs) <= 8 then [chr (frombin (make8 xs))]
            else [chr (frombin (head (chop8 xs)))] ++ decode (drop 8 xs)

send :: String -> String
send = decode . encode
