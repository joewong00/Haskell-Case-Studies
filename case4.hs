-- Case Study 4

import Data.List (sort)
type Party = String
type Ballot = [Party]

count :: Eq a => a -> [a] -> Int
count v xs = length (filter (== v) xs)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

frequency :: Eq a => [a] -> [(Int,a)]
frequency xs = [ (count x xs, x) | x <- rmdups xs ]

votes :: [Party]
votes = ["Red","Blue","Green","Blue","Blue","Red"]

results :: [Party] -> [(Int,Party)]
results = sort . frequency

winner :: [Party] -> Party
winner = snd . last . results 

rmempty :: Eq a => [[a]] -> [[a]]
rmempty xs = filter (/= []) xs

remove :: Eq a => a -> [[a]] -> [[a]]
remove v xs = map (filter (/= v)) xs

ballots :: [Ballot]
ballots = [b1,b2,b3,b4,b5,b6]

b1 = ["Blue","Green"]
b2 = ["Green","Blue","Red"]
b3 = ["Blue"]
b4 = ["Red","Green"]
b5 = ["Blue","Red","Green"]
b6 = ["Green","Red"]

rank :: [Ballot] -> [Party]
rank xs = map snd (results (map head xs))

election :: [Ballot] -> Party
election bs = case rank (rmempty bs) of 
               [p]    -> p
               (p:ps) -> election (remove p bs)




