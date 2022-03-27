import Data.List -- for the sort

ballots :: [[String]]
ballots = [["Red", "Green"],
          ["Blue"],
          ["Green", "Red", "Blue"],
          ["Blue", "Green", "Red"],
          ["Green"]]
          
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a =>  [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

-- map head sur ballots
-- applique head sur la liste de ballots
-- result va prendre la liste et renvoyer un tuple
rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

-- deux cas soit il n'y a q'un type qui reste c'est la réponse
-- soit on recommence
winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
  [c] -> c
  (c:cs) -> winner' (elim c bs)
  
