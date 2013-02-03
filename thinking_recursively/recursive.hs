-- 1. duple n x 
-- returns a list containing n copies of x
duple :: Int -> a -> [a]
duple n x = if n > 1 then x:(duple (n - 1) x) else x:[]

-- 2. invert xs
-- where xs is a list of pairs, returns a list with each pair reversed
invert :: [(a,b)] -> [(b,a)]
invert xs = if null xs then [] else (snd (head xs),fst (head xs)) : invert (tail xs)

-- 3. down xs
-- wraps brackets around each top-level element of xs
down :: [a] -> [[a]]
down xs = if null xs then [] else (head xs : []) : (down (tail xs))

-- 4. swapper x y xs
-- returns a list the same as xs, but with all occurrences of x replaced by y and all occurrences of y replaced by x
swapper :: (Eq a) => a -> a -> [a] -> [a]
swapper a b xs = if null xs then [] else (if a == (head xs) then b else if b == (head xs) then a else (head xs)) : swapper a b (tail xs)

-- 5. list_set
-- returns a list like xs, except that the n-th element, using zero-based indexing, is x
list_set :: [a] -> Int -> a -> [a]
list_set xs n x = if n == 0 then x : tail xs else head xs : list_set (tail xs) (n - 1) x

-- 6. count_occurrences x xs
-- returns the number of occurrences of x in xs
count_occurrences :: (Eq a) => a -> [a] -> Int
count_occurrences x xs = if null xs then 0 else count_occurrences x (tail xs) + if x == head xs then 1 else 0

-- 7. filter_in
-- returns the list of those elements in xs that satisfy the predicate f
filter_in :: (a -> Bool) -> [a] -> [a]
filter_in f xs = if null xs then [] else if f (head xs) then head xs:filter_in f (tail xs) else filter_in f (tail xs)

-- 8. every f xs
-- returns False if any element of xs fails to satisfy f, and returns True otherwise
every :: (a -> Bool) -> [a] -> Bool
every f xs = if null xs then True else if f (head xs) then every f (tail xs) else False

-- 9. exists f xs
-- returns True if any element of xs satisfies f, and returns False otherwise
exists :: (a -> Bool) -> [a] -> Bool
exists f xs = if null xs then False else if f (head xs) then True else exists f (tail xs)

-- 10. up xs
-- removes a pair of parentheses from each top-level element of xs
-- the value of (up (down xs)) is equivalent to xs, but (down (up xs)) is not necessarily xs
up :: [[a]] -> [a]
up xs = if null xs then [] else (head xs) ++ up (tail xs)

-- 11. merge
-- where loi1 and loi2 are lists of integers that are sorted in ascending order, 
-- returns a sorted list of all the integers in loi1 and loi2
merge :: [Int] -> [Int] -> [Int]
merge loi1 loi2 = if null loi1 then loi2 else if null loi2 then loi1 else if (head loi1) < (head loi2) then (head loi1):(merge (tail loi1) loi2) else (head loi2):(merge loi1 (tail loi2))



-- 1. duple' n x 
-- returns a list containing n copies of x
duple' :: Int -> a -> [a]
duple' n x
	| n > 0 = x : duple' (n-1) x
	| n == 0 = []
	| otherwise = error("First argument cannot be < 0")

-- 2. invert' xs
-- where xs is a list of pairs, returns a list with each pair reversed
invert' :: [(a,b)] -> [(b,a)]
invert' [] = []
invert' ((a,b):xs) = (b, a) : invert' xs

-- 3. down xs
-- wraps brackets around each top-level element of xs
down' :: [a] -> [[a]]
down' = down

-- 4. swapper x y xs
-- returns a list the same as xs, but with all occurrences of x replaced by y and all occurrences of y replaced by x
swapper' :: (Eq a) => a -> a -> [a] -> [a]
swapper' = swapper

-- 5. list_set
-- returns a list like xs, except that the n-th element, using zero-based indexing, is x
list_set' :: [a] -> Int -> a -> [a]
list_set' = list_set

-- 6. count_occurrences x xs
-- returns the number of occurrences of x in xs
count_occurrences' :: (Eq a) => a -> [a] -> Int
count_occurrences' = count_occurrences

-- 7. filter_in
-- returns the list of those elements in xs that satisfy the predicate f
filter_in' :: (a -> Bool) -> [a] -> [a]
filter_in' = filter_in

-- 8. every f xs
-- returns False if any element of xs fails to satisfy f, and returns True otherwise
every' :: (a -> Bool) -> [a] -> Bool
every' = every

-- 9. exists f xs
-- returns True if any element of xs satisfies f, and returns False otherwise
exists' :: (a -> Bool) -> [a] -> Bool
exists' = exists

-- 10. up xs
-- removes a pair of parentheses from each top-level element of xs
-- the value of (up (down xs)) is equivalent to xs, but (down (up xs)) is not necessarily xs
up' :: [[a]] -> [a]
up' = up

-- 11. merge
-- where loi1 and loi2 are lists of integers that are sorted in ascending order, 
-- returns a sorted list of all the integers in loi1 and loi2
merge' :: [Int] -> [Int] -> [Int]
merge' = merge
