--------------------------------------------------------------------------------
-- Professor Johnson's versions
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- My versions
--------------------------------------------------------------------------------

-- take_last :: Int -> [a] -> [a]
take_last i xs
	| i < (length xs) = reverse (take i (reverse xs))
	| otherwise = error("Index > list length")


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

-- 3. down' xs
-- wraps brackets around each top-level element of xs
down' :: [a] -> [[a]]
down' [] = []
down' (x:xs) = [x] : down' xs

-- 4. swapper x y xs
-- returns a list the same as xs, but with all occurrences of x replaced by y and all occurrences of y replaced by x
swapper' :: (Eq a) => a -> a -> [a] -> [a]
swapper' x y [] = []
swapper' x y (e:es)
	| e == x = y : curr_swap 
	| e == y = x : curr_swap
	| otherwise = e : curr_swap
	where curr_swap = swapper' x y es

-- 5. list_set
-- returns a list like xs, except that the n-th element, using zero-based indexing, is x
-- NOTE will not work on infinite lists
list_set' :: [a] -> Int -> a -> [a]
list_set' list index x
	| index < (length list) = (take index list) ++ [x] ++ take_last (index - 1) list
	| otherwise = error("Index > list length")

list_set'' :: [a] -> Int -> a -> [a]
list_set'' list@ (l:ist) index x
	| isLegal && index == 0 = x : ist    
	| isLegal = l : list_set'' ist (index - 1) x
	| otherwise = error("Index > list length")
	where isLegal = (index >= 0 && index < (length list))

-- 6. count_occurrences x xs
-- returns the number of occurrences of x in xs
-- We use a guard here so we can use the where keyword.  Until I learn how to curry this anyway.  Maybe a comprehension instead?
count_occurrences' :: (Eq a) => a -> [a] -> Int
count_occurrences' item xs
	| otherwise = length (filter isEqual xs)
	where isEqual x = item == x

-- 7. filter_in
-- returns the list of those elements in xs that satisfy the predicate f
filter_in' :: (a -> Bool) -> [a] -> [a]
filter_in' func xs = filter func xs

-- 8. every f xs
-- returns False if any element of xs fails to satisfy f, and returns True otherwise
-- every' :: (a -> Bool) -> [a] -> Bool
every' :: Eq a => (a -> Bool) -> [a] -> Bool
every' func xs = xs == filter func xs

-- 9. exists f xs
-- returns True if any element of xs satisfies f, and returns False otherwise
exists' :: (a -> Bool) -> [a] -> Bool
exists' func xs = length (filter func xs) > 0

-- 10. up xs
-- removes a pair of parentheses from each top-level element of xs
-- the value of (up (down xs)) is equivalent to xs, but (down (up xs)) is not necessarily xs
up' :: [[a]] -> [a]
-- up' ([x]:xs) = x : up' xs
-- up' = up
-- down' (x:xs) = [x] : down' xs
-- up' [] = []
up' [] = []
up' ([x]:xs) = x : up' xs


-- 11. merge
-- where loi1 and loi2 are lists of integers that are sorted in ascending order, 
-- returns a sorted list of all the integers in loi1 and loi2
merge' :: [Int] -> [Int] -> [Int]
merge' = merge
