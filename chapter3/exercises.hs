-- 1 rewrite length
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- 4 Turn a list into a palindrome
palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs

-- 5 Is a list a palindrome?
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 7 Create a join function
join' :: String -> [String] -> String
join' _ [] = []
join' _ [x] = x
join' joiner (x:xs) = x ++ joiner ++ (join' joiner xs)
