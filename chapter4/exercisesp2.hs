-- Use a fold (choosing the appropriate fold will make your code much simpler)
-- to rewrite and improve upon the asInt function from the section called
-- “Explicit recursion”
import Data.Char
asInt_fold :: String -> Int
asInt_fold xs = foldl asInt_inner 0 xs
    where
        asInt_inner acc x = acc * 10 + digitToInt x

-- How many of the following Prelude functions can you rewrite using list folds?
-- any :: (a -> Bool) -> [a] -> Bool
-- cycle :: [a] -> [a]
-- words :: String -> [String]
-- unlines :: [String] -> String


any' func xs = foldl any_inner False xs
    where
        any_inner bool item
            | bool = bool
            | otherwise = func item

-- cycle is extrapolating to a larger list, so fold/reducing is not appropriate, and is not pheasable.
-- words could not be written as a fold, since it is creating a deeper-nested list, rather than reducing to a single element
-- unlines could be rewritten as a fold, since it is reducing.  I can't think of how to write it though :(
