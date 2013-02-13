-- Use a fold (choosing the appropriate fold will make your code much simpler)
-- to rewrite and improve upon the asInt function from the section called
-- “Explicit recursion”
import Data.Char
asInt_fold :: String -> Int
asInt_fold xs = foldl asInt_inner 0 xs

-- Used by the fold
asInt_inner acc x = acc * 10 + digitToInt x

-- How many of the following Prelude functions can you rewrite using list folds? 2 comments
--
-- any, cycle, words, unlines

