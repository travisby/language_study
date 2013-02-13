-- Write a function splitWith that acts similarly to words, but takes a
-- predicate and a list of any type, and splits its input list on every
-- element for which the predicate returns False

-- So for this, we need to prepend to the list if true, and concatenate a [] if false.
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = [[]]
splitWith func xs = xs_first : (splitWith func xs_last)
    where
        xs_first list = (takeWhile func list)
        xs_last list = (dropWhile func list)
