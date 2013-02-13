import commandLine
-- Write a function splitWith that acts similarly to words, but takes a
-- predicate and a list of any type, and splits its input list on every
-- element for which the predicate returns False
splitWith :: (a -> Bool) -> [a] -> [[a]]


-- Using the command framework from the section called
-- “A simple command line framework”, write a program
-- that prints the first word of each line of its input.
