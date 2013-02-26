-- Rewrite the heads function from our last Quiz so that is uses foldl
-- (instead of foldr). Try to do this using an accumulator function
-- that is point-free, like my version below that uses function composition;
-- if you can't, then just provide a pointy implementation of the necessary
-- function.

-- heads :: [[a]] -> [Int]
-- heads xss = foldr ((:) . head) [] xss 
heads xs = foldl (\ y ys -> y ++ [head ys]) [] xs
