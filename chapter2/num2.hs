-- Returns the second last item in the list
lastButOne xs = xs !! (length xs - 2)
lastButOne' xs = reverse xs !! 1
