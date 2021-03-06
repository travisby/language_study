data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show,Eq)
empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

-- It should add spaces to a document until it is the given number of columns
-- wide. If it is already wider than this value, it should add no spaces.

-- What we do is we fold over a list of Documents with the function "fillInner"
-- fillInner will Concatenate ' ' to a string until it is the correct size.
-- when a Doc is the correct size, it will Concatenate with a Line
fill :: Int -> Doc -> Doc
fill size doc = foldr fillInner Empty listOfLines
	where
		listOfLines = map Text (lines (compact doc))
		fillInner myDoc string
			| length (compact myDoc) >= size = string <> myDoc <> Line
			| otherwise = fillInner (myDoc <> Char ' ') string

-- Our pretty printer does not take nesting into account. Whenever we open
-- parentheses, braces, or brackets, any lines that follow should be indented
-- so that they are aligned with the opening character until a matching closing
-- character is encountered. 
-- 
--data Doc = Empty
--         | Char Char
--         | Text String
--         | Line
--         | Concat Doc Doc
--         | Union Doc Doc

nest :: Int -> Doc -> Doc
nest i (Char a `Concat` b)
        | isOpenner a = Char a `Concat` nest (i+1) b
        | isCloser a = Char a `Concat` nest (i-1) b
        | otherwise = Char a `Concat` nest i b
nest i (Line `Concat` b) = Line `Concat` Text (take i (cycle [space])) `Concat` b
nest i (a `Concat` b) = nest i a `Concat` nest i b
-- Empty, Union
nest _ doc = doc

space :: Char
space = '\t'
isOpenner :: Char -> Bool
isOpenner '{' = True 
isOpenner '(' = True 
isOpenner _ = False

isCloser :: Char -> Bool
isCloser '}' = True 
isCloser ')' = True 
isCloser _ = False

main :: IO()
main = putStrLn $ pretty 2 $ nest 10 doc 

-- doc = Text "Hello" <> Char '{' <> Line <> Text "World!" <> Char '}' <> Line <> Text "Bye"
doc :: Doc
doc = Concat (Text "Hello") (Concat (Char '{') (Concat Line (Concat (Text "World!") (Concat (Char '}') (Concat Line (Text "Bye"))))))
