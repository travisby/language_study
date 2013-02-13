import System.Environment (getArgs)
import Data.Char

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = id


-- Using the command framework from the section called
-- ¿A simple command line framework¿, write a program
-- that converts the case  of its input.

convertAllCases = map convert

convert x
    | isLower x = toUpper x
    | otherwise = toLower x
