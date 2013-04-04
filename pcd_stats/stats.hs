import Control.Monad
main = forever $ do
    putStrLn "Yo, please give us some numbas"
    inputLst <- getLine
    putStrLn $ doWork inputLst

convertFromStringToListOfNumbas str = map myRead (words str)

myRead x = read x :: Int

doWork str = "The min of the list: " ++ show (minimum ourLst) ++ "\n" ++ "The max of the list: " ++ show (maximum ourLst) ++ "\n"
   where ourLst = convertFromStringToListOfNumbas str
