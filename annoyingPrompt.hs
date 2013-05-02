annoyingPrompt :: Int -> IO String
annoyingPrompt n = do
    putStrLn "Enter something: "
    line <- getLine
    guess <- return (read line :: Int)
    if guess == n then return "Go Home number you're drunk"
    else annoyingPrompt n
