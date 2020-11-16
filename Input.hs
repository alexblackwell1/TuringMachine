module Input where

main :: IO()
main = do
    putStrLn "Enter file name: "
    x <- getLine
    contents <- readFile x
    let loop = do
        putStrLn "Enter a string or quit command"
        x <- getLine
        case x of
            "quit" -> do
                putStrLn "Bye!"
                return()
            _ -> do
                putStrLn x
                loop
    loop