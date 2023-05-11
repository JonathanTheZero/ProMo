import Data.Char
import System.IO

-- why???
{-
readFileContent ::String
readFileContent = do
    file <- openFile "one-hit-wonder.txt" ReadMode
    inhalt <- hGetContents file
    inhalt
-}

printText :: IO ()
printText = do
    file <- openFile "one-hit-wonder.txt" ReadMode
    inhalt <- hGetContents file
    putStrLn inhalt
    hClose file

printTextInStyle :: IO ()
printTextInStyle = do
    putStrLn "In Welchem Stil soll der Text geschrieben werden?"
    stil <- getLine
    file <- openFile "one-hit-wonder.txt" ReadMode
    inhalt <- hGetContents file
    if stil == "laut"
        then putStrLn [toUpper x | x <- inhalt]
        else
            if stil == "leise"
                then putStrLn [toLower x | x <- inhalt]
                else putStrLn inhalt
    hClose file

printTextWithName :: IO ()
printTextWithName = do
    putStrLn "In Welchem Stil soll der Text geschrieben werden?"
    stil <- getLine
    putStrLn "Was ist dein Name?"
    name <- getLine
    file <- openFile "one-hit-wonder.txt" ReadMode
    inhalt <- hGetContents file
    let inhaltWName :: String = unlines [unwords [if z == "Macarena" then name else z | z <- words x] | x <- lines inhalt]
    if stil == "laut"
        then putStrLn [toUpper x | x <- inhaltWName]
        else
            if stil == "leise"
                then putStrLn [toLower x | x <- inhaltWName]
                else putStrLn inhaltWName
    hClose file