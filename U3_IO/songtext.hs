import Data.Char
import System.IO

-- why???
{-
readFileContent ::String
readFileContent = do
    rawText <- openFile "one-hit-wonder.txt" ReadMode
    inhalt <- hGetContents rawText
    inhalt
-}

printText :: IO ()
printText = do
    rawText <- openFile "one-hit-wonder.txt" ReadMode
    inhalt <- hGetContents rawText
    putStrLn inhalt

printTextInStyle :: IO ()
printTextInStyle = do
    putStrLn "In Welchem Stil soll der Text geschrieben werden?"
    stil <- getLine
    rawText <- openFile "one-hit-wonder.txt" ReadMode
    inhalt <- hGetContents rawText
    if stil == "laut"
        then putStrLn [toUpper x | x <- inhalt]
        else
            if stil == "leise"
                then putStrLn [toLower x | x <- inhalt]
                else putStrLn inhalt

printTextWithName :: IO ()
printTextWithName = do
    putStrLn "In Welchem Stil soll der Text geschrieben werden?"
    stil <- getLine
    putStrLn "Was ist dein Name?"
    name <- getLine
    rawText <- openFile "one-hit-wonder.txt" ReadMode
    inhalt <- hGetContents rawText
    let inhaltWName :: String = unlines [unwords [if z == "Macarena" then name else z | z <- words x] | x <- lines inhalt]
    if stil == "laut"
        then putStrLn [toUpper x | x <- inhaltWName]
        else
            if stil == "leise"
                then putStrLn [toLower x | x <- inhaltWName]
                else putStrLn inhaltWName