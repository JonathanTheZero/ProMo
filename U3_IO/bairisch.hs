import System.IO

main :: IO ()
main = woerterbuch

hochdeutschUndBairisch :: IO ()
hochdeutschUndBairisch = do
    putStrLn "Deutsches Wort:"
    deutsch <- getLine
    putStrLn "Bairisches Wort:"
    bairisch <- getLine
    if null deutsch || null bairisch
        then return ()
        else do
            putStrLn $ "'" ++ deutsch ++ "' heisst auf Bairisch '" ++ bairisch ++ "'"
            hochdeutschUndBairisch

woerterbuch :: IO ()
woerterbuch = do
    putStrLn "Deutsches Wort:"
    deutsch <- getLine
    putStrLn "Bairisches Wort:"
    bairisch <- getLine
    if null deutsch || null bairisch
        then return ()
        else do
            putStrLn $ "'" ++ deutsch ++ "' heisst auf Bairisch '" ++ bairisch ++ "'"
            datei <- openFile "woerterbuch.txt" AppendMode
            hPutStr datei $ deutsch ++ " " ++ bairisch ++ "\n"
            hClose datei
            woerterbuch
