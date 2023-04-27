main :: IO ()
main = do
    z <- getLine
    if null z
        then return ()
        else do
            if istPalindrom z
                then putStrLn $ z ++ " ist ein Palindrom."
                else putStrLn $ z ++ " ist kein Palindrom."
            main -- rekursiver Aufruf anstelle einer Schleife

istPalindrom w = w == reverse w