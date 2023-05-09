main ::IO()
main = do
    print "Welches Wort wollen Sie übersetzen?"
    wort <- getLine
    inhalt <- readFile "woerterbuch.txt"
    print (passendeEintraege wort (lines inhalt))

passendeEintraegeIterativ :: IO ()
passendeEintraegeIterativ = do
    print "Welches Wort wollen Sie übersetzen?"
    wort <- getLine
    inhalt <- readFile "woerterbuch.txt"
    let tupels:: [(String, String)] = ([(y !! 0, y !! 1) | x <- lines inhalt, let y = words x, head y == wort])
    if null tupels
        then print wort
        else print $ snd $ head tupels

readDict :: IO [(String, String)]
readDict = do
    inhalt <- readFile "woerterbuch.txt"
    let tupels:: [(String, String)] = ([(y !! 0, y !! 1) | x <- lines inhalt, let y = words x])
    return tupels

passendeEintraege :: String -> [String] -> [String]
passendeEintraege s list = do
    let first = drop 1 list
    if head $ words first == s
        then passendeEintraege ++ first
        else None

