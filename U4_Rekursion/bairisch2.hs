main::IO()
main = originalUebersetzen

-- Teilaufgabe 1 b)
originalUebersetzen :: IO()
originalUebersetzen = do
    original <- readFile "original.txt"
    dict <- readFile "woerterbuch.txt"
    let translation = [if not (null y) then head y else x | x <- words original, let y = passendeEintraege x (lines dict)]
    print $ unwords translation

-- Teilaufgabe 1 a) I
passendeEintraegeIterativ :: IO ()
passendeEintraegeIterativ = do
    print "Welches Wort wollen Sie übersetzen?"
    wort <- getLine
    inhalt <- readFile "woerterbuch.txt"
    let tupels :: [(String, String)] = ([(y !! 0, y !! 1) | x <- lines inhalt, let y = words x, head y == wort])
    if null tupels
        then print wort
        else print . snd $ head tupels

readDict :: IO [(String, String)]
readDict = do
    inhalt <- readFile "woerterbuch.txt"
    let tupels :: [(String, String)] = ([(y !! 0, y !! 1) | x <- lines inhalt, let y = words x])
    return tupels

-- Teilaufgabe 1 a) II
passendeEintraegeConsole :: IO()
passendeEintraegeConsole = do
    print "Welches Wort wollen Sie übersetzen?"
    wort <- getLine
    inhalt <- readFile "woerterbuch.txt"
    print (passendeEintraege wort (lines inhalt))

passendeEintraege :: String -> [String] -> [String]
passendeEintraege _ [] = []
passendeEintraege word (entry:entries)
    | head (words entry) == word = last (words entry) : passendeEintraege word entries --wenn erstes Wort gesuchtes deutsches ist, dann gib bayerisches Wort vor dem rekursiven Aufruf zurück
    | otherwise = passendeEintraege word entries