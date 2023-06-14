-- Interactive console version of bairisch2.hs - Teilaufgabe 1 c)
import System.IO
import System.Environment;

main :: IO ()
main = mainArgsVersion

-- BSP: runghc uebersetzer.hs woerterbuch.txt original.txt
mainArgsVersion :: IO()
mainArgsVersion = do
    args <- getArgs
    dict <- readFile $ args !! 0
    text <- readFile $ args !! 1
    resultFile <- openFile "uebersetzung.txt" WriteMode
    let result = originalUebersetzen text dict
    print result
    hPutStr resultFile result
    hClose resultFile

-- Same functionality as above but through CLI and not with Console arguments
mainCLIVersion :: IO ()
mainCLIVersion = do
    print "Name des Wörterbuchs?"
    dictName <- getLine
    dict <- readFile dictName
    print "Name des Texts?"
    textName <- getLine
    text <- readFile textName
    resultFile <- openFile "uebersetzung.txt" WriteMode
    hPutStr resultFile (originalUebersetzen text dict)
    hClose resultFile

originalUebersetzen :: String -> String -> String
originalUebersetzen text dict = do
    let translation = [if not (null y) then head y else x | x <- words text, let y = passendeEintraege x (lines dict)]
    unwords translation

passendeEintraege :: String -> [String] -> [String]
passendeEintraege _ [] = []
passendeEintraege word (entry : entries)
    | head (words entry) == word = last (words entry) : passendeEintraege word entries -- wenn erstes Wort gesuchtes deutsches ist, dann gib bayerisches Wort vor dem rekursiven Aufruf zurück
    | otherwise = passendeEintraege word entries