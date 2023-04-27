import Control.Monad

main :: IO ()
main = do
    putStrLn "Was lernst du?"
    sprache <- getLine
    when (sprache == "Haskell") $ do { putStrLn "Gute Wahl!" }
    putStrLn ("Viel Erfolg beim " ++ sprache ++ " lernen!")

