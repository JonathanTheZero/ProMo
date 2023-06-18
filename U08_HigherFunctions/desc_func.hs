{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}

length' :: Num a1 => [a2] -> a1
length' [] = 0
length' (x : xs) = 1 + length' xs

{-

Geltungsbereich G = L := {|l| elem N ; Liste l}
Eine Liste kann niemals eine negative Länge haben, die Länge ist also mindestens >= 0. 
Der Fall l = 0 ist mit [] als Rekursionsanker explizit festgelegt.
Für alle anderen Listen gilt: gebe 1 + length' der Liste ohne Kopf zurück. 
Da die Länge wie bereits gezeigt positiv ist, schrumpft die Liste immer weiter, bis die Länge 0 erreicht. An dieser Stelle greift dann der Anker. 
Folglich gibt es keine Möglichkeit, dass die Funktion nicht terminiert (ausnahme: Unendliche Liste, diese ist aber nicht Teil der definierten Geltungsbereichs.)

-}