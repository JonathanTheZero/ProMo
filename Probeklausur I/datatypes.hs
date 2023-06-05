data LinkedList a = ListElem a (LinkedList a) | ListSentinel

listToLL :: [a] -> LinkedList a
listToLL [] = ListSentinel
listToLL (x:xs) = ListElem x (listToLL xs)

showLL :: Show a => LinkedList a -> String
showLL ListSentinel = ""
showLL (ListElem x ListSentinel) = show x
showLL (ListElem x xs) = concat [show x, ">>>", showLL xs]
