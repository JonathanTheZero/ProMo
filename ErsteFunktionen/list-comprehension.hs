combinations = 
    let substantive = ["Student", "Professor", "Tutor"]
        adjektive = ["fauler", "fleissiger", "hilfreicher"] in
    [a ++ " " ++ s | a <- adjektive, s <- substantive]
