handleInput :: String -> String
handleInput input
    | getCommand input == "look" = look tail (words input)
    | otherwise = "Not a recognized command."
    where getCommand x = head (words x)
    
look :: String -> String
look target = 

main = io (map handleInput)
 
io f = interact (unlines . f . lines)