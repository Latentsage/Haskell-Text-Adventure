data Object = Object { name :: String, description :: String }

handleInput :: [Object] -> String -> String
handleInput inventory input
    | getCommand input == "look" = look unwords $ tail words $ input inventory
    | otherwise = "Not a recognized command."
    where getCommand x = head (words x)
    
look :: String -> [Object] -> String
look target options = description $ getObjectByName options target

getObjectByName :: [Object] -> String -> Maybe Object
getObjectByName (x:xs) search
    | name x == search = Just x
    | otherwise = getObjectByName xs name
getObjectByName [] = Nothing

main = do let inventory = [Object {name="sword", description="A sword"}]
        io (map (handleInput inventory))
 
io f = interact (unlines . f . lines)