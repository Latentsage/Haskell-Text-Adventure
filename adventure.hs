data Object = Object { name :: String, description :: String }

handleInput :: [Object] -> String -> String
handleInput inventory input
    | getCommand input == "look" = look (unwords (tail (words input))) inventory
    | otherwise = "Not a recognized command."
    where getCommand x = head (words x)
    
look :: String -> [Object] -> String
look target options = case getObjectByName options target of
    Just obj -> description obj
    Nothing -> "I can't find that item."

getObjectByName :: [Object] -> String -> Maybe Object
getObjectByName (x:xs) search
    | name x == search = Just x
    | otherwise = getObjectByName xs search
getObjectByName [] _ = Nothing

main = io (map (handleInput [Object {name="sword", description="A sword"}]))
 
io f = interact (unlines . f . lines)