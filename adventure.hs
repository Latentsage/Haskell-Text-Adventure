import System.Environment
type Inventory = [Object]
data Game = Game Room Inventory

handleInput :: Game -> String -> String
handleInput (Game rooms inventory) input
    | getCommand input == "look" = look (unwords (tail (words input))) inventory
    | otherwise = "Not a recognized command."
    where getCommand x = head (words x)

look :: String -> [Object] -> String
look target options = case getObjectByName options target of
    Just (Object n d) -> d
    Nothing -> "I can't find that item."

loadItem :: String -> Object
loadItem x = Object (head (tail (words x))) (head (tail (tail (words x))))

filterItems :: [String] -> [Object]
filterItems (x:xs) = case (head (words x)) of
    "item" -> [loadItem (x)] ++ filterItems xs
    _ -> filterItems xs
filterItems [] = []

getObjectByName :: [Object] -> Name -> Maybe Object
getObjectByName ((Object n d):xs) name
    | n == name = Just (Object n d)
    | otherwise = getObjectByName xs name
getObjectByName [] _ = Nothing

main = do
    args <- getArgs
    fileText <- readFile (head args)
    let items = filterItems $ lines fileText
    io (map (handleInput (Game (Room "Starter Room" "An empty room" [] []) items)))
 
io f = interact (unlines . f . lines)