import System.Environment
type Name = String
type Description = String
data Object = Object Name Description
data Portal = Portal Room Description
data Room = Room Name Description [Object] [Portal]
data Game = Game [Room] [Object]

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
loadItem x = [Object (head (tail (words x))) (head (tail (tail (words x))))]

filterItems :: [String] -> [Object]
filterItems (x:xs) = case (head (words x)) of
    "item" -> [x] ++ filterItems xs
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
    let items = loadItems $ filterItems $ lines fileText
    io (map (handleInput (Game [] items)))
 
io f = interact (unlines . f . lines)