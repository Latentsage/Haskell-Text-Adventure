import System.Environment

data Object = Object { name :: String, description :: String }

handleInput :: [Object] -> String -> String
handleInput inventory input
    | getCommand input == "look" = look (unwords (tail (words input))) inventory
    | otherwise = "Not a recognized command."
    where getCommand x = head (words x)
    
look :: String -> [Object] -> String
look target options = case getObjectByName target options of
    Just obj -> description obj
    Nothing -> "I can't find that item."
    
loadGame :: [String] -> [Object]
loadGame (x:xs)
    | head a == "item" = [Object {name= (head (tail a)), description=(head (tail (tail a)))}] ++ loadGame xs
    | otherwise = [Object {name="Whoops", description="Durp"}] ++ loadGame xs
    where a = words x
loadGame [] = []

getObjectByName = getDataByProperty name

getDataByProperty :: Eq b => (a -> b) -> b -> [a] -> Maybe a
getDataByProperty prop val (x:xs)
    | prop x == val = Just x
    | otherwise = getDataByProperty prop val xs
getDataByProperty _ _ [] = Nothing

main = do
    args <- getArgs
    fileText <- readFile (head args)
    io (map (handleInput (loadGame $ lines fileText)))
 
io f = interact (unlines . f . lines)