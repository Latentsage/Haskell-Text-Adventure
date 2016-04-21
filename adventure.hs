import System.Environment
import Base
import Rooms


data Game = Game Room Inventory
data Action = Action Output (Game -> Game)

noop = \game -> game

handleInput :: Game -> [Action] -> [String] -> [String]
handleInput (Game room inventory) actions (input:xs)
    | getCommand input == "look" = uncurry handleInput (applyActions game (look (getRest input) inventory)) xs
    | getCommand input == "go" = uncurry handleInput (applyActions game (move (getRest input) room)) xs
    | otherwise = handleInput game (actions ++ [(Action "Not a recognized command." id)]) xs
    where
        getCommand x = head (words x)
        getRest x = case words x of
            (x:xs) -> unwords xs
            [] -> ""
        game = (Game room inventory)
handleInput game (x:xs) [] = map (\(Action out _) -> out) (snd (applyActions game (x:xs)))
handleInput _ [] [] = [""]

applyActions :: Game -> [Action] -> (Game, [Action])
applyActions game actions = ((foldr (.) id (map applyAction actions) game), (map (\(Action out _) -> (Action out noop)) actions))
    
applyAction :: Action -> Game -> Game
applyAction (Action _ f) game = f game

move :: Description -> Room -> [Action]
move dest (Room _ _ _ portals) = case searchPortals dest portals of
    Just (Portal _ trav room) -> [(Action trav (\(Game _ inventory) -> (Game room inventory)))]
    Nothing -> [(Action "There is no such room" (\game -> game))]

look :: String -> [Object] -> [Action]
look target options = case getObjectByName options target of
    Just (Object n d) -> [Action d (\a -> a)]
    Nothing -> [Action "I can't find that item." (\game -> game)]

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
    stuff
    
stuff = do
    io (map handleInput (Game (Room "Starter Room" "An empty room" [] []) items) [] )



io f = interact (unlines . f . lines)