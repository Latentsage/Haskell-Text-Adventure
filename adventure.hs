module Adventure where

import System.Environment
import Base
import Rooms


data Game = Game Room Inventory
data Action = Action Output (Game -> Game)

textAction out = Action out id

noop = \game -> game

handleInput :: Game -> String -> (Game, String)
handleInput (Game (Room name desc mainLoop exits) inventory) input
    | getCommand input == "look" = case getRest input of
        [] -> applyActions game ((textAction name : [textAction desc]) ++ (look (getRest input) (mainLoop)))
        _ -> applyActions game (look (getRest input) (inventory ++ mainLoop))
    | getCommand input == "go" = applyActions game (move (getRest input) room)
    | otherwise = (game, "Not a recognized command.")
    where
        getCommand x = head (words x)
        getRest x = case words x of
            (x:xs) -> unwords xs
            [] -> ""
        game = (Game room inventory)
        room = (Room name desc mainLoop exits)

applyActions :: Game -> [Action] -> (Game, String)
applyActions game actions = ((foldr (.) id (map applyAction actions) game), unlines (map (\(Action out _) -> out) actions))
    
applyAction :: Action -> Game -> Game
applyAction (Action _ f) game = f game

move :: Description -> Room -> [Action]
move dest (Room _ _ _ portals) = case searchPortals dest portals of
    Just (Portal _ trav room) -> [(Action trav (\(Game _ inventory) -> (Game room inventory)))]
    Nothing -> [(Action "There is no such room" (\game -> game))]

look :: String -> [Object] -> [Action]
look [] things = [textAction "You see around you:"] ++ (map textAction (map (\(Object n d) -> n) things))
look target options = case getObjectByName options target of
    Just (Object n d) -> [Action d (\a -> a)]
    Nothing -> [Action "I can't find that item." id]

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

-- main = do
--     args <- getArgs
--     fileText <- readFile (head args)
--     let items = filterItems $ lines fileText
--     mainLoop (Game startingRoom items)



io f = interact (unlines . f . lines)