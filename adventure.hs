import System.Environment
import Base
import Rooms
import Network.Socket


data Game = Game Room Inventory
data Action = Action Output (Game -> Game)

textAction out = Action out id

noop = \game -> game

handleInput :: Game -> String -> (Game, String)
handleInput (Game (Room name desc stuff exits) inventory) input
    | getCommand input == "look" = case getRest input of
        [] -> applyActions game ((textAction name : [textAction desc]) ++ (look (getRest input) (stuff)))
        _ -> applyActions game (look (getRest input) (inventory ++ stuff))
    | getCommand input == "go" = applyActions game (move (getRest input) room)
    | otherwise = (game, "Not a recognized command.")
    where
        getCommand x = head (words x)
        getRest x = case words x of
            (x:xs) -> unwords xs
            [] -> ""
        game = (Game room inventory)
        room = (Room name desc stuff exits)

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

main :: IO()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4242 iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    
    args <- getArgs
    fileText <- readFile (head args)
    let items = filterItems $ lines fileText
    
    mainLoop (Game startingRoom items) sock                              -- unimplemented

mainLoop :: Game -> Socket -> IO()    
mainLoop game = do
    input <- getLine
    putStrLn (snd (handleInput game input))
    mainLoop (fst (handleInput game input))



io f = interact (unlines . f . lines)