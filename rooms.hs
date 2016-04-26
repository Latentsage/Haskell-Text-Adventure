module Rooms where
import Base
import Items

type Traversal = String
data Portal = Portal Description Traversal Room
data Room = Room Name Description [Object] [Portal]

searchPortals desc portals = getDataFromList portals (\(Portal desc2 trav room) -> (desc == desc2))

startingRoom :: Room
startingRoom = Room "Starting Room" "An empty room." [knife] [Portal "The next room" "You step through the door." nextRoom]

nextRoom = Room "The next room" "Also an empty room." [key] [Portal "The starting room" "You step through the door." startingRoom]

foyer = Room "The Grand Foyer" "An elegant cheerily lit room." [] [Portal "" "" dining]

dining = Room "The Dining Room" "A long table is covered in food." [] [Portal "" "" foyer]