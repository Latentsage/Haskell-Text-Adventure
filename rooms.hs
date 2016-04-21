module Rooms where
import Base

type Traversal = String
data Portal = Portal Description Traversal Room
data Room = Room Name Description [Object] [Portal]

searchPortals desc portals = getDataFromList portals (\(Portal desc2 trav room) -> (desc == desc2))

startingRoom :: Room
startingRoom = Room "Starting Room" "An empty room." [] [Portal "The next room" "You step through the door." nextRoom]

nextRoom = Room "The next room" "Also an empty room." [] [Portal "The starting room" "You step through the door." startingRoom]