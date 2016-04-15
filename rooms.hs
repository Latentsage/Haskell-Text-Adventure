module Rooms where

data Portal = Portal Room Description
data Room = Room Name Description [Object] [Portal]