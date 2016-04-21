module Base where

type Name = String
type Description = String
type Output = String
type Inventory = [Object]



data Object = Object Name Description

getDataFromList :: [a] -> (a -> Bool) -> Maybe a
getDataFromList (x:xs) f = case f x of
    True -> Just x
    False -> getDataFromList xs f
getDataFromList [] _ = Nothing