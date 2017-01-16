{-# LANGUAGE PatternGuards #-}

module Parser where

import Cell
import Data.List
import Text.Read

parse :: String -> Cell
parse str = case w!!0 of
    "avg" -> CellExp (GCell (parseExpression str))
    "sum" -> CellExp (GCell (parseExpression str))
    "mul" -> CellExp (GCell (parseExpression str))
    _ -> case val of
        Just v -> CellDouble (GCell v)
        Nothing -> CellString (GCell str)
    where
        w = words str
        val = readMaybe str

parseExpression :: String -> Expression
parseExpression ('a':'v':'g':' ':str) = Expression (getFields str) Average
parseExpression ('s':'u':'m':' ':str) = Expression (getFields str) Addition
parseExpression ('m':'u':'l':' ':str) = Expression (getFields str) Multiplication

splitBy delimiter = foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs

getFields :: String -> [Key]
getFields str = Data.List.concat (map parseFieldRanges (splitBy ',' str))

parseFieldRanges :: String -> [Key]
parseFieldRanges str = case length borders of
    1 -> [parseField (borders!!0)]
    2 -> [(a, b) | a <- [fx..lx], b <- [fy..ly]]
    where
        borders = splitBy '-' str
        (fx, fy) = parseField (borders!!0)
        (lx, ly) = parseField (borders!!1)

parseField :: String -> Key
parseField string = (result!!0!!0, result!!1!!0)
    where result = splitBy '|' string
