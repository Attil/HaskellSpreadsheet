module Spreadsheet where

import Cell
import Data.Char

type Entry a = (Key, a)

type Map a = [Entry a]

type Spreadsheet = Map Cell
type Field = Entry Cell

getVal :: Spreadsheet -> Key -> Double
getVal sheet key = case lookup key sheet of
    Just (CellDouble v) -> value v
    Just (CellExp (GCell v)) -> case op v of
        Addition -> sum [getVal sheet i | i <- references v]
        Multiplication -> product [getVal sheet i | i <- references v]
        Average -> s/l where
            s = sum [getVal sheet i | i <- references v]
            l = realToFrac (length (references v))
    _ -> 0

showVal :: Spreadsheet -> Key -> String
showVal sheet key = case lookup key sheet of
    Just (CellDouble _) -> show $ getVal sheet key
    Just (CellExp _) -> show $ getVal sheet key
    Just (CellString (GCell v)) -> v
    Nothing -> ""

changeVal :: Spreadsheet -> Field -> Spreadsheet
changeVal sheet (k, v) = map replace sheet
    where replace (key, value)  | key == k = (key, v)
                                | otherwise = (key, value)

addVal :: Spreadsheet -> Field -> Spreadsheet
addVal sheet field = field:sheet

deleteVal :: Spreadsheet -> Key -> Spreadsheet
deleteVal [] key = []
deleteVal ((k, v):sheet) key    | key == k = sheet
                                | otherwise = (k, v):deleteVal sheet key

setVal :: Spreadsheet -> Field -> Spreadsheet
setVal sheet (k, v) = case lookup k sheet of
    Just _ -> changeVal sheet (k, v)
    Nothing -> addVal sheet (k, v)

removeRow :: Spreadsheet -> Char -> Spreadsheet
removeRow sheet k = map reduce (filter (\((x, y), v) -> x /= k) sheet)
    where reduce ((x, y), v) | x > k = ((chr $ ord x - 1, y), v)
                               | otherwise = ((x, y), v)

removeCol :: Spreadsheet -> Char -> Spreadsheet
removeCol sheet k = map reduce (filter (\((x, y), v) -> y /= k) sheet)
    where reduce ((x, y), v) | y > k = ((x, chr $ ord y - 1), v)
                               | otherwise = ((x, y), v)

addRow :: Spreadsheet -> Char -> Spreadsheet
addRow sheet k = map increase sheet
    where increase ((x, y), v) | x > k = ((chr $ ord x + 1, y), v)
                               | otherwise = ((x, y), v)

addCol :: Spreadsheet -> Char -> Spreadsheet
addCol sheet k = map increase sheet
    where increase ((x, y), v) | y > k = ((x, chr $ ord y + 1), v)
                               | otherwise = ((x, y), v)

getX :: Field -> Char
getX ((x, y), v) = x

getY :: Field -> Char
getY ((x, y), v) = y

getWidth :: Spreadsheet -> (Char, Char)
getWidth sheet = (minimum (map getX sheet), maximum (map getX sheet))

getHeight :: Spreadsheet -> (Char, Char)
getHeight sheet = (minimum (map getY sheet), maximum (map getY sheet))

printHeader :: Char -> Char -> IO ()
printHeader x maxX = do
    putStr (show x)
    putStr "\t\t|"
    if x == maxX then
        do
            putStrLn ""
            return ()
    else
        printHeader (chr (ord x + 1)) maxX

printSheet :: Spreadsheet -> IO ()
printSheet sheet = printSheet' sheet minX minY
    where
        (minX, _) = getWidth sheet
        (minY, _) = getHeight sheet

printSheet' :: Spreadsheet -> Char -> Char -> IO ()
printSheet' sheet x y = do
    let (minX, maxX) = getWidth sheet
    let (minY, maxY) = getHeight sheet
    if x == minX && y == minY then
        do
            putStr "\t\t|"
            printHeader minY maxY
    else
        do
            return ()
    if y == minY then
        do
            putStr (show x)
            putStr "\t\t|"
    else
        do
            return ()
    if y == maxY then
        do
            putStr (showVal sheet (x, y))
            putStrLn "\t\t|"
    else
        do
            putStr (showVal sheet (x, y))
            putStr "\t\t|"
    case () of _    | y >= maxY && x >= maxX -> return ()
                    | y == maxY -> printSheet' sheet (chr (ord x + 1)) minY
                    | otherwise -> printSheet' sheet x (chr (ord y + 1))