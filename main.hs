import Spreadsheet
import Parser
import Cell

import Data.List

manual = do
	putStrLn "Welcome to Haskell Spreadsheet editor!"
	putStrLn "Please choose an operation:"
	putStrLn "\tnew - new spreadsheet"
	putStrLn "\tsave <filename> - save current spreadsheet"
	putStrLn "\tload <filename> - load spreadsheet from file"
	putStrLn ""
	putStrLn "\tshow - show current spreadsheet"
	putStrLn ""
	putStrLn "\taddRow x - adds row after row x"
	putStrLn "\taddCol x - adds column after column x"
	putStrLn "\tdeleteRow x - deletes row x"
	putStrLn "\tdeleteCol x - deletes col x"
	putStrLn ""
	putStrLn "\tedit <x> <y> <expression> - changes x, y cell to expression if it is supplied, otherwise deletes it. Available expressions are:"
	putStrLn "\t\tasdf - any string"
	putStrLn "\t\t63 - any number"
	putStrLn "\t\tavg <cellrange> - average target cells"
	putStrLn "\t\tsum <cellrange> - sums target cells"
	putStrLn "\t\tmul <cellrange> - calculates a product of target cells"
	putStrLn "\tcell ranges are in format of:"
	putStrLn "\t\ta|b - cell with position of (a, b)"
	putStrLn "\t\ta|b,c|d - cell with position of (a, b) and cell with position of (c, d)"
	putStrLn "\t\ta|b-c|d - cells in a rectangle with (a, b) as the upper-left corner and (c, d) as the bottom-right corner"
	putStrLn ""
	putStrLn "\tquit - exits program"

main =
	work []

work :: Spreadsheet -> IO ()
work spreadsheet = do
	manual
	line <- getLine
	let parameters = words line
	if (parameters!!0 == "quit")
		then return ()
		else do
			case parameters!!0 
				of
				"new" -> do
					putStrLn "Created a new spreadsheet!"
					let s = []
					work s
				"save" -> do
					putStrLn "Saving spreadsheet"
					writeFile (parameters!!1) (show spreadsheet)
					work spreadsheet
				"load" -> do
					putStrLn "Loading spreadsheet"
					file <- readFile $ parameters!!1
					let s = read file
					work s
				"show" -> do
					putStrLn "Presenting current spreadsheet"
					if length spreadsheet /= 0 then
						do
							printSheet spreadsheet
							work spreadsheet
					else
						do
							putStrLn "Empty spreadsheet!"
							work spreadsheet
				"edit" -> do
					putStrLn ("Editing cell with x == " ++ parameters!!1 ++ " and y == " ++ parameters!!2)	
					putStrLn ("New expression: " ++ (intercalate " " (drop 3 parameters)))
					case length parameters of
						3 -> do
							let s = deleteVal spreadsheet (parameters!!1!!0, parameters!!2!!0)
							work s
						_ -> do
							let s = setVal spreadsheet ((parameters!!1!!0, parameters!!2!!0), cell)
								where cell = parse (intercalate " " (drop 3 parameters))
							work s
				"addRow" -> do
					putStrLn "Adding row..."
					let s = addRow spreadsheet (parameters!!1!!0)
					work s
				"addCol" -> do
					putStrLn "Adding column..."
					let s = addCol spreadsheet (parameters!!1!!0)
					work s
				"deleteRow" -> do
					putStrLn "Deleting row..."
					let s = removeRow spreadsheet (parameters!!1!!0)
					work s
				"deleteCol" -> do
					putStrLn "Deleting col..."
					let s = removeCol spreadsheet (parameters!!1!!0)
					work s
				_ -> do
					putStrLn "Unknown operation!"
					work spreadsheet
					