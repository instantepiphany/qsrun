--module Qsrun where
import System.IO
import System.Directory
import System.Cmd
import System.Environment
import Data.Char
import Data.List
import qualified Graphics.UI.Gtk 
--import Graphics.UI.Gtk.Glade

{-
main = do

    withFile "commands.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStrLn contents
        indice <- getChar
        putStrLn (lines contents !! ((digitToInt indice) - 1))
    	rawSystem (head $ words $ lines contents !! ((digitToInt indice) - 1)) (tail $ words $ lines contents !! ((digitToInt indice) - 1)))

data GUI = GUI {
	mainWin :: Window,
	button1 :: Button,
	button2 :: Button,
	button3 :: Button,
	button4 :: Button,
	button5 :: Button,
	button6 :: Button,
	button7 :: Button,
	button8 :: Button,
	button9 :: Button}
-}


dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("run", run)
            , ("add", add)
			, ("view", view)
			, ("remove", remove)
			]


main = do
	initArgs <- getArgs
	if null initArgs then interactive
		else dispatchInit

dispatchInit = do
	(command:args) <- getArgs
	putStrLn "Command arguments must be supplied"

{-	let (Just action) = lookup command dispatch
	action args-}
                                                                                                

run :: [String] -> IO ()
run [fileName, numberString] = do
	handle <- openFile fileName ReadMode
	contents <- hGetContents handle
	let qsScripts = lines contents
	    runCommand = (qsScripts !! ((read numberString) - 1))
	putStrLn ""
	putStrLn "Executing: \n"
	putStrLn runCommand
	hClose handle


interactive :: IO ()
interactive = do
	putStrLn "qsrun is running in interactive mode!"



add :: [String] -> IO ()
add [fileName, qsItem] = appendFile fileName (qsItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
	contents <- readFile fileName
	let qsScripts = lines contents
	    numberedScripts = zipWith (\n line -> show n ++ " - " ++ line) [1..] qsScripts
	putStr $ unlines numberedScripts


remove :: [String] -> IO ()
remove [fileName, numberString] = do
	handle <- openFile fileName ReadMode
	(tempName, tempHandle) <- openTempFile "." "temp"
	contents <- hGetContents handle
	let number = (read numberString - 1)
	    qsScripts = lines contents
	    newqsScripts = delete (qsScripts !! number) qsScripts
	hPutStr tempHandle $ unlines newqsScripts
	hClose handle
	hClose tempHandle
	removeFile fileName
	renameFile tempName fileName



{-

	handle <- openFile "commands.txt" ReadMode
	(tempName, tempHandle) <- openTempFile "." "temp"
	contents <- hGetContents handle
	putStrLn "\nCommands: \n"
	putStrLn (contents ++ "\n")
	putStr "Which command would you like to run? \n"
	indice <- getChar
	let command = (head $ words $ lines contents !! ((digitToInt indice) - 1))
	    arguments = (tail $ words $ lines contents !! ((digitToInt indice) - 1))
	
	putStr command
	putStr " "
	putStrLn (show $ unwords arguments)
	rawSystem command arguments
	junk <- getChar
	hClose handle
	hClose tempHandle
--	renameFile tempName "junk"
    
    -- Cleanup
	removeFile (drop 2 tempName)

	

	--rawSystem "ls" ["/home/jordan/"]
    
-}

{-
main = do      
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents   
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks   
    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"   
    numberString <- getLine   
    let number = read numberString   
        newTodoItems = delete (todoTasks !! number) todoTasks   
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"\
-}