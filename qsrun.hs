--module Qsrun where
import System.IO
import System.Directory
import System.Cmd
import System.Environment
import Data.Char
import Data.List
import Data.Either
import Graphics.UI.Gtk 
import Graphics.UI.Gtk.Glade


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




incorrectArgs :: String -> IO ()
incorrectArgs "add" = do
	putStrLn "Error: Incorrect syntax. Correct syntax is 'qsrun add filename command'"

incorrectArgs "run" = do
	putStrLn "Error: Incorrect syntax. Correct syntax is 'qsrun run filename $commandindex' - Find command index easily by doing 'qsrun view filename'."

incorrectArgs "view" = do
	putStrLn "Error: Incorrect syntax, correct syntax is 'qsrun view filename.txt'"

incorrectArgs "remove" = do
	putStrLn "Error: Incorrect syntax. Correct syntax is 'qsrun remove filename $commandindex' - Find command index easily by doing 'qsrun view filename'."

incorrectArgs _ = do
	putStrLn "Incorrect argument. Valid args are add/view/remove/run. Alternatively run without args for interactive mode."


main = do
	initArgs <- getArgs
	if null initArgs then interactive
		else dispatchInit

dispatchInit = do
	(command:args) <- getArgs
	newExecute command args


newExecute "run" b = run b
newExecute "add" b = add' b
newExecute "view" b = view b
newExecute "remove" b = remove' b
newExecute _ b = incorrectArgs (show b)


run :: [String] -> IO ()
run [fileName, numberString] = do
	handle <- openFile fileName ReadMode
	contents <- hGetContents handle
	let qsScripts = lines contents
	    runCommand = (qsScripts !! ((read numberString) - 1))
	    command = (head $ words $ runCommand)
	    args = (tail $ words $ runCommand)
	
	putStrLn "Executing: "
	putStr runCommand
	putStrLn ""
	rawSystem command args
	hClose handle
run _ = incorrectArgs "run"

interactive :: IO ()
interactive = do
	putStrLn "qsrun is running in interactive mode! Ctrl-C to exit.\n"
	
	putStrLn "Choose an operation: add, view, remove or run.\n"
	opName <- getLine
        putStrLn "\nPlease enter a filename."
	fileName <- getLine
	let view' = do 
	         view ([fileName])
	         error ("Finished printing " ++ fileName)
          in
	    case (opName) of
	       "view" -> view'
        view ([fileName])
        putStrLn ("What do you want to " ++ opName)
        args <- fmap lines getLine
        newExecute opName (fileName:args)


add' :: [String] -> IO ()
add' [fileName, qsItem] = appendFile fileName (qsItem ++ "\n")
add' _ = incorrectArgs "add" 


view :: [String] -> IO ()
view [fileName] = do
	contents <- readFile fileName
	let qsScripts = lines contents
	    numberedScripts = zipWith (\n line -> show n ++ " - " ++ line) [1..] qsScripts
	putStr $ unlines numberedScripts
view _ = incorrectArgs "view"


remove' :: [String] -> IO ()
remove' [fileName, numberString] = do
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
remove' _ = incorrectArgs "remove"
