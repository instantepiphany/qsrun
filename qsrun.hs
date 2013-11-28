--module Qsrun where
import System.IO
import System.Directory
import System.Cmd
import System.Environment
import Data.Char
import Data.List
import Data.Either
import Graphics.UI.Gtk 
import Graphics.UI.Gtk.Builder

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
	button9 :: Button,
	label1 :: Label,
	label2 :: Label,
	label3 :: Label,
	label4 :: Label,
	label5 :: Label,
	label6 :: Label,
	label7 :: Label,
	label8 :: Label,
	label9 :: Label}




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
	if null initArgs then modeGui
		else dispatchInit


modeGui :: IO ()
modeGui = do
        initGUI
        gui <- loadGui "qsrun.glade" "commands.txt" 
	connectGui gui
	mainGUI

loadGui gladepath fileName = do 

       -- Import gui from file
       builder <- builderNew
       builderAddFromFile builder gladepath

       -- bind main window, buttons, and labels
       mw <- builderGetObject builder castToWindow "mainWindow"
       [qs1,qs2,qs3,qs4,qs5,qs6,qs7,qs8,qs9] <-
           mapM (builderGetObject builder castToButton)
	   ["button1","button2","button3","button4","button5","button6","button7","button8","button9"]
       [lb1,lb2,lb3,lb4,lb5,lb6,lb7,lb8,lb9] <-
           mapM (builderGetObject builder castToLabel)
	   ["label1","label2","label3","label4","label5","label6","label7","label8","label9"]

       --open commands.txt and print contents for debug
       handle <- openCommands
       contents <- hGetContents handle
       putStrLn contents

       {-set qs1 [buttonLabel := lines contents !! 0]-}
       {-set qs2 [buttonLabel := lines contents !! 1]-}
       {-set qs3 [buttonLabel := lines contents !! 2]-}
       {-set qs4 [buttonLabel := lines contents !! 3]-}
       {-set qs5 [buttonLabel := lines contents !! 4]-}
       {-set qs6 [buttonLabel := lines contents !! 5]-}
       {-set qs7 [buttonLabel := lines contents !! 6]-}
       {-set qs8 [buttonLabel := lines contents !! 7]-}
       {-set qs9 [buttonLabel := lines contents !! 8]-}
       
       --call main window (and child objects)
       widgetShowAll mw
       
       --return the window, buttons and labels via the GUI datatype
       return $ GUI mw qs1 qs2 qs3 qs4 qs5 qs6 qs7 qs8 qs9 lb1 lb2 lb3 lb4 lb5 lb6 lb7 lb8 lb9
       
openCommands = do
       openFile "commands.txt" ReadMode

connectGui gui = do
        onDestroy (mainWin gui) mainQuit
	onClicked (button1 gui) (run $ words "commands.txt 1 gui")
	onClicked (button2 gui) (run $ words "commands.txt 2 gui")
	onClicked (button3 gui) (run $ words "commands.txt 3 gui")
	onClicked (button4 gui) (run $ words "commands.txt 4 gui")
	onClicked (button5 gui) (run $ words "commands.txt 5 gui")
	onClicked (button6 gui) (run $ words "commands.txt 6 gui")
	onClicked (button7 gui) (run $ words "commands.txt 7 gui")
	onClicked (button8 gui) (run $ words "commands.txt 8 gui")
	onClicked (button9 gui) (run $ words "commands.txt 9 gui")
 
dispatchInit = do
	(command:args) <- getArgs
	newExecute command args

newExecute "run" b = run b
newExecute "add" b = add' b
newExecute "view" b = view b
newExecute "remove" b = remove' b
newExecute _ b = incorrectArgs (show b)

commands index = do
       handle <- openCommands
       contents <- hGetContents handle
       let number = (read index - 1)
	   qsScripts = lines contents
       return (show $ qsScripts !! number)


run :: [String] -> IO ()
run [fileName, numberString] = do
	handle <- openCommands
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
run [fileName, numberString, "gui"] = do
        run [fileName, numberString]
	mainQuit
run _ = incorrectArgs "run"

       
add' :: [String] -> IO ()
add' [fileName, qsItem] = appendFile fileName (qsItem ++ "\n") >> view ([fileName])
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
	handle <- openCommands
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
	putStrLn "\nThe file after the command was removed:\n"
	view ([fileName])
remove' _ = incorrectArgs "remove"
