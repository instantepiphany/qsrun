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
import Control.Concurrent
import Data.IORef
import Control.Monad
import Control.Monad.State

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
data SIGNAL = SIGNAL {
        signal1 :: ConnectId Button,
        signal2 :: ConnectId Button,
        signal3 :: ConnectId Button,
        signal4 :: ConnectId Button,
        signal5 :: ConnectId Button,
        signal6 :: ConnectId Button,
        signal7 :: ConnectId Button,
        signal8 :: ConnectId Button,
        signal9 :: ConnectId Button}



incorrectArgs :: String -> IO ()
incorrectArgs "add" = do
	putStrLn "Error: Incorrect syntax. Correct syntax is 'qsrun add filename command'"

incorrectArgs "run" = do
	putStrLn "Error: Incorrect syntax. Correct syntax is 'qsrun run filename $commandindex' - Find command index easily by doing 'qsrun view filename'."

incorrectArgs "view" = do
	putStrLn "Error: Incorrect syntax, correct syntax is 'qsrun view filename.txt' or optionally omit the filename and the default file 'commands.txt' will be used."

incorrectArgs "remove" = do
	putStrLn "Error: Incorrect syntax. Correct syntax is 'qsrun remove filename $commandindex' - Find command index easily by doing 'qsrun view filename'."

incorrectArgs _ = do
	putStrLn "Incorrect argument. Valid args are add/view/remove/run. Alternatively run without args for interactive mode."


main = do
	initArgs <- getArgs
	if null initArgs then modeGui
		else dispatchInit

configPath = getAppUserDataDirectory "config/qsrun/commands.txt" 
gladePath = getAppUserDataDirectory "config/qsrun/qsrun.glade"
configFolder = getAppUserDataDirectory "config/qsrun/"

modeGui :: IO ()
modeGui = do
        initGUI
	gladefile <- gladePath
        gui <- loadGui gladefile 
	nodeinfo <- connectGui gui
	nodeCheck nodeinfo gui
	mainGUI

loadGui gladepath = do 

       -- Import gui from file
       builder <- builderNew
       builderAddFromFile builder gladepath

       -- bind main window, buttons, and labels
       mw <- builderGetObject builder castToWindow "qsrun"
       [qs1,qs2,qs3,qs4,qs5,qs6,qs7,qs8,qs9] <-
           mapM (builderGetObject builder castToButton)
	   ["button1","button2","button3","button4","button5","button6","button7","button8","button9"]
       [lb1,lb2,lb3,lb4,lb5,lb6,lb7,lb8,lb9] <-
           mapM (builderGetObject builder castToLabel)
	   ["label1","label2","label3","label4","label5","label6","label7","label8","label9"]

       --open config file 
       handle <- openCommands "commands.txt"
       contents <- hGetContents handle
       labelSetLineWrap lb1 True
       labelSetLineWrapMode lb1 WrapAnywhere
       set lb1 [labelText := lines contents !! 0]
       set lb2 [labelText := lines contents !! 1]
       set lb3 [labelText := lines contents !! 2]
       set lb4 [labelText := lines contents !! 3]
       set lb5 [labelText := lines contents !! 4]
       set lb6 [labelText := lines contents !! 5]
       set lb7 [labelText := lines contents !! 6]
       set lb8 [labelText := lines contents !! 7]
       set lb9 [labelText := lines contents !! 8]
       labelSetJustify lb7 JustifyCenter
       --call main window (and child objects)
       widgetShowAll mw 
       --return the window, buttons and labels via the GUI datatype
       return $ GUI mw qs1 qs2 qs3 qs4 qs5 qs6 qs7 qs8 qs9 lb1 lb2 lb3 lb4 lb5 lb6 lb7 lb8 lb9
       


openCommands fileName = do
       folder <- configFolder
       openFile (folder ++ fileName) ReadMode

-- Sets up onClicked events for commands.txt
connectGui gui = do
       onDestroy (mainWin gui) mainQuit
       signal1 <- onClicked (button1 gui) (run $ words "commands.txt 1 gui") 
       signal2 <- onClicked (button2 gui) (run $ words "commands.txt 2 gui")
       signal3 <- onClicked (button3 gui) (run $ words "commands.txt 3 gui")
       signal4 <- onClicked (button4 gui) (run $ words "commands.txt 4 gui")
       signal5 <- onClicked (button5 gui) (run $ words "commands.txt 5 gui")
       signal6 <- onClicked (button6 gui) (run $ words "commands.txt 6 gui")
       signal7 <- onClicked (button7 gui) (run $ words "commands.txt 7 gui")
       signal8 <- onClicked (button8 gui) (run $ words "commands.txt 8 gui")
       signal9 <- onClicked (button9 gui) (run $ words "commands.txt 9 gui")
       return $ SIGNAL signal1 signal2 signal3 signal4 signal5 signal6 signal7 signal8 signal9   

-- Checks each command and disconnects signals from nodes, and adds a new onClicked which sets up the new labels and onClickeds(nodeActivate)
nodeCheck nodeinfo gui = do
       {-handle <- openCommands-}
       {-contents <- hGetContents handle-}
       {-let qsScripts = lines contents-}
	   {-runCommand = (qsScripts !! ((read numberString) - 1))-}
	   {-command = (head $ words $ runCommand)-}
	   {-args = (tail $ words $ runCommand)-}
       {-case command of "node" -> nodeActivate ["testnode.txt"] gui-}
                       {-_ -> return() --run [fileName, numberString, "gui"]-}
       nlabel1 <- labelGetLabel (label1 gui)
       nlabel2 <- labelGetLabel (label2 gui)
       nlabel3 <- labelGetLabel (label3 gui)
       nlabel4 <- labelGetLabel (label4 gui)
       nlabel5 <- labelGetLabel (label5 gui)
       nlabel6 <- labelGetLabel (label6 gui)
       nlabel7 <- labelGetLabel (label7 gui)
       nlabel8 <- labelGetLabel (label8 gui)
       nlabel9 <- labelGetLabel (label9 gui)
       case (head $ words nlabel9) of "node" -> signalDisconnect (signal9 nodeinfo) >> onClicked (button9 gui) (nodeActivate (tail $ words nlabel9) gui nodeinfo) 
       return()

       {-putStrLn command-}
       {-putStrLn (unwords args)-}

-- Sets labels and events to match contents of nodeFile
nodeActivate :: [String] -> GUI -> SIGNAL -> IO ()
nodeActivate [nodeFile] gui nodeinfo = do
       folder <- configFolder
       nodeFile' <- openFile (folder ++ nodeFile) ReadMode 
       contents <- hGetContents nodeFile'
       set (label1 gui) [labelText := lines contents !! 0]
       set (label2 gui) [labelText := lines contents !! 1]
       set (label3 gui) [labelText := lines contents !! 2]
       set (label4 gui) [labelText := lines contents !! 3]
       set (label5 gui) [labelText := lines contents !! 4]
       set (label6 gui) [labelText := lines contents !! 5]
       set (label7 gui) [labelText := lines contents !! 6]
       set (label8 gui) [labelText := lines contents !! 7]
       set (label9 gui) [labelText := lines contents !! 8]
       mapM signalDisconnect [(signal1 nodeinfo),(signal2 nodeinfo),(signal3 nodeinfo),(signal4 nodeinfo),(signal5 nodeinfo),(signal6 nodeinfo),(signal7 nodeinfo),(signal8 nodeinfo),(signal9 nodeinfo)]
       let qsScripts = lines contents
       onClicked (button1 gui) (childNodeCheck nodeFile 1 gui nodeinfo)
       onClicked (button2 gui) (childNodeCheck nodeFile 2 gui nodeinfo)
       onClicked (button3 gui) (childNodeCheck nodeFile 3 gui nodeinfo)
       onClicked (button4 gui) (childNodeCheck nodeFile 4 gui nodeinfo)
       onClicked (button5 gui) (childNodeCheck nodeFile 5 gui nodeinfo)
       onClicked (button6 gui) (childNodeCheck nodeFile 6 gui nodeinfo)
       onClicked (button7 gui) (childNodeCheck nodeFile 7 gui nodeinfo)
       onClicked (button8 gui) (childNodeCheck nodeFile 8 gui nodeinfo)
       onClicked (button9 gui) (childNodeCheck nodeFile 9 gui nodeinfo)

       return()
       {-labelSetText (label1 gui) -}

childNodeCheck nodeFile numberString gui nodeinfo = do
       folder <- configFolder
       nodeFile' <- openFile (folder ++ nodeFile) ReadMode 
       contents <- hGetContents nodeFile'
       let qsScripts = lines contents
           first = head $ words (qsScripts !! (numberString - 1))
           second = head $ tail $ words $ qsScripts !! (numberString - 1)
       case first of "node" -> nodeActivate [second] gui nodeinfo     
                     _ -> run [nodeFile,(show numberString),"gui"]
       return()
dispatchInit = do
       (command:args) <- getArgs
       newExecute command args


newExecute :: String -> [String] -> IO () 
newExecute "run" b = run b
newExecute "add" b = add' b
newExecute "view" b = view b
newExecute "remove" b = remove' b
newExecute _ b = incorrectArgs (show b)

commands index = do
       handle <- openCommands "commands.txt"
       contents <- hGetContents handle
       let number = (read index - 1)
	   qsScripts = lines contents
       return (show $ qsScripts !! number)


run :: [String] -> IO ()
run [fileName, numberString] = do
       handle <- openCommands fileName
       contents <- hGetContents handle
       let qsScripts = lines contents
	   runCommand = (qsScripts !! ((read numberString) - 1))
	   command = (head $ words $ runCommand)
	   args = (tail $ words $ runCommand)
       putStrLn "Executing: "
       putStr runCommand
       putStrLn ""
       forkIO(runB command args)
       hClose handle
run [fileName, numberString, "gui"] = do
       run [fileName, numberString]
       mainQuit
run _ = incorrectArgs "run"

runB command args = do
       rawSystem command args
       return()


add' :: [String] -> IO ()
add' [fileName, qsItem] = appendFile fileName (qsItem ++ "\n") >> view ([fileName])
add' _ = incorrectArgs "add" 


view :: [String] -> IO ()
view [fileName] = do
       contents <- readFile fileName
       let qsScripts = lines contents
	   numberedScripts = zipWith (\n line -> show n ++ " - " ++ line) [1..] qsScripts
       putStr $ unlines numberedScripts
view [] = do
       configFile <- configPath
       view [configFile]
view _ = incorrectArgs "view"


remove' :: [String] -> IO ()
remove' [fileName, numberString] = do
	handle <- openCommands "commands.txt"
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
