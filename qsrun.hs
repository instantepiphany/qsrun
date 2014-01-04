 --module Qsrun where
{-# LANGUAGE RecursiveDo #-}
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
        signal1 :: IORef (IO (ConnectId Button)),
        signal2 :: IORef (IO (ConnectId Button)),
        signal3 :: IORef (IO (ConnectId Button)),
        signal4 :: IORef (IO (ConnectId Button)),
        signal5 :: IORef (IO (ConnectId Button)),
        signal6 :: IORef (IO (ConnectId Button)),
        signal7 :: IORef (IO (ConnectId Button)),
        signal8 :: IORef (IO (ConnectId Button)),
        signal9 :: IORef (IO (ConnectId Button))}



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

getDir = getAppUserDataDirectory

configPath = getDir "config/qsrun/commands.txt" 
gladePath = getDir "config/qsrun/qsrun.glade"
configFolder = getDir "config/qsrun/"

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
       signal1 <- newIORef (onClicked (button1 gui) (run $ words "commands.txt 1 gui"))
       signal2 <- newIORef (onClicked (button2 gui) (run $ words "commands.txt 2 gui"))
       signal3 <- newIORef (onClicked (button3 gui) (run $ words "commands.txt 3 gui"))
       signal4 <- newIORef (onClicked (button4 gui) (run $ words "commands.txt 4 gui"))
       signal5 <- newIORef (onClicked (button5 gui) (run $ words "commands.txt 5 gui"))
       signal6 <- newIORef (onClicked (button6 gui) (run $ words "commands.txt 6 gui"))
       signal7 <- newIORef (onClicked (button7 gui) (run $ words "commands.txt 7 gui"))
       signal8 <- newIORef (onClicked (button8 gui) (run $ words "commands.txt 8 gui"))
       signal9 <- newIORef (onClicked (button9 gui) (run $ words "commands.txt 9 gui"))
       return $ SIGNAL signal1 signal2 signal3 signal4 signal5 signal6 signal7 signal8 signal9   

-- Checks each command and disconnects signals from nodes, and adds a new onClicked which sets up the new labels and onClickeds(nodeActivate)
nodeCheck nodeinfo gui = do
       nlabel1 <- labelGetLabel (label1 gui)
       nlabel2 <- labelGetLabel (label2 gui)
       nlabel3 <- labelGetLabel (label3 gui)
       nlabel4 <- labelGetLabel (label4 gui)
       nlabel5 <- labelGetLabel (label5 gui)
       nlabel6 <- labelGetLabel (label6 gui)
       nlabel7 <- labelGetLabel (label7 gui)
       nlabel8 <- labelGetLabel (label8 gui)
       nlabel9 <- labelGetLabel (label9 gui)

       {-[nlabel1,nlabel2,nlabel3,nlabel4,nlabel5,nlabel6,nlabel7,nlabel8,nlabel9] <- mapM (labelGetLabel) [label1 gui,label2 gui,label3 gui,label4 gui,label5 gui,label6 gui,label7 gui,label8 gui,label9 gui]-}
       [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9'] <- mapM (readIORef) [signal1 nodeinfo,signal2 nodeinfo,signal3 nodeinfo,signal4 nodeinfo,signal5 nodeinfo,signal6 nodeinfo,signal7 nodeinfo,signal8 nodeinfo,signal9 nodeinfo]

       signal1'' <- signal1' 
       signal2'' <- signal2' 
       signal3'' <- signal3' 
       signal4'' <- signal4' 
       signal5'' <- signal5' 
       signal6'' <- signal6' 
       signal7'' <- signal7' 
       signal8'' <- signal8' 
       signal9'' <- signal9'
       {-let discall = mapM signalDisconnect [signal1'',signal2'',signal3'',signal4'',signal5'',signal6'',signal7'',signal8'',signal9'']-}
       discall nodeinfo
       case (head $ words nlabel1) of "node" -> do 
                                                  signalDisconnect signal1''
						  writeIORef (signal1 nodeinfo) (onClicked (button1 gui) $ discall nodeinfo >> nodeActivate (tail $ words nlabel1) gui nodeinfo signal1'')
						  testing1 <- readIORef (signal1 nodeinfo)
						  testing1' <- testing1
						  return()
                                      _ -> return()
       case (head $ words nlabel2) of "node" -> do 
                                                  signalDisconnect signal2''
						  writeIORef (signal2 nodeinfo) (onClicked (button2 gui) $ discall nodeinfo >> nodeActivate (tail $ words nlabel2) gui nodeinfo signal2'')
						  testing2 <- readIORef (signal2 nodeinfo)
						  testing2' <- testing2
						  return()
                                      _ -> return()
       case (head $ words nlabel3) of "node" -> do 
                                                  signalDisconnect signal3''
						  writeIORef (signal3 nodeinfo) (onClicked (button3 gui) $ discall nodeinfo >> nodeActivate (tail $ words nlabel3) gui nodeinfo signal3'')
						  testing3 <- readIORef (signal3 nodeinfo)
						  testing3' <- testing3
						  return()
                                      _ -> return()
       case (head $ words nlabel4) of "node" -> do 
                                                  signalDisconnect signal4''
						  writeIORef (signal4 nodeinfo) (onClicked (button4 gui) $ discall nodeinfo >> nodeActivate (tail $ words nlabel4) gui nodeinfo signal4'')
						  testing4 <- readIORef (signal4 nodeinfo)
						  testing4' <- testing4
						  return()
                                      _ -> return()
       case (head $ words nlabel5) of "node" -> do 
                                                  signalDisconnect signal5''
						  writeIORef (signal5 nodeinfo) (onClicked (button5 gui) $ discall nodeinfo >> nodeActivate (tail $ words nlabel5) gui nodeinfo signal5'')
						  testing5 <- readIORef (signal5 nodeinfo)
						  testing5' <- testing5
						  return()
                                      _ -> return()
       case (head $ words nlabel6) of "node" -> do 
                                                  signalDisconnect signal6''
						  writeIORef (signal6 nodeinfo) (onClicked (button6 gui) $ discall nodeinfo >> nodeActivate (tail $ words nlabel6) gui nodeinfo signal6'')
						  testing6 <- readIORef (signal6 nodeinfo)
						  testing6' <- testing6
						  return()
                                      _ -> return()
       case (head $ words nlabel7) of "node" -> do 
                                                  signalDisconnect signal7''
						  writeIORef (signal7 nodeinfo) (onClicked (button7 gui) $ discall nodeinfo >> nodeActivate (tail $ words nlabel7) gui nodeinfo signal7'')
						  testing7 <- readIORef (signal7 nodeinfo)
						  testing7' <- testing7
						  return()
                                      _ -> return()
       case (head $ words nlabel8) of "node" -> do 
                                                  signalDisconnect signal8''
						  writeIORef (signal8 nodeinfo) (onClicked (button8 gui) $ discall nodeinfo >> nodeActivate (tail $ words nlabel8) gui nodeinfo signal8'')
						  testing8 <- readIORef (signal8 nodeinfo)
						  testing8' <- testing8
						  return()
                                      _ -> return()
       case (head $ words nlabel9) of "node" -> do 
                                                  signalDisconnect signal9'' 
						  writeIORef (signal9 nodeinfo) (onClicked (button9 gui) $ discall nodeinfo >> nodeActivate (tail $ words nlabel9) gui nodeinfo signal9'') 
						  testing <- readIORef (signal9 nodeinfo)
						  testing' <- testing
						  return()
                                      _ -> return()
       discall nodeinfo
       return()

 

discall nodeinfo = do
       test <- readIORef (signal1 nodeinfo)
       test' <- test
       signalDisconnect test'
-- Sets labels and events to match contents of nodeFile
--nodeActivate :: [String] -> GUI -> SIGNAL -> IO ()
nodeActivate [nodeFile] gui nodeinfo nsignal = do
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

       [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9'] <- mapM (readIORef) [signal1 nodeinfo,signal2 nodeinfo,signal3 nodeinfo,signal4 nodeinfo,signal5 nodeinfo,signal6 nodeinfo,signal7 nodeinfo,signal8 nodeinfo,signal9 nodeinfo]

       signal1'' <- signal1' 
       signal2'' <- signal2' 
       signal3'' <- signal3' 
       signal4'' <- signal4' 
       signal5'' <- signal5' 
       signal6'' <- signal6' 
       signal7'' <- signal7' 
       signal8'' <- signal8' 
       signal9'' <- signal9'



       let discall = mapM signalDisconnect [signal1'',signal2'',signal3'',signal4'',signal5'',signal6'',signal7'',signal8'',signal9'']
       discall

       [lb1,lb2,lb3,lb4,lb5,lb6,lb7,lb8,lb9] <- mapM (labelGetText) [(label1 gui),(label2 gui),(label3 gui),(label4 gui),(label5 gui),(label6 gui),(label7 gui),(label8 gui),(label9 gui)]

       {-case (tail x) of "node" -> signalDisconnect nSignal1-}
       signalDisconnect nsignal
       case (head $ words lb1) of "node" -> do 
					      signalDisconnect signal1''
                                              writeIORef (signal1 nodeinfo) $ onClicked (button1 gui) (nodeActivate (tail $ words lb1) gui nodeinfo nsignal)
					      asdf <- readIORef (signal1 nodeinfo)
					      signal1'' <- asdf
					      return()
				  _      -> do
					      signalDisconnect signal1''
				              writeIORef (signal1 nodeinfo) $ onClicked (button1 gui) (run $ words $ nodeFile ++ " 1 gui")
					      asdf <- readIORef (signal1 nodeinfo)
					      signal1'' <-  asdf
					      return()
       case (head $ words lb2) of "node" -> onClicked (button2 gui) (nodeActivate (tail $ words lb2) gui nodeinfo nsignal)
				  _      -> onClicked (button2 gui) (run $ words $ nodeFile ++ " 2 gui")
       case (head $ words lb3) of "node" -> onClicked (button3 gui) (nodeActivate (tail $ words lb3) gui nodeinfo nsignal)
				  _      -> onClicked (button3 gui) (run $ words $ nodeFile ++ " 3 gui")
       case (head $ words lb4) of "node" -> onClicked (button4 gui) (nodeActivate (tail $ words lb4) gui nodeinfo nsignal)
				  _      -> onClicked (button4 gui) (run $ words $ nodeFile ++ " 4 gui")
       case (head $ words lb5) of "node" -> onClicked (button5 gui) (nodeActivate (tail $ words lb5) gui nodeinfo nsignal)
				  _      -> onClicked (button5 gui) (run $ words $ nodeFile ++ " 5 gui")
       case (head $ words lb6) of "node" -> onClicked (button6 gui) (nodeActivate (tail $ words lb6) gui nodeinfo nsignal)
				  _      -> onClicked (button6 gui) (run $ words $ nodeFile ++ " 6 gui")
       case (head $ words lb7) of "node" -> onClicked (button7 gui) (nodeActivate (tail $ words lb7) gui nodeinfo nsignal)
				  _      -> onClicked (button7 gui) (run $ words $ nodeFile ++ " 7 gui")
       case (head $ words lb8) of "node" -> onClicked (button8 gui) $ do 
                                                                        putStrLn "test"
									discall
									signalDisconnect signal1''
									nodeActivate (tail $ words lb8) gui nodeinfo nsignal
				  _      -> onClicked (button8 gui) (run $ words $ nodeFile ++ " 8 gui")
       case (head $ words lb9) of "node" -> onClicked (button9 gui) (nodeActivate (tail $ words lb9) gui nodeinfo nsignal)
                                  _      -> onClicked (button9 gui) (run $ words $ nodeFile ++ " 9 gui")
       discall

       {-onClicked (button2 gui) (childNodeCheck (head x) 2 gui nodeinfo)-}
       {-onClicked (button3 gui) (childNodeCheck (head x) 3 gui nodeinfo)-}
       {-onClicked (button4 gui) (childNodeCheck (head x) 4 gui nodeinfo)-}
       {-onClicked (button5 gui) (childNodeCheck (head x) 5 gui nodeinfo)-}
       {-onClicked (button6 gui) (childNodeCheck (head x) 6 gui nodeinfo)-}
       {-onClicked (button7 gui) (childNodeCheck (head x) 7 gui nodeinfo)-}
       {-onClicked (button8 gui) (childNodeCheck (head x) 8 gui nodeinfo)-}
       {-onClicked (button9 gui) (childNodeCheck (head x) 9 gui nodeinfo)-}
       labelGetText (label1 gui) >>= putStrLn
       return()

{-childNodeCheck nodeFile numberString gui nodeinfo = do-}
       {-folder <- configFolder-}
       {-nodeFile' <- openFile (folder ++ nodeFile) ReadMode -}
       {-contents <- hGetContents nodeFile'-}
       {-let qsScripts = lines contents-}
           {-first = head $ words (qsScripts !! (numberString - 1))-}
           {-second = head $ tail $ words $ qsScripts !! (numberString - 1)-}
       {-case first of "node" -> nodeActivate [second] gui nodeinfo     -}
                     {-_ -> run [nodeFile,(show numberString),"gui"]-}
       {-return()-}

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
	   runCommand = (qsScripts !! (read numberString - 1))
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
