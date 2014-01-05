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
        signal1 :: IORef (ConnectId Button),
        signal2 :: IORef (ConnectId Button),
        signal3 :: IORef (ConnectId Button),
        signal4 :: IORef (ConnectId Button),
        signal5 :: IORef (ConnectId Button),
        signal6 :: IORef (ConnectId Button),
        signal7 :: IORef (ConnectId Button),
        signal8 :: IORef (ConnectId Button),
        signal9 :: IORef (ConnectId Button)}



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
       sequence [labelSetJustify x JustifyCenter | x <- [lb1,lb2,lb3,lb4,lb5,lb6,lb7,lb8,lb9]]
       sequence [miscSetAlignment x 0.5 0.5 | x <- [lb1,lb2,lb3,lb4,lb5,lb6,lb7,lb8,lb9]]
       {-sequence [ miscSetPadding x 10 10 | x <- [lb1,lb2,lb3,lb4,lb5,lb6,lb7,lb8,lb9]]-}

       --call main window (and child objects)
       widgetShowAll mw 

       --return the window, buttons and labels via the GUI datatype
       return $ GUI mw qs1 qs2 qs3 qs4 qs5 qs6 qs7 qs8 qs9 lb1 lb2 lb3 lb4 lb5 lb6 lb7 lb8 lb9
       


openCommands fileName = do
       folder <- configFolder
       openFile (folder ++ fileName) ReadMode




-- Sets up onClicked events for commands.txt
connectGui :: GUI -> IO SIGNAL
connectGui gui = do
       onDestroy (mainWin gui) mainQuit
       signal1' <- onClicked (button1 gui) (run $ words "commands.txt 1 gui")
       signal2' <- onClicked (button2 gui) (run $ words "commands.txt 2 gui")
       signal3' <- onClicked (button3 gui) (run $ words "commands.txt 3 gui")
       signal4' <- onClicked (button4 gui) (run $ words "commands.txt 4 gui")
       signal5' <- onClicked (button5 gui) (run $ words "commands.txt 5 gui")
       signal6' <- onClicked (button6 gui) (run $ words "commands.txt 6 gui")
       signal7' <- onClicked (button7 gui) (run $ words "commands.txt 7 gui")
       signal8' <- onClicked (button8 gui) (run $ words "commands.txt 8 gui")
       signal9' <- onClicked (button9 gui) (run $ words "commands.txt 9 gui")

       signal1 <- newIORef signal1' 
       signal2 <- newIORef signal2'
       signal3 <- newIORef signal3' 
       signal4 <- newIORef signal4'
       signal5 <- newIORef signal5'
       signal6 <- newIORef signal6'
       signal7 <- newIORef signal7'
       signal8 <- newIORef signal8'
       signal9 <- newIORef signal9'
       return $ SIGNAL signal1 signal2 signal3 signal4 signal5 signal6 signal7 signal8 signal9  





-- Checks each command and disconnects signals from nodes, and adds a new onClicked which sets up the new labels and onClickeds(nodeActivate)
nodeCheck :: SIGNAL -> GUI -> IO()
nodeCheck nodeinfo gui = do
       lb1 <- labelGetLabel (label1 gui)
       lb2 <- labelGetLabel (label2 gui)
       lb3 <- labelGetLabel (label3 gui)
       lb4 <- labelGetLabel (label4 gui)
       lb5 <- labelGetLabel (label5 gui)
       lb6 <- labelGetLabel (label6 gui)
       lb7 <- labelGetLabel (label7 gui)
       lb8 <- labelGetLabel (label8 gui)
       lb9 <- labelGetLabel (label9 gui)

      
       [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9'] <- mapM (readIORef) [signal1 nodeinfo,signal2 nodeinfo,signal3 nodeinfo,signal4 nodeinfo,signal5 nodeinfo,signal6 nodeinfo,signal7 nodeinfo,signal8 nodeinfo,signal9 nodeinfo]

       case (head $ words lb1) of "node" -> do 
                                                  signalDisconnect signal1'
						  test1 <- onClicked (button1 gui) $ discall nodeinfo >> nA lb1
						  writeIORef (signal1 nodeinfo) test1
						  return()
                                  _ -> return()
       case (head $ words lb2) of "node" -> do 
                                                  signalDisconnect signal2'
                                                  test2 <- onClicked (button2 gui) $ discall nodeinfo >> nA lb2
						  writeIORef (signal2 nodeinfo) test2
						  return()
                                  _ -> return()
       case (head $ words lb3) of "node" -> do 
                                                  signalDisconnect signal3'
                                                  test3 <- onClicked (button3 gui) $ discall nodeinfo >> nA lb3
						  writeIORef (signal3 nodeinfo) test3
						  return()
                                  _ -> return()
       case (head $ words lb4) of "node" -> do 
                                                  signalDisconnect signal4'
                                                  test4 <- onClicked (button4 gui) $ discall nodeinfo >> nA lb4
						  writeIORef (signal4 nodeinfo) test4
						  return()
                                  _ -> return()
       case (head $ words lb5) of "node" -> do 
                                                  signalDisconnect signal5'
                                                  test5 <- onClicked (button5 gui) $ discall nodeinfo >> nA lb5
						  writeIORef (signal5 nodeinfo) test5
						  return()
                                  _ -> return()
       case (head $ words lb6) of "node" -> do 
                                                  signalDisconnect signal6'
                                                  test6 <- onClicked (button6 gui) $ discall nodeinfo >> nA lb6
						  writeIORef (signal6 nodeinfo) test6
						  return()
                                  _ -> return()
       case (head $ words lb7) of "node" -> do 
                                                  signalDisconnect signal7'
                                                  test7 <- onClicked (button7 gui) $ discall nodeinfo >> nA lb7
						  writeIORef (signal7 nodeinfo) test7
						  return()
                                  _ -> return()
       case (head $ words lb8) of "node" -> do 
                                                  signalDisconnect signal8'
                                                  test8 <- onClicked (button8 gui) $ discall nodeinfo >> nA lb8
						  writeIORef (signal8 nodeinfo) test8
						  return()
                                  _ -> return() 
       case (head $ words lb9) of "node" -> do 
                                                  signalDisconnect signal9' 
                                                  test9 <- onClicked (button9 gui) $ discall nodeinfo >> nA lb9 
						  writeIORef (signal9 nodeinfo) test9 
						  return()
                                  _ -> return()
       where nA lb = nodeActivate (tail $ words lb) gui nodeinfo
			      
       

 

discall nodeinfo = do
       [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9'] <- mapM (readIORef) [signal1 nodeinfo,signal2 nodeinfo,signal3 nodeinfo,signal4 nodeinfo,signal5 nodeinfo,signal6 nodeinfo,signal7 nodeinfo,signal8 nodeinfo,signal9 nodeinfo]
       
       mapM signalDisconnect [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9']

setL label filecontents index = do
       set label [labelText := lines filecontents !! index]

--Sets labels and events to match contents of nodeFile
nodeActivate :: [String] -> GUI -> SIGNAL -> IO ()
nodeActivate [nodeFile] gui nodeinfo = do
       folder <- configFolder
       nodeFile' <- openFile (folder ++ nodeFile) ReadMode 
       contents <- hGetContents nodeFile'
       setL (label1 gui) contents 0
       -- use map here? ++?
       --sequence [setL (x gui) contents | x <- [label1,label2,label3,label4,label5,label6,label7,label8,label9]]
       {-set (label1 gui) [labelText := lines contents !! 0]-}
       {-set (label2 gui) [labelText := lines contents !! 1]-}
       {-set (label3 gui) [labelText := lines contents !! 2]-}
       {-set (label4 gui) [labelText := lines contents !! 3]-}
       {-set (label5 gui) [labelText := lines contents !! 4]-}
       {-set (label6 gui) [labelText := lines contents !! 5]-}
       {-set (label7 gui) [labelText := lines contents !! 6]-}
       {-set (label8 gui) [labelText := lines contents !! 7]-}
       {-set (label9 gui) [labelText := lines contents !! 8]-}

       [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9'] <- mapM (readIORef) [signal1 nodeinfo,signal2 nodeinfo,signal3 nodeinfo,signal4 nodeinfo,signal5 nodeinfo,signal6 nodeinfo,signal7 nodeinfo,signal8 nodeinfo,signal9 nodeinfo]

       discall nodeinfo

       [lb1,lb2,lb3,lb4,lb5,lb6,lb7,lb8,lb9] <- mapM (labelGetText) [(label1 gui),(label2 gui),(label3 gui),(label4 gui),(label5 gui),(label6 gui),(label7 gui),(label8 gui),(label9 gui)]

       
       case (head $ words lb1) of "node" -> do 
                                              test1 <- onClicked (button1 gui) (nA lb1)
                                              writeIORef (signal1 nodeinfo) test1
					      return()
					      
				  _      -> do
                                              test1 <- onClicked (button1 gui) (run $ words $ nodeFile ++ " 1 gui")
				              writeIORef (signal1 nodeinfo) test1
					      return()
       case (head $ words lb2) of "node" -> do 
                                              test2 <- onClicked (button2 gui) (nA lb2)
                                              writeIORef (signal2 nodeinfo) test2
					      return()
				  _      -> do
                                              test2 <- onClicked (button2 gui) (run $ words $ nodeFile ++ " 2 gui")
				              writeIORef (signal2 nodeinfo) test2
					      return()
       case (head $ words lb3) of "node" -> do 
                                              test3 <- onClicked (button3 gui) (nA lb3)
                                              writeIORef (signal3 nodeinfo) test3
					      return()
				  _      -> do
                                              test3 <- onClicked (button3 gui) (run $ words $ nodeFile ++ " 3 gui")
				              writeIORef (signal3 nodeinfo) test3
					      return()
       case (head $ words lb4) of "node" -> do 
                                              test4 <- onClicked (button4 gui) (nA lb4)
                                              writeIORef (signal4 nodeinfo) test4
					      return()

				  _      -> do
				              test4 <- onClicked (button4 gui) (run $ words $ nodeFile ++ " 4 gui")
					      writeIORef (signal4 nodeinfo) test4
					      return()
       case (head $ words lb5) of "node" -> do 
                                              test5 <- onClicked (button5 gui) (nA lb5)
                                              writeIORef (signal5 nodeinfo) test5
					      return()

				  _      -> do
				              test5 <- onClicked (button5 gui) (run $ words $ nodeFile ++ " 5 gui")
					      writeIORef (signal5 nodeinfo) test5
					      return()       
       case (head $ words lb6) of "node" -> do 
                                              test6 <- onClicked (button6 gui) (nA lb6)
                                              writeIORef (signal6 nodeinfo) test6
					      return()

				  _      -> do
				              test6 <- onClicked (button6 gui) (run $ words $ nodeFile ++ " 6 gui")
					      writeIORef (signal6 nodeinfo) test6
					      return()       
       case (head $ words lb7) of "node" -> do 
                                              test7 <- onClicked (button7 gui) (nA lb7)
                                              writeIORef (signal7 nodeinfo) test7
					      return()

				  _      -> do
				              test7 <- onClicked (button7 gui) (run $ words $ nodeFile ++ " 7 gui")
					      writeIORef (signal7 nodeinfo) test7
					      return()       
       case (head $ words lb8) of "node" -> do 
                                              test8 <- onClicked (button8 gui) (nA lb8)
                                              writeIORef (signal8 nodeinfo) test8
					      return()

				  _      -> do
				              test8 <- onClicked (button8 gui) (run $ words $ nodeFile ++ " 8 gui")
					      writeIORef (signal8 nodeinfo) test8
					      return()       
       case (head $ words lb9) of "node" -> do 
                                              test9 <- onClicked (button9 gui) (nA lb9)
                                              writeIORef (signal9 nodeinfo) test9
					      return()

				  _      -> do
				              test9 <- onClicked (button9 gui) (run $ words $ nodeFile ++ " 9 gui")
					      writeIORef (signal9 nodeinfo) test9
					      return()
       where nA lb = nodeActivate (tail $ words lb) gui nodeinfo
             


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
