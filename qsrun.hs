 --module Qsrun where
{-# LANGUAGE RecursiveDo #-}
import           System.Cmd
import           System.Directory
import           System.IO
--import System.Environment
--import Data.Char
--import Data.List
--import Data.Either
import           Graphics.UI.Gtk
--import Graphics.UI.Gtk.Builder
import           Control.Concurrent
import           Data.IORef
--import Control.Monad
import           Control.Monad.State

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

main :: IO()
main = do
	initGUI
	gladefile <- gladePath
        gui <- loadGui gladefile
	nodeinfo <- connectGui gui
	nodeCheck nodeinfo gui
	mainGUI
getDir :: String -> IO FilePath
getDir = getAppUserDataDirectory

configPath :: IO FilePath
configPath = getDir "config/qsrun/commands.txt"

gladePath :: IO FilePath
gladePath = getDir "config/qsrun/qsrun.glade"

configFolder :: IO FilePath
configFolder = getDir "config/qsrun/"


loadGui :: FilePath -> IO GUI
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


openCommands :: String -> IO Handle
openCommands fileName = do
       folder <- configFolder
       openFile (folder ++ fileName) ReadMode




-- Sets up onClicked events for commands.txt
connectGui :: GUI -> IO SIGNAL
connectGui gui = do
       on (mainWin gui) deleteEvent $ liftIO $ mainQuit  >> return False
       signal1' <- on (button1 gui) buttonActivated (run $ words "commands.txt 1 gui")
       signal2' <- on (button2 gui) buttonActivated (run $ words "commands.txt 2 gui")
       signal3' <- on (button3 gui) buttonActivated (run $ words "commands.txt 3 gui")
       signal4' <- on (button4 gui) buttonActivated (run $ words "commands.txt 4 gui")
       signal5' <- on (button5 gui) buttonActivated (run $ words "commands.txt 5 gui")
       signal6' <- on (button6 gui) buttonActivated (run $ words "commands.txt 6 gui")
       signal7' <- on (button7 gui) buttonActivated (run $ words "commands.txt 7 gui")
       signal8' <- on (button8 gui) buttonActivated (run $ words "commands.txt 8 gui")
       signal9' <- on (button9 gui) buttonActivated (run $ words "commands.txt 9 gui")

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
						  test1 <- on (button1 gui) buttonActivated $ discall nodeinfo >> nA lb1
						  writeIORef (signal1 nodeinfo) test1
						  return()
                                  _ -> return()
       case (head $ words lb2) of "node" -> do
                                                  signalDisconnect signal2'
                                                  test2 <- on (button2 gui) buttonActivated $ discall nodeinfo >> nA lb2
						  writeIORef (signal2 nodeinfo) test2
						  return()
                                  _ -> return()
       case (head $ words lb3) of "node" -> do
                                                  signalDisconnect signal3'
                                                  test3 <- on (button3 gui) buttonActivated $ discall nodeinfo >> nA lb3
						  writeIORef (signal3 nodeinfo) test3
						  return()
                                  _ -> return()
       case (head $ words lb4) of "node" -> do
                                                  signalDisconnect signal4'
                                                  test4 <- on (button4 gui) buttonActivated $ discall nodeinfo >> nA lb4
						  writeIORef (signal4 nodeinfo) test4
						  return()
                                  _ -> return()
       case (head $ words lb5) of "node" -> do
                                                  signalDisconnect signal5'
                                                  test5 <- on (button5 gui) buttonActivated $ discall nodeinfo >> nA lb5
						  writeIORef (signal5 nodeinfo) test5
						  return()
                                  _ -> return()
       case (head $ words lb6) of "node" -> do
                                                  signalDisconnect signal6'
                                                  test6 <- on (button6 gui) buttonActivated $ discall nodeinfo >> nA lb6
						  writeIORef (signal6 nodeinfo) test6
						  return()
                                  _ -> return()
       case (head $ words lb7) of "node" -> do
                                                  signalDisconnect signal7'
                                                  test7 <- on (button7 gui) buttonActivated $ discall nodeinfo >> nA lb7
						  writeIORef (signal7 nodeinfo) test7
						  return()
                                  _ -> return()
       case (head $ words lb8) of "node" -> do
                                                  signalDisconnect signal8'
                                                  test8 <- on (button8 gui) buttonActivated $ discall nodeinfo >> nA lb8
						  writeIORef (signal8 nodeinfo) test8
						  return()
                                  _ -> return()
       case (head $ words lb9) of "node" -> do
                                                  signalDisconnect signal9'
                                                  test9 <- on (button9 gui) buttonActivated $ discall nodeinfo >> nA lb9
						  writeIORef (signal9 nodeinfo) test9
						  return()
                                  _ -> return()
       where nA lb = nodeActivate (tail $ words lb) gui nodeinfo





discall nodeinfo = do
       [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9'] <- mapM (readIORef) [signal1 nodeinfo,signal2 nodeinfo,signal3 nodeinfo,signal4 nodeinfo,signal5 nodeinfo,signal6 nodeinfo,signal7 nodeinfo,signal8 nodeinfo,signal9 nodeinfo]

       mapM signalDisconnect [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9']

setL label filecontents ind = do

  let line = (lines filecontents !! ind)
  --case (words line !! 1) of "-" -> set label [labelText := head $ words line]
  --                          _   -> set label [labelText := line]

  if (elem "-" $ words line) then do 
                                       putStrLn ("Label " ++ (show ind) ++ " is \"" ++ line ++ "\" and is a node")
                                       set label [labelText := (words $ lines filecontents !! ind) !! 0]
                             else do
                                       putStrLn ("Label " ++ (show ind) ++ " is \"" ++ line ++ "\" and is not a node")
                                       set label [labelText := lines filecontents !! ind]

--Takes a list of functions requiring 1 input, and a list of corresponding inputs, and returns a list of complete functions (that can then be run by sequence)
fPair :: [t -> a] -> [t] -> [a]  
fPair [x] (y:ys) = (x y):(fPair [x] ys)
fPair (x:xs) (y:ys) = (x y):(fPair xs ys)
fPair _ _ = []

--Sets labels and events to match contents of nodeFile
nodeActivate :: [String] -> GUI -> SIGNAL -> IO ()
nodeActivate [nodeFile] gui nodeinfo = do
       folder <- configFolder
       nodeFile' <- openFile (folder ++ nodeFile) ReadMode
       contents <- hGetContents nodeFile'
       sequence $ fPair [setL (x gui) contents | x <- [label1,label2,label3,label4,label5,label6,label7,label8,label9]] [0..8]



       [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9'] <- mapM (readIORef) [signal1 nodeinfo,signal2 nodeinfo,signal3 nodeinfo,signal4 nodeinfo,signal5 nodeinfo,signal6 nodeinfo,signal7 nodeinfo,signal8 nodeinfo,signal9 nodeinfo]

       discall nodeinfo

       [lb1,lb2,lb3,lb4,lb5,lb6,lb7,lb8,lb9] <- mapM (labelGetText) [(label1 gui),(label2 gui),(label3 gui),(label4 gui),(label5 gui),(label6 gui),(label7 gui),(label8 gui),(label9 gui)]


       case (head $ words lb1) of "node" -> do
                                              test1 <- on (button1 gui) buttonActivated (nA lb1)
                                              writeIORef (signal1 nodeinfo) test1
					      return()

				  _      -> do
                                              test1 <- on (button1 gui) buttonActivated (run $ words $ nodeFile ++ " 1 gui")
				              writeIORef (signal1 nodeinfo) test1
					      return()
       case (head $ words lb2) of "node" -> do
                                              test2 <- on (button2 gui) buttonActivated (nA lb2)
                                              writeIORef (signal2 nodeinfo) test2
					      return()
				  _      -> do
                                              test2 <- on (button2 gui) buttonActivated (run $ words $ nodeFile ++ " 2 gui")
				              writeIORef (signal2 nodeinfo) test2
					      return()
       case (head $ words lb3) of "node" -> do
                                              test3 <- on (button3 gui) buttonActivated (nA lb3)
                                              writeIORef (signal3 nodeinfo) test3
					      return()
				  _      -> do
                                              test3 <- on (button3 gui) buttonActivated (run $ words $ nodeFile ++ " 3 gui")
				              writeIORef (signal3 nodeinfo) test3
					      return()
       case (head $ words lb4) of "node" -> do
                                              test4 <- on (button4 gui) buttonActivated (nA lb4)
                                              writeIORef (signal4 nodeinfo) test4
					      return()

				  _      -> do
				              test4 <- on (button4 gui) buttonActivated (run $ words $ nodeFile ++ " 4 gui")
					      writeIORef (signal4 nodeinfo) test4
					      return()
       case (head $ words lb5) of "node" -> do
                                              test5 <- on (button5 gui) buttonActivated (nA lb5)
                                              writeIORef (signal5 nodeinfo) test5
					      return()

				  _      -> do
				              test5 <- on (button5 gui) buttonActivated (run $ words $ nodeFile ++ " 5 gui")
					      writeIORef (signal5 nodeinfo) test5
					      return()
       case (head $ words lb6) of "node" -> do
                                              test6 <- on (button6 gui) buttonActivated (nA lb6)
                                              writeIORef (signal6 nodeinfo) test6
					      return()

				  _      -> do
				              test6 <- on (button6 gui) buttonActivated (run $ words $ nodeFile ++ " 6 gui")
					      writeIORef (signal6 nodeinfo) test6
					      return()
       case (head $ words lb7) of "node" -> do
                                              test7 <- on (button7 gui) buttonActivated (nA lb7)
                                              writeIORef (signal7 nodeinfo) test7
					      return()

				  _      -> do
				              test7 <- on (button7 gui) buttonActivated (run $ words $ nodeFile ++ " 7 gui")
					      writeIORef (signal7 nodeinfo) test7
					      return()
       case (head $ words lb8) of "node" -> do
                                              test8 <- on (button8 gui) buttonActivated (nA lb8)
                                              writeIORef (signal8 nodeinfo) test8
					      return()

				  _      -> do
				              test8 <- on (button8 gui) buttonActivated (run $ words $ nodeFile ++ " 8 gui")
					      writeIORef (signal8 nodeinfo) test8
					      return()
       case (head $ words lb9) of "node" -> do
                                              test9 <- on (button9 gui) buttonActivated (nA lb9)
                                              writeIORef (signal9 nodeinfo) test9
					      return()

				  _      -> do
				              test9 <- on (button9 gui) buttonActivated (run $ words $ nodeFile ++ " 9 gui")
					      writeIORef (signal9 nodeinfo) test9
					      return()
       where nA lb = nodeActivate (tail $ words lb) gui nodeinfo




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

runB :: String -> [String] -> IO ()
runB command args = do
       rawSystem command args
       return()


