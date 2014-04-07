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

       let labels = [lb1,lb2,lb3,lb4,lb5,lb6,lb7,lb8,lb9]

       sequence $ fPair [setL x contents | x <- labels] [0..8]
       sequence [labelSetJustify x JustifyCenter | x <- labels]
       sequence [miscSetAlignment x 0.5 0.5 | x <- labels]

       --call main window (and child objects)
       widgetShowAll mw

       --return the window, buttons and labels via the GUI datatype
       return $ GUI mw qs1 qs2 qs3 qs4 qs5 qs6 qs7 qs8 qs9 lb1 lb2 lb3 lb4 lb5 lb6 lb7 lb8 lb9


openCommands :: String -> IO Handle
openCommands fileName = do
       folder <- configFolder
       openFile (folder ++ fileName) ReadMode




-- Sets up buttonActivated events for commands.txt
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

       --open config file
       handle <- openCommands "commands.txt"
       contents <- hGetContents handle
  
       lb1 <- labelGetLabel (label1 gui)
       lb2 <- labelGetLabel (label2 gui)
       lb3 <- labelGetLabel (label3 gui)
       lb4 <- labelGetLabel (label4 gui)
       lb5 <- labelGetLabel (label5 gui)
       lb6 <- labelGetLabel (label6 gui)
       lb7 <- labelGetLabel (label7 gui)
       lb8 <- labelGetLabel (label8 gui)
       lb9 <- labelGetLabel (label9 gui)

       let readrefs = mapM (readIORef) [signal1 nodeinfo,signal2 nodeinfo,signal3 nodeinfo,signal4 nodeinfo,signal5 nodeinfo,signal6 nodeinfo,signal7 nodeinfo,signal8 nodeinfo,signal9 nodeinfo]
       [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9'] <- readrefs
       
       let buttons = [button1 gui,button2 gui,button3 gui,button4 gui,button5 gui,button6 gui,button7 gui,button8 gui,button9 gui]
       let labels = [lb1,lb2,lb3,lb4,lb5,lb6,lb7,lb8,lb9]
       let signals = [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9']
       let iorefs = [signal1 nodeinfo,signal2 nodeinfo,signal3 nodeinfo,signal4 nodeinfo,signal5 nodeinfo,signal6 nodeinfo,signal7 nodeinfo,signal8 nodeinfo,signal9 nodeinfo]


       ---- all 3 are working, implement within nodeActivate, and move to top level functions? if possible.
       let labelcheck ind = if (elem "-" $ words $ (lines contents) !! ind) then do
                                                                                signalDisconnect $ signals !! ind
                                                                                tempio <- on (buttons !! ind) buttonActivated $ run $ words $ "commands.txt " ++ show (ind + 1) ++ " gui"
						                                writeIORef (iorefs !! ind) tempio
                                                                                signals <- readrefs
						                                return()
                                                                            else do
                                                                                return()                   
           
       let nodecheck ind = case (head $ words $ (lines contents) !! ind) of "node" -> do
                                                                                signalDisconnect $ signals !! ind
                                                                                tempio <- on (buttons !! ind) buttonActivated $ discall nodeinfo >> nA (labels !! ind)
						                                writeIORef (iorefs !! ind) tempio
                                                                                signals <- readrefs
						                                return()
                                                                            _ -> return()
       let nodelabelcheck ind = if (elem "-" $ words $ (lines contents) !! ind) && (elem "node" $ words $ (lines contents) !! ind) 
                                 then do
                                   putStrLn "nodelabelcheck run"
                                   signal <- readIORef (iorefs !! ind)
                                   signalDisconnect $ signal
                                   tempio <- on (buttons !! ind) buttonActivated $ discall nodeinfo >> nA (unwords $ drop 2 $ words $ (lines contents) !! ind)
				   writeIORef (iorefs !! ind) tempio
				   return()
                                 else do
                                   return()

                                               
       sequence $ map nodecheck [0..8]
       sequence $ map labelcheck [0..8]
       sequence $ map nodelabelcheck [0..8]
       hClose handle
       return()
       where nA lb = nodeActivate (tail $ words lb) gui nodeinfo


discall :: SIGNAL -> IO [()]
discall nodeinfo = do
       [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9'] <- mapM (readIORef) [signal1 nodeinfo,signal2 nodeinfo,signal3 nodeinfo,signal4 nodeinfo,signal5 nodeinfo,signal6 nodeinfo,signal7 nodeinfo,signal8 nodeinfo,signal9 nodeinfo]

       mapM signalDisconnect [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9']

setL :: LabelClass o => o -> [Char] -> Int -> IO ()
setL label filecontents ind = do

  let line = (lines filecontents !! ind)
  
  if (elem "-" $ words line) then do 
                                       putStrLn ("Label " ++ (show $ ind + 1) ++ " is \"" ++ line ++ "\" and has a label")
                                       set label [labelText := (words $ lines filecontents !! ind) !! 0]
                             else do
                                       putStrLn ("Label " ++ (show $ ind + 1) ++ " is \"" ++ line ++ "\" and has no label")
                                       set label [labelText := lines filecontents !! ind]

--Takes a list of functions requiring 1 input, and a list of corresponding inputs, and returns a list of complete functions (that can then be run by sequence)
fPair :: [t -> a] -> [t] -> [a]  
fPair (x:xs) (y:ys) = (x y):(fPair xs ys)
fPair _ _ = []

--Sets labels and events to match contents of nodeFile
nodeActivate :: [String] -> GUI -> SIGNAL -> IO ()
nodeActivate [nodeFile] gui nodeinfo = do
       handle <- openCommands nodeFile
       contents <- hGetContents handle
       sequence $ fPair [setL (x gui) contents | x <- [label1,label2,label3,label4,label5,label6,label7,label8,label9]] [0..8]

       discall nodeinfo

       [lb1,lb2,lb3,lb4,lb5,lb6,lb7,lb8,lb9] <- mapM (labelGetText) [(label1 gui),(label2 gui),(label3 gui),(label4 gui),(label5 gui),(label6 gui),(label7 gui),(label8 gui),(label9 gui)]
       [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9'] <- mapM (readIORef) [signal1 nodeinfo,signal2 nodeinfo,signal3 nodeinfo,signal4 nodeinfo,signal5 nodeinfo,signal6 nodeinfo,signal7 nodeinfo,signal8 nodeinfo,signal9 nodeinfo]

       let buttons = [button1 gui,button2 gui,button3 gui,button4 gui,button5 gui,button6 gui,button7 gui,button8 gui,button9 gui]
       let labels = [lb1,lb2,lb3,lb4,lb5,lb6,lb7,lb8,lb9]
       let signals = [signal1',signal2',signal3',signal4',signal5',signal6',signal7',signal8',signal9']
       let iorefs = [signal1 nodeinfo,signal2 nodeinfo,signal3 nodeinfo,signal4 nodeinfo,signal5 nodeinfo,signal6 nodeinfo,signal7 nodeinfo,signal8 nodeinfo,signal9 nodeinfo]
       let nodeactivate ind = case (head $ words (labels !! ind)) of "node" -> do
                                                                       signalDisconnect $ signals !! ind
                                                                       tempio <- on (buttons !! ind) buttonActivated (nA $ labels !! ind)
                                                                       writeIORef (iorefs !! ind) tempio
					                               return()

		                                                     _      -> do
                                                                       signalDisconnect $ signals !! ind
                                                                       tempio <- on (buttons !! ind) buttonActivated (run $ words $ nodeFile ++ " " ++ (show $ ind + 1) ++ " gui")
				                                       writeIORef (iorefs !! ind) tempio
					                               return()

       let labelcheck ind = if (elem "-" $ words $ (lines contents) !! ind) then do
                                                                           signal <- readIORef (iorefs !! ind)
                                                                           signalDisconnect signal
                                                                           tempio <- on (buttons !! ind) buttonActivated $ run $ words $ nodeFile ++ " " ++ (show $ ind + 1) ++ " gui" 
						                           writeIORef (iorefs !! ind) tempio
						                           return()
                                                                  else do
                                                                           return()
       let nodelabelcheck ind = if (elem "-" $ words $ (lines contents) !! ind) && (elem "node" $ words $ (lines contents) !! ind) 
                                 then do
                                   signal <- readIORef (iorefs !! ind)
                                   signalDisconnect $ signal
                                   tempio <- on (buttons !! ind) buttonActivated $ discall nodeinfo >> nA (unwords $ drop 2 $ words $ (lines contents) !! ind)
				   writeIORef (iorefs !! ind) tempio
				   return()
                                 else do
                                   return()
                                                                           
       sequence $ map nodeactivate [0..8]
       sequence $ map labelcheck [0..8]
       sequence $ map nodelabelcheck [0..8]
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

       let labelcheck ind = if (elem "-" $ words $ (lines contents) !! (ind - 1)) then do
                                                                             let lcommand = (words runCommand) !! 2
                                                                             let largs = (drop 3 $ words runCommand)
                                                                             putStrLn $ "DEBUG: RUN: " ++ lcommand ++ (show largs) 
                                                                             forkIO(rawSystem lcommand largs >> return())
						                             return()
                                                                            else do
                                                                             putStrLn $ "DEBUG: RUN: " ++ command ++ (show args) 
                                                                             forkIO(rawSystem command args >> return())
                                                                             return()
       labelcheck $ read numberString

       
       
       hClose handle
run [fileName, numberString, "gui"] = do
       run [fileName, numberString]

       mainQuit
run x = do
       putStrLn $ unwords x

