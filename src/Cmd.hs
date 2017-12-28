module Cmd (cmdMain) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import qualified Data.List.Split as Spl
import Core

initsettings = PhotoSetting{jpegPath = "./", rawPath ="./", binPath = "./bin", deleteFiles = False, rawEnding = ".raw", jpegEnding = ".jpg"} :: PhotoSetting

cmdMain :: IO ()
cmdMain = do
  hSetBuffering stdout NoBuffering
  inputHandler (initsettings,"Welcome to the JPEG RAW Organizer \nType \"help\" to see all commands\n")

inputHandler :: (PhotoSetting, String) -> IO ()
inputHandler (settings, message) = do
  hSetBuffering stdout NoBuffering
  putStrLn message
  input <- readLn :: IO String
  if input == "quit"
    then putStrLn "Beenden ..."
    else inputHandler (handleCommand (toTuple input) settings)


handleCommand :: (String, String) -> PhotoSetting ->(PhotoSetting,String)
handleCommand ("help",_) settings = (settings,getHelp)
handleCommand("printSettings",_) settings = (settings,show settings)
handleCommand("RawPath",path) settings = (settings {rawPath = path},"rawPath is now " ++ show path)
handleCommand("JpegPath",path) settings= (settings {jpegPath = path},"jpegPath is now " ++ show path)
handleCommand("BinPath",path) settings= (settings {binPath = path},"binPath is now "++ show path)
handleCommand("RawEnding",ending) settings= (settings {rawEnding = ending},"rawEnding is now " ++ show ending)
handleCommand("JpegEnding",ending) settings= (settings {jpegEnding = ending}, "jpegEnding is now "++ show ending)
handleCommand("DeleteFiles",delete) settings = (settings {deleteFiles = stringToBool delete},"delete Files: " ++ show (stringToBool delete))
handleCommand (cmd,_) settings =(settings,"! Unknown Command: " ++ show cmd++"\r\n"++getHelp)



getHelp :: String
getHelp  = concat
    ["-------------------------------------------\r\n"
    ,"HELP:\r\n"
    ,"+ Type \"quit\" to leave the program.\r\n"
    ,"+ Type \"help\" for help.\r\n"
    ,"-------------------------------------------\r\n"]


toTuple :: String -> (String,String)
toTuple input = do
  let splitted = Spl.splitOn " " input
  if length splitted == 2
    then (head splitted, last splitted)
    else (head splitted,"")

stringToBool :: String -> Bool
stringToBool "True" = True
stringToBool "true" = True
stringToBool "1" = True
stringToBool "j" = True
stringToBool "yes" = True
stringToBool str = False
