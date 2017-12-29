module Cmd (cmdMain) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Directory
import qualified Data.List.Split as Spl
import qualified Data.Text as T
import Core

initsettings = PhotoSetting{jpegPath =  uniformFilePath "C:/Users/Andreas\\Documents/Git/Studium/studienarbeit-haskell_io/photo/"
                          , rawPath = uniformFilePath "C:/Users/Andreas/Documents/Git/Studium/studienarbeit-haskell_io/photo/RAW"
                          , binPath = uniformFilePath "C:/Users/Andreas/Documents/Git/Studium/studienarbeit-haskell_io/photo/bin"
                          , deleteFiles = False
                          , rawEnding = uniformFileExtension ".raw"
                          , jpegEnding = uniformFileExtension ".jpg"} :: PhotoSetting

cmdMain :: IO ()
cmdMain = do
  hSetBuffering stdout NoBuffering
  currDir <- getCurrentDirectory
  inputHandler (initInitSettings initsettings currDir,"Welcome to the JPEG RAW Organizer \nType \"help\" to see all commands\n")
  -- inputHandler (initsettings,"Welcome to the JPEG RAW Organizer \nType \"help\" to see all commands\n")

inputHandler :: (PhotoSetting, String) -> IO ()
inputHandler (settings, message) = do
  hSetBuffering stdout NoBuffering
  putStrLn message
  input <- getLine :: IO String

  if input == "quit" || input == ":q"
    then putStrLn "Bye ..."
    else do
      handledCommand <- handleCommand (toTuple input) settings
      inputHandler handledCommand


handleCommand :: (String, String) -> PhotoSetting -> IO (PhotoSetting,String)
handleCommand (cmd,arg) settings
  | (cmd == "help" || cmd == "h") = return (settings,getHelp)
  | (cmd == "showSettings" || cmd == "sett") = return (settings,show settings)
  | (cmd == "showDifferences" || cmd == "diff") = do
      diff <- calcDifference settings
      return (settings,diff)
  | cmd == "start" = do
      processed <- startProcess settings
      return (settings,processed)
  | (cmd == "RawPath" || cmd == "rp") = return (settings {rawPath = uniformFilePath arg},"rawPath is now " ++ show (uniformFilePath arg))
  | (cmd == "JpegPath" || cmd == "jp") = return (settings {jpegPath = uniformFilePath arg},"jpegPath is now " ++ show (uniformFilePath arg))
  | (cmd == "BinPath" || cmd == "bp") = return (settings {binPath = uniformFilePath arg},"binPath is now " ++ show (uniformFilePath arg))
  | (cmd == "RawEnding" || cmd == "rend") = return (settings {rawEnding = uniformFileExtension arg},"rawEnding is now " ++ show  (uniformFileExtension arg))
  | (cmd == "JpegEnding" || cmd == "jend") = return (settings {jpegEnding = uniformFileExtension arg},"jpegEnding is now " ++ show (uniformFileExtension arg))
  | (cmd == "FlagDeleteFiles" || cmd == "fdf") = return (settings {deleteFiles = stringToBool arg},"delete Files: " ++ show (stringToBool arg))
  | otherwise = return (settings,"! Unknown Command: " ++ show cmd++"\r\n"++getHelp)



getHelp :: String
getHelp  = concat
    ["-------------------------------------------\r\n"
    ,"HELP:\r\n"
    ,"+ Type \"quit\" or \":q\" to leave the program.\r\n"
    ,"+ Type \"help\" or \"h\" for help.\r\n"
    ,"+ Type \"showSettings\" or \"sett\" to print the actual settings to the console.\r\n"
    ,"+ Type \"showDifferences\" or \"diff\" shows the list of differing raw files.\r\n"
    ,"+ Type \"start\" starts to delete or move the differing files.\r\n"
    ,"+ Type \"RawPath\" or \"rp\" [absolute Path] sets the path to the raw files.\r\n"
    ,"+ Type \"JpegPath\" or \"jp\" [absolute Path] sets the path to the jpeg files.\r\n"
    ,"+ Type \"BinPath\" or \"bp\" [absolute Path] sets the path to the bin folder.\r\n"
    ,"+ Type \"RawEnding\" or \"rend\" [.fileEnding] sets the file ending of the raw files.\r\n"
    ,"+ Type \"JpegEnding\" or \"jend\" [.fileEnding] sets the file ending of the jpeg files.\r\n"
    ,"+ Type \"FlagDeleteFiles\" or \"fdf\" [True/False] sets the flag if the files should be deleted or moved to the bin folder.\r\n"
    ,"-------------------------------------------\r\n"
    ]


toTuple :: String -> (String,String)
toTuple input = do
  let splitted = Spl.splitOn " " input
  if length splitted == 2
    then (head splitted, last splitted)
    else (head splitted,"")

stringToBool :: String -> Bool
stringToBool str = (str == "True" || str == "true")


calcDifference ::  PhotoSetting -> IO String
calcDifference  settings = do
   list <- loadAndGetDifference settings
   let message = "Differences: \r\n" ++ formatPhotoFileList list
   return message

startProcess :: PhotoSetting -> IO String
startProcess settings = do
  if deleteFiles settings
    then do
      deletedFiles <- deleteDifferenceFiles settings
      let message = "Deleted: \r\n"++ formatPhotoFileList deletedFiles
      return message
    else do
      movedFiles <- moveDifferenceFilesToBin settings
      let message ="Moved: \r\n" ++ formatPhotoFileList movedFiles
      return message

uniformFilePath :: String -> String
uniformFilePath path = do
  let nPath = map (\c -> if c == '\\' then '/' else c) path
  if length path > 0
    then if last nPath /= '/'
        then nPath ++ "/"
        else nPath
    else "./"

uniformFileExtension :: String -> String
uniformFileExtension ext = do
  if length ext >0
    then if head ext /= '.'
          then "." ++ ext
          else ext
    else ".*"

initInitSettings :: PhotoSetting -> String -> PhotoSetting
initInitSettings settings path = do
  let firstStep = settings {jpegPath = uniformFilePath path}
  let secondStep = firstStep {rawPath = uniformFilePath $ jpegPath firstStep ++ "RAW"}
  secondStep {binPath = uniformFilePath $ jpegPath firstStep ++ "bin"}

formatPhotoFileList :: [PhotoFile] -> String
formatPhotoFileList list = concat [ "----------------------------------------------\r\n"
                                  , concat (map (\x -> "- \"" ++ fileName x ++ fileExtension x ++"\"\r\n") list)
                                  , "----------------------------------------------\r\n"
                                  ]
