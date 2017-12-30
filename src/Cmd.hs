-- |The module to handle all comands entered at the console.
module Cmd (cmdMain, inputHandler, handleCommand, toTuple, stringToBool, uniformFilePath, uniformFileExtension,  formatPhotoFileList) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Directory
import qualified Data.Text as T
import Core


initsettings = PhotoSetting{jpegPath =  uniformFilePath "C:/Users/Andreas\\Documents/Git/Studium/studienarbeit-haskell_io/photo/"
                          , rawPath = uniformFilePath "C:/Users/Andreas/Documents/Git/Studium/studienarbeit-haskell_io/photo/RAW"
                          , binPath = uniformFilePath "C:/Users/Andreas/Documents/Git/Studium/studienarbeit-haskell_io/photo/bin"
                          , deleteFiles = False
                          , rawEnding = uniformFileExtension ".raw"
                          , jpegEnding = uniformFileExtension ".jpg"} :: PhotoSetting

-- |Starts to listen for commands on the console.
cmdMain :: IO ()
cmdMain = do
  hSetBuffering stdout NoBuffering
  currDir <- getCurrentDirectory
  inputHandler (initInitSettings initsettings currDir,"Welcome to the JPEG RAW Organizer \nType \"help\" to see all commands\n")
  -- inputHandler (initsettings,"Welcome to the JPEG RAW Organizer \nType \"help\" to see all commands\n")

-- |Handles the user input by reading a line from the console recursively.
inputHandler :: (PhotoSetting, String) -- ^Tuple containing the current settings
                                       -- of the program and an input string.
            -> IO ()                   -- ^An empty io monade.
inputHandler (settings, message) = do
  hSetBuffering stdout NoBuffering
  putStrLn message
  input <- getLine :: IO String

  if input == "quit" || input == ":q"
    then putStrLn "Bye ..."
    else do
      handledCommand <- handleCommand (toTuple input) settings
      inputHandler handledCommand

-- |Takes the command the user entered and does some action depeding on it.
handleCommand :: (String, String)         -- ^Tupel containing the command and its optional arguments.
              -> PhotoSetting             -- ^The current settings of the program.
              -> IO (PhotoSetting,String) -- ^Tupel of the maybe changed settings and a
                                          -- message about the action performed.
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


-- |Returns the string describing all possible command of the program.
getHelp :: String -- ^String describing all possible commands.
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

-- |Takes the user input string and split it at the first blank character
-- into the command and its optional argument. If the entered command has no
-- argument the argument string is empty.
toTuple :: String         -- ^The string the user write on the console.
       -> (String,String) -- ^Tuple of the command identifier and it optional
                          -- arguments (maybe an empty string)
toTuple input = do
  let splitted = T.breakOn (T.pack " ") (T.pack input)
  let first = T.unpack  (fst splitted)
  let second = (T.unpack $ T.strip (snd splitted))
  (first,second)



-- |Parses a string to a boolean.
-- Accepted are \"True\" and \"true\" all other string result in False.
stringToBool :: String -- ^The string to be parsed.
             -> Bool   -- ^The string parsed to an boolean.
stringToBool str = (str == "True" || str == "true")

-- |Loads the raw and jpeg files at the specified paths and returns a string listing the
-- differing raw files.
calcDifference ::  PhotoSetting -- ^The current settings of the program.
                -> IO String    -- ^String listing the raw files differing from the jpeg files.
calcDifference  settings = do
   list <- loadAndGetDifference settings
   let message = "Differences: \r\n" ++ formatPhotoFileList list
   return message

-- |Depending on the deleteFiles flag in the settings the raw files differing
-- from the jpeg files at the specified paths are deleted or moved to a bin folder.
startProcess :: PhotoSetting -- ^The current settings of the program.
            -> IO String     -- ^String listing all deleted or moved raw files.
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

-- |Brings a given file path to an universal format by replacing backslash
-- with slash and adding a slash at the end of the path if there is no one.
-- If the given file path is an empty string the relativ path of the program
-- \"./\" is returned.
uniformFilePath :: String -- ^The file path to be uniformed as a string.
                -> String -- ^The uniformed file path as a string.
uniformFilePath path = do
  let nPath = map (\c -> if c == '\\' then '/' else c) path
  if length path > 0
    then if last nPath /= '/'
        then nPath ++ "/"
        else nPath
    else "./"


-- |Brings a given file extension to an universal format by replacing adding a
-- dot at the beginning if there is no one.
-- If the given extension is an empty string ".*" is returned.
uniformFileExtension :: String -- ^The extension to be uniformed as a string.
                     -> String -- ^The uniformed extension as a string.
uniformFileExtension ext = do
  if length ext >0
    then if head ext /= '.'
          then "." ++ ext
          else ext
    else ".*"

-- |Initializes the given settings by setting the paths relativ to the
-- given path.
initInitSettings :: PhotoSetting -- ^The current settings of the program.
                 -> String       -- ^The path all setting paths should be set relativ to.
                 -> PhotoSetting -- ^The settings with the changed paths.
initInitSettings settings path = do
  let firstStep = settings {jpegPath = uniformFilePath path}
  let secondStep = firstStep {rawPath = uniformFilePath $ jpegPath firstStep ++ "RAW"}
  secondStep {binPath = uniformFilePath $ jpegPath firstStep ++ "bin"}

-- |Takes a list of PhotoFiles and returns a string listing all files well
-- formatted.
formatPhotoFileList :: [PhotoFile] -- ^The list of photo files that should be formatted as a string.
                    -> String      -- ^The string listing all given photo files well formatted.
formatPhotoFileList list = concat [ "----------------------------------------------\r\n"
                                  , concat (map (\x -> "- \"" ++ fileName x ++ fileExtension x ++"\"\r\n") list)
                                  , "----------------------------------------------\r\n"
                                  ]
