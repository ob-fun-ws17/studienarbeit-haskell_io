module Cmd (cmdMain) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

cmdMain :: IO ()
cmdMain = do
  hSetBuffering stdout NoBuffering
  putStr "Welcome to the JPEG RAW Organizer \nType \"help\" to see all commands\n"
  inputHandler

inputHandler :: IO ()
inputHandler = do
  hSetBuffering stdout NoBuffering
  input <- readLn :: IO String
  handleCommand input


handleCommand :: String -> IO ()
handleCommand "quit" = putStrLn "Beenden..."
handleCommand "help" = do
  printHelp
  inputHandler
handleCommand cmd = do
  putStrLn ("! Unknown Command: " ++ cmd)
  printHelp
  inputHandler


printHelp :: IO ()
printHelp = do
  putStrLn ("-------------------------------------------")
  putStrLn ("HELP:")
  putStrLn ("+ Type \"quit\" to leave the program.")
  putStrLn ("+ Type \"help\" for help.")
  putStrLn ("-------------------------------------------")
