-- | Main module of the JpegRawOrganizer.
module Main where
import System.Environment
import Cmd

-- | The main method.
main :: IO ()
main = do
      arguments <- getArgs
      if length arguments == 1 && (head arguments) == "gui"
        then putStrLn "start the GUI! -- Which GUI? -- OUR GUI! -- We have no GUI! -- ok ... sorry..."
        else cmdMain
