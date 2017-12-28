module Core(PhotoSetting (..), PhotoFile, getTestString) where

import System.Directory
import System.FilePath
import Control.Monad

data PhotoFile = PhotoFile
  { path :: String
  , fileName :: String
  , fileExtension :: String
  }
  deriving (Show)


data PhotoSetting = PhotoSetting
  { jpegPath :: String
  , rawPath :: String
  , binPath :: String
  , deleteFiles :: Bool
  , rawEnding :: String
  , jpegEnding :: String
  }
  deriving (Show,Eq)


getPhotos :: FilePath -> String -> [PhotoFile]
getPhotos path extension = map toPhotoFile (getMocks path extension)

toPhotoFile :: FilePath -> PhotoFile
toPhotoFile file = PhotoFile{path = (dropFileName file), fileName = (takeBaseName file), fileExtension = (takeExtension file)}

getTestString :: String
getTestString = "test"



getMocks :: FilePath -> String -> [FilePath]
getMocks path extension = [path++"/"++"a"++extension,path++"/"++"b"++extension,path++"/"++"c"++extension,path++"/"++"d"++extension]

-- getFileList :: FilePath -> [FilePath]
-- getFileList path = listDirectory path
