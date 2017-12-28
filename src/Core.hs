module Core(PhotoSetting (..), PhotoFile(..), loadAndGetDifference) where

import System.Directory
import System.FilePath
import Control.Monad
import Data.List

data PhotoFile = PhotoFile
  { path :: String
  , fileName :: String
  , fileExtension :: String
  }
  deriving (Show)
instance Eq PhotoFile where
   PhotoFile a1 b1 c1 == PhotoFile a2 b2 c2 = b1 == b2


data PhotoSetting = PhotoSetting
  { jpegPath :: String
  , rawPath :: String
  , binPath :: String
  , deleteFiles :: Bool
  , rawEnding :: String
  , jpegEnding :: String
  }
  deriving (Show,Eq)

toPhotoFile :: FilePath -> PhotoFile
toPhotoFile file =  PhotoFile{path = (dropFileName file), fileName = (takeBaseName file), fileExtension = (takeExtension file)}


loadAndGetDifference :: PhotoSetting -> IO [PhotoFile]
loadAndGetDifference settings = do
  jpegs <- listDirectory $ toFilePath (jpegPath settings)
  raws <- listDirectory $ toFilePath (rawPath settings)
  let jpegFotoFile = filter (\x -> fileExtension x == jpegEnding settings)(map toPhotoFile jpegs)
  let rawFotoFile = filter (\x -> fileExtension x == rawEnding settings)(map toPhotoFile raws)
  return $ rawFotoFile \\ jpegFotoFile
  -- (map toPhotoFile (listDirectory $ toFilePath (rawPath settings))) \\ (map toPhotoFile(listDirectory $ toFilePath (jpegPath settings)))

toFilePath :: String -> FilePath
toFilePath x = x



-- Delete content below Later
getMocks :: FilePath -> String -> [FilePath]
getMocks path extension = [path++"/"++"a"++extension,path++"/"++"b"++extension,path++"/"++"c"++extension,path++"/"++"d"++extension]

getMocks2 :: FilePath -> String -> [FilePath]
getMocks2 path extension = [path++"/"++"a"++extension,path++"/"++"bc"++extension,path++"/"++"c"++extension,path++"/"++"d"++extension]

getPhotos :: FilePath -> String -> [PhotoFile]
getPhotos path extension = map toPhotoFile (getMocks path extension)

getPhotos2 :: FilePath -> String -> [PhotoFile]
getPhotos2 path extension = map toPhotoFile (getMocks2 path extension)
