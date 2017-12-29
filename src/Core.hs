module Core(PhotoSetting (..), PhotoFile(..), loadAndGetDifference, deleteDifferenceFiles, moveDifferenceFilesToBin) where

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
instance Ord PhotoFile where
  PhotoFile a1 b1 c1 <= PhotoFile a2 b2 c2 = b1 <= b2


data PhotoSetting = PhotoSetting
  { jpegPath :: String
  , rawPath :: String
  , binPath :: String
  , deleteFiles :: Bool
  , rawEnding :: String
  , jpegEnding :: String
  }
  deriving (Eq)
instance Show PhotoSetting where
  show (PhotoSetting a b c d e f) = concat [ "PhotoSettings: \r\n"
                                           , "jpegPath:    "++ show a ++ "\r\n"
                                           , "rawPath:     "++ show b ++ "\r\n"
                                           , "binPath:     "++ show c ++ "\r\n"
                                           , "jpegEnding:  "++ show f ++ "\r\n"
                                           , "rawEnding:   "++ show e ++ "\r\n"
                                           , "deleteFiles: "++ show d ++ "\r\n"]

toPhotoFile :: FilePath -> PhotoFile
toPhotoFile file =  PhotoFile{path = (dropFileName file), fileName = (takeBaseName file), fileExtension = (takeExtension file)}

concatFilePath :: PhotoFile -> FilePath
concatFilePath photo = path photo ++ fileName photo ++ fileExtension photo

loadAndGetDifference :: PhotoSetting -> IO [PhotoFile]
loadAndGetDifference settings = do
  let jPath = jpegPath settings
  let rPath = rawPath settings
  jpegs <- loadPhotoFiles jPath (jpegEnding settings)
  raws <- loadPhotoFiles rPath (rawEnding settings)
  let jpegFotoFile = addPathToPhotoFiles jpegs jPath
  let rawFotoFile = addPathToPhotoFiles raws rPath
  return $ rawFotoFile \\ jpegFotoFile

loadPhotoFiles :: FilePath -> String -> IO [PhotoFile]
loadPhotoFiles path extension  = do
  files <- listDirectory path
  return $ filter (\x -> fileExtension x == extension)(map toPhotoFile files)

deleteDifferenceFiles :: PhotoSetting -> IO [PhotoFile]
deleteDifferenceFiles settings = do
  pfiles <- loadAndGetDifference settings
  let paths = getFilepathsFromPhotoFiles pfiles
  deleteListOfFiles paths
  return pfiles

moveDifferenceFilesToBin :: PhotoSetting -> IO [PhotoFile]
moveDifferenceFilesToBin settings = do
  pfiles <- loadAndGetDifference settings
  let npfiles = addPathToPhotoFiles pfiles (binPath settings)
  let oldPaths =  sort $ getFilepathsFromPhotoFiles pfiles
  let newPaths = sort $ getFilepathsFromPhotoFiles npfiles
  createDirectoryIfMissing True (binPath settings)
  moved <- moveListOfFiles (oldPaths,newPaths)
  if (length (fst moved) == 0)
    then return []
    else
      return pfiles

moveListOfFiles :: ([FilePath],[FilePath]) -> IO ([FilePath],[FilePath])
moveListOfFiles (oldpaths,newpaths) = do
  if length oldpaths == length newpaths
    then do
      if length oldpaths > 0
        then do
          let oldPath = head oldpaths
          let newPath = head newpaths
          moved <- moveListOfFiles (drop 1 oldpaths,drop 1 newpaths)
          renamePath oldPath newPath
          return (oldPath : fst moved, newPath : snd moved)
        else return ([],[])
    else return ([],[])


deleteListOfFiles :: [FilePath] -> IO [FilePath]
deleteListOfFiles paths = do
  if length paths > 0
    then do
      let toDelete = head paths
      deleted <- deleteListOfFiles (drop 1 paths)
      removePathForcibly toDelete
      return (toDelete : deleted)
    else
      return []

getFilepathsFromPhotoFiles ::  [PhotoFile] -> [FilePath]
getFilepathsFromPhotoFiles files = map concatFilePath files

addPathToPhotoFiles :: [PhotoFile] -> FilePath -> [PhotoFile]
addPathToPhotoFiles files path = map (\x -> x {path = path}) files
