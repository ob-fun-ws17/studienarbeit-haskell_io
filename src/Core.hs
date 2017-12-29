-- | The coreModule is the core of the Programm, it handles core functionality.
-- this functionallity is in reading file indexes of directories, comapring the files read, and deleting or moving files.
-- to be more concrete, the core reads (and filters) a directory containing so called jpeg files and one containing so called raw files
-- then it comapares the two lists (jpeg list and raw list). in order to find all raw files, that have no matching jpeg file. (called differences)
-- in the last step, the core, depending on the settings, deletes or moves all differences. 
-- Info: For simplicitie's sake the files matching the rawEnding of the settings are
-- calleds raw files and the files matching the jpegEnding are called jpeg files even they could have any ending and could be files of any type.
module Core(PhotoSetting (..), PhotoFile(..), loadAndGetDifference, deleteDifferenceFiles, moveDifferenceFilesToBin) where

import System.Directory
import System.FilePath
import Control.Monad
import Data.List

-- | data PhotoFile
-- This data represents a Photo File, or generally spoken a File.
data PhotoFile = PhotoFile
  { path :: String -- ^ the Path where te File is located ex.: \/home\/photos
  , fileName :: String -- ^ the name of the File ex.: picture
  , fileExtension :: String -- ^ the fileextension ex.: .jpg
  }
  deriving (Show)
-- | in this instance of eq, two PhotoFiles are equal, iff their names are equal.
instance Eq PhotoFile where
  PhotoFile a1 b1 c1 == PhotoFile a2 b2 c2 = b1 == b2
instance Ord PhotoFile where
  PhotoFile a1 b1 c1 <= PhotoFile a2 b2 c2 = b1 <= b2

-- | data PhotoSetting
-- PhotoSetting is a Data structure, that reprensents parameters
-- which have to be set, bevore the Programm can do its job. 
data PhotoSetting = PhotoSetting
  { jpegPath :: String -- ^ the Path where the Jpegs are located. (the path with the less ammount of files.)
  , rawPath :: String -- ^ the Path where the Raw Files are located. (the path, containing some files to move or delete.)
  , binPath :: String -- ^ the Path where Raw files, should be moved to.
  , deleteFiles :: Bool -- ^ a flag. if True, the files will be deleted. If False, the files will be moved to the specified binPath.
  , rawEnding :: String -- ^ the file extension of the files, which should be classified as \"Raw files\" for example \".CR2\"
  , jpegEnding :: String -- ^ the filex extension of the files, which should be classified as \"Jpeg file\" for example \".jpg\" 
  }
  deriving (Eq)
  -- | this instance of show, formats the Photosettings String to make it appear more beautiful. 
instance Show PhotoSetting where
  show (PhotoSetting a b c d e f) = concat [ "PhotoSettings: \r\n"
                                            , "jpegPath:    "++ show a ++ "\r\n"
                                            , "rawPath:     "++ show b ++ "\r\n"
                                            , "binPath:     "++ show c ++ "\r\n"
                                            , "jpegEnding:  "++ show f ++ "\r\n"
                                            , "rawEnding:   "++ show e ++ "\r\n"
                                            , "deleteFiles: "++ show d ++ "\r\n"]

-- | toPhotoFiles gets a FilePath aka String, which contains the Path, the Filename and the fileextension. 
-- then it cuts the Path in three Parts and returns a PhotoFile. 
toPhotoFile :: FilePath -- ^ a FilePath containing the Path, a Filename and a File extension.
            -> PhotoFile -- ^ a PhotoFile Representation generated from the Path.
toPhotoFile file =  PhotoFile{path = (dropFileName file), fileName = (takeBaseName file), fileExtension = (takeExtension file)}
-- | concatFilePath is the reverse function of toPhotoFile.
-- It needs a PhotoFile, and concats its part to a fully qualified FilePath, with path, filename and file extension.
concatFilePath :: PhotoFile -- ^ a Photofile Representation
                -> FilePath -- ^ The FilePath, representing the Photofile.
concatFilePath photo = path photo ++ fileName photo ++ fileExtension photo

-- | loadAndGetDifference receives PhotoSettings.
-- at first it load both, in the settings specified paths (rawPath and jpegPath)
-- then it creates PhotoFiles out of the FileLists and filters them, using the
-- raw- and jpegending, also specified in the settings. 
-- then it compares both lists, and returns a list of all \"Raw Files\", that have no 
-- counterpart in the \"Jpeg Files\" list.
loadAndGetDifference :: PhotoSetting -- ^ the PhotoSettings, containing all relevant paths. 
                      -> IO [PhotoFile] -- ^ a List of \"Raw\" PhotoFiles, which have not counterpart in the "Jpeg" File list.
loadAndGetDifference settings = do
  let jPath = jpegPath settings
  let rPath = rawPath settings
  jpegs <- loadPhotoFiles jPath (jpegEnding settings)
  raws <- loadPhotoFiles rPath (rawEnding settings)
  let jpegFotoFile = addPathToPhotoFiles jpegs jPath
  let rawFotoFile = addPathToPhotoFiles raws rPath
  return $ rawFotoFile \\ jpegFotoFile -- \\ returns all files that are in rawFotoFile but not in jpegFotoFile
-- | loadPhotoFiles receives a FilePath and a Fileextension
-- and retunrs a list of all files, in the specified path with the specified extension.
loadPhotoFiles :: FilePath -- ^ the FilePath to read / load from.
                -> String -- ^ the File extension of the considered files.
                -> IO [PhotoFile] -- ^ a List of PhotoFiles (Files), which are located in the specified path.
loadPhotoFiles path extension  = do
  files <- listDirectory path
  return $ filter (\x -> fileExtension x == extension)(map toPhotoFile files)
-- | deleteDifferenceFiles
-- gets some PhotoSettings. At first it loads from the specific paths, 
-- and calculates the Raw Files, wich have no counterpart in the Jpeg List
-- then it deletes the calculated Raw Files. 
deleteDifferenceFiles :: PhotoSetting -- ^ PhotoSettings, containing the relevant Paths and extensions. 
                      -> IO [PhotoFile] -- ^ A List of PhotoFiles, that have been deleted.
deleteDifferenceFiles settings = do
  pfiles <- loadAndGetDifference settings
  let paths = getFilepathsFromPhotoFiles pfiles
  deleteListOfFiles paths
  return pfiles

-- | moveDifferenceFilesToBin
-- gets some PhotoSettings. At first it loads from the specific paths, 
-- and calculates the Raw Files, wich have no counterpart in the Jpeg List
-- then it moves the calculated Raw Files into the specified binPath
moveDifferenceFilesToBin :: PhotoSetting -- ^ PhotoSettings, containing the relevant Paths and extensions.
                          -> IO [PhotoFile] -- ^ a List of PhotoFiles, that have been moved to the binPath.
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
-- | moveListOfFiles
-- gets a list of Filepaths (old), contining the current location of the files to move
-- and a second list of FilePaths (new), containing the destination path of the files.
-- then it moves all files from the old to the new paths during a recursion.  
moveListOfFiles :: ([FilePath],[FilePath]) -- ^ a tuple of old an new file paths. IMPORTANT:
                                          -- a file, that has its path in the \"old\" list at index 2, 
                                          -- needs to have its new Ppath in the \"new\" list also at index 2.
                -> IO ([FilePath],[FilePath]) -- ^ the tuple of old an new file paths, reduced by the head.
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

-- | deleteListOfFiles
-- gets a list of FilePaths and deletes the addressed files during a recursion
deleteListOfFiles :: [FilePath] -- ^ a list, containing filePaths / files to delete.
                  -> IO [FilePath] --  ^ the input list, reduced by its head.
deleteListOfFiles paths = do
  if length paths > 0
    then do
      let toDelete = head paths
      deleted <- deleteListOfFiles (drop 1 paths)
      removePathForcibly toDelete
      return (toDelete : deleted)
    else
      return []
-- | getFilepathsFromPhotofile
-- gets a list of Photofiles, and produces a list of FilePaths out of it.
getFilepathsFromPhotoFiles ::  [PhotoFile] -- ^ a list of Photofiles, which are needed the FilePaths from
                            -> [FilePath] -- ^ the list of FilePaths, made out of the list of PhotoFiles.
getFilepathsFromPhotoFiles files = map concatFilePath files

-- | addPathToPhotoFiles
-- gets a list of Photofiles and a FilePath, and sets 
-- the specified path as path of any photoFile in the list.
-- its like an giant "setter" setting the same attribute in all
-- objects in the list aka produces a new list with the path as path of each listitem.
addPathToPhotoFiles :: [PhotoFile] -- ^ a list of PhotoFiles, where the path sould be set. 
                    -> FilePath -- ^ a path to set as path of all Photofiles in the list. 
                    -> [PhotoFile] -- ^ a list of Photofiles with the specified path set.
addPathToPhotoFiles files path = map (\x -> x {path = path}) files
