module CoreSpec (spec) where

import Core
import Test.Hspec
import Test.QuickCheck

-- addPathToPhotoFiles
spec :: Spec
spec = do
  -- | tests for toPhotoFile
  describe "toPhotoFile" $ do
    it "transforms a FilePath '/home/TestFile.test' to a PhotoFile" $ do
      let file = PhotoFile{path = "/home/", fileName = "TestFile", fileExtension = ".test"}
      let path = "/home/TestFile.test"
      toPhotoFile path `shouldBe` file
    it "transforms a FilePath '/home/TestFile' to a PhotoFile" $ do
      let file = PhotoFile{path = "/home/", fileName = "TestFile", fileExtension = ""}
      let path = "/home/TestFile"
      toPhotoFile path `shouldBe` file
    it "transforms a FilePath 'TestFile.test' to a PhotoFile" $ do
      let file = PhotoFile{path = "", fileName = "TestFile", fileExtension = ".test"}
      let path = "TestFile.test"
      toPhotoFile path `shouldBe` file
    it "transforms a FilePath '/home/.TestFile' to a PhotoFile" $ do
      let file = PhotoFile{path = "/home/", fileName = "", fileExtension = ".TestFile"}
      let path = "/home/.TestFile"
      toPhotoFile path `shouldBe` file
    it "transforms a empty FilePath '' to a PhotoFile" $ do
      let file = PhotoFile{path = "", fileName = "", fileExtension = ""}
      let path = ""
      toPhotoFile path `shouldBe` file
  -- | tests for concatFilePath
  describe "concatFilePath" $ do
    it "transforms a PhotoFile to a FilePath '/home/TestFile.test'" $ do
        let file = PhotoFile{path = "/home/", fileName = "TestFile", fileExtension = ".test"}
        let path = "/home/TestFile.test"
        concatFilePath file `shouldBe` path
    it "transforms a PhotoFile to a FilePath '/home/TestFile'" $ do
        let file = PhotoFile{path = "/home/", fileName = "TestFile", fileExtension = ""}
        let path = "/home/TestFile"
        concatFilePath file `shouldBe` path
    it "transforms a PhotoFile to a FilePath 'TestFile.test'" $ do
        let file = PhotoFile{path = "", fileName = "TestFile", fileExtension = ".test"}
        let path = "TestFile.test"
        concatFilePath file `shouldBe` path
    it "transforms a PhotoFile to a FilePath '/home/.test'" $ do
        let file = PhotoFile{path = "/home/", fileName = "", fileExtension = ".test"}
        let path = "/home/.test"
        concatFilePath file `shouldBe` path
    it "transforms a empty PhotoFile to a FilePath ''" $ do
        let file = PhotoFile{path = "", fileName = "", fileExtension = ""}
        let path = ""
        concatFilePath file `shouldBe` path
  -- | tests for getFilepathsFromPhotoFiles
  describe "getFilepathsFromPhotoFiles" $ do
    it "get a list of FilePaths from a list of three PhotoFiles" $ do
      let files = [ PhotoFile{path = "/home/", fileName = "TestFile1", fileExtension = ".test"}
                  , PhotoFile{path = "/home/", fileName = "TestFile2", fileExtension = ".test"}
                  , PhotoFile{path = "/home/", fileName = "TestFile3", fileExtension = ".test"}
                  ]
      let paths = [ "/home/TestFile1.test"
                  , "/home/TestFile2.test"
                  , "/home/TestFile3.test"
                  ]
      getFilepathsFromPhotoFiles files `shouldBe` paths
    it "get a list of FilePaths from a list of one PhotoFile" $ do
      let files = [ PhotoFile{path = "/home/", fileName = "TestFile1", fileExtension = ".test"}]
      let paths = [ "/home/TestFile1.test"]
      getFilepathsFromPhotoFiles files `shouldBe` paths
    it "get a list of FilePaths from a empty list of PhotoFiles" $ do
      let files = []
      let paths = []
      getFilepathsFromPhotoFiles files `shouldBe` paths
  -- | tests for addPathToPhotoFiles
  describe "addPathToPhotoFiles" $ do
    it "gets a list of PhotoFiles and a FilePath and set it to all Files." $ do
        let files = [ PhotoFile{path = "/home/", fileName = "TestFile1", fileExtension = ".test"}
                    , PhotoFile{path = "/home/", fileName = "TestFile2", fileExtension = ".test"}
                    , PhotoFile{path = "/home/", fileName = "TestFile3", fileExtension = ".test"}
                    ]
        let newPath = "/new/path/" :: FilePath
        let result = [ PhotoFile{path = newPath, fileName = "TestFile1", fileExtension = ".test"}
                     , PhotoFile{path = newPath, fileName = "TestFile2", fileExtension = ".test"}
                     , PhotoFile{path = newPath, fileName = "TestFile3", fileExtension = ".test"}
                     ]
        addPathToPhotoFiles files newPath `shouldBe` result
    it "gets a list of PhotoFiles and a empty FilePath and set it to all Files." $ do
        let files = [ PhotoFile{path = "/home/", fileName = "TestFile1", fileExtension = ".test"}
                    , PhotoFile{path = "/home/", fileName = "TestFile2", fileExtension = ".test"}
                    , PhotoFile{path = "/home/", fileName = "TestFile3", fileExtension = ".test"}
                    ]
        let newPath = "" :: FilePath
        let result = [ PhotoFile{path = newPath, fileName = "TestFile1", fileExtension = ".test"}
                     , PhotoFile{path = newPath, fileName = "TestFile2", fileExtension = ".test"}
                     , PhotoFile{path = newPath, fileName = "TestFile3", fileExtension = ".test"}
                     ]
        addPathToPhotoFiles files newPath `shouldBe` result
    it "gets a empty list of PhotoFiles and a FilePath and set it to all Files." $ do
        let files = []
        let newPath = "/new/path/" :: FilePath
        let result = []
        addPathToPhotoFiles files newPath `shouldBe` result
    it "gets a list of one PhotoFile and a FilePath and set it to all Files." $ do
        let files = [ PhotoFile{path = "/home/", fileName = "TestFile1", fileExtension = ".test"}]
        let newPath = "/new/path/" :: FilePath
        let result = [ PhotoFile{path = newPath, fileName = "TestFile1", fileExtension = ".test"}]
        addPathToPhotoFiles files newPath `shouldBe` result
