{-# LANGUAGE ScopedTypeVariables #-}
module CmdSpec (spec) where

import Cmd
import Core
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  -- | tests for uniformFileExtension
  describe "uniformFileExtension" $ do
    it "uniformes the file extension jpg" $
      uniformFileExtension "jpg" `shouldBe` ".jpg"
    it "uniformes the file extension .jpg" $
      uniformFileExtension ".jpg" `shouldBe` ".jpg"
    it "uniformes the empty file extension" $
      uniformFileExtension "" `shouldBe` ".*"
  -- | tests for uniformFilePath
  describe "uniformFilePath" $ do
    it "uniforms the path C:/user/noUser" $
      uniformFilePath "C:/user/noUser" `shouldBe` "C:/user/noUser/"
    it "uniforms the path C:\\user\\noUser" $
      uniformFilePath "C:\\user\\noUser" `shouldBe` "C:/user/noUser/"
    it "uniforms the path C:/user/noUser/" $
      uniformFilePath "C:/user/noUser/" `shouldBe` "C:/user/noUser/"
    it "uniforms the path C:\\user\\noUser\\" $
      uniformFilePath "C:\\user\\noUser\\" `shouldBe` "C:/user/noUser/"
    it "uniforms the empty path " $
      uniformFilePath "" `shouldBe` "./"
  -- | tests for uniformFileExtension
  describe "toTuple" $ do
    it "split string to tuple: 'cmd'" $
      toTuple "cmd" `shouldBe` ("cmd", "")
    it "split string to tuple: 'cmd '" $
      toTuple "cmd " `shouldBe` ("cmd", "")
    it "split string to tuple: 'cmd  '" $
      toTuple "cmd  " `shouldBe` ("cmd", "")
    it "split string to tuple: 'cmd arg'" $
      toTuple "cmd arg" `shouldBe` ("cmd", "arg")
    it "split string to tuple: 'cmd arg arg2'" $
      toTuple "cmd arg arg2" `shouldBe` ("cmd", "arg arg2")
    it "split string to tuple: 'cmd arg arg2 arg3'" $
      toTuple "cmd arg arg2 arg3" `shouldBe` ("cmd", "arg arg2 arg3")
    it "split string to tuple: ''" $
      toTuple "" `shouldBe` ("","")
  -- | tests for stringToBool
  describe "stringToBool" $ do
    it "it convertes true to a boolean" $
      stringToBool "true" `shouldBe` True
    it "it convertes True to a boolean" $
      stringToBool "True" `shouldBe` True
    it "it convertes false to a boolean" $
      stringToBool "false" `shouldBe` False
    it "it convertes something to a boolean" $
      stringToBool "something" `shouldBe` False
    it "it convertes 's1 s2 s3' to a boolean" $
      stringToBool "s1 s2 s3" `shouldBe` False
    it "it convertes '' to a boolean" $
      stringToBool "" `shouldBe` False
  -- | tests for formatPhotoFileList
  describe "formatPhotoFileList" $ do
    it "formats a list of PhotoFiles" $ do
      let fileList = [PhotoFile{path = "/home/", fileName = "TestFile", fileExtension = ".test"}]
      let result = concat [ "----------------------------------------------\r\n"
                          , "- \"TestFile.test\"\r\n"
                          , "----------------------------------------------\r\n"
                          ]
      formatPhotoFileList fileList `shouldBe` result
