{-# LANGUAGE ScopedTypeVariables #-}
module LibSpec (spec) where

import Cmd (uniformFileExtension, uniformFilePath)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
-- | tests for uniformFileExtension
spec =
describe "uniformFileExtension" $ do
it "uniformes the file extension jpg" $
uniformFileExtension "jpg" `shouldBe` ".jpg"
it "uniformes the file extension .jpg" $
uniformFileExtension ".jpg" `shouldBe` ".jpg"
it "uniformes the empty file extension" $
uniformFileExtension "" `shouldBe` ".*"
-- | tests for uniformFilePath
spec =
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
-- spec =
-- describe "uniformFileExtension" $ do
-- it "uniformes the file extension jpg" $
-- uniformFileExtension "jpg" `shouldBe` ".jpg"
-- it "uniformes the file extension .jpg" $
-- uniformFileExtension ".jpg" `shouldBe` ".jpg"
-- it "uniformes the empty file extension" $
-- uniformFileExtension "" `shouldBe` ".*"
