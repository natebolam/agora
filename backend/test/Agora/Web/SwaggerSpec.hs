module Agora.Web.SwaggerSpec (spec) where

import Data.Aeson (eitherDecode, toJSON)
import qualified Data.ByteString.Lazy as LBS
import Servant.Swagger.Test (validateEveryToJSON)
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

import Agora.Arbitrary ()
import Agora.Web.API (agoraAPI)
import Agora.Web.Swagger (agoraApiSwagger, swaggerSpecFilePath)

spec :: Spec
spec = do
  describe "Swagger docs match the real API behavior" $
    validateEveryToJSON agoraAPI
  describe "File spec in the repository" $ do
    fileContents <- runIO $ LBS.readFile swaggerSpecFilePath
    it "is equal to the generated one" $ case eitherDecode fileContents of
      Left err       -> expectationFailure err
      Right fileSpec -> fileSpec `shouldBe` toJSON agoraApiSwagger
