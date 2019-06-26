module Agora.Web.SwaggerSpec (spec) where

import Data.Aeson (toJSON)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither', prettyPrintParseException)
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
    fileContents <- runIO $ BS.readFile swaggerSpecFilePath
    it "is equal to the generated one" $ case decodeEither' fileContents of
      Left err       -> expectationFailure $ prettyPrintParseException err
      Right fileSpec -> fileSpec `shouldBe` toJSON agoraApiSwagger
