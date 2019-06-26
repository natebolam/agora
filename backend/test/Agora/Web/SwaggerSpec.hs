module Agora.Web.SwaggerSpec (spec) where

import Servant.Swagger.Test (validateEveryToJSON)
import Test.Hspec (Spec, describe)

import Agora.Arbitrary ()
import Agora.Web.API (agoraAPI)
import Agora.Web.Swagger ()

spec :: Spec
spec = describe "Swagger docs match the real API behavior" $
  validateEveryToJSON agoraAPI
