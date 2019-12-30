module Agora.Web.LoggingSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Loot.Log (LogConfig (..), NameSelector (..), Severity (..), withLogging, BackendConfig (..), logInfo, logDebug)
import Monad.Capabilities (emptyCaps)
import Fmt ((+|), (|+))

import Agora.Mode (setEncoding)

spec :: Spec
spec = describe "Logging" $
  it "Logging of unicode symbols" $ do
    setEncoding
    let testString1 = "čušpajž日本語"
    let testString2 = "ØCrypto Pool"
    putTextLn testString1
    putTextLn testString2
    usingReaderT emptyCaps $
      withLogging (LogConfig [StdErr] Debug) CallstackName $ do
        logInfo ("" +| testString1 |+ "")
        logDebug ("" +| testString2 |+ "")
