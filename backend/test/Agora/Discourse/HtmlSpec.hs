module Agora.Discourse.HtmlSpec
      ( spec
      ) where

import qualified Data.ByteString.Lazy as BS
import Test.QuickCheck ((===), withMaxSuccess)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Agora.Arbitrary ()
import Agora.Types
import Agora.Discourse

spec :: Spec
spec = describe "Test parsing" $ do
  it "Parsing of default body" $ withMaxSuccess 3 $ \propHash ->
    let shorten = shortenHash propHash in
    parseHtmlParts (defaultDescription shorten) === Right (defaultHtmlParts shorten)

  it "Test1" $ do
    let expected = HtmlParts
            { hpShort = "Short description here"
            , hpLong = "Long description here<br>"
            , hpFileLink = Just "Proposal_link"
            }
    testParsing "test1_discourse.html" expected

  it "Test2" $ do
    let expected = HtmlParts
            { hpShort = "Short description here"
            , hpLong = "<p id=\"p1\">Long description here. Paragraph1. Sentence.</p>" <>
                       "<p id=\"p2\">Long description here. Paragraph2. Sentence. </p>"
            , hpFileLink = Just "Proposal_link"
            }
    testParsing "test2_discourse.html" expected

  it "Test3 (no long description)" $ do
    let expected = HtmlParts
            { hpShort = "Short description here"
            , hpLong = ""
            , hpFileLink = Just "Proposal_link"
            }
    testParsing "test3_discourse.html" expected

  it "Test4 (no archive reference)" $ do
    let expected = HtmlParts
            { hpShort = "Short description here"
            , hpLong = "Long description without link to archive"
            , hpFileLink = Nothing
            }
    testParsing "test4_discourse.html" expected

  it "Test5 (empty href)" $ do
    let expected = HtmlParts
            { hpShort = "Short description here"
            , hpLong = "Long description"
            , hpFileLink = Just ""
            }
    testParsing "test5_discourse.html" expected

  it "Test6 (real example)" $ do
    let expected = HtmlParts
            { hpShort = "This is a short description. kek"
            , hpLong = "<p>This is a long description.</p>"
            , hpFileLink = Just "https://www.google.com/"
            }
    testParsing "test6_discourse.html" expected

  it "Test7 (real example)" $ do
    let expected = HtmlParts
            { hpShort = "This is a short description. kek jfdhjffj hdjf hfj hdjg hfjg hdfjghdj"
            , hpLong = "<p>This is a long description.<br>krkekrkerj<br>fkjdkgjfgfg<br>fldkkdjgkdf</p>"
            , hpFileLink = Just "https://www.google.com/"
            }
    testParsing "test7_discourse.html" expected

  it "Test8 (real example with extra html after proposal link)" $ do
    let expected = HtmlParts
            { hpShort = "This is a short description. Sentence1.<br>Sentence2.<br>Sentence3."
            , hpLong = "<p>This is a long description.<br>krkekrkerj<br>fkjdkgjfgfg<br>fldkkdjgkdf</p>"
            , hpFileLink = Just "https://www.google.com/"
            }
    testParsing "test8_discourse.html" expected

  it "Test9 (real example with extra html after proposal link)" $ do
    let expected = HtmlParts
            { hpShort = "This is a short description. Sentence1.<br>Sentence2."
            , hpLong = "<p>Sentence3.</p><p>This is a long description.<br>krkekrkerj<br>fkjdkgjfgfg<br>fldkkdjgkdf</p>"
            , hpFileLink = Just "https://www.google.com/"
            }
    testParsing "test9_discourse.html" expected

testParsing :: FilePath -> HtmlParts Text -> IO ()
testParsing path expected = do
  let root = "resources/discourse/"
  content <- BS.readFile (root <> path)
  case parseHtmlParts (decodeUtf8 content) of
    Left err  -> expectationFailure err
    Right res -> res `shouldBe` expected
