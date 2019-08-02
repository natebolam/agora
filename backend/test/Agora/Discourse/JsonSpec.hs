module Agora.Discourse.JsonSpec
      ( spec
      ) where

import Data.Aeson (FromJSON, eitherDecode')
import qualified Data.ByteString.Lazy as BS
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Agora.Discourse

spec :: Spec
spec = describe "JSON API decoding" $ do
  it "CreatedTopic" $
    testDecoding "created_topic.json" (CreatedTopic 51 57)

  it "CategoryTopics" $ do
    let topic1 = TopicHead 7 "Welcome to the Tezos Agora Forum"
    let topic2 = TopicHead 2 "About the Agora category"
    testDecoding "category_topics.json" (CategoryTopics 30 [topic1, topic2])

  it "CategoryList" $ do
    let category1 = Category 3 "Agora"
    let category2 = Category 5 "Proposals"
    testDecoding "category_list.json" (CategoryList [category1, category2])

  it "Topic" $ do
    let postTitle = "<p><i>Description for Psd1ynUB will be added by a moderator</i></p><a>Proposal archive</a>"
    let topic = MkTopic 56 "Psd1ynUB" $ one $ Post 50 56 postTitle :: Topic
    testDecoding "topic.json" topic

testDecoding
  :: (Show a, Eq a, FromJSON a)
  => FilePath -> a -> IO ()
testDecoding path expected = do
  let root = "resources/discourse/"
  content <- BS.readFile (root <> path)
  case eitherDecode' content of
    Left err  -> expectationFailure err
    Right res -> res `shouldBe` expected
