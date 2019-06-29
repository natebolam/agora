{-|
`Arbitrary` instances for Agora datatypes.
-}
module Agora.Arbitrary
       ( arbitraryByteString
       , arbitrarySentence
       , detGen
       ) where

import qualified Data.ByteString as BS
import Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (DiffTime, UTCTime (..), secondsToDiffTime)
import Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum, choose, elements, vector, vectorOf)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random

import Agora.Types
import Agora.Web.Types

-- | Helper for generating arbitrary @ByteString@s of a given length.
arbitraryByteString :: Int -> Gen ByteString
arbitraryByteString = fmap BS.pack . vector

-- | Vocabulary for generating random texts.
randomWords :: [Text]
randomWords = ["patak", "bardaq", "skovoroda"]

-- | Helper for generating a random sentence with a given length.
arbitrarySentence :: Int -> Gen Text
arbitrarySentence n = T.unwords <$> vectorOf n (elements randomWords)

-- | Runs a generator (@Gen@ value) with a given integer seed
-- and size 30 (which is the same as @generate@ uses)
detGen :: Int -> Gen a -> a
detGen seed gen = unGen gen (mkQCGen seed) 30

instance Arbitrary DiffTime where
  arbitrary = secondsToDiffTime <$> choose (0, 86400)

instance Arbitrary Day where
  arbitrary = fromGregorian
    <$> choose (1995, 2050)
    <*> choose (1, 12)
    <*> choose (1, 31)

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary (Hash a) where
  arbitrary = Hash . encodeBase58 bitcoinAlphabet <$> arbitraryByteString 32

instance Arbitrary Proposal where
  arbitrary = Proposal
    <$> arbitrary
    <*> arbitrarySentence 3
    <*> arbitrarySentence 10

instance Arbitrary PeriodType where
  arbitrary = arbitraryBoundedEnum

deriving instance Arbitrary PeriodNum
deriving instance Arbitrary Level
deriving instance Arbitrary Cycle
deriving instance Arbitrary Votes
deriving instance Arbitrary Rolls

instance Arbitrary Period where
  arbitrary = genericArbitrary

instance Arbitrary VoteStats where
  arbitrary = genericArbitrary

instance Arbitrary Decision where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Ballots where
  arbitrary = genericArbitrary

instance Arbitrary PeriodInfo where
  arbitrary = genericArbitrary

instance Arbitrary Baker where
  arbitrary = Baker <$> arbitrary <*> arbitrary <*> arbitrarySentence 2

instance Arbitrary ProposalVote where
  arbitrary = genericArbitrary

instance Arbitrary Ballot where
  arbitrary = genericArbitrary
