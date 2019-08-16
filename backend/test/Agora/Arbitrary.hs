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
import System.Random (Random (..))
import Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum, choose, elements, vector, vectorOf)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random

import Agora.Types
import Agora.Util
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

-- This instance is used for names/descriptions
instance Arbitrary Text where
  arbitrary = arbitrarySentence 5

instance Arbitrary (Hash a) where
  arbitrary = Hash . encodeBase58 bitcoinAlphabet <$> arbitraryByteString 32

instance Arbitrary Baker where
  arbitrary = Baker <$> arbitrary <*> arbitrary <*> arbitrarySentence 2 <*> pure Nothing

instance Arbitrary Proposal where
  arbitrary = Proposal
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (Just <$> arbitrarySentence 2)
    <*> (Just <$> arbitrarySentence 5)
    <*> (Just <$> arbitrarySentence 15)
    <*> arbitrary
    <*> pure Nothing
    <*> pure Nothing
    <*> arbitrary
    <*> arbitrary

instance Arbitrary PeriodType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary VoteType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary (Id a) where
  arbitrary = Id <$> choose (0, 1000000)

instance Arbitrary Level where
  arbitrary = Level <$> choose (0, 1000000)

deriving instance Random Level

instance Arbitrary Cycle where
  arbitrary = Cycle <$> choose (0, 8)

instance Arbitrary Votes where
  arbitrary = Votes <$> choose (0, 10000)

instance Arbitrary Rolls where
  arbitrary = Rolls <$> choose (0, 10000)

instance Arbitrary Quorum where
  arbitrary = Quorum <$> choose (0, 10000)

instance Arbitrary Period where
  arbitrary = genericArbitrary

instance Arbitrary PeriodTimeInfo where
  arbitrary = genericArbitrary

instance Arbitrary VoteStats where
  arbitrary = do
    restVoters <- arbitrary
    voters <- arbitrary
    cast <- arbitrary
    notCast <- arbitrary
    pure $ VoteStats (cast + voters) (cast + notCast + voters)
      (fromIntegral voters) (fromIntegral voters + restVoters)

instance Arbitrary Decision where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Ballots where
  arbitrary =
    Ballots <$>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    choose (0, 100) <*>
    choose (0, 100)

instance Arbitrary PeriodInfo where
  arbitrary = genericArbitrary

instance Arbitrary ProposalVote where
  arbitrary = genericArbitrary

instance Arbitrary Ballot where
  arbitrary = genericArbitrary

deriving instance Arbitrary Limit
deriving instance Arbitrary Amount

instance Arbitrary PaginationData where
  arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (PaginatedList a) where
  arbitrary = genericArbitrary
