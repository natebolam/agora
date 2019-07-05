{-|
Dump for generic stuff which has nowhere else to go
-}
module Agora.Util
       ( NetworkAddress (..)
       , prettyL
       , pretty
       ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Fmt (Buildable (..), (+|), (|+))
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (Read (..), read)
import qualified Text.Show

---------------------------------------------------------------------------
-- Network-related stuff
---------------------------------------------------------------------------

-- | Datatype which contains info about socket network address
data NetworkAddress = NetworkAddress
  { naHost :: !Text
  , naPort :: !Word16
  } deriving (Eq, Ord, Generic)

instance Buildable NetworkAddress where
  build NetworkAddress {..} = ""+|naHost|+":"+|naPort|+""

instance Show NetworkAddress where
  show = toString . pretty

instance Read NetworkAddress where
  readsPrec _ = ReadP.readP_to_S addrParser
    where
      addrParser = NetworkAddress
        <$> (parseHost <* ReadP.char ':')
        <*> parsePort
      parseHost = toText <$> ReadP.munch (/= ':')
      parsePort = ReadP.readS_to_P reads

instance IsString NetworkAddress where
  fromString = read

instance FromJSON NetworkAddress where
  parseJSON = withText "NetworkAddress" $ pure . read . toString

instance ToJSON NetworkAddress where
  toJSON = String . pretty

---------------------------------------------------------------------------
-- Generic stuff
---------------------------------------------------------------------------

prettyL :: Buildable a => a -> LText
prettyL = toLazyText . build

pretty :: Buildable a => a -> Text
pretty = toStrict . prettyL
