{-|
Dump for generic stuff which has nowhere else to go
-}
module Agora.Util
       ( NetworkAddress (..)
       , TagEnum (..)
       , prettyL
       , pretty
       , untagConstructorOptions
       ) where

import Data.Aeson (FromJSON (..), Options (..), SumEncoding (..), ToJSON (..), Value (..), withText)
import Data.Aeson.Options (defaultOptions)
import Data.List (elemIndex, (!!))
import qualified Data.Swagger as S
import qualified Data.Swagger.Internal.Schema as S
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromText, toLazyText)
import Data.Typeable (typeRep)
import Fmt (Buildable (..), (+|), (|+))
import Lens.Micro.Platform ((?=))
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

-- | Class for enums which have a defined text representation
class (Typeable a, Bounded a, Enum a) => TagEnum a where
  {-# MINIMAL toTag | enumVals #-}
  enumVals :: Proxy a -> [Text]
  enumVals _ = map toTag $ enumFromTo @a minBound maxBound

  enumName :: Proxy a -> Text
  enumName = show . typeRep

  enumDesc :: Proxy a -> Text
  enumDesc = enumName

  toTag :: a -> Text
  toTag a = enumVals (Proxy @a) !! fromEnum a

  fromTag :: Text -> Maybe a
  fromTag t = toEnum <$> elemIndex t (enumVals $ Proxy @a)

instance {-# OVERLAPPABLE #-} TagEnum a => Buildable a where
  build = fromText . toTag

instance {-# OVERLAPPABLE #-} TagEnum a => ToJSON a where
  toJSON = String . toTag

instance {-# OVERLAPPABLE #-} TagEnum a => FromJSON a where
  parseJSON = withText (toString $ enumName $ Proxy @a) $ \t ->
    maybe (fail $ "Invalid value: " ++ toString t) pure $
    fromTag t

instance {-# OVERLAPPABLE #-} TagEnum a => S.ToSchema a where
  declareNamedSchema _ =
    return $ S.named (enumName $ Proxy @a) $ mempty `executingState` do
      S.description ?= enumDesc (Proxy @a)
      S.enum_ ?= map String (enumVals $ Proxy @a)

-- | Options which miss names of constructors in ADT.
untagConstructorOptions :: Options
untagConstructorOptions = defaultOptions {sumEncoding = UntaggedValue}
