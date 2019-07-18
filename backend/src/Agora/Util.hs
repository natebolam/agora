{-|
Dump for generic stuff which has nowhere else to go
-}
module Agora.Util
       ( NetworkAddress (..)
       , ConnString (..)
       , HasId (..)
       , PaginationData (..)
       , PaginatedList (..)
       , paginateWithId
       , TagEnum (..)
       , prettyL
       , pretty
       , untagConstructorOptions
       ) where

import Data.Aeson (FromJSON (..), Options (..), SumEncoding (..), ToJSON (..), Value (..), withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.List (elemIndex, (!!))
import qualified Data.Swagger as S
import qualified Data.Swagger.Internal.Schema as S
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromText, toLazyText)
import Data.Typeable (typeRep)
import Fmt (Buildable (..), (+|), (|+))
import Lens.Micro.Platform ((?=))
import Servant.Util (PaginationSpec (..))
import Servant.Util.Dummy (paginate)
import Servant.Util.Internal.Util (unPositive)
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
-- API-related stuff
---------------------------------------------------------------------------

-- | Class which represent the types values of which have unique IDs.
class HasId s where
    type IdT s :: *
    type IdT s = s

    getId :: s -> IdT s

    default getId :: (IdT s ~ s) => s -> IdT s
    getId = id

-- | Datatype which represents pagination parameters which are returned
-- alongside the paginated data.
data PaginationData = PaginationData
  { pdTotal  :: !Word32
  , pdOffset :: !Word32
  , pdLimit  :: !(Maybe Word32)
  , pdLastId :: !(Maybe Word32)
  } deriving (Show, Eq, Generic)

-- | Object which represents a paginated list of results with metadata.
data PaginatedList a = PaginatedList
  { plPagination :: !PaginationData
  , plResults    :: ![a]
  } deriving (Show, Eq, Generic)

-- | Helper function for paginating a list of values which have IDs.
paginateWithId
  :: (HasId a, Integral (IdT a))
  => PaginationSpec
  -> Maybe (IdT a)
  -> [a]
  -> PaginatedList a
paginateWithId ps@PaginationSpec{..} (fmap fromIntegral -> pdLastId) ls =
  let pdTotal = fromIntegral $ length ls
      pdOffset = fromIntegral psOffset
      pdLimit = fromIntegral . unPositive <$> psLimit
      ls' = sortOn (Down . getId) ls
      ls'' = maybe ls' (\i -> dropWhile ((> i) . fromIntegral . getId) ls') pdLastId
      results = paginate ps ls''
  in PaginatedList PaginationData {..} results

---------------------------------------------------------------------------
-- DB-related stuff
---------------------------------------------------------------------------

-- | Newtype which denotes LIBPQ connection string.
-- Moved here to avoid import cycles between `Agora.Config` and `Agora.DB`.
-- Syntax: https://www.postgresql.org/docs/9.5/libpq-connect.html#LIBPQ-CONNSTRING
newtype ConnString = ConnString
  { unConnString :: ByteString
  } deriving (Show, Eq, Ord)

instance FromJSON ConnString where
  parseJSON = withText "ConnString" $ pure . ConnString . encodeUtf8

instance ToJSON ConnString where
  toJSON = String . decodeUtf8 . unConnString

instance Buildable ConnString where
  build (ConnString s) = ""+|decodeUtf8 @Text s|+""

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

---------------------------------------------------------------------------
-- Derivations
---------------------------------------------------------------------------

deriveJSON defaultOptions ''PaginationData
deriveJSON defaultOptions ''PaginatedList
