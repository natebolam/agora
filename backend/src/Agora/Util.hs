{-|
Dump for generic stuff which has nowhere else to go
-}
module Agora.Util
       ( NetworkAddress (..)
       , ConnString (..)
       , hoistClientEnv
       , HasId (..)
       , Limit (..)
       , Amount (..)
       , PaginationData (..)
       , PaginatedList (..)
       , buildFromJSON
       , plResultsL
       , paginateWithId
       , ApiUsername (..)
       , ApiKey (..)
       , TagEnum (..)
       , buildTag
       , toJSONTag
       , parseJSONTag
       , declareNamedSchemaTag
       , supressException
       , ordNubBy
       , prettyL
       , pretty
       , untagConstructorOptions
       , snakeCaseOptions
       ) where

import Data.Aeson (FromJSON (..), Options (..), SumEncoding (..), ToJSON (..), Value (..), encode,
                   withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Parser)
import Data.Char (isUpper, toLower)
import Data.List (elemIndex, (!!))
import qualified Data.Map.Strict as M
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Swagger.Internal.Schema as S
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromText, toLazyText)
import Data.Time.Units (Second, toMicroseconds)
import Data.Typeable (typeRep)
import Fmt (Buildable (..), Builder, (+|), (|+))
import Lens.Micro.Platform (makeLensesFor, (?=))
import Loot.Log (MonadLogging)
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Client (BaseUrl, ClientEnv, ClientM, ServantError, parseBaseUrl, runClientM,
                       showBaseUrl)
import Servant.Util (ForResponseLog (..), PaginationSpec (..), buildListForResponse)
import Servant.Util.Dummy (paginate)
import Servant.Util.Internal.Util (unPositive)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (Read (..), read)
import qualified Text.Show
import qualified Universum.Unsafe as U
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO
import qualified UnliftIO.Concurrent as UIO

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

-- | Helper function for creating HTTP Servant clients which
-- throw particular errors.
hoistClientEnv
  :: (Exception e, MonadUnliftIO m)
  => (ServantError -> e)
  -> ClientEnv
  -> (forall x . ClientM x -> m x)
hoistClientEnv errWrapper env clientM = liftIO $
  runClientM clientM env >>= \case
    Left e  -> UIO.throwIO $ errWrapper e
    Right x -> pure x

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
  { pdRest   :: !Amount
  , pdLimit  :: !(Maybe Limit)
  , pdLastId :: !(Maybe Word32)
  } deriving (Show, Eq, Generic)

-- | Object which represents a paginated list of results with metadata.
data PaginatedList a = PaginatedList
  { plPagination :: !PaginationData
  , plResults    :: ![a]
  } deriving (Show, Eq, Generic)

newtype Amount = Amount Word32
  deriving (Eq, Ord, Show, Generic, Num, Real, Integral, Enum, Buildable)

newtype Limit = Limit Word32
  deriving (Eq, Ord, Show, Generic, Num, Real, Integral, Enum, FromHttpApiData, Buildable)

buildFromJSON :: ToJSON a => a -> Builder
buildFromJSON x = "" +| decodeUtf8 @Text (encode x) |+ ""

instance (ToJSON a, Buildable (ForResponseLog a))
         => Buildable (ForResponseLog (PaginatedList a)) where
  build (ForResponseLog (PaginatedList pd ls)) = "{pagination: " +| buildFromJSON pd |+
    ", results: " +| buildListForResponse (take 5) (ForResponseLog ls) |+ "}"

-- | Helper function for paginating a list of values which have IDs.
paginateWithId
  :: (HasId a, Integral (IdT a))
  => PaginationSpec
  -> Maybe (IdT a)
  -> [a]
  -> PaginatedList a
paginateWithId ps@PaginationSpec{..} lastId ls =
  let pdLimit = fromIntegral . unPositive <$> psLimit
      ls' = sortOn (Down . getId) ls
      ls'' = maybe ls' (\i -> dropWhile ((>= i) . getId) ls') lastId
      results = paginate ps ls''
      pdRest = fromIntegral $
          if length results < length ls'' then (length ls'' - length results)
          else 0
      pdLastId = case results of
        [] -> Nothing
        _  -> Just $ fromIntegral $ getId $ U.last results
  in PaginatedList PaginationData {..} results

---------------------------------------------------------------------------
-- Discourse API-related stuff
---------------------------------------------------------------------------
newtype ApiUsername = ApiUsername Text
  deriving (Eq, Show, Generic, ToHttpApiData)

newtype ApiKey = ApiKey Text
  deriving (Eq, Show, Generic, ToHttpApiData)

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

-- | Supress an exception in passed action
-- and retry in @retryIn@ seconds until it succeeds.
supressException
  :: forall e m a .
  ( MonadUnliftIO m
  , MonadLogging m
  , Exception e
  )
  => Second      -- ^ When action should be retried if something went wrong
  -> (e -> m ()) -- ^ Action in a case if something went wrong
  -> m a         -- ^ Action where Tezos client errors should be suspended
  -> m a
supressException retryIn onFail action =
  action `UIO.catch` \(e :: e) -> do
    onFail e
    UIO.threadDelay $ fromIntegral $ toMicroseconds retryIn
    supressException retryIn onFail action

-- | Fast `nubBy` for items with `Ord` key.
ordNubBy :: Ord b => (a -> b) -> [a] -> [a]
ordNubBy f = map snd . M.toList . M.fromList . map (\v -> (f v, v))

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

buildTag :: TagEnum a => a -> Builder
buildTag = fromText . toTag

toJSONTag :: TagEnum a => a -> Value
toJSONTag = String . toTag

parseJSONTag :: forall a . TagEnum a => Value -> Parser a
parseJSONTag = withText (toString $ enumName $ Proxy @a) $ \t ->
  maybe (fail $ "Invalid value: " ++ toString t) pure $
  fromTag t

declareNamedSchemaTag :: forall a proxy . TagEnum a => proxy a -> S.Declare (S.Definitions S.Schema) S.NamedSchema
declareNamedSchemaTag _ =
  return $ S.named (enumName $ Proxy @a) $ mempty `executingState` do
    S.description ?= enumDesc (Proxy @a)
    S.enum_ ?= map String (enumVals $ Proxy @a)

-- | Options which miss names of constructors in ADT.
untagConstructorOptions :: Options
untagConstructorOptions = defaultOptions {sumEncoding = UntaggedValue}

-- | Options which transform field names to snake_case.
snakeCaseOptions :: Options
snakeCaseOptions =
  defaultOptions {fieldLabelModifier = toSnakeCase . fieldLabelModifier defaultOptions }
  where
    toSnakeCase :: String -> String
    toSnakeCase = foldl (\r c ->
                           if isUpper c then r ++ ['_', toLower c]
                           else r ++ [c]) ""

makeLensesFor [("plResults", "plResultsL")] ''PaginatedList

-- This instance for `BaseUrl` is provided by `servant-client-core` only
-- starting from version 0.15, and we using an LTS which ships 0.14...
instance FromJSON BaseUrl where
  parseJSON = withText "BaseUrl" $
    maybe (fail "Invalid URL") pure . parseBaseUrl . toString

instance Buildable BaseUrl where
  build b = "" +| toText (showBaseUrl b) |+ ""

---------------------------------------------------------------------------
-- Derivations
---------------------------------------------------------------------------

deriveJSON defaultOptions ''ApiUsername
deriveJSON defaultOptions ''ApiKey
deriveJSON defaultOptions ''Limit
deriveJSON defaultOptions ''Amount
deriveJSON defaultOptions ''PaginationData
deriveJSON defaultOptions ''PaginatedList
