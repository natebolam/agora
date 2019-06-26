{-|
CLI params for providing config files to the node and logic
of reading those files.
-}
module Agora.Config.Reading
       ( ConfigError (..)
       , readConfigs
       , configPathsParser
       ) where

import Control.Applicative.Combinators.NonEmpty as NonEmpty (some)
import Data.Aeson (Result (..), Value (..), fromJSON)
import qualified Data.HashMap.Strict as HM
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Loot.Config (finalise)
import qualified Options.Applicative as Opt

import Agora.Config.Definition

-- | Exception which is thrown when a config cannot be read or parsed.
data ConfigError
  = ConfigReadingError !String
  | ConfigParsingError !String
  | ConfigIncomplete ![String]
  deriving (Show, Eq, Generic)

instance Exception ConfigError

-- | Reads provided YAML files in order and yields a merged
-- JSON value.
readConfigsValue
  :: (MonadIO m, MonadThrow m)
  => NonEmpty FilePath       -- ^ Paths to config files
  -> m Value
readConfigsValue = foldM addConfigValue (Object mempty)
  where
    addConfigValue prev =
      fmap (mergeOverride prev) . readOneConfig
    readOneConfig file = liftIO $
      decodeFileEither file >>= either rethrowParseException pure
    rethrowParseException = throwM . ConfigReadingError . prettyPrintParseException
    mergeOverride (Object o1) (Object o2) =
      Object $ HM.unionWith mergeOverride o1 o2
    mergeOverride _ b = b

-- | Reads provided YAML files in order and yields a `Final` config,
-- initialized with provided default config.
readConfigs
  :: (MonadIO m, MonadThrow m)
  => NonEmpty FilePath       -- ^ Paths to config files
  -> AgoraConfigRecP         -- ^ Default config
  -> m AgoraConfigRec
readConfigs files defaultCfg = do
  let successOrThrow (Error s)   = throwM $ ConfigParsingError s
      successOrThrow (Success a) = pure a
  val <- readConfigsValue files
  cfg <- successOrThrow $ fromJSON val
  either (throwM . ConfigIncomplete) pure $
    finalise $ defaultCfg `mappend` cfg

-- | CLI parser for config paths.
configPathsParser :: Opt.Parser (NonEmpty FilePath)
configPathsParser = NonEmpty.some $ Opt.strOption $
  Opt.short 'c' <>
  Opt.long "config" <>
  Opt.metavar "FILEPATH" <>
  Opt.help "Path to configuration file. Multiple -c options can \
           \be provided, in which case configuration is merged. \
           \The order matters, the latter one overrides the former."
