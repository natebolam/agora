module Main where

import Loot.Log (NameSelector (..), basicConfig, withLogging)
import Monad.Capabilities (emptyCaps)

import Agora.Config (configPathsParser, defaultAgoraConfig, readConfigs, withConfig)
import Agora.Web.Server (runAgora)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

main :: IO ()
main = do
  configPaths <- execParser $
    info (helper <*> configPathsParser) $
    fullDesc <> progDesc "Agora backend node."

  config <- readConfigs configPaths defaultAgoraConfig

  usingReaderT emptyCaps $
    withConfig config $
    withLogging basicConfig CallstackName runAgora
