module Main where

import Agora.Config (configPathsParser, readConfigs)
import Agora.Mode (runAgoraReal)
import Agora.Web.Server (runAgora)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

main :: IO ()
main = do
  configPaths <- execParser $
    info (helper <*> configPathsParser) $
    fullDesc <> progDesc "Agora backend node."

  config <- readConfigs configPaths
  runAgoraReal config runAgora
