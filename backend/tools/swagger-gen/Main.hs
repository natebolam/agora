module Main where

import Data.Yaml (encodeFile)

import Agora.Web.Swagger (agoraApiSwagger, swaggerSpecFilePath)

main :: IO ()
main = do
  putStrLn $ "Writing Swagger spec to " <> swaggerSpecFilePath <> "..."
  encodeFile swaggerSpecFilePath agoraApiSwagger
  putText "done"

