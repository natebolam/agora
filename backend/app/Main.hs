module Main where

import Agora.Web.Server (runAgoraWithDocs)

main :: IO ()
main = do
  putStrLn ("Serving Agora API on 8190" :: Text)
  runAgoraWithDocs 8190
