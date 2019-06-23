module Main where

import Agora.Web.Server (runAgora)

main :: IO ()
main = do
  putStrLn ("Serving Agora API on 8190" :: Text)
  runAgora 8190
