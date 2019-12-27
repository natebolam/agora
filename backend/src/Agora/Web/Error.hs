{-|
Module which contains the exception classes used in Agora
server handlers and defines the way they are transformed
to Servant errors.
-}
module Agora.Web.Error
       ( ToServerError (..)
       , AgoraAPIError (..)
       ) where

import Fmt (Buildable (..), (+|), (|+))
import Servant (ServerError (..), err404, err500)

import Agora.Util

-- | Class of exceptions which can be transformed to @ServerError@
class Exception e => ToServerError e where
    toServerError :: e -> ServerError

-- | Exceptions which are thrown by API handlers.
data AgoraAPIError
  = NotFound !Text
  | StageMetasNotFilledYet
  | InternalError !Text
  deriving (Show, Eq, Generic)

instance Exception AgoraAPIError

instance Buildable AgoraAPIError where
  build (NotFound desc)          = "Resource not found: "+|desc|+""
  build StageMetasNotFilledYet  = "Stage metas is not initialized from Tezos node"
  build (InternalError desc)     = "Internal error: "+|desc|+""

instance ToServerError AgoraAPIError where
  toServerError (NotFound desc) =
    err404 { errBody = encodeUtf8 desc }
  toServerError StageMetasNotFilledYet =
    err500 { errBody = encodeUtf8 $ pretty StageMetasNotFilledYet }
  toServerError (InternalError desc) =
    err500 { errBody = encodeUtf8 desc }
