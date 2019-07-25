{-|
Module which contains the exception classes used in Agora
server handlers and defines the way they are transformed
to Servant errors.
-}
module Agora.Web.Error
       ( ToServantErr (..)
       , AgoraAPIError (..)
       ) where

import Fmt (Buildable (..), (+|), (|+))
import Servant (ServantErr (..), err404, err500)

import Agora.Util

-- | Class of exceptions which can be transformed to @ServantErr@
class Exception e => ToServantErr e where
    toServantErr :: e -> ServantErr

-- | Exceptions which are thrown by API handlers.
data AgoraAPIError
  = NotFound !Text
  | PeriodMetasNotFilledYet
  | InternalError !Text
  deriving (Show, Eq, Generic)

instance Exception AgoraAPIError

instance Buildable AgoraAPIError where
  build (NotFound desc)          = "Resource not found: "+|desc|+""
  build PeriodMetasNotFilledYet  = "Period metas is not initialized from Tezos node"
  build (InternalError desc)     = "Internal error: "+|desc|+""

instance ToServantErr AgoraAPIError where
  toServantErr (NotFound desc) =
    err404 { errBody = encodeUtf8 desc }
  toServantErr PeriodMetasNotFilledYet =
    err500 { errBody = encodeUtf8 $ pretty PeriodMetasNotFilledYet }
  toServantErr (InternalError desc) =
    err500 { errBody = encodeUtf8 desc }
