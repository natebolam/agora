{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

{-|
Type-level definition of Agora configuration.
-}
module Agora.Config.Definition
       ( AgoraConfig
       , AgoraConfigRecP
       , AgoraConfigRec
       , ConfigCap
       , AgoraConfigCap
       , HasConfig
       , HasAgoraConfig
       , MonadConfig (..)
       , MonadAgoraConfig
       , newConfig
       , withConfig
       , askAgoraConfig
       , fromAgoraConfig

       -- * Re-exports
       , option
       , sub
       , finalise
       , finaliseDeferredUnsafe
       ) where

import Control.Monad.Reader (withReaderT)
import Lens.Micro.Platform (Getting)
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec, finalise,
                    finaliseDeferredUnsafe, option, sub)
import Loot.Log (LogConfig)
import Monad.Capabilities (CapImpl, CapsT, Context (..), HasContext, HasNoCap, addCap, askContext,
                           newContext)
import Servant.Client (BaseUrl)

import Agora.Node.Types (BakerInfo)
import Agora.Types (ContractHash)
import Agora.Util (ApiKey, ApiUsername, ConnString, NetworkAddress)

-- | Type-level definition of Agora config.
type AgoraConfig =
  '[ "api" ::<
     '[ "listen_addr" ::: NetworkAddress
      , "serve_docs" ::: Bool
      ]
   , "logging" ::: LogConfig
   , "contract" ::<
     '[ "address" ::: ContractHash
      ]
   , "node_addr" ::: BaseUrl
   , "mytezosbaker_url" ::: BaseUrl
   , "db" ::<
     '[ "conn_string" ::: ConnString
      , "max_connections" ::: Int
      ]
   , "discourse" ::<
      '[ "host"         ::: BaseUrl
       , "category"     ::: Text
       , "api_username" ::: ApiUsername
       , "api_key"      ::: ApiKey
       ]
   , "predefined_bakers" ::: [BakerInfo]
   ]

type AgoraConfigRecP = ConfigRec 'Partial AgoraConfig
type AgoraConfigRec = ConfigRec 'Final AgoraConfig

---------------------------------------------------------------------------
-- Configuration capabilities
---------------------------------------------------------------------------

-- | Type of a capability of storing the config
type ConfigCap cfg = Context (ConfigRec 'Final cfg)
type AgoraConfigCap = ConfigCap AgoraConfig

type HasConfig cfg caps = HasContext (ConfigRec 'Final cfg) caps
type HasAgoraConfig caps = HasConfig AgoraConfig caps

newConfig :: forall cfg m. ConfigRec 'Final cfg -> CapImpl (ConfigCap cfg) '[] m
newConfig = newContext

-- | Monadic class which provides the methods for accessing the configuration.
class Monad m => MonadConfig cfg m where
  askConfig :: m (ConfigRec 'Final cfg)
  fromConfig :: Getting a (ConfigRec 'Final cfg) a -> m a

type MonadAgoraConfig m = MonadConfig AgoraConfig m

instance (HasContext (ConfigRec 'Final cfg) caps, Monad m) =>
         MonadConfig cfg (CapsT caps m) where
  askConfig = askContext
  fromConfig getter = (^. getter) <$> askConfig

-- | Method for providing the config capability
withConfig
  :: forall cfg m caps a. (Typeable cfg, HasNoCap (ConfigCap cfg) caps)
  => ConfigRec 'Final cfg
  -> CapsT (ConfigCap cfg ': caps) m a
  -> CapsT caps m a
withConfig = withReaderT . addCap . newConfig

-- Helpers for avoiding weird type errors
askAgoraConfig :: MonadAgoraConfig m => m AgoraConfigRec
askAgoraConfig = askConfig

fromAgoraConfig :: MonadAgoraConfig m => Getting a AgoraConfigRec a -> m a
fromAgoraConfig = fromConfig
