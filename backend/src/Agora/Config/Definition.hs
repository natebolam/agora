{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-|
Type-level definition of Agora configuration.
-}
module Agora.Config.Definition
       ( AgoraConfig
       , AgoraConfigRecP
       , AgoraConfigRec
       , defaultAgoraConfig
       , ConfigCap
       , HasConfig
       , MonadConfig (..)
       , newConfig
       , withConfig
       ) where

import Control.Monad.Reader (withReaderT)
import Lens.Micro.Platform (Getting, (?~))
import Loot.Config ((:::), ConfigKind (Final, Partial), ConfigRec, option)
import Monad.Capabilities (CapImpl, CapsT, Context (..), HasContext, HasNoCap, addCap, askContext,
                           newContext)

import Agora.Util (NetworkAddress)

-- | Type-level definition of Agora config.
type AgoraConfig =
  '[ "listen_addr" ::: NetworkAddress
   , "serve_docs" ::: Bool
     -- TODO: add more values
   ]

type AgoraConfigRecP = ConfigRec 'Partial AgoraConfig
type AgoraConfigRec = ConfigRec 'Final AgoraConfig

-- | Default partial config.
defaultAgoraConfig :: AgoraConfigRecP
defaultAgoraConfig = mempty
  & option #serve_docs ?~ True

---------------------------------------------------------------------------
-- Configuration capabilities
---------------------------------------------------------------------------

-- | Type of a capability of storing the config
type ConfigCap cfg = Context (ConfigRec 'Final cfg)

type HasConfig cfg caps = HasContext (ConfigRec 'Final cfg) caps

newConfig :: forall cfg m. ConfigRec 'Final cfg -> CapImpl (ConfigCap cfg) '[] m
newConfig = newContext

-- | Monadic class which provides the methods for accessing the configuration.
class Monad m => MonadConfig cfg m where
  askConfig :: m (ConfigRec 'Final cfg)
  fromConfig :: Getting a (ConfigRec 'Final cfg) a -> m a

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
