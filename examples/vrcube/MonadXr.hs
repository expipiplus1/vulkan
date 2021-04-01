{-# language TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module MonadXr where

import           AutoApply
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           OpenXR.Core10
import           OpenXR.Extensions.XR_KHR_vulkan_enable

data XrHandles = XrHandles
  { xrInstance :: Instance
  , xrSystemId :: SystemId
  }

class MonadXr m where
  getInstance :: m Instance
  getSystemId :: m SystemId

instance Monad m => MonadXr (ReaderT XrHandles m) where
  getInstance = asks xrInstance
  getSystemId = asks xrSystemId

newtype Xr a = Xr { unXr :: ReaderT XrHandles (ResourceT IO) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadResource
    , MonadIO
    , MonadUnliftIO
    , MonadXr
    )

runXr :: XrHandles -> Xr a -> ResourceT IO a
runXr h = flip runReaderT h . unXr

autoapplyDecs
  (<> "'")
  [ 'getInstance
  , 'getSystemId
  ]
  [ 'allocate
  ]
  [ 'getSystem
  , 'getSystemProperties
  , 'enumerateViewConfigurations
  , 'getViewConfigurationProperties
  , 'enumerateViewConfigurationViews
  , 'getVulkanGraphicsRequirementsKHR
  , 'getVulkanInstanceExtensionsKHR
  , 'getVulkanDeviceExtensionsKHR
  , 'getVulkanGraphicsDeviceKHR
  , 'withSession
  , 'enumerateReferenceSpaces
  , 'useSession
  , 'withSwapchain
  ]
