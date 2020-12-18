{-# language OverloadedLists #-}
module InitXr
  ( withXr
  , XrReqs(..)
  , XrView(..)
  ) where

import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8         as BS8
import           Data.Foldable
import           Data.Vector                    ( fromList )
import           MonadXr
import           OpenXR.Core10
import           OpenXR.Extensions.XR_KHR_vulkan_enable
import           OpenXR.Version
import           OpenXR.Zero
import qualified Vulkan.Requirement            as Vk
import           Vulkan.Utils.Misc
import qualified Vulkan.Version                as Vk

withXr :: (SystemProperties '[] -> XrReqs () -> Xr a) -> ResourceT IO a
withXr a = do
  handles <- initXrInstance
  runXr handles $ do
    info <- getSystemProperties'
    reqs <- xrVulkanReqs
    a info reqs

-- | Create a pretty basic OpenXR Instance with the Vulkan extension
initXrInstance :: MonadResource m => m XrHandles
initXrInstance = do
  availableExtensionNames <- fmap extensionName
    <$> enumerateInstanceExtensionProperties Nothing
  (exts, _) <- partitionOptReqIO "OpenXR Extension"
                                 (toList availableExtensionNames)
                                 []
                                 [KHR_VULKAN_ENABLE_EXTENSION_NAME]
  let ici = zero
        { applicationInfo       = zero { applicationName = "Haskell VR Example"
                                       , apiVersion      = CURRENT_API_VERSION
                                       }
        , enabledExtensionNames = fromList exts
        }

  -- Initialize a system
  (_, xrInstance) <- withInstance ici allocate
  xrSystemId      <- getSystem
    xrInstance
    zero { formFactor = FORM_FACTOR_HEAD_MOUNTED_DISPLAY }
  pure XrHandles { .. }

xrVulkanReqs :: Xr (XrReqs ())
xrVulkanReqs = do
  GraphicsRequirementsVulkanKHR { minApiVersionSupported = ~(MAKE_VERSION ma mi pa) } <-
    getVulkanGraphicsRequirementsKHR'
  instanceExts <- BS8.split ' ' <$> getVulkanInstanceExtensionsKHR'
  deviceExts   <- BS8.split ' ' <$> getVulkanDeviceExtensionsKHR'
  pure $ XrReqs
    ( Vk.RequireInstanceVersion
        (Vk.MAKE_VERSION (fromIntegral ma) (fromIntegral mi) pa)
    : [ Vk.RequireInstanceExtension Nothing n 0 | n <- instanceExts ]
    )
    ([ Vk.RequireDeviceExtension Nothing n 0 | n <- deviceExts ])
    ()

data XrView = XrView
  { xrViewConfig      :: ViewConfigurationType
  , xrViewConfigProps :: ViewConfigurationProperties
  , xrViewConfigViews :: [ViewConfigurationView '[]]
  }
  deriving Show

data XrReqs a = XrReqs
  { xrVkInstReqs     :: [Vk.InstanceRequirement]
  , xrVkDeviceReqs   :: [Vk.DeviceRequirement]
  , xrPhysicalDevice :: a
  }

----------------------------------------------------------------
--
----------------------------------------------------------------


