{-# language OverloadedLists #-}
module Main
  ( main
  ) where

import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Foldable
import           Data.Traversable
import qualified Data.Vector                   as V
import qualified HasVulkan                     as Vk
import           InitVk
import           InitXr
import           MonadVulkan                   as Vk
import           MonadXr
import           OpenXR.CStruct.Extends
import           OpenXR.Core10
import           OpenXR.Extensions.XR_KHR_vulkan_enable
import           Say
import           System.IO                      ( stderr )
import           Text.Pretty.Simple             ( pHPrint )
import qualified Vulkan.Core10                 as Vk
import           Vulkan.Core10                  ( Format(Format) )
import           Vulkan.Utils.QueueAssignment
import           Vulkan.Zero
import           XrUtils

main :: IO ()
main = runResourceT $ do
  sayErr "Available Extensions:"
  traverse_ sayErrShow =<< enumerateInstanceExtensionProperties Nothing
  withXr $ \info reqs -> do
    pHPrint stderr info
    inst                 <- InitVk.createInstance reqs
    desiredDev           <- getVulkanGraphicsDeviceKHR' (Vk.instanceHandle inst)
    (phys, pdi, dev, qs) <- InitVk.createDevice
      inst
      reqs { xrPhysicalDevice = desiredDev }
    let rtInfo = pdiRTInfo pdi
    vma <- createVMA inst phys dev
    runV inst phys rtInfo dev qs vma $ do
      session <- withVulkanXrSession
      pHPrint stderr =<< enumerateReferenceSpaces session
      _ <-
        useSession'
            session
            zero
              { primaryViewConfigurationType =
                VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO
              }
          $ \_ -> do
              go session
              -- TODO: exit session properly
              requestExitSession session
      pure ()

go :: Session -> V ()
go session = do
  let rsci = ReferenceSpaceCreateInfo
        { referenceSpaceType   = REFERENCE_SPACE_TYPE_LOCAL
        , poseInReferenceSpace = identityPose
        }
  (_, swapchainFormats) <- fmap (fmap (Format . fromIntegral))
    <$> enumerateSwapchainFormats session
  sayErrShow swapchainFormats

  viewConfigs <-
    enumerateViewConfigurations'
    >>= traverse
          (\xrViewConfig -> do
            xrViewConfigProps <- getViewConfigurationProperties' xrViewConfig
            xrViewConfigViews <- toList
              <$> enumerateViewConfigurationViews' xrViewConfig
            pure XrView { .. }
          )
    .   toList
  let viewConfig :: XrView
      viewConfig = head viewConfigs

  swapchains <- for (xrViewConfigViews viewConfig) $ \view -> do
    let swapchainCreateInfo :: SwapchainCreateInfo '[]
        swapchainCreateInfo = SwapchainCreateInfo
          { next        = ()
          , createFlags = zero
          , usageFlags  = SWAPCHAIN_USAGE_COLOR_ATTACHMENT_BIT
                            .|. SWAPCHAIN_USAGE_SAMPLED_BIT
          , format = (\(Format f) -> fromIntegral f) . V.head $ swapchainFormats
          , sampleCount = 1
          , width       = recommendedImageRectWidth view
          , height      = recommendedImageRectHeight view
          , faceCount   = 1
          , arraySize   = 1
          , mipCount    = 1
          }
    pHPrint stderr swapchainCreateInfo
    (_, (_, swapchain)) <- withSwapchain' session swapchainCreateInfo
    swapchainImages     <- enumerateSwapchainImages @SwapchainImageVulkanKHR
      swapchain
    sayErrShow swapchainImages
    pure (swapchain, swapchainImages)

  refSpace              <- withReferenceSpace session rsci allocate

  pure ()

withVulkanXrSession :: V Session
withVulkanXrSession = do
  inst <- Vk.getInstance
  phys <- Vk.getPhysicalDevice
  dev  <- Vk.getDevice
  qs   <- Vk.getQueues
  let vkBinding :: GraphicsBindingVulkanKHR
      vkBinding = GraphicsBindingVulkanKHR
        { instance'        = Vk.instanceHandle inst
        , physicalDevice   = Vk.physicalDeviceHandle phys
        , device           = Vk.deviceHandle dev
        , queueFamilyIndex = unQueueFamilyIndex . fst . graphicsQueue $ qs
        , queueIndex       = 0 -- TODO, expose this from assignQueues
        }
  systemId <- getSystemId
  snd <$> withSession'
    (zero { systemId, createFlags = zero } ::& vkBinding :& ())
