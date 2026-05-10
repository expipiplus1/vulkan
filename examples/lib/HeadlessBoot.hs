{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Shared boot prelude for the headless examples (compute, triangle-headless).
Mirrors 'WindowedBoot.withWindowedVk' but with no surface, no swapchain — just
an instance, a logical device with the standard 'Queues' triple, and a VMA
allocator.

Headless callers project the queue they actually need from 'hvQueues'
(@qCompute@ for compute, @qGraphics@ for triangle-headless) — there's no
per-example queue-family configuration.
-}
module HeadlessBoot
  ( HeadlessConfig (..)
  , HeadlessVk (..)
  , withHeadlessVk
  , submitAndWait
  ) where

import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Say (sayErr)
import VkResources (Queues)
import qualified Vma
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12 (pattern API_VERSION_1_2)
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Requirement (DeviceRequirement, InstanceRequirement (..))
import Vulkan.Utils.GCT (withDevice)
import qualified Vulkan.Utils.Init.Headless as Init
import Vulkan.Utils.Initialization (physicalDeviceName)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA
import WindowedBoot (debugMessengerCreateInfo)

{- | Per-example knobs for headless boot. No queue-family predicate here —
the helper always provisions the standard G/C/T 'Queues' triple.
-}
data HeadlessConfig = HeadlessConfig
  { hcAppName :: Text
  , hcInstanceReqs :: [InstanceRequirement]
  , hcDeviceReqs :: [DeviceRequirement]
  }

-- | Long-lived handles produced by 'withHeadlessVk'.
data HeadlessVk = HeadlessVk
  { hvInstance :: Vk.Instance
  , hvPhysicalDevice :: Vk.PhysicalDevice
  , hvDevice :: Vk.Device
  , hvAllocator :: VMA.Allocator
  , hvQueues :: Queues (QueueFamilyIndex, Vk.Queue)
  }

{- | Open a Vulkan instance + headless device + VMA allocator. Logs the
chosen device's name to stderr.
-}
withHeadlessVk
  :: (MonadResource m, MonadFail m, MonadThrow m)
  => HeadlessConfig
  -> m HeadlessVk
withHeadlessVk HeadlessConfig{..} = do
  inst <-
    Init.withInstance
      ( Just
          zero
            { Vk.applicationName = Just (Text.encodeUtf8 hcAppName)
            , Vk.apiVersion = API_VERSION_1_2
            }
      )
      ( RequireInstanceExtension Nothing EXT_DEBUG_UTILS_EXTENSION_NAME minBound
          : hcInstanceReqs
      )
      [RequireInstanceLayer "VK_LAYER_KHRONOS_validation" minBound]
  _ <- withDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing allocate
  (phys, dev, qs) <- withDevice inst Nothing hcDeviceReqs
  vma <- Vma.createVMA zero API_VERSION_1_2 inst phys dev
  sayErr . ("Using device: " <>) =<< physicalDeviceName phys
  pure
    HeadlessVk
      { hvInstance = inst
      , hvPhysicalDevice = phys
      , hvDevice = dev
      , hvAllocator = vma
      , hvQueues = qs
      }

{- | Submit a single command buffer to the queue, wait up to a second on a
fresh fence for it to complete, throw on timeout.
-}
submitAndWait
  :: (MonadResource m, MonadThrow m)
  => Vk.Device
  -> Vk.Queue
  -> Vk.CommandBuffer
  -> String
  -- ^ Message to throw if the wait times out.
  -> m ()
submitAndWait dev queue commandBuffer timeoutMessage = do
  (_, fence) <- Vk.withFence dev zero Nothing allocate
  let submitInfo =
        zero{Vk.commandBuffers = [Vk.commandBufferHandle commandBuffer]}
  Vk.queueSubmit queue [SomeStruct submitInfo] fence
  let fenceTimeout = 1e9 -- 1 second
  liftIO (Vk.waitForFences dev [fence] True fenceTimeout) >>= \case
    Vk.TIMEOUT -> throwString timeoutMessage
    _ -> pure ()
