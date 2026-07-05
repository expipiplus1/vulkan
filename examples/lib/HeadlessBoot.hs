{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Shared boot prelude for the headless examples.
Mirrors 'WindowedBoot.withWindowedVk' but with no surface, no swapchain — just
an instance, a logical device with the standard 'Queues' triple, and a VMA
allocator.

Headless callers project the queue they actually need from 'queues'
(@qCompute@ for compute examples, @qGraphics@ for graphics examples) — there's
no per-example queue-family configuration.
-}
module HeadlessBoot
  ( HeadlessConfig (..)
  , HeadlessVk (..)
  , withHeadlessVk
  , submitAndWait
  , submitAndWaitFor
  ) where

import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, allocate, release)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import Say (sayErr)
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core13 (pattern API_VERSION_1_3)
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Requirement (DeviceRequirement, InstanceRequirement (..))
import qualified Vulkan.Utils.Init.Headless as Init
import Vulkan.Utils.Initialization (physicalDeviceName)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex)
import Vulkan.Utils.Queues (Queues, allocateDevice)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA
import VulkanMemoryAllocator.Utils (allocatorCreateInfo)
import WindowedBoot (debugMessengerCreateInfo)

{- | Per-example knobs for headless boot. No queue-family predicate here —
the helper always provisions the standard G/C/T 'Queues' triple.
-}
data HeadlessConfig = HeadlessConfig
  { appName :: Text
  , instanceReqs :: [InstanceRequirement]
  , deviceReqs :: [DeviceRequirement]
  , vmaFlags :: VMA.AllocatorCreateFlags
  -- ^ VMA flags (e.g. @ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT@ for BDA).
  }

-- | Long-lived handles produced by 'withHeadlessVk'.
data HeadlessVk = HeadlessVk
  { inst :: Vk.Instance
  , physicalDevice :: Vk.PhysicalDevice
  , device :: Vk.Device
  , allocator :: VMA.Allocator
  , queues :: Queues (QueueFamilyIndex, Vk.Queue)
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
    Init.allocateInstance
      ( Just
          zero
            { Vk.applicationName = Just (Text.encodeUtf8 appName)
            , Vk.apiVersion = API_VERSION_1_3
            }
      )
      ( RequireInstanceExtension Nothing EXT_DEBUG_UTILS_EXTENSION_NAME minBound
          : instanceReqs
      )
      [RequireInstanceLayer "VK_LAYER_KHRONOS_validation" minBound]
  _ <- withDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing allocate
  (phys, dev, qs) <- allocateDevice inst Nothing deviceReqs
  (_, vma) <-
    VMA.withAllocator (allocatorCreateInfo vmaFlags API_VERSION_1_3 inst phys dev) allocate
  sayErr . ("Using device: " <>) =<< physicalDeviceName phys
  pure
    HeadlessVk
      { inst
      , physicalDevice = phys
      , device = dev
      , allocator = vma
      , queues = qs
      }

{- | Submit a single command buffer to the queue, wait up to a second on a
fresh fence for it to complete, throw on timeout. For longer-running work (e.g.
a heavy compute dispatch) use 'submitAndWaitFor' with an explicit budget.
-}
submitAndWait
  :: (MonadResource m, MonadThrow m)
  => Vk.Device
  -> Vk.Queue
  -> Vk.CommandBuffer
  -> String
  -> m ()
submitAndWait = submitAndWaitFor (1 * 1000 * 1000 * 1000) -- 1 second

-- | As 'submitAndWait', but with a caller-supplied timeout in nanoseconds.
submitAndWaitFor
  :: (MonadResource m, MonadThrow m)
  => Word64
  -- ^ Timeout in nanoseconds.
  -> Vk.Device
  -> Vk.Queue
  -> Vk.CommandBuffer
  -> String
  -- ^ Message to throw if the wait times out.
  -> m ()
submitAndWaitFor fenceTimeout dev queue commandBuffer timeoutMessage = do
  (fenceKey, fence) <- Vk.withFence dev zero Nothing allocate
  let submitInfo =
        zero
          { Vk.commandBuffers = [Vk.commandBufferHandle commandBuffer]
          }
  Vk.queueSubmit queue [SomeStruct submitInfo] fence
  liftIO (Vk.waitForFences dev [fence] True fenceTimeout) >>= \case
    Vk.TIMEOUT -> throwString timeoutMessage
    _ -> release fenceKey
