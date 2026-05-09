{-# LANGUAGE RecordWildCards #-}

{-| Shared 'Allocator' construction for VMA-using examples. Each caller passes
its own create flags and target Vulkan API version.
-}
module Vma
  ( createVMA
  ) where

import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.Word (Word32)
import Foreign.Ptr (castFunPtr)
import qualified Vulkan.Core10 as Vk
import Vulkan.Dynamic (DeviceCmds (DeviceCmds, pVkGetDeviceProcAddr), InstanceCmds (InstanceCmds, pVkGetInstanceProcAddr))
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

createVMA
  :: (MonadResource m)
  => VMA.AllocatorCreateFlags
  -> Word32
  -- ^ Target Vulkan API version
  -> Vk.Instance
  -> Vk.PhysicalDevice
  -> Vk.Device
  -> m VMA.Allocator
createVMA flags' apiVer inst phys dev =
  snd <$> VMA.withAllocator vmaCI allocate
  where
    vmaCI =
      zero
        { VMA.flags = flags'
        , VMA.physicalDevice = Vk.physicalDeviceHandle phys
        , VMA.device = dh
        , VMA.instance' = ih
        , VMA.vulkanApiVersion = apiVer
        , VMA.vulkanFunctions = Just funs
        }
    funs =
      zero
        { VMA.vkGetInstanceProcAddr = castFunPtr pVkGetInstanceProcAddr
        , VMA.vkGetDeviceProcAddr = castFunPtr pVkGetDeviceProcAddr
        }
    Vk.Instance ih InstanceCmds{pVkGetInstanceProcAddr} = inst
    Vk.Device dh DeviceCmds{pVkGetDeviceProcAddr} = dev
