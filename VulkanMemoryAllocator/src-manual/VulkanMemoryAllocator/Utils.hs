{-# LANGUAGE NamedFieldPuns #-}

{-| Hand-written scaffolding over the generated bindings.

'allocatorCreateInfo' fills in the one fiddly part of bringing up an
allocator with the dynamically loaded @vulkan@ bindings: VMA must be
pointed, via 'VulkanFunctions', at the same function pointers the
'Vk.Instance' and 'Vk.Device' were loaded with, and those live inside
the handles' command records. Pass the result to 'createAllocator' or
'withAllocator':

> (_, allocator) <-
>   withAllocator
>     (allocatorCreateInfo zero API_VERSION_1_3 inst phys dev)
>     allocate

For anything beyond the common case (heap limits, allocation callbacks,
…) update the returned create info before use.
-}
module VulkanMemoryAllocator.Utils
  ( allocatorCreateInfo
  ) where

import Data.Word (Word32)
import Foreign.Ptr (castFunPtr)
import qualified Vulkan.Core10 as Vk
import Vulkan.Dynamic (DeviceCmds (DeviceCmds, pVkGetDeviceProcAddr), InstanceCmds (InstanceCmds, pVkGetInstanceProcAddr))
import Vulkan.Zero (zero)
import VulkanMemoryAllocator (AllocatorCreateFlags, AllocatorCreateInfo, VulkanFunctions)
import qualified VulkanMemoryAllocator as AllocatorCreateInfo (AllocatorCreateInfo (..))
import qualified VulkanMemoryAllocator as VulkanFunctions (VulkanFunctions (..))

{- | A create info for an allocator serving the given device, with
'VulkanFunctions' wired to the function pointers the handles were loaded
with.
-}
allocatorCreateInfo
  :: AllocatorCreateFlags
  -> Word32
  {- ^ The Vulkan API version the application targets — the @apiVersion@
  of its 'Vk.ApplicationInfo', e.g. @API_VERSION_1_3@.
  -}
  -> Vk.Instance
  -> Vk.PhysicalDevice
  -> Vk.Device
  -> AllocatorCreateInfo
allocatorCreateInfo flags apiVersion inst phys dev =
  zero
    { AllocatorCreateInfo.flags = flags
    , AllocatorCreateInfo.physicalDevice = Vk.physicalDeviceHandle phys
    , AllocatorCreateInfo.device = dh
    , AllocatorCreateInfo.instance' = ih
    , AllocatorCreateInfo.vulkanApiVersion = apiVersion
    , AllocatorCreateInfo.vulkanFunctions = Just funs
    }
  where
    funs :: VulkanFunctions
    funs =
      zero
        { VulkanFunctions.vkGetInstanceProcAddr = castFunPtr pVkGetInstanceProcAddr
        , VulkanFunctions.vkGetDeviceProcAddr = castFunPtr pVkGetDeviceProcAddr
        }
    Vk.Instance ih InstanceCmds{pVkGetInstanceProcAddr} = inst
    Vk.Device dh DeviceCmds{pVkGetDeviceProcAddr} = dev
