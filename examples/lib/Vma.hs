{-# LANGUAGE RecordWildCards #-}

-- | Shared 'Allocator' construction for VMA-using examples. Each caller passes
-- its own create flags and target Vulkan API version.
module Vma
  ( createVMA
  ) where

import           Control.Monad.Trans.Resource ( MonadResource, allocate )
import           Data.Word                    ( Word32 )
import           Foreign.Ptr                  ( castFunPtr )
import           Vulkan.Core10                ( Device(..)
                                              , Instance(..)
                                              , PhysicalDevice
                                              , deviceHandle
                                              , instanceHandle
                                              , physicalDeviceHandle
                                              )
import           Vulkan.Dynamic               ( DeviceCmds(DeviceCmds, pVkGetDeviceProcAddr)
                                              , InstanceCmds(InstanceCmds, pVkGetInstanceProcAddr)
                                              )
import           Vulkan.Zero                  ( zero )
import           VulkanMemoryAllocator        ( Allocator
                                              , AllocatorCreateFlags
                                              , AllocatorCreateInfo(..)
                                              , VulkanFunctions(..)
                                              , withAllocator
                                              )

createVMA
  :: MonadResource m
  => AllocatorCreateFlags
  -> Word32                -- ^ Target Vulkan API version
  -> Instance
  -> PhysicalDevice
  -> Device
  -> m Allocator
createVMA flags' apiVer inst phys dev =
  snd
    <$> withAllocator
          zero
            { flags            = flags'
            , physicalDevice   = physicalDeviceHandle phys
            , device           = deviceHandle dev
            , instance'        = instanceHandle inst
            , vulkanApiVersion = apiVer
            , vulkanFunctions  = Just $ case inst of
              Instance _ InstanceCmds {..} -> case dev of
                Device _ DeviceCmds {..} -> zero
                  { vkGetInstanceProcAddr = castFunPtr pVkGetInstanceProcAddr
                  , vkGetDeviceProcAddr   = castFunPtr pVkGetDeviceProcAddr
                  }
            }
          allocate
