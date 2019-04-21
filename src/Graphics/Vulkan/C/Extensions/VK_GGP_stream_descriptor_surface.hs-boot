{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( GgpStreamDescriptor
  , VkStreamDescriptorSurfaceCreateFlagsGGP
  , VkStreamDescriptorSurfaceCreateInfoGGP
  , FN_vkCreateStreamDescriptorSurfaceGGP
  , PFN_vkCreateStreamDescriptorSurfaceGGP
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkInstance
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )


-- No documentation found for TopLevel "GgpStreamDescriptor"
type GgpStreamDescriptor = Word32
  

data VkStreamDescriptorSurfaceCreateFlagsGGP

data VkStreamDescriptorSurfaceCreateInfoGGP

type FN_vkCreateStreamDescriptorSurfaceGGP = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkStreamDescriptorSurfaceCreateInfoGGP) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateStreamDescriptorSurfaceGGP = FunPtr FN_vkCreateStreamDescriptorSurfaceGGP
