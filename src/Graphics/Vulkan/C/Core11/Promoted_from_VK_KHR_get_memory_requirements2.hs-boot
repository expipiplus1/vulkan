{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( VkBufferMemoryRequirementsInfo2
  , VkImageMemoryRequirementsInfo2
  , VkImageSparseMemoryRequirementsInfo2
  , VkMemoryRequirements2
  , VkMemoryRequirements2KHR
  , VkSparseImageMemoryRequirements2
  , FN_vkGetBufferMemoryRequirements2
  , PFN_vkGetBufferMemoryRequirements2
  , FN_vkGetImageMemoryRequirements2
  , PFN_vkGetImageMemoryRequirements2
  , FN_vkGetImageSparseMemoryRequirements2
  , PFN_vkGetImageSparseMemoryRequirements2
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
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )


data VkBufferMemoryRequirementsInfo2

data VkImageMemoryRequirementsInfo2

data VkImageSparseMemoryRequirementsInfo2

data VkMemoryRequirements2

-- No documentation found for TopLevel "VkMemoryRequirements2KHR"
type VkMemoryRequirements2KHR = VkMemoryRequirements2

data VkSparseImageMemoryRequirements2

type FN_vkGetBufferMemoryRequirements2 = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
type PFN_vkGetBufferMemoryRequirements2 = FunPtr FN_vkGetBufferMemoryRequirements2

type FN_vkGetImageMemoryRequirements2 = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
type PFN_vkGetImageMemoryRequirements2 = FunPtr FN_vkGetImageMemoryRequirements2

type FN_vkGetImageSparseMemoryRequirements2 = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ()
type PFN_vkGetImageSparseMemoryRequirements2 = FunPtr FN_vkGetImageSparseMemoryRequirements2
