{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage
  ( VkImageStencilUsageCreateInfoEXT(..)
  , pattern VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
  , pattern VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageUsageFlags
  )


-- No documentation found for TopLevel "VkImageStencilUsageCreateInfoEXT"
data VkImageStencilUsageCreateInfoEXT = VkImageStencilUsageCreateInfoEXT
  { -- No documentation found for Nested "VkImageStencilUsageCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageStencilUsageCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageStencilUsageCreateInfoEXT" "stencilUsage"
  vkStencilUsage :: VkImageUsageFlags
  }
  deriving (Eq, Show)

instance Storable VkImageStencilUsageCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageStencilUsageCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageStencilUsageCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageStencilUsageCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkStencilUsage (poked :: VkImageStencilUsageCreateInfoEXT))

instance Zero VkImageStencilUsageCreateInfoEXT where
  zero = VkImageStencilUsageCreateInfoEXT VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
                                          zero
                                          zero

-- No documentation found for TopLevel "VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME"
pattern VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME = "VK_EXT_separate_stencil_usage"

-- No documentation found for TopLevel "VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION"
pattern VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT = VkStructureType 1000246000
