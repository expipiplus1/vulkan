{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.C.Core10.Core
  ( VkBaseInStructure
  , VkBaseOutStructure
  , VkBool32
  , VkFlags
  , VkFormat
  , VkObjectType
  , VkResult
  , VkStructureType
  , VkVendorId
  ) where

import Data.Word
  ( Word32
  )





data VkBaseInStructure

data VkBaseOutStructure

data VkBool32

-- | VkFlags - Vulkan bitmasks
--
-- = Description
--
-- Bitmasks are passed to many commands and structures to compactly
-- represent options, but 'VkFlags' is not used directly in the API.
-- Instead, a @Vk*Flags@ type which is an alias of 'VkFlags', and whose
-- name matches the corresponding @Vk*FlagBits@ that are valid for that
-- type, is used.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkColorComponentFlags'
type VkFlags = Word32

data VkFormat

data VkObjectType

data VkResult

data VkStructureType

data VkVendorId
