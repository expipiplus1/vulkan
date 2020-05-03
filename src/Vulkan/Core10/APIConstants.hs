{-# language CPP #-}
module Vulkan.Core10.APIConstants  ( pattern LOD_CLAMP_NONE
                                   , LUID_SIZE_KHR
                                   , QUEUE_FAMILY_EXTERNAL_KHR
                                   , MAX_DEVICE_GROUP_SIZE_KHR
                                   , MAX_DRIVER_NAME_SIZE_KHR
                                   , MAX_DRIVER_INFO_SIZE_KHR
                                   , SHADER_UNUSED_NV
                                   , MAX_PHYSICAL_DEVICE_NAME_SIZE
                                   , pattern MAX_PHYSICAL_DEVICE_NAME_SIZE
                                   , UUID_SIZE
                                   , pattern UUID_SIZE
                                   , LUID_SIZE
                                   , pattern LUID_SIZE
                                   , MAX_EXTENSION_NAME_SIZE
                                   , pattern MAX_EXTENSION_NAME_SIZE
                                   , MAX_DESCRIPTION_SIZE
                                   , pattern MAX_DESCRIPTION_SIZE
                                   , MAX_MEMORY_TYPES
                                   , pattern MAX_MEMORY_TYPES
                                   , MAX_MEMORY_HEAPS
                                   , pattern MAX_MEMORY_HEAPS
                                   , REMAINING_MIP_LEVELS
                                   , pattern REMAINING_MIP_LEVELS
                                   , REMAINING_ARRAY_LAYERS
                                   , pattern REMAINING_ARRAY_LAYERS
                                   , WHOLE_SIZE
                                   , pattern WHOLE_SIZE
                                   , ATTACHMENT_UNUSED
                                   , pattern ATTACHMENT_UNUSED
                                   , QUEUE_FAMILY_IGNORED
                                   , pattern QUEUE_FAMILY_IGNORED
                                   , QUEUE_FAMILY_EXTERNAL
                                   , pattern QUEUE_FAMILY_EXTERNAL
                                   , QUEUE_FAMILY_FOREIGN_EXT
                                   , pattern QUEUE_FAMILY_FOREIGN_EXT
                                   , SUBPASS_EXTERNAL
                                   , pattern SUBPASS_EXTERNAL
                                   , MAX_DEVICE_GROUP_SIZE
                                   , pattern MAX_DEVICE_GROUP_SIZE
                                   , MAX_DRIVER_NAME_SIZE
                                   , pattern MAX_DRIVER_NAME_SIZE
                                   , MAX_DRIVER_INFO_SIZE
                                   , pattern MAX_DRIVER_INFO_SIZE
                                   , SHADER_UNUSED_KHR
                                   , pattern SHADER_UNUSED_KHR
                                   , pattern NULL_HANDLE
                                   , IsHandle
                                   , Bool32(..)
                                   , PipelineCacheHeaderVersion(..)
                                   ) where

import Data.Word (Word32)
import Data.Word (Word64)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.BaseType (Bool32(..))
import Vulkan.Core10.Enums.PipelineCacheHeaderVersion (PipelineCacheHeaderVersion(..))
-- No documentation found for TopLevel "VK_LOD_CLAMP_NONE"
pattern LOD_CLAMP_NONE :: Float
pattern LOD_CLAMP_NONE = 1000.0


-- No documentation found for TopLevel "VK_LUID_SIZE_KHR"
type LUID_SIZE_KHR = LUID_SIZE


-- No documentation found for TopLevel "VK_QUEUE_FAMILY_EXTERNAL_KHR"
type QUEUE_FAMILY_EXTERNAL_KHR = QUEUE_FAMILY_EXTERNAL


-- No documentation found for TopLevel "VK_MAX_DEVICE_GROUP_SIZE_KHR"
type MAX_DEVICE_GROUP_SIZE_KHR = MAX_DEVICE_GROUP_SIZE


-- No documentation found for TopLevel "VK_MAX_DRIVER_NAME_SIZE_KHR"
type MAX_DRIVER_NAME_SIZE_KHR = MAX_DRIVER_NAME_SIZE


-- No documentation found for TopLevel "VK_MAX_DRIVER_INFO_SIZE_KHR"
type MAX_DRIVER_INFO_SIZE_KHR = MAX_DRIVER_INFO_SIZE


-- No documentation found for TopLevel "VK_SHADER_UNUSED_NV"
type SHADER_UNUSED_NV = SHADER_UNUSED_KHR


type MAX_PHYSICAL_DEVICE_NAME_SIZE = 256

-- No documentation found for TopLevel "VK_MAX_PHYSICAL_DEVICE_NAME_SIZE"
pattern MAX_PHYSICAL_DEVICE_NAME_SIZE :: forall a . Integral a => a
pattern MAX_PHYSICAL_DEVICE_NAME_SIZE = 256


type UUID_SIZE = 16

-- No documentation found for TopLevel "VK_UUID_SIZE"
pattern UUID_SIZE :: forall a . Integral a => a
pattern UUID_SIZE = 16


type LUID_SIZE = 8

-- No documentation found for TopLevel "VK_LUID_SIZE"
pattern LUID_SIZE :: forall a . Integral a => a
pattern LUID_SIZE = 8


type MAX_EXTENSION_NAME_SIZE = 256

-- No documentation found for TopLevel "VK_MAX_EXTENSION_NAME_SIZE"
pattern MAX_EXTENSION_NAME_SIZE :: forall a . Integral a => a
pattern MAX_EXTENSION_NAME_SIZE = 256


type MAX_DESCRIPTION_SIZE = 256

-- No documentation found for TopLevel "VK_MAX_DESCRIPTION_SIZE"
pattern MAX_DESCRIPTION_SIZE :: forall a . Integral a => a
pattern MAX_DESCRIPTION_SIZE = 256


type MAX_MEMORY_TYPES = 32

-- No documentation found for TopLevel "VK_MAX_MEMORY_TYPES"
pattern MAX_MEMORY_TYPES :: forall a . Integral a => a
pattern MAX_MEMORY_TYPES = 32


type MAX_MEMORY_HEAPS = 16

-- No documentation found for TopLevel "VK_MAX_MEMORY_HEAPS"
pattern MAX_MEMORY_HEAPS :: forall a . Integral a => a
pattern MAX_MEMORY_HEAPS = 16


type REMAINING_MIP_LEVELS = 0xffffffff

-- No documentation found for TopLevel "VK_REMAINING_MIP_LEVELS"
pattern REMAINING_MIP_LEVELS :: Word32
pattern REMAINING_MIP_LEVELS = 0xffffffff


type REMAINING_ARRAY_LAYERS = 0xffffffff

-- No documentation found for TopLevel "VK_REMAINING_ARRAY_LAYERS"
pattern REMAINING_ARRAY_LAYERS :: Word32
pattern REMAINING_ARRAY_LAYERS = 0xffffffff


type WHOLE_SIZE = 0xffffffffffffffff

-- No documentation found for TopLevel "VK_WHOLE_SIZE"
pattern WHOLE_SIZE :: Word64
pattern WHOLE_SIZE = 0xffffffffffffffff


type ATTACHMENT_UNUSED = 0xffffffff

-- No documentation found for TopLevel "VK_ATTACHMENT_UNUSED"
pattern ATTACHMENT_UNUSED :: Word32
pattern ATTACHMENT_UNUSED = 0xffffffff


type QUEUE_FAMILY_IGNORED = 0xffffffff

-- No documentation found for TopLevel "VK_QUEUE_FAMILY_IGNORED"
pattern QUEUE_FAMILY_IGNORED :: Word32
pattern QUEUE_FAMILY_IGNORED = 0xffffffff


type QUEUE_FAMILY_EXTERNAL = 0xfffffffe

-- No documentation found for TopLevel "VK_QUEUE_FAMILY_EXTERNAL"
pattern QUEUE_FAMILY_EXTERNAL :: Word32
pattern QUEUE_FAMILY_EXTERNAL = 0xfffffffe


type QUEUE_FAMILY_FOREIGN_EXT = 0xfffffffd

-- No documentation found for TopLevel "VK_QUEUE_FAMILY_FOREIGN_EXT"
pattern QUEUE_FAMILY_FOREIGN_EXT :: Word32
pattern QUEUE_FAMILY_FOREIGN_EXT = 0xfffffffd


type SUBPASS_EXTERNAL = 0xffffffff

-- No documentation found for TopLevel "VK_SUBPASS_EXTERNAL"
pattern SUBPASS_EXTERNAL :: Word32
pattern SUBPASS_EXTERNAL = 0xffffffff


type MAX_DEVICE_GROUP_SIZE = 32

-- No documentation found for TopLevel "VK_MAX_DEVICE_GROUP_SIZE"
pattern MAX_DEVICE_GROUP_SIZE :: forall a . Integral a => a
pattern MAX_DEVICE_GROUP_SIZE = 32


type MAX_DRIVER_NAME_SIZE = 256

-- No documentation found for TopLevel "VK_MAX_DRIVER_NAME_SIZE"
pattern MAX_DRIVER_NAME_SIZE :: forall a . Integral a => a
pattern MAX_DRIVER_NAME_SIZE = 256


type MAX_DRIVER_INFO_SIZE = 256

-- No documentation found for TopLevel "VK_MAX_DRIVER_INFO_SIZE"
pattern MAX_DRIVER_INFO_SIZE :: forall a . Integral a => a
pattern MAX_DRIVER_INFO_SIZE = 256


type SHADER_UNUSED_KHR = 0xffffffff

-- No documentation found for TopLevel "VK_SHADER_UNUSED_KHR"
pattern SHADER_UNUSED_KHR :: Word32
pattern SHADER_UNUSED_KHR = 0xffffffff


-- | VK_NULL_HANDLE - Reserved non-valid object handle
--
-- = See Also
--
-- No cross-references are available
pattern NULL_HANDLE :: IsHandle a => a
pattern NULL_HANDLE <- ((== zero) -> True)
  where NULL_HANDLE = zero

-- | A class for things which can be created with 'NULL_HANDLE'
class (Eq a, Zero a) => IsHandle a where

