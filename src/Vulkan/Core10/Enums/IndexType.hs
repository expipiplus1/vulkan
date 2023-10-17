{-# language CPP #-}
-- No documentation found for Chapter "IndexType"
module Vulkan.Core10.Enums.IndexType  (IndexType( INDEX_TYPE_UINT16
                                                , INDEX_TYPE_UINT32
                                                , INDEX_TYPE_UINT8_EXT
                                                , INDEX_TYPE_NONE_KHR
                                                , ..
                                                )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkIndexType - Type of index buffer indices
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR',
-- 'Vulkan.Extensions.VK_NV_displacement_micromap.AccelerationStructureTrianglesDisplacementMicromapNV',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.AccelerationStructureTrianglesOpacityMicromapEXT',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.BindIndexBufferIndirectCommandNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutTokenNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer',
-- 'Vulkan.Extensions.VK_KHR_maintenance5.cmdBindIndexBuffer2KHR'
newtype IndexType = IndexType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'INDEX_TYPE_UINT16' specifies that indices are 16-bit unsigned integer
-- values.
pattern INDEX_TYPE_UINT16 = IndexType 0

-- | 'INDEX_TYPE_UINT32' specifies that indices are 32-bit unsigned integer
-- values.
pattern INDEX_TYPE_UINT32 = IndexType 1

-- | 'INDEX_TYPE_UINT8_EXT' specifies that indices are 8-bit unsigned integer
-- values.
pattern INDEX_TYPE_UINT8_EXT = IndexType 1000265000

-- | 'INDEX_TYPE_NONE_KHR' specifies that no indices are provided.
pattern INDEX_TYPE_NONE_KHR = IndexType 1000165000

{-# COMPLETE
  INDEX_TYPE_UINT16
  , INDEX_TYPE_UINT32
  , INDEX_TYPE_UINT8_EXT
  , INDEX_TYPE_NONE_KHR ::
    IndexType
  #-}

conNameIndexType :: String
conNameIndexType = "IndexType"

enumPrefixIndexType :: String
enumPrefixIndexType = "INDEX_TYPE_"

showTableIndexType :: [(IndexType, String)]
showTableIndexType =
  [ (INDEX_TYPE_UINT16, "UINT16")
  , (INDEX_TYPE_UINT32, "UINT32")
  , (INDEX_TYPE_UINT8_EXT, "UINT8_EXT")
  , (INDEX_TYPE_NONE_KHR, "NONE_KHR")
  ]

instance Show IndexType where
  showsPrec =
    enumShowsPrec
      enumPrefixIndexType
      showTableIndexType
      conNameIndexType
      (\(IndexType x) -> x)
      (showsPrec 11)

instance Read IndexType where
  readPrec =
    enumReadPrec
      enumPrefixIndexType
      showTableIndexType
      conNameIndexType
      IndexType
