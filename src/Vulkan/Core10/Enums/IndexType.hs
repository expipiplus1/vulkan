{-# language CPP #-}
-- No documentation found for Chapter "IndexType"
module Vulkan.Core10.Enums.IndexType  (IndexType( INDEX_TYPE_UINT16
                                                , INDEX_TYPE_UINT32
                                                , INDEX_TYPE_NONE_KHR
                                                , INDEX_TYPE_UINT8
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
-- = Description
--
-- -   'INDEX_TYPE_UINT16' specifies that indices are 16-bit unsigned
--     integer values.
--
-- -   'INDEX_TYPE_UINT32' specifies that indices are 32-bit unsigned
--     integer values.
--
-- -   'INDEX_TYPE_NONE_KHR' specifies that no indices are provided.
--
-- -   'INDEX_TYPE_UINT8' specifies that indices are 8-bit unsigned integer
--     values.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_NV_ray_tracing_linear_swept_spheres.AccelerationStructureGeometryLinearSweptSpheresDataNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing_linear_swept_spheres.AccelerationStructureGeometrySpheresDataNV',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR',
-- 'Vulkan.Extensions.VK_NV_displacement_micromap.AccelerationStructureTrianglesDisplacementMicromapNV',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.AccelerationStructureTrianglesOpacityMicromapEXT',
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.BindIndexBufferIndirectCommandEXT',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.BindIndexBufferIndirectCommandNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutTokenNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.cmdBindIndexBuffer2',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.cmdBindIndexBuffer2'
newtype IndexType = IndexType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkIndexType" "VK_INDEX_TYPE_UINT16"
pattern INDEX_TYPE_UINT16 = IndexType 0

-- No documentation found for Nested "VkIndexType" "VK_INDEX_TYPE_UINT32"
pattern INDEX_TYPE_UINT32 = IndexType 1

-- No documentation found for Nested "VkIndexType" "VK_INDEX_TYPE_NONE_KHR"
pattern INDEX_TYPE_NONE_KHR = IndexType 1000165000

-- No documentation found for Nested "VkIndexType" "VK_INDEX_TYPE_UINT8"
pattern INDEX_TYPE_UINT8 = IndexType 1000265000

{-# COMPLETE
  INDEX_TYPE_UINT16
  , INDEX_TYPE_UINT32
  , INDEX_TYPE_NONE_KHR
  , INDEX_TYPE_UINT8 ::
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
  , (INDEX_TYPE_NONE_KHR, "NONE_KHR")
  , (INDEX_TYPE_UINT8, "UINT8")
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
