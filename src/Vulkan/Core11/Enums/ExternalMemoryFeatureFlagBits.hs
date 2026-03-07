{-# language CPP #-}
-- No documentation found for Chapter "ExternalMemoryFeatureFlagBits"
module Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits  ( ExternalMemoryFeatureFlags
                                                          , ExternalMemoryFeatureFlagBits( EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
                                                                                         , EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
                                                                                         , EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
                                                                                         , ..
                                                                                         )
                                                          ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type ExternalMemoryFeatureFlags = ExternalMemoryFeatureFlagBits

-- | VkExternalMemoryFeatureFlagBits - Bitmask specifying features of an
-- external memory handle type
--
-- = Description
--
-- -   'EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT' specifies that tensors,
--     images or buffers created with the specified parameters and handle
--     type /must/ use the mechanisms defined by
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedRequirements'
--     and
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'
--     or
--     'Vulkan.Extensions.VK_ARM_tensors.MemoryDedicatedAllocateInfoTensorARM'
--     to create (or import) a dedicated allocation for the tensor, image
--     or buffer.
--
-- -   'EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT' specifies that handles of
--     this type /can/ be exported from Vulkan memory objects.
--
-- -   'EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT' specifies that handles of
--     this type /can/ be imported as Vulkan memory objects.
--
-- Because their semantics in external APIs roughly align with that of an
-- image or buffer with a dedicated allocation in Vulkan, implementations
-- are /required/ to report 'EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT'
-- for the following external handle types:
--
-- -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT'
--
-- -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT'
--
-- -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT'
--
-- -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     for images only
--
-- -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_SCREEN_BUFFER_BIT_QNX'
--     for images only
--
-- -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLTEXTURE_BIT_EXT'
--
-- Implementations /must/ not report
-- 'EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT' for buffers with external
-- handle type
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'.
-- Implementations /must/ not report
-- 'EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT' for buffers with external
-- handle type
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_SCREEN_BUFFER_BIT_QNX'.
-- Implementations /must/ not report
-- 'EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT' for tensors, images or
-- buffers with external handle type
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT',
-- or
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT'.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_capabilities VK_KHR_external_memory_capabilities>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'ExternalMemoryFeatureFlags'
newtype ExternalMemoryFeatureFlagBits = ExternalMemoryFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBits" "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT"
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT = ExternalMemoryFeatureFlagBits 0x00000001

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBits" "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT"
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT = ExternalMemoryFeatureFlagBits 0x00000002

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBits" "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT"
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT = ExternalMemoryFeatureFlagBits 0x00000004

conNameExternalMemoryFeatureFlagBits :: String
conNameExternalMemoryFeatureFlagBits = "ExternalMemoryFeatureFlagBits"

enumPrefixExternalMemoryFeatureFlagBits :: String
enumPrefixExternalMemoryFeatureFlagBits = "EXTERNAL_MEMORY_FEATURE_"

showTableExternalMemoryFeatureFlagBits :: [(ExternalMemoryFeatureFlagBits, String)]
showTableExternalMemoryFeatureFlagBits =
  [
    ( EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
    , "DEDICATED_ONLY_BIT"
    )
  ,
    ( EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
    , "EXPORTABLE_BIT"
    )
  ,
    ( EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
    , "IMPORTABLE_BIT"
    )
  ]

instance Show ExternalMemoryFeatureFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixExternalMemoryFeatureFlagBits
      showTableExternalMemoryFeatureFlagBits
      conNameExternalMemoryFeatureFlagBits
      (\(ExternalMemoryFeatureFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ExternalMemoryFeatureFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixExternalMemoryFeatureFlagBits
      showTableExternalMemoryFeatureFlagBits
      conNameExternalMemoryFeatureFlagBits
      ExternalMemoryFeatureFlagBits
