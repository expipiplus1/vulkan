{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance8 - device extension
--
-- = VK_KHR_maintenance8
--
-- [__Name String__]
--     @VK_KHR_maintenance8@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     575
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance8] @zmike%0A*Here describe the issue or question you have about the VK_KHR_maintenance8 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance8.adoc VK_KHR_maintenance8>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-07
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Jon Leech, Khronos
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Spencer Fricke, LunarG
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Piers Daniell, NVIDIA
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Ricardo Garcia, Igalia
--
--     -   Lionel Landwerlin, Intel
--
--     -   Rick Hammerstone, Qualcomm
--
--     -   Daniel Story, Nintendo
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Caterina Shablia, Collabora
--
--     -   Georg Lehmann, Valve
--
--     -   Shahbaz Youssefi, Google
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance8 VK_KHR_maintenance8>
-- adds a collection of minor features, none of which would warrant an
-- entire extension of their own.
--
-- The new features are as follows:
--
-- -   Allow copies between depth\/stencil and “matching” color attachments
--
-- -   Allow @dstCache@ in
--     'Vulkan.Core10.PipelineCache.mergePipelineCaches' to be implicitly
--     synchronized.
--
-- -   Require src\/dst sync scopes to work when doing queue family
--     ownership transfers
--
-- -   Support @Offset@ (as an alternative to @ConstOffset@) image operand
--     in texture sampling and fetch operations
--
-- -   Use the SPIR-V definition of @OpSRem@ and @OpSMod@, making these
--     operations produce well-defined results for negative operands
--
-- -   Loosen layer restrictions when blitting from 3D images to other
--     image types
--
-- -   Add space for an additional 64 access flags for use with
--     VkMemoryBarrier2, VkBufferMemoryBarrier2, and VkImageMemoryBarrier2
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance8FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDependency2',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.BufferMemoryBarrier2',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.ImageMemoryBarrier2':
--
--     -   'MemoryBarrierAccessFlags3KHR'
--
-- == New Enums
--
-- -   'AccessFlagBits3KHR'
--
-- -   'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PipelineCacheCreateFlagBits'
--
-- == New Bitmasks
--
-- -   'AccessFlags3KHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_8_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_8_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits':
--
--     -   'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_QUEUE_FAMILY_OWNERSHIP_TRANSFER_USE_ALL_STAGES_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PipelineCacheCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_INTERNALLY_SYNCHRONIZED_MERGE_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_BARRIER_ACCESS_FLAGS_3_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_8_FEATURES_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2024-06-20 (Jon Leech)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_maintenance8 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance8  ( PhysicalDeviceMaintenance8FeaturesKHR(..)
                                              , MemoryBarrierAccessFlags3KHR(..)
                                              , AccessFlags3KHR
                                              , AccessFlagBits3KHR( ACCESS_3_NONE_KHR
                                                                  , ..
                                                                  )
                                              , KHR_MAINTENANCE_8_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE_8_SPEC_VERSION
                                              , KHR_MAINTENANCE_8_EXTENSION_NAME
                                              , pattern KHR_MAINTENANCE_8_EXTENSION_NAME
                                              ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Flags64)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_BARRIER_ACCESS_FLAGS_3_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_8_FEATURES_KHR))
-- | VkPhysicalDeviceMaintenance8FeaturesKHR - Structure describing whether
-- the implementation supports maintenance8 functionality
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance8FeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceMaintenance8FeaturesKHR', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance8 VK_KHR_maintenance8>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance8FeaturesKHR = PhysicalDeviceMaintenance8FeaturesKHR
  { -- | #features-maintenance8# @maintenance8@ indicates that the implementation
    -- supports the following:
    --
    -- -   Allow copies between depth\/stencil and “matching” color attachments
    --
    -- -   Allow @dstCache@ in
    --     'Vulkan.Core10.PipelineCache.mergePipelineCaches' to be implicitly
    --     synchronized.
    --
    -- -   Require src\/dst sync scopes to work when doing queue family
    --     ownership transfers
    --
    -- -   Support @Offset@ (as an alternative to @ConstOffset@) image operand
    --     in texture sampling and fetch operations
    --
    -- -   Use the SPIR-V definition of OpSRem and OpSMod, making these
    --     operations produce well-defined results for negative operands
    --
    -- -   Loosen layer restrictions when blitting from 3D images to other
    --     image types
    --
    -- -   Add space for an additional 64 access flags for use with
    --     VkMemoryBarrier2, VkBufferMemoryBarrier2, and VkImageMemoryBarrier2
    maintenance8 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance8FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance8FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance8FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance8FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_8_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (maintenance8))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_8_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance8FeaturesKHR where
  peekCStruct p = do
    maintenance8 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance8FeaturesKHR
             (bool32ToBool maintenance8)

instance Storable PhysicalDeviceMaintenance8FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance8FeaturesKHR where
  zero = PhysicalDeviceMaintenance8FeaturesKHR
           zero


-- | VkMemoryBarrierAccessFlags3KHR - Structure specifying additional access
-- flags
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance8 VK_KHR_maintenance8>,
-- 'AccessFlags3KHR', 'Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryBarrierAccessFlags3KHR = MemoryBarrierAccessFlags3KHR
  { -- | @srcAccessMask3@ is a 'AccessFlags3KHR' mask of access flags to be
    -- included in the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-access-scopes first access scope>.
    --
    -- #VUID-VkMemoryBarrierAccessFlags3KHR-srcAccessMask3-parameter#
    -- @srcAccessMask3@ /must/ be a valid combination of 'AccessFlagBits3KHR'
    -- values
    srcAccessMask3 :: AccessFlags3KHR
  , -- | @dstAccessMask3@ is a 'AccessFlags3KHR' mask of access flags to be
    -- included in the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-access-scopes second access scope>.
    --
    -- #VUID-VkMemoryBarrierAccessFlags3KHR-dstAccessMask3-parameter#
    -- @dstAccessMask3@ /must/ be a valid combination of 'AccessFlagBits3KHR'
    -- values
    dstAccessMask3 :: AccessFlags3KHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryBarrierAccessFlags3KHR)
#endif
deriving instance Show MemoryBarrierAccessFlags3KHR

instance ToCStruct MemoryBarrierAccessFlags3KHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryBarrierAccessFlags3KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_BARRIER_ACCESS_FLAGS_3_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccessFlags3KHR)) (srcAccessMask3)
    poke ((p `plusPtr` 24 :: Ptr AccessFlags3KHR)) (dstAccessMask3)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_BARRIER_ACCESS_FLAGS_3_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct MemoryBarrierAccessFlags3KHR where
  peekCStruct p = do
    srcAccessMask3 <- peek @AccessFlags3KHR ((p `plusPtr` 16 :: Ptr AccessFlags3KHR))
    dstAccessMask3 <- peek @AccessFlags3KHR ((p `plusPtr` 24 :: Ptr AccessFlags3KHR))
    pure $ MemoryBarrierAccessFlags3KHR
             srcAccessMask3 dstAccessMask3

instance Storable MemoryBarrierAccessFlags3KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryBarrierAccessFlags3KHR where
  zero = MemoryBarrierAccessFlags3KHR
           zero
           zero


type AccessFlags3KHR = AccessFlagBits3KHR

-- | VkAccessFlagBits3KHR - Access flags for VkAccessFlags3KHR
--
-- = Description
--
-- -   'ACCESS_3_NONE_KHR' specifies no additional accesses.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance8 VK_KHR_maintenance8>,
-- 'AccessFlags3KHR'
newtype AccessFlagBits3KHR = AccessFlagBits3KHR Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkAccessFlagBits3KHR" "VK_ACCESS_3_NONE_KHR"
pattern ACCESS_3_NONE_KHR = AccessFlagBits3KHR 0x0000000000000000

conNameAccessFlagBits3KHR :: String
conNameAccessFlagBits3KHR = "AccessFlagBits3KHR"

enumPrefixAccessFlagBits3KHR :: String
enumPrefixAccessFlagBits3KHR = "ACCESS_3_NONE_KHR"

showTableAccessFlagBits3KHR :: [(AccessFlagBits3KHR, String)]
showTableAccessFlagBits3KHR = [(ACCESS_3_NONE_KHR, "")]

instance Show AccessFlagBits3KHR where
  showsPrec =
    enumShowsPrec
      enumPrefixAccessFlagBits3KHR
      showTableAccessFlagBits3KHR
      conNameAccessFlagBits3KHR
      (\(AccessFlagBits3KHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read AccessFlagBits3KHR where
  readPrec =
    enumReadPrec
      enumPrefixAccessFlagBits3KHR
      showTableAccessFlagBits3KHR
      conNameAccessFlagBits3KHR
      AccessFlagBits3KHR

type KHR_MAINTENANCE_8_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_8_SPEC_VERSION"
pattern KHR_MAINTENANCE_8_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE_8_SPEC_VERSION = 1


type KHR_MAINTENANCE_8_EXTENSION_NAME = "VK_KHR_maintenance8"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_8_EXTENSION_NAME"
pattern KHR_MAINTENANCE_8_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE_8_EXTENSION_NAME = "VK_KHR_maintenance8"

