{-# language CPP #-}
-- | = Name
--
-- VK_NV_displacement_micromap - device extension
--
-- == VK_NV_displacement_micromap
--
-- [__Name String__]
--     @VK_NV_displacement_micromap@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     398
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>
--
--     -   __This is a /provisional/ extension and /must/ be used with
--         caution. See the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#boilerplate-provisional-header description>
--         of provisional header files for enablement and stability
--         details.__
--
-- [__Contact__]
--
--     -   Christoph Kubisch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_displacement_micromap] @pixeljetstream%0A*Here describe the issue or question you have about the VK_NV_displacement_micromap extension* >
--
--     -   Eric Werness
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-17
--
-- [__Interactions and External Dependencies__]
--     TBD
--
-- [__Contributors__]
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
-- == Description
--
-- Ray tracing can very efficiently render from geometry which has very
-- fine detail, but when using only a basic triangle representation, memory
-- consumption can be an issue. This extension adds the ability to add a
-- /displacement map/ to add more detail to triangles in an acceleration
-- structure with an efficient in-memory format. The format is externally
-- visible to allow the application to compress its internal geometry
-- representations into the compressed format ahead of time. This format
-- adds displacements along a defined vector to subtriangle vertices which
-- are subdivided from the main triangles.
--
-- This extension provides:
--
-- -   a new 'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapTypeEXT'
--     format for the displacement micromap,
--
-- -   a structure to extend
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR'
--     to attach a displacement micromap to the geometry of the
--     acceleration structure,
--
-- -   enums extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.BuildAccelerationStructureFlagBitsKHR'
--     to allow for updates.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR':
--
--     -   'AccelerationStructureTrianglesDisplacementMicromapNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDisplacementMicromapFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDisplacementMicromapPropertiesNV'
--
-- == New Enums
--
-- -   'DisplacementMicromapFormatNV'
--
-- == New Enum Constants
--
-- -   'NV_DISPLACEMENT_MICROMAP_EXTENSION_NAME'
--
-- -   'NV_DISPLACEMENT_MICROMAP_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.BuildAccelerationStructureFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_DISPLACEMENT_MICROMAP_UPDATE_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapTypeEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_opacity_micromap.MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_DISPLACEMENT_MICROMAP_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_PROPERTIES_NV'
--
-- == Issues
--
-- (1) What is the status of this extension?
--
-- -   Provisional and expected to change. The broad structure and encoding
--     format are stable, but there will likely be changes to the
--     structures, enumerant values, and shader interface.
--
-- == Version History
--
-- -   Revision 1, 2023-03-17 (Eric Werness)
--
--     -   Initial public revision
--
-- == See Also
--
-- 'AccelerationStructureTrianglesDisplacementMicromapNV',
-- 'DisplacementMicromapFormatNV',
-- 'PhysicalDeviceDisplacementMicromapFeaturesNV',
-- 'PhysicalDeviceDisplacementMicromapPropertiesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_displacement_micromap Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_displacement_micromap  ( PhysicalDeviceDisplacementMicromapFeaturesNV(..)
                                                      , PhysicalDeviceDisplacementMicromapPropertiesNV(..)
                                                      , AccelerationStructureTrianglesDisplacementMicromapNV(..)
                                                      , DisplacementMicromapFormatNV( DISPLACEMENT_MICROMAP_FORMAT_64_TRIANGLES_64_BYTES_NV
                                                                                    , DISPLACEMENT_MICROMAP_FORMAT_256_TRIANGLES_128_BYTES_NV
                                                                                    , DISPLACEMENT_MICROMAP_FORMAT_1024_TRIANGLES_128_BYTES_NV
                                                                                    , ..
                                                                                    )
                                                      , NV_DISPLACEMENT_MICROMAP_SPEC_VERSION
                                                      , pattern NV_DISPLACEMENT_MICROMAP_SPEC_VERSION
                                                      , NV_DISPLACEMENT_MICROMAP_EXTENSION_NAME
                                                      , pattern NV_DISPLACEMENT_MICROMAP_EXTENSION_NAME
                                                      , MicromapEXT(..)
                                                      , MicromapUsageEXT(..)
                                                      , DeviceOrHostAddressConstKHR(..)
                                                      , BuildAccelerationStructureFlagBitsKHR(..)
                                                      , BuildAccelerationStructureFlagsKHR
                                                      , MicromapTypeEXT(..)
                                                      ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
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
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressConstKHR)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Extensions.Handles (MicromapEXT)
import Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapUsageEXT)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_DISPLACEMENT_MICROMAP_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_PROPERTIES_NV))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressConstKHR(..))
import Vulkan.Extensions.Handles (MicromapEXT(..))
import Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapTypeEXT(..))
import Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapUsageEXT(..))
-- | VkPhysicalDeviceDisplacementMicromapFeaturesNV - Structure describing
-- the ray tracing displacement micromap features that can be supported by
-- an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDisplacementMicromapFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDisplacementMicromapFeaturesNV' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_displacement_micromap VK_NV_displacement_micromap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDisplacementMicromapFeaturesNV = PhysicalDeviceDisplacementMicromapFeaturesNV
  { -- | #features-displacementMicromap# @displacementMicromap@ indicates whether
    -- the implementation supports the displacement micromap feature.
    displacementMicromap :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDisplacementMicromapFeaturesNV)
#endif
deriving instance Show PhysicalDeviceDisplacementMicromapFeaturesNV

instance ToCStruct PhysicalDeviceDisplacementMicromapFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDisplacementMicromapFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (displacementMicromap))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDisplacementMicromapFeaturesNV where
  peekCStruct p = do
    displacementMicromap <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDisplacementMicromapFeaturesNV
             (bool32ToBool displacementMicromap)

instance Storable PhysicalDeviceDisplacementMicromapFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDisplacementMicromapFeaturesNV where
  zero = PhysicalDeviceDisplacementMicromapFeaturesNV
           zero


-- | VkPhysicalDeviceDisplacementMicromapPropertiesNV - Structure describing
-- the displacement micromap properties of a physical device
--
-- = Description
--
-- If the 'PhysicalDeviceDisplacementMicromapPropertiesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_displacement_micromap VK_NV_displacement_micromap>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDisplacementMicromapPropertiesNV = PhysicalDeviceDisplacementMicromapPropertiesNV
  { -- | @maxDisplacementMicromapSubdivisionLevel@ is the maximum allowed
    -- @subdivisionLevel@ for displacement micromaps.
    maxDisplacementMicromapSubdivisionLevel :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDisplacementMicromapPropertiesNV)
#endif
deriving instance Show PhysicalDeviceDisplacementMicromapPropertiesNV

instance ToCStruct PhysicalDeviceDisplacementMicromapPropertiesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDisplacementMicromapPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxDisplacementMicromapSubdivisionLevel)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceDisplacementMicromapPropertiesNV where
  peekCStruct p = do
    maxDisplacementMicromapSubdivisionLevel <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceDisplacementMicromapPropertiesNV
             maxDisplacementMicromapSubdivisionLevel

instance Storable PhysicalDeviceDisplacementMicromapPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDisplacementMicromapPropertiesNV where
  zero = PhysicalDeviceDisplacementMicromapPropertiesNV
           zero


-- | VkAccelerationStructureTrianglesDisplacementMicromapNV - Structure
-- specifying a displacement micromap in a bottom-level acceleration
-- structure
--
-- = Description
--
-- If 'AccelerationStructureTrianglesDisplacementMicromapNV' is included in
-- the @pNext@ chain of a
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR'
-- structure, that geometry will reference that micromap.
--
-- For each triangle in the geometry, the acceleration structure build
-- fetches an index from @indexBuffer@ using @indexType@ and @indexStride@.
-- That triangle uses the displacement micromap information from @micromap@
-- at that index plus @baseTriangle@.
--
-- Only one of @pUsageCounts@ or @ppUsageCounts@ /can/ be a valid pointer,
-- the other /must/ be @NULL@. The elements of the non-@NULL@ array
-- describe the total count used to build this geometry. For a given
-- @format@ and @subdivisionLevel@ the number of triangles in this geometry
-- matching those values after indirection /must/ be equal to the sum of
-- matching @count@ provided.
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureTrianglesDisplacementMicromapNV-pUsageCounts-07992#
--     Only one of @pUsageCounts@ or @ppUsageCounts@ /can/ be a valid
--     pointer, the other /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureTrianglesDisplacementMicromapNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_DISPLACEMENT_MICROMAP_NV'
--
-- -   #VUID-VkAccelerationStructureTrianglesDisplacementMicromapNV-displacementBiasAndScaleFormat-parameter#
--     @displacementBiasAndScaleFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkAccelerationStructureTrianglesDisplacementMicromapNV-displacementVectorFormat-parameter#
--     @displacementVectorFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkAccelerationStructureTrianglesDisplacementMicromapNV-indexType-parameter#
--     @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   #VUID-VkAccelerationStructureTrianglesDisplacementMicromapNV-pUsageCounts-parameter#
--     If @usageCountsCount@ is not @0@, and @pUsageCounts@ is not @NULL@,
--     @pUsageCounts@ /must/ be a valid pointer to an array of
--     @usageCountsCount@
--     'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapUsageEXT'
--     structures
--
-- -   #VUID-VkAccelerationStructureTrianglesDisplacementMicromapNV-ppUsageCounts-parameter#
--     If @usageCountsCount@ is not @0@, and @ppUsageCounts@ is not @NULL@,
--     @ppUsageCounts@ /must/ be a valid pointer to an array of
--     @usageCountsCount@ valid pointers to
--     'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapUsageEXT'
--     structures
--
-- -   #VUID-VkAccelerationStructureTrianglesDisplacementMicromapNV-micromap-parameter#
--     @micromap@ /must/ be a valid 'Vulkan.Extensions.Handles.MicromapEXT'
--     handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_displacement_micromap VK_NV_displacement_micromap>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'Vulkan.Extensions.Handles.MicromapEXT',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapUsageEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureTrianglesDisplacementMicromapNV = AccelerationStructureTrianglesDisplacementMicromapNV
  { -- | @displacementBiasAndScaleFormat@ is the format of displacement bias and
    -- scale used in this displacement micromap reference.
    displacementBiasAndScaleFormat :: Format
  , -- | @displacementVectorFormat@ is the format of displacement vector used in
    -- this displacement micromap reference.
    displacementVectorFormat :: Format
  , -- | @displacementBiasAndScaleBuffer@ is the address containing the bias and
    -- scale.
    displacementBiasAndScaleBuffer :: DeviceOrHostAddressConstKHR
  , -- | @displacementBiasAndScaleStride@ is the byte stride between bias and
    -- scale values.
    displacementBiasAndScaleStride :: DeviceSize
  , -- | @displacementVectorBuffer@ is the address containing the displacement
    -- vector values.
    displacementVectorBuffer :: DeviceOrHostAddressConstKHR
  , -- | @displacementVectorStride@ is the byte stride between displacement
    -- vector values.
    displacementVectorStride :: DeviceSize
  , -- | @displacedMicromapPrimitiveFlags@ is the address containing the
    -- primitive flags.
    displacedMicromapPrimitiveFlags :: DeviceOrHostAddressConstKHR
  , -- | @displacedMicromapPrimitiveFlagsStride@ is the byte stride between
    -- primitive flag values.
    displacedMicromapPrimitiveFlagsStride :: DeviceSize
  , -- | @indexType@ is the type of triangle indices used when indexing this
    -- micromap.
    indexType :: IndexType
  , -- | @indexBuffer@ is the address containing the triangle indices.
    indexBuffer :: DeviceOrHostAddressConstKHR
  , -- | @indexStride@ is the byte stride between triangle indices.
    indexStride :: DeviceSize
  , -- | @baseTriangle@ is the base value added to the non-negative triangle
    -- indices.
    baseTriangle :: Word32
  , -- | @usageCountsCount@ specifies the number of usage counts structures that
    -- will be used to determine the size of this micromap.
    usageCountsCount :: Word32
  , -- | @pUsageCounts@ is a pointer to an array of
    -- 'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapUsageEXT' structures.
    usageCounts :: Vector MicromapUsageEXT
  , -- | @ppUsageCounts@ is a pointer to an array of pointers to
    -- 'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapUsageEXT' structures.
    usageCounts :: Vector MicromapUsageEXT
  , -- | @micromap@ is the handle to the micromap object to include in this
    -- geometry.
    micromap :: MicromapEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureTrianglesDisplacementMicromapNV)
#endif
deriving instance Show AccelerationStructureTrianglesDisplacementMicromapNV

instance ToCStruct AccelerationStructureTrianglesDisplacementMicromapNV where
  withCStruct x f = allocaBytes 128 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureTrianglesDisplacementMicromapNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_DISPLACEMENT_MICROMAP_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (displacementBiasAndScaleFormat)
    lift $ poke ((p `plusPtr` 20 :: Ptr Format)) (displacementVectorFormat)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (displacementBiasAndScaleBuffer) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (displacementBiasAndScaleStride)
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr DeviceOrHostAddressConstKHR)) (displacementVectorBuffer) . ($ ())
    lift $ poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (displacementVectorStride)
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr DeviceOrHostAddressConstKHR)) (displacedMicromapPrimitiveFlags) . ($ ())
    lift $ poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (displacedMicromapPrimitiveFlagsStride)
    lift $ poke ((p `plusPtr` 72 :: Ptr IndexType)) (indexType)
    ContT $ pokeCStruct ((p `plusPtr` 80 :: Ptr DeviceOrHostAddressConstKHR)) (indexBuffer) . ($ ())
    lift $ poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (indexStride)
    lift $ poke ((p `plusPtr` 96 :: Ptr Word32)) (baseTriangle)
    let pUsageCountsLength = Data.Vector.length $ (usageCounts)
    lift $ unless (fromIntegral pUsageCountsLength == (usageCountsCount) || pUsageCountsLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pUsageCounts must be empty or have 'usageCountsCount' elements" Nothing Nothing
    let ppUsageCountsLength = Data.Vector.length $ (usageCounts)
    lift $ unless (fromIntegral ppUsageCountsLength == (usageCountsCount) || ppUsageCountsLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "ppUsageCounts must be empty or have 'usageCountsCount' elements" Nothing Nothing
    lift $ poke ((p `plusPtr` 100 :: Ptr Word32)) ((usageCountsCount))
    pUsageCounts'' <- if Data.Vector.null (usageCounts)
      then pure nullPtr
      else do
        pPUsageCounts <- ContT $ allocaBytes @MicromapUsageEXT (((Data.Vector.length (usageCounts))) * 12)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPUsageCounts `plusPtr` (12 * (i)) :: Ptr MicromapUsageEXT) (e)) ((usageCounts))
        pure $ pPUsageCounts
    lift $ poke ((p `plusPtr` 104 :: Ptr (Ptr MicromapUsageEXT))) pUsageCounts''
    ppUsageCounts'' <- if Data.Vector.null (usageCounts)
      then pure nullPtr
      else do
        pPpUsageCounts <- ContT $ allocaBytes @(Ptr MicromapUsageEXT) (((Data.Vector.length (usageCounts))) * 8)
        Data.Vector.imapM_ (\i e -> do
          ppUsageCounts' <- ContT $ withCStruct (e)
          lift $ poke (pPpUsageCounts `plusPtr` (8 * (i)) :: Ptr (Ptr MicromapUsageEXT)) ppUsageCounts') ((usageCounts))
        pure $ pPpUsageCounts
    lift $ poke ((p `plusPtr` 112 :: Ptr (Ptr (Ptr MicromapUsageEXT)))) ppUsageCounts''
    lift $ poke ((p `plusPtr` 120 :: Ptr MicromapEXT)) (micromap)
    lift $ f
  cStructSize = 128
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_DISPLACEMENT_MICROMAP_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr Format)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 72 :: Ptr IndexType)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 80 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 96 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 120 :: Ptr MicromapEXT)) (zero)
    lift $ f

instance Zero AccelerationStructureTrianglesDisplacementMicromapNV where
  zero = AccelerationStructureTrianglesDisplacementMicromapNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           mempty
           mempty
           zero


-- | VkDisplacementMicromapFormatNV - Format enum for displacement micromaps
--
-- = Description
--
-- Note
--
-- For compactness, these values are stored as 16-bit in some structures.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_displacement_micromap VK_NV_displacement_micromap>
newtype DisplacementMicromapFormatNV = DisplacementMicromapFormatNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'DISPLACEMENT_MICROMAP_FORMAT_64_TRIANGLES_64_BYTES_NV' indicates that
-- the given micromap format encodes 64 micro-triangles worth of
-- displacements in 64 bytes as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#displacement-micromap-encoding Displacement Micromap Encoding>.
pattern DISPLACEMENT_MICROMAP_FORMAT_64_TRIANGLES_64_BYTES_NV = DisplacementMicromapFormatNV 1

-- | 'DISPLACEMENT_MICROMAP_FORMAT_256_TRIANGLES_128_BYTES_NV' indicates that
-- the given micromap format encodes 256 micro-triangles worth of
-- displacements in 128 bytes as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#displacement-micromap-encoding Displacement Micromap Encoding>.
pattern DISPLACEMENT_MICROMAP_FORMAT_256_TRIANGLES_128_BYTES_NV = DisplacementMicromapFormatNV 2

-- | 'DISPLACEMENT_MICROMAP_FORMAT_1024_TRIANGLES_128_BYTES_NV' indicates
-- that the given micromap format encodes 1024 micro-triangles worth of
-- displacements in 128 bytes as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#displacement-micromap-encoding Displacement Micromap Encoding>.
pattern DISPLACEMENT_MICROMAP_FORMAT_1024_TRIANGLES_128_BYTES_NV = DisplacementMicromapFormatNV 3

{-# COMPLETE
  DISPLACEMENT_MICROMAP_FORMAT_64_TRIANGLES_64_BYTES_NV
  , DISPLACEMENT_MICROMAP_FORMAT_256_TRIANGLES_128_BYTES_NV
  , DISPLACEMENT_MICROMAP_FORMAT_1024_TRIANGLES_128_BYTES_NV ::
    DisplacementMicromapFormatNV
  #-}

conNameDisplacementMicromapFormatNV :: String
conNameDisplacementMicromapFormatNV = "DisplacementMicromapFormatNV"

enumPrefixDisplacementMicromapFormatNV :: String
enumPrefixDisplacementMicromapFormatNV = "DISPLACEMENT_MICROMAP_FORMAT_"

showTableDisplacementMicromapFormatNV :: [(DisplacementMicromapFormatNV, String)]
showTableDisplacementMicromapFormatNV =
  [
    ( DISPLACEMENT_MICROMAP_FORMAT_64_TRIANGLES_64_BYTES_NV
    , "64_TRIANGLES_64_BYTES_NV"
    )
  ,
    ( DISPLACEMENT_MICROMAP_FORMAT_256_TRIANGLES_128_BYTES_NV
    , "256_TRIANGLES_128_BYTES_NV"
    )
  ,
    ( DISPLACEMENT_MICROMAP_FORMAT_1024_TRIANGLES_128_BYTES_NV
    , "1024_TRIANGLES_128_BYTES_NV"
    )
  ]

instance Show DisplacementMicromapFormatNV where
  showsPrec =
    enumShowsPrec
      enumPrefixDisplacementMicromapFormatNV
      showTableDisplacementMicromapFormatNV
      conNameDisplacementMicromapFormatNV
      (\(DisplacementMicromapFormatNV x) -> x)
      (showsPrec 11)

instance Read DisplacementMicromapFormatNV where
  readPrec =
    enumReadPrec
      enumPrefixDisplacementMicromapFormatNV
      showTableDisplacementMicromapFormatNV
      conNameDisplacementMicromapFormatNV
      DisplacementMicromapFormatNV

type NV_DISPLACEMENT_MICROMAP_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_DISPLACEMENT_MICROMAP_SPEC_VERSION"
pattern NV_DISPLACEMENT_MICROMAP_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DISPLACEMENT_MICROMAP_SPEC_VERSION = 1


type NV_DISPLACEMENT_MICROMAP_EXTENSION_NAME = "VK_NV_displacement_micromap"

-- No documentation found for TopLevel "VK_NV_DISPLACEMENT_MICROMAP_EXTENSION_NAME"
pattern NV_DISPLACEMENT_MICROMAP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DISPLACEMENT_MICROMAP_EXTENSION_NAME = "VK_NV_displacement_micromap"

