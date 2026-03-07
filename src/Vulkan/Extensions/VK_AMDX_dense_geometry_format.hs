{-# language CPP #-}
-- | = Name
--
-- VK_AMDX_dense_geometry_format - device extension
--
-- = VK_AMDX_dense_geometry_format
--
-- [__Name String__]
--     @VK_AMDX_dense_geometry_format@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     479
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4 Vulkan Version 1.4>
--
--     -   __This is a /provisional/ extension and /must/ be used with
--         caution. See the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#boilerplate-provisional-header description>
--         of provisional header files for enablement and stability
--         details.__
--
-- [__Contact__]
--
--     -   Stu Smith
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMDX_dense_geometry_format] @stu-s%0A*Here describe the issue or question you have about the VK_AMDX_dense_geometry_format extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_AMDX_dense_geometry_format.adoc VK_AMDX_dense_geometry_format>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-07-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Stu Smith, AMD
--
--     -   Josh Barczak, AMD
--
--     -   Carsten Benthin, AMD
--
--     -   David McAllister, AMD
--
-- == Description
--
-- This extension adds the ability to build ray tracing acceleration
-- structures from pre-compressed @Dense Geometry Format@ geometry data.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryKHR':
--
--     -   'AccelerationStructureDenseGeometryFormatTrianglesDataAMDX'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDenseGeometryFormatFeaturesAMDX'
--
-- == New Enums
--
-- -   'CompressedTriangleFormatAMDX'
--
-- == New Enum Constants
--
-- -   'AMDX_DENSE_GEOMETRY_FORMAT_EXTENSION_NAME'
--
-- -   'AMDX_DENSE_GEOMETRY_FORMAT_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX'
--
-- -   'Vulkan.Core10.APIConstants.COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX'
--
-- -   Extending
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2':
--
--     -   'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_COMPRESSED_DATA_DGF1_BIT_AMDX'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryTypeKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_DENSE_GEOMETRY_FORMAT_TRIANGLES_AMDX'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DENSE_GEOMETRY_FORMAT_TRIANGLES_DATA_AMDX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DENSE_GEOMETRY_FORMAT_FEATURES_AMDX'
--
-- == Issues
--
-- None.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-07-10 (Stu Smith)
--
--     -   Initial revision.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_AMDX_dense_geometry_format Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMDX_dense_geometry_format  ( PhysicalDeviceDenseGeometryFormatFeaturesAMDX(..)
                                                        , AccelerationStructureDenseGeometryFormatTrianglesDataAMDX(..)
                                                        , CompressedTriangleFormatAMDX( COMPRESSED_TRIANGLE_FORMAT_DGF1_AMDX
                                                                                      , ..
                                                                                      )
                                                        , AMDX_DENSE_GEOMETRY_FORMAT_SPEC_VERSION
                                                        , pattern AMDX_DENSE_GEOMETRY_FORMAT_SPEC_VERSION
                                                        , AMDX_DENSE_GEOMETRY_FORMAT_EXTENSION_NAME
                                                        , pattern AMDX_DENSE_GEOMETRY_FORMAT_EXTENSION_NAME
                                                        , DeviceOrHostAddressConstKHR(..)
                                                        , GeometryTypeKHR(..)
                                                        , COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX
                                                        , pattern COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX
                                                        , COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX
                                                        , pattern COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX
                                                        ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (AccelerationStructureTrianglesOpacityMicromapEXT)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressConstKHR)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DENSE_GEOMETRY_FORMAT_TRIANGLES_DATA_AMDX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DENSE_GEOMETRY_FORMAT_FEATURES_AMDX))
import Vulkan.Core10.APIConstants (COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX)
import Vulkan.Core10.APIConstants (COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX)
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressConstKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryTypeKHR(..))
import Vulkan.Core10.APIConstants (pattern COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX)
import Vulkan.Core10.APIConstants (pattern COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX)
-- | VkPhysicalDeviceDenseGeometryFormatFeaturesAMDX - Structure describing
-- dense geometry format features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDenseGeometryFormatFeaturesAMDX' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceDenseGeometryFormatFeaturesAMDX', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_dense_geometry_format VK_AMDX_dense_geometry_format>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDenseGeometryFormatFeaturesAMDX = PhysicalDeviceDenseGeometryFormatFeaturesAMDX
  { -- | #features-denseGeometryFormat# @denseGeometryFormat@ specifies whether
    -- the implementation supports DGF1 compressed geometry data.
    denseGeometryFormat :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDenseGeometryFormatFeaturesAMDX)
#endif
deriving instance Show PhysicalDeviceDenseGeometryFormatFeaturesAMDX

instance ToCStruct PhysicalDeviceDenseGeometryFormatFeaturesAMDX where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDenseGeometryFormatFeaturesAMDX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DENSE_GEOMETRY_FORMAT_FEATURES_AMDX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (denseGeometryFormat))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DENSE_GEOMETRY_FORMAT_FEATURES_AMDX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDenseGeometryFormatFeaturesAMDX where
  peekCStruct p = do
    denseGeometryFormat <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDenseGeometryFormatFeaturesAMDX
             (bool32ToBool denseGeometryFormat)

instance Storable PhysicalDeviceDenseGeometryFormatFeaturesAMDX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDenseGeometryFormatFeaturesAMDX where
  zero = PhysicalDeviceDenseGeometryFormatFeaturesAMDX
           zero


-- | VkAccelerationStructureDenseGeometryFormatTrianglesDataAMDX - Structure
-- specifying acceleration structure DGF compressed triangle data
--
-- = Description
--
-- If @format@ is 'COMPRESSED_TRIANGLE_FORMAT_DGF1_AMDX', @numVertices@
-- specifies the sum of vertex counts across all blocks.
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureDenseGeometryFormatTrianglesDataAMDX-compressedData-10885#
--     The buffer from which @compressedData.deviceAddress@ is queried
--     /must/ have been created with the
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_COMPRESSED_DATA_DGF1_BIT_AMDX'
--     usage flag set
--
-- -   #VUID-VkAccelerationStructureDenseGeometryFormatTrianglesDataAMDX-denseGeometryFormat-10886#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-denseGeometryFormat ::denseGeometryFormat>
--     feature /must/ be enabled
--
-- -   #VUID-VkAccelerationStructureDenseGeometryFormatTrianglesDataAMDX-format-10887#
--     If @format@ is VK_COMPRESSED_TRIANGLE_FORMAT_DGF1_AMDX, then
--     @compressedData@ /must/ be aligned to
--     'Vulkan.Core10.APIConstants.COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX'
--     (128) bytes
--
-- -   #VUID-VkAccelerationStructureDenseGeometryFormatTrianglesDataAMDX-format-10888#
--     If @format@ is VK_COMPRESSED_TRIANGLE_FORMAT_DGF1_AMDX, then
--     @dataSize@ /must/ be a multiple of
--     'Vulkan.Core10.APIConstants.COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX'
--     (128) bytes
--
-- -   #VUID-VkAccelerationStructureDenseGeometryFormatTrianglesDataAMDX-pNext-10890#
--     @pNext@ /must/ be @NULL@ or a pointer to a valid
--     'Vulkan.Extensions.VK_EXT_opacity_micromap.AccelerationStructureTrianglesOpacityMicromapEXT'
--     structure
--
-- -   #VUID-VkAccelerationStructureDenseGeometryFormatTrianglesDataAMDX-pNext-10891#
--     If @pNext@ is a pointer to a valid
--     'Vulkan.Extensions.VK_EXT_opacity_micromap.AccelerationStructureTrianglesOpacityMicromapEXT'
--     structure, the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-micromap micromap>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureDenseGeometryFormatTrianglesDataAMDX-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DENSE_GEOMETRY_FORMAT_TRIANGLES_DATA_AMDX'
--
-- -   #VUID-VkAccelerationStructureDenseGeometryFormatTrianglesDataAMDX-compressedData-parameter#
--     @compressedData@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR'
--     union
--
-- -   #VUID-VkAccelerationStructureDenseGeometryFormatTrianglesDataAMDX-format-parameter#
--     @format@ /must/ be a valid 'CompressedTriangleFormatAMDX' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_dense_geometry_format VK_AMDX_dense_geometry_format>,
-- 'CompressedTriangleFormatAMDX',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureDenseGeometryFormatTrianglesDataAMDX (es :: [Type]) = AccelerationStructureDenseGeometryFormatTrianglesDataAMDX
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @compressedData@ specifies the base address of the compressed data.
    compressedData :: DeviceOrHostAddressConstKHR
  , -- | @dataSize@ specifies the size of the compressed data.
    dataSize :: DeviceSize
  , -- | @numTriangles@ specifies the total number of triangles encoded in the
    -- compressed data.
    numTriangles :: Word32
  , -- | @numVertices@ specifies the number of vertices in the compressed data.
    numVertices :: Word32
  , -- | @maxPrimitiveIndex@ specifies the maximum primitive index encoded in the
    -- compressed data.
    maxPrimitiveIndex :: Word32
  , -- | @maxGeometryIndex@ specifies the maximum geometry index encoded in the
    -- compressed data.
    maxGeometryIndex :: Word32
  , -- | @format@ specifies the 'CompressedTriangleFormatAMDX' format of the
    -- compressed data.
    format :: CompressedTriangleFormatAMDX
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureDenseGeometryFormatTrianglesDataAMDX (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (AccelerationStructureDenseGeometryFormatTrianglesDataAMDX es)

instance Extensible AccelerationStructureDenseGeometryFormatTrianglesDataAMDX where
  extensibleTypeName = "AccelerationStructureDenseGeometryFormatTrianglesDataAMDX"
  setNext AccelerationStructureDenseGeometryFormatTrianglesDataAMDX{..} next' = AccelerationStructureDenseGeometryFormatTrianglesDataAMDX{next = next', ..}
  getNext AccelerationStructureDenseGeometryFormatTrianglesDataAMDX{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends AccelerationStructureDenseGeometryFormatTrianglesDataAMDX e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @AccelerationStructureTrianglesOpacityMicromapEXT = Just f
    | otherwise = Nothing

instance ( Extendss AccelerationStructureDenseGeometryFormatTrianglesDataAMDX es
         , PokeChain es ) => ToCStruct (AccelerationStructureDenseGeometryFormatTrianglesDataAMDX es) where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureDenseGeometryFormatTrianglesDataAMDX{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DENSE_GEOMETRY_FORMAT_TRIANGLES_DATA_AMDX)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (compressedData) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (dataSize)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (numTriangles)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (numVertices)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (maxPrimitiveIndex)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (maxGeometryIndex)
    lift $ poke ((p `plusPtr` 48 :: Ptr CompressedTriangleFormatAMDX)) (format)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DENSE_GEOMETRY_FORMAT_TRIANGLES_DATA_AMDX)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr CompressedTriangleFormatAMDX)) (zero)
    lift $ f

instance es ~ '[] => Zero (AccelerationStructureDenseGeometryFormatTrianglesDataAMDX es) where
  zero = AccelerationStructureDenseGeometryFormatTrianglesDataAMDX
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkCompressedTriangleFormatAMDX - Available compressed triangle formats
--
-- = Description
--
-- -   'COMPRESSED_TRIANGLE_FORMAT_DGF1_AMDX' specifies that the compressed
--     triangle data is in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#dense-geometry-format Dense Geometry Format>,
--     version 1, consisting of an array of 128B DGF blocks.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_dense_geometry_format VK_AMDX_dense_geometry_format>,
-- 'AccelerationStructureDenseGeometryFormatTrianglesDataAMDX'
newtype CompressedTriangleFormatAMDX = CompressedTriangleFormatAMDX Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkCompressedTriangleFormatAMDX" "VK_COMPRESSED_TRIANGLE_FORMAT_DGF1_AMDX"
pattern COMPRESSED_TRIANGLE_FORMAT_DGF1_AMDX = CompressedTriangleFormatAMDX 0

{-# COMPLETE COMPRESSED_TRIANGLE_FORMAT_DGF1_AMDX :: CompressedTriangleFormatAMDX #-}

conNameCompressedTriangleFormatAMDX :: String
conNameCompressedTriangleFormatAMDX = "CompressedTriangleFormatAMDX"

enumPrefixCompressedTriangleFormatAMDX :: String
enumPrefixCompressedTriangleFormatAMDX = "COMPRESSED_TRIANGLE_FORMAT_DGF1_AMDX"

showTableCompressedTriangleFormatAMDX :: [(CompressedTriangleFormatAMDX, String)]
showTableCompressedTriangleFormatAMDX =
  [
    ( COMPRESSED_TRIANGLE_FORMAT_DGF1_AMDX
    , ""
    )
  ]

instance Show CompressedTriangleFormatAMDX where
  showsPrec =
    enumShowsPrec
      enumPrefixCompressedTriangleFormatAMDX
      showTableCompressedTriangleFormatAMDX
      conNameCompressedTriangleFormatAMDX
      (\(CompressedTriangleFormatAMDX x) -> x)
      (showsPrec 11)

instance Read CompressedTriangleFormatAMDX where
  readPrec =
    enumReadPrec
      enumPrefixCompressedTriangleFormatAMDX
      showTableCompressedTriangleFormatAMDX
      conNameCompressedTriangleFormatAMDX
      CompressedTriangleFormatAMDX

type AMDX_DENSE_GEOMETRY_FORMAT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMDX_DENSE_GEOMETRY_FORMAT_SPEC_VERSION"
pattern AMDX_DENSE_GEOMETRY_FORMAT_SPEC_VERSION :: forall a . Integral a => a
pattern AMDX_DENSE_GEOMETRY_FORMAT_SPEC_VERSION = 1


type AMDX_DENSE_GEOMETRY_FORMAT_EXTENSION_NAME = "VK_AMDX_dense_geometry_format"

-- No documentation found for TopLevel "VK_AMDX_DENSE_GEOMETRY_FORMAT_EXTENSION_NAME"
pattern AMDX_DENSE_GEOMETRY_FORMAT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMDX_DENSE_GEOMETRY_FORMAT_EXTENSION_NAME = "VK_AMDX_dense_geometry_format"

