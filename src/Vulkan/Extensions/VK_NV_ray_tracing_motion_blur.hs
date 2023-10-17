{-# language CPP #-}
-- | = Name
--
-- VK_NV_ray_tracing_motion_blur - device extension
--
-- == VK_NV_ray_tracing_motion_blur
--
-- [__Name String__]
--     @VK_NV_ray_tracing_motion_blur@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     328
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
--
-- [__Contact__]
--
--     -   Eric Werness
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-06-16
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_ray_tracing_motion_blur.html SPV_NV_ray_tracing_motion_blur>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_ray_tracing_motion_blur.txt GL_NV_ray_tracing_motion_blur>
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Ashwin Lele, NVIDIA
--
-- == Description
--
-- Ray tracing support in the API provides an efficient mechanism to
-- intersect rays against static geometry, but rendering algorithms often
-- want to support motion, which is more efficiently supported with
-- motion-specific algorithms. This extension adds a set of mechanisms to
-- support fast tracing of moving geometry:
--
-- -   A ray pipeline trace call which takes a time parameter
--
-- -   Flags to enable motion support in an acceleration structure
--
-- -   Support for time-varying vertex positions in a geometry
--
-- -   Motion instances to move existing instances over time
--
-- The motion represented here is parameterized across a normalized
-- timestep between 0.0 and 1.0. A motion trace using @OpTraceRayMotionNV@
-- provides a time within that normalized range to be used when
-- intersecting that ray with geometry. The geometry can be provided with
-- motion by a combination of adding a second vertex position for time of
-- 1.0 using 'AccelerationStructureGeometryMotionTrianglesDataNV' and
-- providing multiple transforms in the instance using
-- 'AccelerationStructureMotionInstanceNV'.
--
-- == New Structures
--
-- -   'AccelerationStructureMatrixMotionInstanceNV'
--
-- -   'AccelerationStructureMotionInstanceNV'
--
-- -   'AccelerationStructureSRTMotionInstanceNV'
--
-- -   'SRTDataNV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateInfoKHR':
--
--     -   'AccelerationStructureMotionInfoNV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR':
--
--     -   'AccelerationStructureGeometryMotionTrianglesDataNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingMotionBlurFeaturesNV'
--
-- == New Unions
--
-- -   'AccelerationStructureMotionInstanceDataNV'
--
-- == New Enums
--
-- -   'AccelerationStructureMotionInstanceTypeNV'
--
-- == New Bitmasks
--
-- -   'AccelerationStructureMotionInfoFlagsNV'
--
-- -   'AccelerationStructureMotionInstanceFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_RAY_TRACING_MOTION_BLUR_EXTENSION_NAME'
--
-- -   'NV_RAY_TRACING_MOTION_BLUR_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_CREATE_MOTION_BIT_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.BuildAccelerationStructureFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_MOTION_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_ALLOW_MOTION_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MOTION_TRIANGLES_DATA_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MOTION_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MOTION_BLUR_FEATURES_NV'
--
-- == Issues
--
-- (1) What size is VkAccelerationStructureMotionInstanceNV?
--
-- -   Added a note on the structure size and made the stride explicit in
--     the language.
--
-- (2) Allow arrayOfPointers for motion TLAS?
--
-- -   Yes, with a packed encoding to minimize the amount of data sent for
--     metadata.
--
-- == Version History
--
-- -   Revision 1, 2020-06-16 (Eric Werness, Ashwin Lele)
--
--     -   Initial external release
--
-- == See Also
--
-- 'AccelerationStructureGeometryMotionTrianglesDataNV',
-- 'AccelerationStructureMatrixMotionInstanceNV',
-- 'AccelerationStructureMotionInfoFlagsNV',
-- 'AccelerationStructureMotionInfoNV',
-- 'AccelerationStructureMotionInstanceDataNV',
-- 'AccelerationStructureMotionInstanceFlagsNV',
-- 'AccelerationStructureMotionInstanceNV',
-- 'AccelerationStructureMotionInstanceTypeNV',
-- 'AccelerationStructureSRTMotionInstanceNV',
-- 'PhysicalDeviceRayTracingMotionBlurFeaturesNV', 'SRTDataNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_ray_tracing_motion_blur  ( PhysicalDeviceRayTracingMotionBlurFeaturesNV(..)
                                                        , AccelerationStructureGeometryMotionTrianglesDataNV(..)
                                                        , AccelerationStructureMotionInfoNV(..)
                                                        , SRTDataNV(..)
                                                        , AccelerationStructureSRTMotionInstanceNV(..)
                                                        , AccelerationStructureMatrixMotionInstanceNV(..)
                                                        , AccelerationStructureMotionInstanceNV(..)
                                                        , AccelerationStructureMotionInstanceDataNV(..)
                                                        , AccelerationStructureMotionInfoFlagsNV(..)
                                                        , AccelerationStructureMotionInstanceFlagsNV(..)
                                                        , AccelerationStructureMotionInstanceTypeNV( ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_STATIC_NV
                                                                                                   , ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_MATRIX_MOTION_NV
                                                                                                   , ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_SRT_MOTION_NV
                                                                                                   , ..
                                                                                                   )
                                                        , NV_RAY_TRACING_MOTION_BLUR_SPEC_VERSION
                                                        , pattern NV_RAY_TRACING_MOTION_BLUR_SPEC_VERSION
                                                        , NV_RAY_TRACING_MOTION_BLUR_EXTENSION_NAME
                                                        , pattern NV_RAY_TRACING_MOTION_BLUR_EXTENSION_NAME
                                                        , TransformMatrixKHR(..)
                                                        , AccelerationStructureInstanceKHR(..)
                                                        , DeviceOrHostAddressConstKHR(..)
                                                        , GeometryInstanceFlagBitsKHR(..)
                                                        , GeometryInstanceFlagsKHR
                                                        , BuildAccelerationStructureFlagBitsKHR(..)
                                                        , BuildAccelerationStructureFlagsKHR
                                                        , AccelerationStructureCreateFlagBitsKHR(..)
                                                        , AccelerationStructureCreateFlagsKHR
                                                        ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.Bits (shiftL)
import Data.Bits (shiftR)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Exception.Base (bracket)
import Data.Bits ((.&.))
import Data.Bits ((.|.))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureInstanceKHR)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressConstKHR)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.VK_KHR_acceleration_structure (TransformMatrixKHR)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MOTION_TRIANGLES_DATA_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MOTION_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MOTION_BLUR_FEATURES_NV))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureCreateFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureInstanceKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressConstKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (TransformMatrixKHR(..))
-- | VkPhysicalDeviceRayTracingMotionBlurFeaturesNV - Structure describing
-- the ray tracing motion blur features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceRayTracingMotionBlurFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceRayTracingMotionBlurFeaturesNV' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur VK_NV_ray_tracing_motion_blur>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingMotionBlurFeaturesNV = PhysicalDeviceRayTracingMotionBlurFeaturesNV
  { -- | #features-rayTracingMotionBlur# @rayTracingMotionBlur@ indicates whether
    -- the implementation supports the motion blur feature.
    rayTracingMotionBlur :: Bool
  , -- | #features-rayTracingMotionBlurPipelineTraceRaysIndirect#
    -- @rayTracingMotionBlurPipelineTraceRaysIndirect@ indicates whether the
    -- implementation supports indirect ray tracing commands with the motion
    -- blur feature enabled.
    rayTracingMotionBlurPipelineTraceRaysIndirect :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingMotionBlurFeaturesNV)
#endif
deriving instance Show PhysicalDeviceRayTracingMotionBlurFeaturesNV

instance ToCStruct PhysicalDeviceRayTracingMotionBlurFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingMotionBlurFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MOTION_BLUR_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rayTracingMotionBlur))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (rayTracingMotionBlurPipelineTraceRaysIndirect))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MOTION_BLUR_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRayTracingMotionBlurFeaturesNV where
  peekCStruct p = do
    rayTracingMotionBlur <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    rayTracingMotionBlurPipelineTraceRaysIndirect <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceRayTracingMotionBlurFeaturesNV
             (bool32ToBool rayTracingMotionBlur)
             (bool32ToBool rayTracingMotionBlurPipelineTraceRaysIndirect)

instance Storable PhysicalDeviceRayTracingMotionBlurFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingMotionBlurFeaturesNV where
  zero = PhysicalDeviceRayTracingMotionBlurFeaturesNV
           zero
           zero


-- | VkAccelerationStructureGeometryMotionTrianglesDataNV - Structure
-- specifying vertex motion in a bottom-level acceleration structure
--
-- = Description
--
-- If 'AccelerationStructureGeometryMotionTrianglesDataNV' is included in
-- the @pNext@ chain of a
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR'
-- structure, the basic vertex positions are used for the position of the
-- triangles in the geometry at time 0.0 and the @vertexData@ in
-- 'AccelerationStructureGeometryMotionTrianglesDataNV' is used for the
-- vertex positions at time 1.0, with positions linearly interpolated at
-- intermediate times.
--
-- Indexing for 'AccelerationStructureGeometryMotionTrianglesDataNV'
-- @vertexData@ is equivalent to the basic vertex position data.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur VK_NV_ray_tracing_motion_blur>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureGeometryMotionTrianglesDataNV = AccelerationStructureGeometryMotionTrianglesDataNV
  { -- | @vertexData@ is a pointer to vertex data for this geometry at time 1.0
    vertexData :: DeviceOrHostAddressConstKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureGeometryMotionTrianglesDataNV)
#endif
deriving instance Show AccelerationStructureGeometryMotionTrianglesDataNV

instance ToCStruct AccelerationStructureGeometryMotionTrianglesDataNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryMotionTrianglesDataNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MOTION_TRIANGLES_DATA_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (vertexData) . ($ ())
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MOTION_TRIANGLES_DATA_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ f

instance Zero AccelerationStructureGeometryMotionTrianglesDataNV where
  zero = AccelerationStructureGeometryMotionTrianglesDataNV
           zero


-- | VkAccelerationStructureMotionInfoNV - Structure specifying the
-- parameters of a newly created acceleration structure object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur VK_NV_ray_tracing_motion_blur>,
-- 'AccelerationStructureMotionInfoFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureMotionInfoNV = AccelerationStructureMotionInfoNV
  { -- | @maxInstances@ is the maximum number of instances that /may/ be used in
    -- the motion top-level acceleration structure.
    maxInstances :: Word32
  , -- | @flags@ is 0 and reserved for future use.
    --
    -- #VUID-VkAccelerationStructureMotionInfoNV-flags-zerobitmask# @flags@
    -- /must/ be @0@
    flags :: AccelerationStructureMotionInfoFlagsNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureMotionInfoNV)
#endif
deriving instance Show AccelerationStructureMotionInfoNV

instance ToCStruct AccelerationStructureMotionInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureMotionInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MOTION_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxInstances)
    poke ((p `plusPtr` 20 :: Ptr AccelerationStructureMotionInfoFlagsNV)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MOTION_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct AccelerationStructureMotionInfoNV where
  peekCStruct p = do
    maxInstances <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    flags <- peek @AccelerationStructureMotionInfoFlagsNV ((p `plusPtr` 20 :: Ptr AccelerationStructureMotionInfoFlagsNV))
    pure $ AccelerationStructureMotionInfoNV
             maxInstances flags

instance Storable AccelerationStructureMotionInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureMotionInfoNV where
  zero = AccelerationStructureMotionInfoNV
           zero
           zero


-- | VkSRTDataNV - Structure specifying a transform in SRT decomposition
--
-- = Description
--
-- This transform decomposition consists of three elements. The first is a
-- matrix S, consisting of a scale, shear, and translation, usually used to
-- define the pivot point of the following rotation. This matrix is
-- constructed from the parameters above by:
--
-- \[S =
-- \left(
--     \begin{matrix}
--         sx & a  & b  & pvx \\
--         0  & sy & c  & pvy \\
--         0  & 0  & sz & pvz
--     \end{matrix}
-- \right)\]
--
-- The rotation quaternion is defined as:
--
-- -   @R@ = [ @qx@, @qy@, @qz@, @qw@ ]
--
-- This is a rotation around a conceptual normalized axis [ ax, ay, az ] of
-- amount @theta@ such that:
--
-- -   [ @qx@, @qy@, @qz@ ] = sin(@theta@\/2) × [ @ax@, @ay@, @az@ ]
--
-- and
--
-- -   @qw@ = cos(@theta@\/2)
--
-- Finally, the transform has a translation T constructed from the
-- parameters above by:
--
-- \[T =
-- \left(
--     \begin{matrix}
--         1 & 0 & 0 & tx \\
--         0 & 1 & 0 & ty \\
--         0 & 0 & 1 & tz
--     \end{matrix}
-- \right)\]
--
-- The effective derived transform is then given by
--
-- -   @T@ × @R@ × @S@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur VK_NV_ray_tracing_motion_blur>,
-- 'AccelerationStructureSRTMotionInstanceNV'
data SRTDataNV = SRTDataNV
  { -- | @sx@ is the x component of the scale of the transform
    sx :: Float
  , -- | @a@ is one component of the shear for the transform
    a :: Float
  , -- | @b@ is one component of the shear for the transform
    b :: Float
  , -- | @pvx@ is the x component of the pivot point of the transform
    pvx :: Float
  , -- | @sy@ is the y component of the scale of the transform
    sy :: Float
  , -- | @c@ is one component of the shear for the transform
    c :: Float
  , -- | @pvy@ is the y component of the pivot point of the transform
    pvy :: Float
  , -- | @sz@ is the z component of the scale of the transform
    sz :: Float
  , -- | @pvz@ is the z component of the pivot point of the transform
    pvz :: Float
  , -- | @qx@ is the x component of the rotation quaternion
    qx :: Float
  , -- | @qy@ is the y component of the rotation quaternion
    qy :: Float
  , -- | @qz@ is the z component of the rotation quaternion
    qz :: Float
  , -- | @qw@ is the w component of the rotation quaternion
    qw :: Float
  , -- | @tx@ is the x component of the post-rotation translation
    tx :: Float
  , -- | @ty@ is the y component of the post-rotation translation
    ty :: Float
  , -- | @tz@ is the z component of the post-rotation translation
    tz :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SRTDataNV)
#endif
deriving instance Show SRTDataNV

instance ToCStruct SRTDataNV where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SRTDataNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (sx))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (a))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (b))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (pvx))
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (sy))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (c))
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (pvy))
    poke ((p `plusPtr` 28 :: Ptr CFloat)) (CFloat (sz))
    poke ((p `plusPtr` 32 :: Ptr CFloat)) (CFloat (pvz))
    poke ((p `plusPtr` 36 :: Ptr CFloat)) (CFloat (qx))
    poke ((p `plusPtr` 40 :: Ptr CFloat)) (CFloat (qy))
    poke ((p `plusPtr` 44 :: Ptr CFloat)) (CFloat (qz))
    poke ((p `plusPtr` 48 :: Ptr CFloat)) (CFloat (qw))
    poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (tx))
    poke ((p `plusPtr` 56 :: Ptr CFloat)) (CFloat (ty))
    poke ((p `plusPtr` 60 :: Ptr CFloat)) (CFloat (tz))
    f
  cStructSize = 64
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 28 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 32 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 36 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 40 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 44 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 48 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 56 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 60 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct SRTDataNV where
  peekCStruct p = do
    sx <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    a <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    b <- peek @CFloat ((p `plusPtr` 8 :: Ptr CFloat))
    pvx <- peek @CFloat ((p `plusPtr` 12 :: Ptr CFloat))
    sy <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    c <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    pvy <- peek @CFloat ((p `plusPtr` 24 :: Ptr CFloat))
    sz <- peek @CFloat ((p `plusPtr` 28 :: Ptr CFloat))
    pvz <- peek @CFloat ((p `plusPtr` 32 :: Ptr CFloat))
    qx <- peek @CFloat ((p `plusPtr` 36 :: Ptr CFloat))
    qy <- peek @CFloat ((p `plusPtr` 40 :: Ptr CFloat))
    qz <- peek @CFloat ((p `plusPtr` 44 :: Ptr CFloat))
    qw <- peek @CFloat ((p `plusPtr` 48 :: Ptr CFloat))
    tx <- peek @CFloat ((p `plusPtr` 52 :: Ptr CFloat))
    ty <- peek @CFloat ((p `plusPtr` 56 :: Ptr CFloat))
    tz <- peek @CFloat ((p `plusPtr` 60 :: Ptr CFloat))
    pure $ SRTDataNV
             (coerce @CFloat @Float sx)
             (coerce @CFloat @Float a)
             (coerce @CFloat @Float b)
             (coerce @CFloat @Float pvx)
             (coerce @CFloat @Float sy)
             (coerce @CFloat @Float c)
             (coerce @CFloat @Float pvy)
             (coerce @CFloat @Float sz)
             (coerce @CFloat @Float pvz)
             (coerce @CFloat @Float qx)
             (coerce @CFloat @Float qy)
             (coerce @CFloat @Float qz)
             (coerce @CFloat @Float qw)
             (coerce @CFloat @Float tx)
             (coerce @CFloat @Float ty)
             (coerce @CFloat @Float tz)

instance Storable SRTDataNV where
  sizeOf ~_ = 64
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SRTDataNV where
  zero = SRTDataNV
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
           zero
           zero
           zero


-- | VkAccelerationStructureSRTMotionInstanceNV - Structure specifying a
-- single acceleration structure SRT motion instance for building into an
-- acceleration structure geometry
--
-- = Description
--
-- The C language specification does not define the ordering of bit-fields,
-- but in practice, this struct produces the correct layout with existing
-- compilers. The intended bit pattern is for the following:
--
-- If a compiler produces code that diverges from that pattern,
-- applications /must/ employ another method to set values according to the
-- correct bit pattern.
--
-- The transform for a SRT motion instance at a point in time is derived
-- from component-wise linear interpolation of the two SRT transforms. That
-- is, for a @time@ in [0,1] the resulting transform is
--
-- -   @transformT0@ × (1 - @time@) + @transformT1@ × @time@
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur VK_NV_ray_tracing_motion_blur>,
-- 'AccelerationStructureMotionInstanceDataNV',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryInstanceFlagsKHR',
-- 'SRTDataNV'
data AccelerationStructureSRTMotionInstanceNV = AccelerationStructureSRTMotionInstanceNV
  { -- | @transformT0@ is a 'SRTDataNV' structure describing a transformation to
    -- be applied to the acceleration structure at time 0.
    transformT0 :: SRTDataNV
  , -- | @transformT1@ is a 'SRTDataNV' structure describing a transformation to
    -- be applied to the acceleration structure at time 1.
    transformT1 :: SRTDataNV
  , -- | @instanceCustomIndex@ is a 24-bit user-specified index value accessible
    -- to ray shaders in the @InstanceCustomIndexKHR@ built-in.
    --
    -- @instanceCustomIndex@ and @mask@ occupy the same memory as if a single
    -- @uint32_t@ was specified in their place
    --
    -- -   @instanceCustomIndex@ occupies the 24 least significant bits of that
    --     memory
    --
    -- -   @mask@ occupies the 8 most significant bits of that memory
    instanceCustomIndex :: Word32
  , -- | @mask@ is an 8-bit visibility mask for the geometry. The instance /may/
    -- only be hit if @Cull Mask & instance.mask != 0@
    mask :: Word32
  , -- | @instanceShaderBindingTableRecordOffset@ is a 24-bit offset used in
    -- calculating the hit shader binding table index.
    --
    -- @instanceShaderBindingTableRecordOffset@ and @flags@ occupy the same
    -- memory as if a single @uint32_t@ was specified in their place
    --
    -- -   @instanceShaderBindingTableRecordOffset@ occupies the 24 least
    --     significant bits of that memory
    --
    -- -   @flags@ occupies the 8 most significant bits of that memory
    instanceShaderBindingTableRecordOffset :: Word32
  , -- | @flags@ is an 8-bit mask of
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryInstanceFlagBitsKHR'
    -- values to apply to this instance.
    --
    -- #VUID-VkAccelerationStructureSRTMotionInstanceNV-flags-parameter#
    -- @flags@ /must/ be a valid combination of
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryInstanceFlagBitsKHR'
    -- values
    flags :: GeometryInstanceFlagsKHR
  , -- | @accelerationStructureReference@ is either:
    --
    -- -   a device address containing the value obtained from
    --     'Vulkan.Extensions.VK_KHR_acceleration_structure.getAccelerationStructureDeviceAddressKHR'
    --     or
    --     'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
    --     (used by device operations which reference acceleration structures)
    --     or,
    --
    -- -   a 'Vulkan.Extensions.Handles.AccelerationStructureKHR' object (used
    --     by host operations which reference acceleration structures).
    accelerationStructureReference :: Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureSRTMotionInstanceNV)
#endif
deriving instance Show AccelerationStructureSRTMotionInstanceNV

instance ToCStruct AccelerationStructureSRTMotionInstanceNV where
  withCStruct x f = allocaBytes 144 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureSRTMotionInstanceNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr SRTDataNV)) (transformT0)
    poke ((p `plusPtr` 64 :: Ptr SRTDataNV)) (transformT1)
    poke ((p `plusPtr` 128 :: Ptr Word32)) (((coerce @_ @Word32 (mask)) `shiftL` 24) .|. (instanceCustomIndex))
    poke ((p `plusPtr` 132 :: Ptr Word32)) (((coerce @_ @Word32 (flags)) `shiftL` 24) .|. (instanceShaderBindingTableRecordOffset))
    poke ((p `plusPtr` 136 :: Ptr Word64)) (accelerationStructureReference)
    f
  cStructSize = 144
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr SRTDataNV)) (zero)
    poke ((p `plusPtr` 64 :: Ptr SRTDataNV)) (zero)
    poke ((p `plusPtr` 136 :: Ptr Word64)) (zero)
    f

instance FromCStruct AccelerationStructureSRTMotionInstanceNV where
  peekCStruct p = do
    transformT0 <- peekCStruct @SRTDataNV ((p `plusPtr` 0 :: Ptr SRTDataNV))
    transformT1 <- peekCStruct @SRTDataNV ((p `plusPtr` 64 :: Ptr SRTDataNV))
    instanceCustomIndex <- peek @Word32 ((p `plusPtr` 128 :: Ptr Word32))
    let instanceCustomIndex' = ((instanceCustomIndex .&. coerce @Word32 0xffffff))
    mask <- peek @Word32 ((p `plusPtr` 128 :: Ptr Word32))
    let mask' = ((((mask `shiftR` 24)) .&. coerce @Word32 0xff))
    instanceShaderBindingTableRecordOffset <- peek @Word32 ((p `plusPtr` 132 :: Ptr Word32))
    let instanceShaderBindingTableRecordOffset' = ((instanceShaderBindingTableRecordOffset .&. coerce @Word32 0xffffff))
    flags <- peek @GeometryInstanceFlagsKHR ((p `plusPtr` 132 :: Ptr GeometryInstanceFlagsKHR))
    let flags' = ((((flags `shiftR` 24)) .&. coerce @Word32 0xff))
    accelerationStructureReference <- peek @Word64 ((p `plusPtr` 136 :: Ptr Word64))
    pure $ AccelerationStructureSRTMotionInstanceNV
             transformT0
             transformT1
             instanceCustomIndex'
             mask'
             instanceShaderBindingTableRecordOffset'
             flags'
             accelerationStructureReference

instance Storable AccelerationStructureSRTMotionInstanceNV where
  sizeOf ~_ = 144
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureSRTMotionInstanceNV where
  zero = AccelerationStructureSRTMotionInstanceNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureMatrixMotionInstanceNV - Structure specifying a
-- single acceleration structure matrix motion instance for building into
-- an acceleration structure geometry
--
-- = Description
--
-- The C language specification does not define the ordering of bit-fields,
-- but in practice, this struct produces the correct layout with existing
-- compilers. The intended bit pattern is for the following:
--
-- If a compiler produces code that diverges from that pattern,
-- applications /must/ employ another method to set values according to the
-- correct bit pattern.
--
-- The transform for a matrix motion instance at a point in time is derived
-- by component-wise linear interpolation of the two transforms. That is,
-- for a @time@ in [0,1] the resulting transform is
--
-- -   @transformT0@ × (1 - @time@) + @transformT1@ × @time@
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur VK_NV_ray_tracing_motion_blur>,
-- 'AccelerationStructureMotionInstanceDataNV',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryInstanceFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.TransformMatrixKHR'
data AccelerationStructureMatrixMotionInstanceNV = AccelerationStructureMatrixMotionInstanceNV
  { -- | @transformT0@ is a
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.TransformMatrixKHR'
    -- structure describing a transformation to be applied to the acceleration
    -- structure at time 0.
    transformT0 :: TransformMatrixKHR
  , -- | @transformT1@ is a
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.TransformMatrixKHR'
    -- structure describing a transformation to be applied to the acceleration
    -- structure at time 1.
    transformT1 :: TransformMatrixKHR
  , -- | @instanceCustomIndex@ is a 24-bit user-specified index value accessible
    -- to ray shaders in the @InstanceCustomIndexKHR@ built-in.
    --
    -- @instanceCustomIndex@ and @mask@ occupy the same memory as if a single
    -- @uint32_t@ was specified in their place
    --
    -- -   @instanceCustomIndex@ occupies the 24 least significant bits of that
    --     memory
    --
    -- -   @mask@ occupies the 8 most significant bits of that memory
    instanceCustomIndex :: Word32
  , -- | @mask@ is an 8-bit visibility mask for the geometry. The instance /may/
    -- only be hit if @Cull Mask & instance.mask != 0@
    mask :: Word32
  , -- | @instanceShaderBindingTableRecordOffset@ is a 24-bit offset used in
    -- calculating the hit shader binding table index.
    --
    -- @instanceShaderBindingTableRecordOffset@ and @flags@ occupy the same
    -- memory as if a single @uint32_t@ was specified in their place
    --
    -- -   @instanceShaderBindingTableRecordOffset@ occupies the 24 least
    --     significant bits of that memory
    --
    -- -   @flags@ occupies the 8 most significant bits of that memory
    instanceShaderBindingTableRecordOffset :: Word32
  , -- | @flags@ is an 8-bit mask of
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryInstanceFlagBitsKHR'
    -- values to apply to this instance.
    --
    -- #VUID-VkAccelerationStructureMatrixMotionInstanceNV-flags-parameter#
    -- @flags@ /must/ be a valid combination of
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryInstanceFlagBitsKHR'
    -- values
    flags :: GeometryInstanceFlagsKHR
  , -- | @accelerationStructureReference@ is either:
    --
    -- -   a device address containing the value obtained from
    --     'Vulkan.Extensions.VK_KHR_acceleration_structure.getAccelerationStructureDeviceAddressKHR'
    --     or
    --     'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
    --     (used by device operations which reference acceleration structures)
    --     or,
    --
    -- -   a 'Vulkan.Extensions.Handles.AccelerationStructureKHR' object (used
    --     by host operations which reference acceleration structures).
    accelerationStructureReference :: Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureMatrixMotionInstanceNV)
#endif
deriving instance Show AccelerationStructureMatrixMotionInstanceNV

instance ToCStruct AccelerationStructureMatrixMotionInstanceNV where
  withCStruct x f = allocaBytes 112 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureMatrixMotionInstanceNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr TransformMatrixKHR)) (transformT0)
    poke ((p `plusPtr` 48 :: Ptr TransformMatrixKHR)) (transformT1)
    poke ((p `plusPtr` 96 :: Ptr Word32)) (((coerce @_ @Word32 (mask)) `shiftL` 24) .|. (instanceCustomIndex))
    poke ((p `plusPtr` 100 :: Ptr Word32)) (((coerce @_ @Word32 (flags)) `shiftL` 24) .|. (instanceShaderBindingTableRecordOffset))
    poke ((p `plusPtr` 104 :: Ptr Word64)) (accelerationStructureReference)
    f
  cStructSize = 112
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr TransformMatrixKHR)) (zero)
    poke ((p `plusPtr` 48 :: Ptr TransformMatrixKHR)) (zero)
    poke ((p `plusPtr` 104 :: Ptr Word64)) (zero)
    f

instance FromCStruct AccelerationStructureMatrixMotionInstanceNV where
  peekCStruct p = do
    transformT0 <- peekCStruct @TransformMatrixKHR ((p `plusPtr` 0 :: Ptr TransformMatrixKHR))
    transformT1 <- peekCStruct @TransformMatrixKHR ((p `plusPtr` 48 :: Ptr TransformMatrixKHR))
    instanceCustomIndex <- peek @Word32 ((p `plusPtr` 96 :: Ptr Word32))
    let instanceCustomIndex' = ((instanceCustomIndex .&. coerce @Word32 0xffffff))
    mask <- peek @Word32 ((p `plusPtr` 96 :: Ptr Word32))
    let mask' = ((((mask `shiftR` 24)) .&. coerce @Word32 0xff))
    instanceShaderBindingTableRecordOffset <- peek @Word32 ((p `plusPtr` 100 :: Ptr Word32))
    let instanceShaderBindingTableRecordOffset' = ((instanceShaderBindingTableRecordOffset .&. coerce @Word32 0xffffff))
    flags <- peek @GeometryInstanceFlagsKHR ((p `plusPtr` 100 :: Ptr GeometryInstanceFlagsKHR))
    let flags' = ((((flags `shiftR` 24)) .&. coerce @Word32 0xff))
    accelerationStructureReference <- peek @Word64 ((p `plusPtr` 104 :: Ptr Word64))
    pure $ AccelerationStructureMatrixMotionInstanceNV
             transformT0
             transformT1
             instanceCustomIndex'
             mask'
             instanceShaderBindingTableRecordOffset'
             flags'
             accelerationStructureReference

instance Storable AccelerationStructureMatrixMotionInstanceNV where
  sizeOf ~_ = 112
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureMatrixMotionInstanceNV where
  zero = AccelerationStructureMatrixMotionInstanceNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureMotionInstanceNV - Structure specifying a single
-- acceleration structure motion instance for building into an acceleration
-- structure geometry
--
-- = Description
--
-- Note
--
-- If writing this other than with a standard C compiler, note that the
-- final structure should be 152 bytes in size.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureMotionInstanceNV-type-parameter# @type@
--     /must/ be a valid 'AccelerationStructureMotionInstanceTypeNV' value
--
-- -   #VUID-VkAccelerationStructureMotionInstanceNV-flags-zerobitmask#
--     @flags@ /must/ be @0@
--
-- -   #VUID-VkAccelerationStructureMotionInstanceNV-staticInstance-parameter#
--     If @type@ is
--     'ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_STATIC_NV', the
--     @staticInstance@ member of @data@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureInstanceKHR'
--     structure
--
-- -   #VUID-VkAccelerationStructureMotionInstanceNV-matrixMotionInstance-parameter#
--     If @type@ is
--     'ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_MATRIX_MOTION_NV', the
--     @matrixMotionInstance@ member of @data@ /must/ be a valid
--     'AccelerationStructureMatrixMotionInstanceNV' structure
--
-- -   #VUID-VkAccelerationStructureMotionInstanceNV-srtMotionInstance-parameter#
--     If @type@ is
--     'ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_SRT_MOTION_NV', the
--     @srtMotionInstance@ member of @data@ /must/ be a valid
--     'AccelerationStructureSRTMotionInstanceNV' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur VK_NV_ray_tracing_motion_blur>,
-- 'AccelerationStructureMotionInstanceDataNV',
-- 'AccelerationStructureMotionInstanceFlagsNV',
-- 'AccelerationStructureMotionInstanceTypeNV'
data AccelerationStructureMotionInstanceNV = AccelerationStructureMotionInstanceNV
  { -- | @type@ is a 'AccelerationStructureMotionInstanceTypeNV' enumerant
    -- identifying which type of motion instance this is and which type of the
    -- union is valid.
    type' :: AccelerationStructureMotionInstanceTypeNV
  , -- | @flags@ is currently unused, but is required to keep natural alignment
    -- of @data@.
    flags :: AccelerationStructureMotionInstanceFlagsNV
  , -- | @data@ is a 'AccelerationStructureMotionInstanceDataNV' containing
    -- motion instance data for this instance.
    data' :: AccelerationStructureMotionInstanceDataNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureMotionInstanceNV)
#endif
deriving instance Show AccelerationStructureMotionInstanceNV

instance ToCStruct AccelerationStructureMotionInstanceNV where
  withCStruct x f = allocaBytes 152 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureMotionInstanceNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr AccelerationStructureMotionInstanceTypeNV)) (type')
    lift $ poke ((p `plusPtr` 4 :: Ptr AccelerationStructureMotionInstanceFlagsNV)) (flags)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr AccelerationStructureMotionInstanceDataNV)) (data') . ($ ())
    lift $ f
  cStructSize = 152
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr AccelerationStructureMotionInstanceTypeNV)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr AccelerationStructureMotionInstanceDataNV)) (zero) . ($ ())
    lift $ f

instance Zero AccelerationStructureMotionInstanceNV where
  zero = AccelerationStructureMotionInstanceNV
           zero
           zero
           zero


data AccelerationStructureMotionInstanceDataNV
  = StaticInstance AccelerationStructureInstanceKHR
  | MatrixMotionInstance AccelerationStructureMatrixMotionInstanceNV
  | SrtMotionInstance AccelerationStructureSRTMotionInstanceNV
  deriving (Show)

instance ToCStruct AccelerationStructureMotionInstanceDataNV where
  withCStruct x f = allocaBytes 144 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr AccelerationStructureMotionInstanceDataNV -> AccelerationStructureMotionInstanceDataNV -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    StaticInstance v -> lift $ poke (castPtr @_ @AccelerationStructureInstanceKHR p) (v)
    MatrixMotionInstance v -> lift $ poke (castPtr @_ @AccelerationStructureMatrixMotionInstanceNV p) (v)
    SrtMotionInstance v -> lift $ poke (castPtr @_ @AccelerationStructureSRTMotionInstanceNV p) (v)
  pokeZeroCStruct :: Ptr AccelerationStructureMotionInstanceDataNV -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 144
  cStructAlignment = 8

instance Zero AccelerationStructureMotionInstanceDataNV where
  zero = SrtMotionInstance zero


-- | VkAccelerationStructureMotionInfoFlagsNV - Reserved for future use
--
-- = Description
--
-- 'AccelerationStructureMotionInfoFlagsNV' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur VK_NV_ray_tracing_motion_blur>,
-- 'AccelerationStructureMotionInfoNV'
newtype AccelerationStructureMotionInfoFlagsNV = AccelerationStructureMotionInfoFlagsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameAccelerationStructureMotionInfoFlagsNV :: String
conNameAccelerationStructureMotionInfoFlagsNV = "AccelerationStructureMotionInfoFlagsNV"

enumPrefixAccelerationStructureMotionInfoFlagsNV :: String
enumPrefixAccelerationStructureMotionInfoFlagsNV = ""

showTableAccelerationStructureMotionInfoFlagsNV :: [(AccelerationStructureMotionInfoFlagsNV, String)]
showTableAccelerationStructureMotionInfoFlagsNV = []

instance Show AccelerationStructureMotionInfoFlagsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixAccelerationStructureMotionInfoFlagsNV
      showTableAccelerationStructureMotionInfoFlagsNV
      conNameAccelerationStructureMotionInfoFlagsNV
      (\(AccelerationStructureMotionInfoFlagsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read AccelerationStructureMotionInfoFlagsNV where
  readPrec =
    enumReadPrec
      enumPrefixAccelerationStructureMotionInfoFlagsNV
      showTableAccelerationStructureMotionInfoFlagsNV
      conNameAccelerationStructureMotionInfoFlagsNV
      AccelerationStructureMotionInfoFlagsNV

-- | VkAccelerationStructureMotionInstanceFlagsNV - Reserved for future use
--
-- = Description
--
-- 'AccelerationStructureMotionInstanceFlagsNV' is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur VK_NV_ray_tracing_motion_blur>,
-- 'AccelerationStructureMotionInstanceNV'
newtype AccelerationStructureMotionInstanceFlagsNV = AccelerationStructureMotionInstanceFlagsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameAccelerationStructureMotionInstanceFlagsNV :: String
conNameAccelerationStructureMotionInstanceFlagsNV = "AccelerationStructureMotionInstanceFlagsNV"

enumPrefixAccelerationStructureMotionInstanceFlagsNV :: String
enumPrefixAccelerationStructureMotionInstanceFlagsNV = ""

showTableAccelerationStructureMotionInstanceFlagsNV :: [(AccelerationStructureMotionInstanceFlagsNV, String)]
showTableAccelerationStructureMotionInstanceFlagsNV = []

instance Show AccelerationStructureMotionInstanceFlagsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixAccelerationStructureMotionInstanceFlagsNV
      showTableAccelerationStructureMotionInstanceFlagsNV
      conNameAccelerationStructureMotionInstanceFlagsNV
      (\(AccelerationStructureMotionInstanceFlagsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read AccelerationStructureMotionInstanceFlagsNV where
  readPrec =
    enumReadPrec
      enumPrefixAccelerationStructureMotionInstanceFlagsNV
      showTableAccelerationStructureMotionInstanceFlagsNV
      conNameAccelerationStructureMotionInstanceFlagsNV
      AccelerationStructureMotionInstanceFlagsNV

-- | VkAccelerationStructureMotionInstanceTypeNV - Enum specifying a type of
-- acceleration structure motion instance data for building into an
-- acceleration structure geometry
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur VK_NV_ray_tracing_motion_blur>,
-- 'AccelerationStructureMotionInstanceNV'
newtype AccelerationStructureMotionInstanceTypeNV = AccelerationStructureMotionInstanceTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_STATIC_NV' specifies that
-- the instance is a static instance with no instance motion.
pattern ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_STATIC_NV = AccelerationStructureMotionInstanceTypeNV 0

-- | 'ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_MATRIX_MOTION_NV' specifies
-- that the instance is a motion instance with motion specified by
-- interpolation between two matrices.
pattern ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_MATRIX_MOTION_NV = AccelerationStructureMotionInstanceTypeNV 1

-- | 'ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_SRT_MOTION_NV' specifies
-- that the instance is a motion instance with motion specified by
-- interpolation in the SRT decomposition.
pattern ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_SRT_MOTION_NV = AccelerationStructureMotionInstanceTypeNV 2

{-# COMPLETE
  ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_STATIC_NV
  , ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_MATRIX_MOTION_NV
  , ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_SRT_MOTION_NV ::
    AccelerationStructureMotionInstanceTypeNV
  #-}

conNameAccelerationStructureMotionInstanceTypeNV :: String
conNameAccelerationStructureMotionInstanceTypeNV = "AccelerationStructureMotionInstanceTypeNV"

enumPrefixAccelerationStructureMotionInstanceTypeNV :: String
enumPrefixAccelerationStructureMotionInstanceTypeNV = "ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_"

showTableAccelerationStructureMotionInstanceTypeNV :: [(AccelerationStructureMotionInstanceTypeNV, String)]
showTableAccelerationStructureMotionInstanceTypeNV =
  [
    ( ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_STATIC_NV
    , "STATIC_NV"
    )
  ,
    ( ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_MATRIX_MOTION_NV
    , "MATRIX_MOTION_NV"
    )
  ,
    ( ACCELERATION_STRUCTURE_MOTION_INSTANCE_TYPE_SRT_MOTION_NV
    , "SRT_MOTION_NV"
    )
  ]

instance Show AccelerationStructureMotionInstanceTypeNV where
  showsPrec =
    enumShowsPrec
      enumPrefixAccelerationStructureMotionInstanceTypeNV
      showTableAccelerationStructureMotionInstanceTypeNV
      conNameAccelerationStructureMotionInstanceTypeNV
      (\(AccelerationStructureMotionInstanceTypeNV x) -> x)
      (showsPrec 11)

instance Read AccelerationStructureMotionInstanceTypeNV where
  readPrec =
    enumReadPrec
      enumPrefixAccelerationStructureMotionInstanceTypeNV
      showTableAccelerationStructureMotionInstanceTypeNV
      conNameAccelerationStructureMotionInstanceTypeNV
      AccelerationStructureMotionInstanceTypeNV

type NV_RAY_TRACING_MOTION_BLUR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_MOTION_BLUR_SPEC_VERSION"
pattern NV_RAY_TRACING_MOTION_BLUR_SPEC_VERSION :: forall a . Integral a => a
pattern NV_RAY_TRACING_MOTION_BLUR_SPEC_VERSION = 1


type NV_RAY_TRACING_MOTION_BLUR_EXTENSION_NAME = "VK_NV_ray_tracing_motion_blur"

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_MOTION_BLUR_EXTENSION_NAME"
pattern NV_RAY_TRACING_MOTION_BLUR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_RAY_TRACING_MOTION_BLUR_EXTENSION_NAME = "VK_NV_ray_tracing_motion_blur"

