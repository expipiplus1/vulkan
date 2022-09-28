{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_shader_float_controls"
module Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls  ( PhysicalDeviceFloatControlsProperties(..)
                                                                 , StructureType(..)
                                                                 , ShaderFloatControlsIndependence(..)
                                                                 ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES))
import Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceFloatControlsProperties - Structure describing
-- properties supported by VK_KHR_shader_float_controls
--
-- = Description
--
-- If the 'PhysicalDeviceFloatControlsProperties' structure is included in
-- the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_float_controls VK_KHR_shader_float_controls>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core12.Enums.ShaderFloatControlsIndependence.ShaderFloatControlsIndependence',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFloatControlsProperties = PhysicalDeviceFloatControlsProperties
  { -- | #extension-features-denormBehaviorIndependence#
    -- @denormBehaviorIndependence@ is a
    -- 'Vulkan.Core12.Enums.ShaderFloatControlsIndependence.ShaderFloatControlsIndependence'
    -- value indicating whether, and how, denorm behavior can be set
    -- independently for different bit widths.
    denormBehaviorIndependence :: ShaderFloatControlsIndependence
  , -- | #extension-features-roundingModeIndependence# @roundingModeIndependence@
    -- is a
    -- 'Vulkan.Core12.Enums.ShaderFloatControlsIndependence.ShaderFloatControlsIndependence'
    -- value indicating whether, and how, rounding modes can be set
    -- independently for different bit widths.
    roundingModeIndependence :: ShaderFloatControlsIndependence
  , -- | #extension-limits-shaderSignedZeroInfNanPreserveFloat16#
    -- @shaderSignedZeroInfNanPreserveFloat16@ is a boolean value indicating
    -- whether sign of a zero, Nans and \(\pm\infty\) /can/ be preserved in
    -- 16-bit floating-point computations. It also indicates whether the
    -- @SignedZeroInfNanPreserve@ execution mode /can/ be used for 16-bit
    -- floating-point types.
    shaderSignedZeroInfNanPreserveFloat16 :: Bool
  , -- | #extension-limits-shaderSignedZeroInfNanPreserveFloat32#
    -- @shaderSignedZeroInfNanPreserveFloat32@ is a boolean value indicating
    -- whether sign of a zero, Nans and \(\pm\infty\) /can/ be preserved in
    -- 32-bit floating-point computations. It also indicates whether the
    -- @SignedZeroInfNanPreserve@ execution mode /can/ be used for 32-bit
    -- floating-point types.
    shaderSignedZeroInfNanPreserveFloat32 :: Bool
  , -- | #extension-limits-shaderSignedZeroInfNanPreserveFloat64#
    -- @shaderSignedZeroInfNanPreserveFloat64@ is a boolean value indicating
    -- whether sign of a zero, Nans and \(\pm\infty\) /can/ be preserved in
    -- 64-bit floating-point computations. It also indicates whether the
    -- @SignedZeroInfNanPreserve@ execution mode /can/ be used for 64-bit
    -- floating-point types.
    shaderSignedZeroInfNanPreserveFloat64 :: Bool
  , -- | #extension-limits-shaderDenormPreserveFloat16#
    -- @shaderDenormPreserveFloat16@ is a boolean value indicating whether
    -- denormals /can/ be preserved in 16-bit floating-point computations. It
    -- also indicates whether the @DenormPreserve@ execution mode /can/ be used
    -- for 16-bit floating-point types.
    shaderDenormPreserveFloat16 :: Bool
  , -- | #extension-limits-shaderDenormPreserveFloat32#
    -- @shaderDenormPreserveFloat32@ is a boolean value indicating whether
    -- denormals /can/ be preserved in 32-bit floating-point computations. It
    -- also indicates whether the @DenormPreserve@ execution mode /can/ be used
    -- for 32-bit floating-point types.
    shaderDenormPreserveFloat32 :: Bool
  , -- | #extension-limits-shaderDenormPreserveFloat64#
    -- @shaderDenormPreserveFloat64@ is a boolean value indicating whether
    -- denormals /can/ be preserved in 64-bit floating-point computations. It
    -- also indicates whether the @DenormPreserve@ execution mode /can/ be used
    -- for 64-bit floating-point types.
    shaderDenormPreserveFloat64 :: Bool
  , -- | #extension-limits-shaderDenormFlushToZeroFloat16#
    -- @shaderDenormFlushToZeroFloat16@ is a boolean value indicating whether
    -- denormals /can/ be flushed to zero in 16-bit floating-point
    -- computations. It also indicates whether the @DenormFlushToZero@
    -- execution mode /can/ be used for 16-bit floating-point types.
    shaderDenormFlushToZeroFloat16 :: Bool
  , -- | #extension-limits-shaderDenormFlushToZeroFloat32#
    -- @shaderDenormFlushToZeroFloat32@ is a boolean value indicating whether
    -- denormals /can/ be flushed to zero in 32-bit floating-point
    -- computations. It also indicates whether the @DenormFlushToZero@
    -- execution mode /can/ be used for 32-bit floating-point types.
    shaderDenormFlushToZeroFloat32 :: Bool
  , -- | #extension-limits-shaderDenormFlushToZeroFloat64#
    -- @shaderDenormFlushToZeroFloat64@ is a boolean value indicating whether
    -- denormals /can/ be flushed to zero in 64-bit floating-point
    -- computations. It also indicates whether the @DenormFlushToZero@
    -- execution mode /can/ be used for 64-bit floating-point types.
    shaderDenormFlushToZeroFloat64 :: Bool
  , -- | #extension-limits-shaderRoundingModeRTEFloat16#
    -- @shaderRoundingModeRTEFloat16@ is a boolean value indicating whether an
    -- implementation supports the round-to-nearest-even rounding mode for
    -- 16-bit floating-point arithmetic and conversion instructions. It also
    -- indicates whether the @RoundingModeRTE@ execution mode /can/ be used for
    -- 16-bit floating-point types.
    shaderRoundingModeRTEFloat16 :: Bool
  , -- | #extension-limits-shaderRoundingModeRTEFloat32#
    -- @shaderRoundingModeRTEFloat32@ is a boolean value indicating whether an
    -- implementation supports the round-to-nearest-even rounding mode for
    -- 32-bit floating-point arithmetic and conversion instructions. It also
    -- indicates whether the @RoundingModeRTE@ execution mode /can/ be used for
    -- 32-bit floating-point types.
    shaderRoundingModeRTEFloat32 :: Bool
  , -- | #extension-limits-shaderRoundingModeRTEFloat64#
    -- @shaderRoundingModeRTEFloat64@ is a boolean value indicating whether an
    -- implementation supports the round-to-nearest-even rounding mode for
    -- 64-bit floating-point arithmetic and conversion instructions. It also
    -- indicates whether the @RoundingModeRTE@ execution mode /can/ be used for
    -- 64-bit floating-point types.
    shaderRoundingModeRTEFloat64 :: Bool
  , -- | #extension-limits-shaderRoundingModeRTZFloat16#
    -- @shaderRoundingModeRTZFloat16@ is a boolean value indicating whether an
    -- implementation supports the round-towards-zero rounding mode for 16-bit
    -- floating-point arithmetic and conversion instructions. It also indicates
    -- whether the @RoundingModeRTZ@ execution mode /can/ be used for 16-bit
    -- floating-point types.
    shaderRoundingModeRTZFloat16 :: Bool
  , -- | #extension-limits-shaderRoundingModeRTZFloat32#
    -- @shaderRoundingModeRTZFloat32@ is a boolean value indicating whether an
    -- implementation supports the round-towards-zero rounding mode for 32-bit
    -- floating-point arithmetic and conversion instructions. It also indicates
    -- whether the @RoundingModeRTZ@ execution mode /can/ be used for 32-bit
    -- floating-point types.
    shaderRoundingModeRTZFloat32 :: Bool
  , -- | #extension-limits-shaderRoundingModeRTZFloat64#
    -- @shaderRoundingModeRTZFloat64@ is a boolean value indicating whether an
    -- implementation supports the round-towards-zero rounding mode for 64-bit
    -- floating-point arithmetic and conversion instructions. It also indicates
    -- whether the @RoundingModeRTZ@ execution mode /can/ be used for 64-bit
    -- floating-point types.
    shaderRoundingModeRTZFloat64 :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFloatControlsProperties)
#endif
deriving instance Show PhysicalDeviceFloatControlsProperties

instance ToCStruct PhysicalDeviceFloatControlsProperties where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFloatControlsProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderFloatControlsIndependence)) (denormBehaviorIndependence)
    poke ((p `plusPtr` 20 :: Ptr ShaderFloatControlsIndependence)) (roundingModeIndependence)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat16))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat32))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat64))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (shaderDenormPreserveFloat16))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (shaderDenormPreserveFloat32))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (shaderDenormPreserveFloat64))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (shaderDenormFlushToZeroFloat16))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (shaderDenormFlushToZeroFloat32))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (shaderDenormFlushToZeroFloat64))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTEFloat16))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTEFloat32))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTEFloat64))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTZFloat16))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTZFloat32))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTZFloat64))
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderFloatControlsIndependence)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ShaderFloatControlsIndependence)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFloatControlsProperties where
  peekCStruct p = do
    denormBehaviorIndependence <- peek @ShaderFloatControlsIndependence ((p `plusPtr` 16 :: Ptr ShaderFloatControlsIndependence))
    roundingModeIndependence <- peek @ShaderFloatControlsIndependence ((p `plusPtr` 20 :: Ptr ShaderFloatControlsIndependence))
    shaderSignedZeroInfNanPreserveFloat16 <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    shaderSignedZeroInfNanPreserveFloat32 <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    shaderSignedZeroInfNanPreserveFloat64 <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    shaderDenormPreserveFloat16 <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    shaderDenormPreserveFloat32 <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    shaderDenormPreserveFloat64 <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    shaderDenormFlushToZeroFloat16 <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    shaderDenormFlushToZeroFloat32 <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    shaderDenormFlushToZeroFloat64 <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    shaderRoundingModeRTEFloat16 <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    shaderRoundingModeRTEFloat32 <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    shaderRoundingModeRTEFloat64 <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    shaderRoundingModeRTZFloat16 <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    shaderRoundingModeRTZFloat32 <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    shaderRoundingModeRTZFloat64 <- peek @Bool32 ((p `plusPtr` 80 :: Ptr Bool32))
    pure $ PhysicalDeviceFloatControlsProperties
             denormBehaviorIndependence
             roundingModeIndependence
             (bool32ToBool shaderSignedZeroInfNanPreserveFloat16)
             (bool32ToBool shaderSignedZeroInfNanPreserveFloat32)
             (bool32ToBool shaderSignedZeroInfNanPreserveFloat64)
             (bool32ToBool shaderDenormPreserveFloat16)
             (bool32ToBool shaderDenormPreserveFloat32)
             (bool32ToBool shaderDenormPreserveFloat64)
             (bool32ToBool shaderDenormFlushToZeroFloat16)
             (bool32ToBool shaderDenormFlushToZeroFloat32)
             (bool32ToBool shaderDenormFlushToZeroFloat64)
             (bool32ToBool shaderRoundingModeRTEFloat16)
             (bool32ToBool shaderRoundingModeRTEFloat32)
             (bool32ToBool shaderRoundingModeRTEFloat64)
             (bool32ToBool shaderRoundingModeRTZFloat16)
             (bool32ToBool shaderRoundingModeRTZFloat32)
             (bool32ToBool shaderRoundingModeRTZFloat64)

instance Storable PhysicalDeviceFloatControlsProperties where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFloatControlsProperties where
  zero = PhysicalDeviceFloatControlsProperties
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
           zero

