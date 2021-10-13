{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_integer_dot_product - device extension
--
-- == VK_KHR_shader_integer_dot_product
--
-- [__Name String__]
--     @VK_KHR_shader_integer_dot_product@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     281
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_integer_dot_product] @kevinpetit%0A<<Here describe the issue or question you have about the VK_KHR_shader_integer_dot_product extension>> >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_integer_dot_product.asciidoc VK_KHR_shader_integer_dot_product>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-06-16
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_integer_dot_product.html SPV_KHR_integer_dot_product>.
--
--     -   This extension interacts with @VK_KHR_shader_float16_int8@.
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Jeff Bolz, NVidia
--
--     -   Spencer Fricke, Samsung
--
--     -   Jesse Hall, Google
--
--     -   John Kessenich, Google
--
--     -   Graeme Leese, Broadcom
--
--     -   Einar Hov, Arm Ltd.
--
--     -   Stuart Brady, Arm Ltd.
--
--     -   Pablo Cascon, Arm Ltd.
--
--     -   Tobias Hector, AMD
--
--     -   Jeff Leger, Qualcomm
--
--     -   Ruihao Zhang, Qualcomm
--
--     -   Pierre Boudier, NVidia
--
--     -   Jon Leech, The Khronos Group
--
--     -   Tom Olson, Arm Ltd.
--
-- == Description
--
-- This extension adds support for the integer dot product SPIR-V
-- instructions defined in SPV_KHR_integer_dot_product. These instructions
-- are particularly useful for neural network inference and training but
-- find uses in other general purpose compute applications as well.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderIntegerDotProductFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderIntegerDotProductPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME'
--
-- -   'KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DotProductInputAllKHR DotProductInputAllKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DotProductInput4x8BitKHR DotProductInput4x8BitKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DotProductInput4x8BitPackedKHR DotProductInput4x8BitPackedKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DotProductKHR DotProductKHR>
--
-- == Version History
--
-- -   Revision 1, 2021-06-16 (Kévin Petit)
--
--     -   Initial revision
--
-- = See Also
--
-- 'PhysicalDeviceShaderIntegerDotProductFeaturesKHR',
-- 'PhysicalDeviceShaderIntegerDotProductPropertiesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_integer_dot_product Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_integer_dot_product  ( PhysicalDeviceShaderIntegerDotProductFeaturesKHR(..)
                                                            , PhysicalDeviceShaderIntegerDotProductPropertiesKHR(..)
                                                            , KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION
                                                            , pattern KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION
                                                            , KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME
                                                            , pattern KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME
                                                            ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
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
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES_KHR))
-- | VkPhysicalDeviceShaderIntegerDotProductFeaturesKHR - Structure
-- describing integer dot product features that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShaderIntegerDotProductFeaturesKHR'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderIntegerDotProductFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderIntegerDotProductFeaturesKHR' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_integer_dot_product VK_KHR_shader_integer_dot_product>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderIntegerDotProductFeaturesKHR = PhysicalDeviceShaderIntegerDotProductFeaturesKHR
  { -- | #features-shaderIntegerDotProduct# @shaderIntegerDotProduct@ specifies
    -- whether shader modules /can/ declare the @DotProductInputAllKHR@,
    -- @DotProductInput4x8BitKHR@, @DotProductInput4x8BitPackedKHR@ and
    -- @DotProductKHR@ capabilities.
    shaderIntegerDotProduct :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderIntegerDotProductFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderIntegerDotProductFeaturesKHR

instance ToCStruct PhysicalDeviceShaderIntegerDotProductFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderIntegerDotProductFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderIntegerDotProduct))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderIntegerDotProductFeaturesKHR where
  peekCStruct p = do
    shaderIntegerDotProduct <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderIntegerDotProductFeaturesKHR
             (bool32ToBool shaderIntegerDotProduct)

instance Storable PhysicalDeviceShaderIntegerDotProductFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderIntegerDotProductFeaturesKHR where
  zero = PhysicalDeviceShaderIntegerDotProductFeaturesKHR
           zero


-- | VkPhysicalDeviceShaderIntegerDotProductPropertiesKHR - Structure
-- containing information about integer dot product support for a physical
-- device
--
-- = Description
--
-- If the 'PhysicalDeviceShaderIntegerDotProductPropertiesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- These are properties of the integer dot product acceleration information
-- of a physical device.
--
-- Note
--
-- A dot product operation is deemed accelerated if its implementation
-- provides a performance advantage over application-provided code composed
-- from elementary instructions and\/or other dot product instructions,
-- either because the implementation uses optimized machine code sequences
-- whose generation from application-provided code cannot be guaranteed or
-- because it uses hardware features that cannot otherwise be targeted from
-- application-provided code.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_integer_dot_product VK_KHR_shader_integer_dot_product>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderIntegerDotProductPropertiesKHR = PhysicalDeviceShaderIntegerDotProductPropertiesKHR
  { -- | @integerDotProduct8BitUnsignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 8-bit unsigned
    -- dot product operations using the @OpUDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct8BitUnsignedAccelerated :: Bool
  , -- | @integerDotProduct8BitSignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 8-bit signed
    -- dot product operations using the @OpSDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct8BitSignedAccelerated :: Bool
  , -- | @integerDotProduct8BitMixedSignednessAccelerated@ is a boolean that will
    -- be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 8-bit mixed
    -- signedness dot product operations using the @OpSUDotKHR@ SPIR-V
    -- instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct8BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProduct4x8BitPackedUnsignedAccelerated@ is a boolean that
    -- will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 8-bit
    -- unsigned dot product operations from operands packed into 32-bit
    -- integers using the @OpUDotKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct4x8BitPackedUnsignedAccelerated :: Bool
  , -- | @integerDotProduct4x8BitPackedSignedAccelerated@ is a boolean that will
    -- be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 8-bit signed
    -- dot product operations from operands packed into 32-bit integers using
    -- the @OpSDotKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct4x8BitPackedSignedAccelerated :: Bool
  , -- | @integerDotProduct4x8BitPackedMixedSignednessAccelerated@ is a boolean
    -- that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for
    -- 8-bit mixed signedness dot product operations from operands packed into
    -- 32-bit integers using the @OpSUDotKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct4x8BitPackedMixedSignednessAccelerated :: Bool
  , -- | @integerDotProduct16BitUnsignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 16-bit unsigned
    -- dot product operations using the @OpUDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct16BitUnsignedAccelerated :: Bool
  , -- | @integerDotProduct16BitSignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 16-bit signed
    -- dot product operations using the @OpSDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct16BitSignedAccelerated :: Bool
  , -- | @integerDotProduct16BitMixedSignednessAccelerated@ is a boolean that
    -- will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 16-bit
    -- mixed signedness dot product operations using the @OpSUDotKHR@ SPIR-V
    -- instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct16BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProduct32BitUnsignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 32-bit unsigned
    -- dot product operations using the @OpUDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct32BitUnsignedAccelerated :: Bool
  , -- | @integerDotProduct32BitSignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 32-bit signed
    -- dot product operations using the @OpSDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct32BitSignedAccelerated :: Bool
  , -- | @integerDotProduct32BitMixedSignednessAccelerated@ is a boolean that
    -- will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 32-bit
    -- mixed signedness dot product operations using the @OpSUDotKHR@ SPIR-V
    -- instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct32BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProduct64BitUnsignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 64-bit unsigned
    -- dot product operations using the @OpUDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct64BitUnsignedAccelerated :: Bool
  , -- | @integerDotProduct64BitSignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 64-bit signed
    -- dot product operations using the @OpSDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct64BitSignedAccelerated :: Bool
  , -- | @integerDotProduct64BitMixedSignednessAccelerated@ is a boolean that
    -- will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 64-bit
    -- mixed signedness dot product operations using the @OpSUDotKHR@ SPIR-V
    -- instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct64BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating8BitUnsignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 8-bit unsigned accumulating saturating dot product
    -- operations using the @OpUDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating8BitUnsignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating8BitSignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 8-bit signed accumulating saturating dot product operations
    -- using the @OpSDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating8BitSignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating8BitMixedSignednessAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 8-bit mixed signedness accumulating saturating dot product
    -- operations using the @OpSUDotAccSatKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating8BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating4x8BitPackedUnsignedAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 8-bit unsigned accumulating saturating dot product
    -- operations from operands packed into 32-bit integers using the
    -- @OpUDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating4x8BitPackedUnsignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating4x8BitPackedSignedAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 8-bit signed accumulating saturating dot product operations
    -- from operands packed into 32-bit integers using the @OpSDotAccSatKHR@
    -- SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating4x8BitPackedSignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating4x8BitPackedMixedSignednessAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 8-bit mixed signedness accumulating saturating dot product
    -- operations from operands packed into 32-bit integers using the
    -- @OpSUDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating4x8BitPackedMixedSignednessAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating16BitUnsignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 16-bit unsigned accumulating saturating dot product
    -- operations using the @OpUDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating16BitUnsignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating16BitSignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 16-bit signed accumulating saturating dot product operations
    -- using the @OpSDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating16BitSignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating16BitMixedSignednessAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 16-bit mixed signedness accumulating saturating dot product
    -- operations using the @OpSUDotAccSatKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating16BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating32BitUnsignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 32-bit unsigned accumulating saturating dot product
    -- operations using the @OpUDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating32BitUnsignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating32BitSignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 32-bit signed accumulating saturating dot product operations
    -- using the @OpSDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating32BitSignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating32BitMixedSignednessAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 32-bit mixed signedness accumulating saturating dot product
    -- operations using the @OpSUDotAccSatKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating32BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating64BitUnsignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 64-bit unsigned accumulating saturating dot product
    -- operations using the @OpUDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating64BitUnsignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating64BitSignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 64-bit signed accumulating saturating dot product operations
    -- using the @OpSDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating64BitSignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating64BitMixedSignednessAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 64-bit mixed signedness accumulating saturating dot product
    -- operations using the @OpSUDotAccSatKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating64BitMixedSignednessAccelerated :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderIntegerDotProductPropertiesKHR)
#endif
deriving instance Show PhysicalDeviceShaderIntegerDotProductPropertiesKHR

instance ToCStruct PhysicalDeviceShaderIntegerDotProductPropertiesKHR where
  withCStruct x f = allocaBytes 136 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderIntegerDotProductPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (integerDotProduct8BitUnsignedAccelerated))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (integerDotProduct8BitSignedAccelerated))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (integerDotProduct8BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (integerDotProduct4x8BitPackedUnsignedAccelerated))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (integerDotProduct4x8BitPackedSignedAccelerated))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (integerDotProduct4x8BitPackedMixedSignednessAccelerated))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (integerDotProduct16BitUnsignedAccelerated))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (integerDotProduct16BitSignedAccelerated))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (integerDotProduct16BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (integerDotProduct32BitUnsignedAccelerated))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (integerDotProduct32BitSignedAccelerated))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (integerDotProduct32BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (integerDotProduct64BitUnsignedAccelerated))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (integerDotProduct64BitSignedAccelerated))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (integerDotProduct64BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating8BitUnsignedAccelerated))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating8BitSignedAccelerated))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating8BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating4x8BitPackedUnsignedAccelerated))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating4x8BitPackedSignedAccelerated))
    poke ((p `plusPtr` 96 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating4x8BitPackedMixedSignednessAccelerated))
    poke ((p `plusPtr` 100 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating16BitUnsignedAccelerated))
    poke ((p `plusPtr` 104 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating16BitSignedAccelerated))
    poke ((p `plusPtr` 108 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating16BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 112 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating32BitUnsignedAccelerated))
    poke ((p `plusPtr` 116 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating32BitSignedAccelerated))
    poke ((p `plusPtr` 120 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating32BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 124 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating64BitUnsignedAccelerated))
    poke ((p `plusPtr` 128 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating64BitSignedAccelerated))
    poke ((p `plusPtr` 132 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating64BitMixedSignednessAccelerated))
    f
  cStructSize = 136
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
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
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 96 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 100 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 104 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 108 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 112 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 116 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 120 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 124 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 128 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 132 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderIntegerDotProductPropertiesKHR where
  peekCStruct p = do
    integerDotProduct8BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    integerDotProduct8BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    integerDotProduct8BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    integerDotProduct4x8BitPackedUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    integerDotProduct4x8BitPackedSignedAccelerated <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    integerDotProduct4x8BitPackedMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    integerDotProduct16BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    integerDotProduct16BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    integerDotProduct16BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    integerDotProduct32BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    integerDotProduct32BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    integerDotProduct32BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    integerDotProduct64BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    integerDotProduct64BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    integerDotProduct64BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating8BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating8BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 80 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating8BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 84 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating4x8BitPackedUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 88 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating4x8BitPackedSignedAccelerated <- peek @Bool32 ((p `plusPtr` 92 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating4x8BitPackedMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 96 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating16BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 100 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating16BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 104 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating16BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 108 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating32BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 112 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating32BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 116 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating32BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 120 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating64BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 124 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating64BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 128 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating64BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 132 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderIntegerDotProductPropertiesKHR
             (bool32ToBool integerDotProduct8BitUnsignedAccelerated) (bool32ToBool integerDotProduct8BitSignedAccelerated) (bool32ToBool integerDotProduct8BitMixedSignednessAccelerated) (bool32ToBool integerDotProduct4x8BitPackedUnsignedAccelerated) (bool32ToBool integerDotProduct4x8BitPackedSignedAccelerated) (bool32ToBool integerDotProduct4x8BitPackedMixedSignednessAccelerated) (bool32ToBool integerDotProduct16BitUnsignedAccelerated) (bool32ToBool integerDotProduct16BitSignedAccelerated) (bool32ToBool integerDotProduct16BitMixedSignednessAccelerated) (bool32ToBool integerDotProduct32BitUnsignedAccelerated) (bool32ToBool integerDotProduct32BitSignedAccelerated) (bool32ToBool integerDotProduct32BitMixedSignednessAccelerated) (bool32ToBool integerDotProduct64BitUnsignedAccelerated) (bool32ToBool integerDotProduct64BitSignedAccelerated) (bool32ToBool integerDotProduct64BitMixedSignednessAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating8BitUnsignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating8BitSignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating8BitMixedSignednessAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating4x8BitPackedUnsignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating4x8BitPackedSignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating4x8BitPackedMixedSignednessAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating16BitUnsignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating16BitSignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating16BitMixedSignednessAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating32BitUnsignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating32BitSignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating32BitMixedSignednessAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating64BitUnsignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating64BitSignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating64BitMixedSignednessAccelerated)

instance Storable PhysicalDeviceShaderIntegerDotProductPropertiesKHR where
  sizeOf ~_ = 136
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderIntegerDotProductPropertiesKHR where
  zero = PhysicalDeviceShaderIntegerDotProductPropertiesKHR
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


type KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION"
pattern KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION = 1


type KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME = "VK_KHR_shader_integer_dot_product"

-- No documentation found for TopLevel "VK_KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME"
pattern KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME = "VK_KHR_shader_integer_dot_product"

