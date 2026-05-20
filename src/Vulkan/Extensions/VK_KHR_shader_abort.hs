{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_abort - device extension
--
-- = VK_KHR_shader_abort
--
-- [__Name String__]
--     @VK_KHR_shader_abort@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     234
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_constant_data VK_KHR_shader_constant_data>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_abort.html SPV_KHR_abort>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_abort] @tobski%0A*Here describe the issue or question you have about the VK_KHR_shader_abort extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_abort.adoc VK_KHR_shader_abort>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-03-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Piers Daniell, Nvidia
--
--     -   Craig Graham, Samsung
--
--     -   Erik Hogeman, ARM
--
--     -   Ralph Potter, Samsung
--
--     -   Vikram Tarikere, IMG
--
-- == Description
--
-- This extension enables the use of the @OpAbortKHR@ instruction in
-- shaders.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_device_fault.DeviceFaultDebugInfoKHR':
--
--     -   'DeviceFaultShaderAbortMessageInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderAbortFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderAbortPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_ABORT_EXTENSION_NAME'
--
-- -   'KHR_SHADER_ABORT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_FAULT_SHADER_ABORT_MESSAGE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ABORT_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ABORT_PROPERTIES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2024-08-22 (Tobias Hector)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_shader_abort Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_abort  ( PhysicalDeviceShaderAbortFeaturesKHR(..)
                                              , PhysicalDeviceShaderAbortPropertiesKHR(..)
                                              , DeviceFaultShaderAbortMessageInfoKHR(..)
                                              , KHR_SHADER_ABORT_SPEC_VERSION
                                              , pattern KHR_SHADER_ABORT_SPEC_VERSION
                                              , KHR_SHADER_ABORT_EXTENSION_NAME
                                              , pattern KHR_SHADER_ABORT_EXTENSION_NAME
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
import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_FAULT_SHADER_ABORT_MESSAGE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ABORT_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ABORT_PROPERTIES_KHR))
-- | VkPhysicalDeviceShaderAbortFeaturesKHR - Structure describing whether
-- shader abort is supported or not
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderAbortFeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderAbortFeaturesKHR', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_abort VK_KHR_shader_abort>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderAbortFeaturesKHR = PhysicalDeviceShaderAbortFeaturesKHR
  { -- | #features-shaderAbort# @shaderAbort@ specifies whether shaders /can/
    -- declare the @AbortKHR@ capability.
    shaderAbort :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderAbortFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderAbortFeaturesKHR

instance ToCStruct PhysicalDeviceShaderAbortFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderAbortFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ABORT_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderAbort))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ABORT_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderAbortFeaturesKHR where
  peekCStruct p = do
    shaderAbort <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderAbortFeaturesKHR
             (bool32ToBool shaderAbort)

instance Storable PhysicalDeviceShaderAbortFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderAbortFeaturesKHR where
  zero = PhysicalDeviceShaderAbortFeaturesKHR
           zero


-- | VkPhysicalDeviceShaderAbortPropertiesKHR - Structure describing the
-- maximum size of a shader abort message
--
-- = Members
--
-- The 'PhysicalDeviceShaderAbortPropertiesKHR' structure describes the
-- following:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderAbortPropertiesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_abort VK_KHR_shader_abort>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderAbortPropertiesKHR = PhysicalDeviceShaderAbortPropertiesKHR
  { -- | #limits-maxShaderAbortMessageSize# @maxShaderAbortMessageSize@ indicates
    -- the maximum size of a shader abort message that the application /can/
    -- specify.
    maxShaderAbortMessageSize :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderAbortPropertiesKHR)
#endif
deriving instance Show PhysicalDeviceShaderAbortPropertiesKHR

instance ToCStruct PhysicalDeviceShaderAbortPropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderAbortPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ABORT_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (maxShaderAbortMessageSize)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ABORT_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    f

instance FromCStruct PhysicalDeviceShaderAbortPropertiesKHR where
  peekCStruct p = do
    maxShaderAbortMessageSize <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    pure $ PhysicalDeviceShaderAbortPropertiesKHR
             maxShaderAbortMessageSize

instance Storable PhysicalDeviceShaderAbortPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderAbortPropertiesKHR where
  zero = PhysicalDeviceShaderAbortPropertiesKHR
           zero


-- | VkDeviceFaultShaderAbortMessageInfoKHR - Structure specifying message
-- data from @OpAbortKHR@
--
-- = Description
--
-- This structure /can/ be included in the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_device_fault.DeviceFaultDebugInfoKHR' to
-- retrieve messages returned by @OpAbortKHR@ instructions.
--
-- @pMessageData@ is populated with a series of (size,payload) pairs, each
-- aligned to 8-byte boundaries. The size in each pair is a 64-bit integer
-- indicating the number of bytes in the subsequent payload. The data in
-- payload is laid out in the exact manner specified in the @OpAbortKHR@
-- instruction by the message type, with no modifications. If multiple
-- messages are present, the next message size will always be at the
-- following 8-byte aligned offset after the payload ends.
--
-- Implementations /must/ report the message reported by the first
-- @OpAbortKHR@ instruction executed in an invocation for this device. They
-- /may/ report additional messages if other invocations continued to
-- execute after the abort instruction was executed.
--
-- Even though shading languages may provide definitions for printf-style
-- abort commands, no formatting is performed by the Vulkan implementation.
-- Applications should consult documentation for the shader language they
-- are using on how abort messages are packed, so that they can unpack them
-- after they are queried.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Extensions.VK_KHR_device_fault.DeviceFaultDebugInfoKHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_abort VK_KHR_shader_abort>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceFaultShaderAbortMessageInfoKHR = DeviceFaultShaderAbortMessageInfoKHR
  { -- | @messageDataSize@ is the size of @pMessageData@ in bytes. If
    -- @pMessageData@ is @NULL@, this value is populated by the implementation.
    messageDataSize :: Word64
  , -- | @pMessageData@ is @NULL@ or a pointer to @messageDataSize@ bytes of
    -- data, which will be populated with data for messages reported via
    -- @OpAbortKHR@. If @pMessageData@ is @NULL@ then @messageDataSize@ will be
    -- populated with the required size.
    messageData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceFaultShaderAbortMessageInfoKHR)
#endif
deriving instance Show DeviceFaultShaderAbortMessageInfoKHR

instance ToCStruct DeviceFaultShaderAbortMessageInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceFaultShaderAbortMessageInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_FAULT_SHADER_ABORT_MESSAGE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (messageDataSize)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (messageData)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_FAULT_SHADER_ABORT_MESSAGE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DeviceFaultShaderAbortMessageInfoKHR where
  peekCStruct p = do
    messageDataSize <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    pMessageData <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ DeviceFaultShaderAbortMessageInfoKHR
             messageDataSize pMessageData

instance Storable DeviceFaultShaderAbortMessageInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceFaultShaderAbortMessageInfoKHR where
  zero = DeviceFaultShaderAbortMessageInfoKHR
           zero
           zero


type KHR_SHADER_ABORT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_ABORT_SPEC_VERSION"
pattern KHR_SHADER_ABORT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_ABORT_SPEC_VERSION = 1


type KHR_SHADER_ABORT_EXTENSION_NAME = "VK_KHR_shader_abort"

-- No documentation found for TopLevel "VK_KHR_SHADER_ABORT_EXTENSION_NAME"
pattern KHR_SHADER_ABORT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_ABORT_EXTENSION_NAME = "VK_KHR_shader_abort"

