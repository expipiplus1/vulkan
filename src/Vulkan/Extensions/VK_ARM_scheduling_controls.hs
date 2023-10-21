{-# language CPP #-}
-- | = Name
--
-- VK_ARM_scheduling_controls - device extension
--
-- == VK_ARM_scheduling_controls
--
-- [__Name String__]
--     @VK_ARM_scheduling_controls@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     418
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_core_builtins VK_ARM_shader_core_builtins>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_scheduling_controls] @kpet%0A*Here describe the issue or question you have about the VK_ARM_scheduling_controls extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-08-23
--
-- [__Interactions and External Dependencies__]
--     None
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Mikel Garai, Arm Ltd.
--
-- == Description
--
-- This extension exposes a collection of controls to modify the scheduling
-- behaviour of Arm Mali devices.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Device.DeviceQueueCreateInfo',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DeviceQueueShaderCoreControlCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSchedulingControlsFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceSchedulingControlsPropertiesARM'
--
-- == New Enums
--
-- -   'PhysicalDeviceSchedulingControlsFlagBitsARM'
--
-- == New Bitmasks
--
-- -   'PhysicalDeviceSchedulingControlsFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_SCHEDULING_CONTROLS_EXTENSION_NAME'
--
-- -   'ARM_SCHEDULING_CONTROLS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_QUEUE_SHADER_CORE_CONTROL_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_PROPERTIES_ARM'
--
-- == New SPIR-V Capabilities
--
-- None.
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-08-23 (Kévin Petit)
--
--     -   Initial revision
--
-- == See Also
--
-- 'DeviceQueueShaderCoreControlCreateInfoARM',
-- 'PhysicalDeviceSchedulingControlsFeaturesARM',
-- 'PhysicalDeviceSchedulingControlsFlagBitsARM',
-- 'PhysicalDeviceSchedulingControlsFlagsARM',
-- 'PhysicalDeviceSchedulingControlsPropertiesARM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_ARM_scheduling_controls Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_scheduling_controls  ( DeviceQueueShaderCoreControlCreateInfoARM(..)
                                                     , PhysicalDeviceSchedulingControlsFeaturesARM(..)
                                                     , PhysicalDeviceSchedulingControlsPropertiesARM(..)
                                                     , PhysicalDeviceSchedulingControlsFlagsARM
                                                     , PhysicalDeviceSchedulingControlsFlagBitsARM( PHYSICAL_DEVICE_SCHEDULING_CONTROLS_SHADER_CORE_COUNT_ARM
                                                                                                  , ..
                                                                                                  )
                                                     , ARM_SCHEDULING_CONTROLS_SPEC_VERSION
                                                     , pattern ARM_SCHEDULING_CONTROLS_SPEC_VERSION
                                                     , ARM_SCHEDULING_CONTROLS_EXTENSION_NAME
                                                     , pattern ARM_SCHEDULING_CONTROLS_EXTENSION_NAME
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
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Flags64)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_SHADER_CORE_CONTROL_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_FEATURES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_PROPERTIES_ARM))
-- | VkDeviceQueueShaderCoreControlCreateInfoARM - Control the number of
-- shader cores used by queues
--
-- = Description
--
-- Queues created without specifying
-- 'DeviceQueueShaderCoreControlCreateInfoARM' will default to using all
-- the shader cores available.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_scheduling_controls VK_ARM_scheduling_controls>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceQueueShaderCoreControlCreateInfoARM = DeviceQueueShaderCoreControlCreateInfoARM
  { -- | @shaderCoreCount@ is the number of shader cores this queue uses.
    --
    -- #VUID-VkDeviceQueueShaderCoreControlCreateInfoARM-shaderCoreCount-09399#
    -- @shaderCoreCount@ /must/ be greater than 0 and less than or equal to the
    -- total number of shader cores as reported via
    -- 'Vulkan.Extensions.VK_ARM_shader_core_builtins.PhysicalDeviceShaderCoreBuiltinsPropertiesARM'::@shaderCoreCount@.
    shaderCoreCount :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceQueueShaderCoreControlCreateInfoARM)
#endif
deriving instance Show DeviceQueueShaderCoreControlCreateInfoARM

instance ToCStruct DeviceQueueShaderCoreControlCreateInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceQueueShaderCoreControlCreateInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_SHADER_CORE_CONTROL_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (shaderCoreCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_SHADER_CORE_CONTROL_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceQueueShaderCoreControlCreateInfoARM where
  peekCStruct p = do
    shaderCoreCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DeviceQueueShaderCoreControlCreateInfoARM
             shaderCoreCount

instance Storable DeviceQueueShaderCoreControlCreateInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceQueueShaderCoreControlCreateInfoARM where
  zero = DeviceQueueShaderCoreControlCreateInfoARM
           zero


-- | VkPhysicalDeviceSchedulingControlsFeaturesARM - Structure describing
-- scheduling controls features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceSchedulingControlsFeaturesARM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceSchedulingControlsFeaturesARM' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_scheduling_controls VK_ARM_scheduling_controls>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSchedulingControlsFeaturesARM = PhysicalDeviceSchedulingControlsFeaturesARM
  { -- | #features-schedulingControls# @schedulingControls@ indicates that the
    -- implementation supports scheduling controls.
    schedulingControls :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSchedulingControlsFeaturesARM)
#endif
deriving instance Show PhysicalDeviceSchedulingControlsFeaturesARM

instance ToCStruct PhysicalDeviceSchedulingControlsFeaturesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSchedulingControlsFeaturesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (schedulingControls))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSchedulingControlsFeaturesARM where
  peekCStruct p = do
    schedulingControls <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceSchedulingControlsFeaturesARM
             (bool32ToBool schedulingControls)

instance Storable PhysicalDeviceSchedulingControlsFeaturesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSchedulingControlsFeaturesARM where
  zero = PhysicalDeviceSchedulingControlsFeaturesARM
           zero


-- | VkPhysicalDeviceSchedulingControlsPropertiesARM - Structure containing
-- scheduling control properties of a physical device
--
-- = Members
--
-- -   #limits-schedulingControlsFlags#@schedulingControlsFlags@ specifies
--     the specific scheduling controls that a physical device supports.
--
-- = Description
--
-- If the 'PhysicalDeviceSchedulingControlsPropertiesARM' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_scheduling_controls VK_ARM_scheduling_controls>,
-- 'PhysicalDeviceSchedulingControlsFlagsARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSchedulingControlsPropertiesARM = PhysicalDeviceSchedulingControlsPropertiesARM
  { -- | #VUID-VkPhysicalDeviceSchedulingControlsPropertiesARM-schedulingControlsFlags-parameter#
    -- @schedulingControlsFlags@ /must/ be a valid combination of
    -- 'PhysicalDeviceSchedulingControlsFlagBitsARM' values
    --
    -- #VUID-VkPhysicalDeviceSchedulingControlsPropertiesARM-schedulingControlsFlags-requiredbitmask#
    -- @schedulingControlsFlags@ /must/ not be @0@
    schedulingControlsFlags :: PhysicalDeviceSchedulingControlsFlagsARM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSchedulingControlsPropertiesARM)
#endif
deriving instance Show PhysicalDeviceSchedulingControlsPropertiesARM

instance ToCStruct PhysicalDeviceSchedulingControlsPropertiesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSchedulingControlsPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceSchedulingControlsFlagsARM)) (schedulingControlsFlags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceSchedulingControlsFlagsARM)) (zero)
    f

instance FromCStruct PhysicalDeviceSchedulingControlsPropertiesARM where
  peekCStruct p = do
    schedulingControlsFlags <- peek @PhysicalDeviceSchedulingControlsFlagsARM ((p `plusPtr` 16 :: Ptr PhysicalDeviceSchedulingControlsFlagsARM))
    pure $ PhysicalDeviceSchedulingControlsPropertiesARM
             schedulingControlsFlags

instance Storable PhysicalDeviceSchedulingControlsPropertiesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSchedulingControlsPropertiesARM where
  zero = PhysicalDeviceSchedulingControlsPropertiesARM
           zero


type PhysicalDeviceSchedulingControlsFlagsARM = PhysicalDeviceSchedulingControlsFlagBitsARM

-- | VkPhysicalDeviceSchedulingControlsFlagBitsARM - Bitmask specifying
-- scheduling controls supported by a physical device
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_scheduling_controls VK_ARM_scheduling_controls>
newtype PhysicalDeviceSchedulingControlsFlagBitsARM = PhysicalDeviceSchedulingControlsFlagBitsARM Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PHYSICAL_DEVICE_SCHEDULING_CONTROLS_SHADER_CORE_COUNT_ARM' indicates
-- that a 'DeviceQueueShaderCoreControlCreateInfoARM' structure /may/ be
-- included in the @pNext@ chain of a
-- 'Vulkan.Core10.Device.DeviceQueueCreateInfo' or
-- 'Vulkan.Core10.Device.DeviceCreateInfo' structure.
pattern PHYSICAL_DEVICE_SCHEDULING_CONTROLS_SHADER_CORE_COUNT_ARM = PhysicalDeviceSchedulingControlsFlagBitsARM 0x0000000000000001

conNamePhysicalDeviceSchedulingControlsFlagBitsARM :: String
conNamePhysicalDeviceSchedulingControlsFlagBitsARM = "PhysicalDeviceSchedulingControlsFlagBitsARM"

enumPrefixPhysicalDeviceSchedulingControlsFlagBitsARM :: String
enumPrefixPhysicalDeviceSchedulingControlsFlagBitsARM = "PHYSICAL_DEVICE_SCHEDULING_CONTROLS_SHADER_CORE_COUNT_ARM"

showTablePhysicalDeviceSchedulingControlsFlagBitsARM :: [(PhysicalDeviceSchedulingControlsFlagBitsARM, String)]
showTablePhysicalDeviceSchedulingControlsFlagBitsARM =
  [
    ( PHYSICAL_DEVICE_SCHEDULING_CONTROLS_SHADER_CORE_COUNT_ARM
    , ""
    )
  ]

instance Show PhysicalDeviceSchedulingControlsFlagBitsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixPhysicalDeviceSchedulingControlsFlagBitsARM
      showTablePhysicalDeviceSchedulingControlsFlagBitsARM
      conNamePhysicalDeviceSchedulingControlsFlagBitsARM
      (\(PhysicalDeviceSchedulingControlsFlagBitsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PhysicalDeviceSchedulingControlsFlagBitsARM where
  readPrec =
    enumReadPrec
      enumPrefixPhysicalDeviceSchedulingControlsFlagBitsARM
      showTablePhysicalDeviceSchedulingControlsFlagBitsARM
      conNamePhysicalDeviceSchedulingControlsFlagBitsARM
      PhysicalDeviceSchedulingControlsFlagBitsARM

type ARM_SCHEDULING_CONTROLS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_ARM_SCHEDULING_CONTROLS_SPEC_VERSION"
pattern ARM_SCHEDULING_CONTROLS_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_SCHEDULING_CONTROLS_SPEC_VERSION = 1


type ARM_SCHEDULING_CONTROLS_EXTENSION_NAME = "VK_ARM_scheduling_controls"

-- No documentation found for TopLevel "VK_ARM_SCHEDULING_CONTROLS_EXTENSION_NAME"
pattern ARM_SCHEDULING_CONTROLS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_SCHEDULING_CONTROLS_EXTENSION_NAME = "VK_ARM_scheduling_controls"

