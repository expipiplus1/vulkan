{-# language CPP #-}
-- | = Name
--
-- VK_NV_device_diagnostics_config - device extension
--
-- == VK_NV_device_diagnostics_config
--
-- [__Name String__]
--     @VK_NV_device_diagnostics_config@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     301
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Kedarnath Thangudu
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_device_diagnostics_config] @kthangudu%0A*Here describe the issue or question you have about the VK_NV_device_diagnostics_config extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-04-06
--
-- [__Contributors__]
--
--     -   Kedarnath Thangudu, NVIDIA
--
--     -   Thomas Klein, NVIDIA
--
-- == Description
--
-- Applications using Nvidia Nsight™ Aftermath SDK for Vulkan to integrate
-- device crash dumps into their error reporting mechanisms, /may/ use this
-- extension to configure options related to device crash dump creation.
--
-- Version 2 of this extension adds
-- 'DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_ERROR_REPORTING_BIT_NV' which
-- when set enables enhanced reporting of shader execution errors.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DeviceDiagnosticsConfigCreateInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDiagnosticsConfigFeaturesNV'
--
-- == New Enums
--
-- -   'DeviceDiagnosticsConfigFlagBitsNV'
--
-- == New Bitmasks
--
-- -   'DeviceDiagnosticsConfigFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME'
--
-- -   'NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV'
--
-- == Version History
--
-- -   Revision 1, 2019-11-21 (Kedarnath Thangudu)
--
--     -   Internal revisions
--
-- -   Revision 2, 2022-04-06 (Kedarnath Thangudu)
--
--     -   Added a config bit
--         'DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_ERROR_REPORTING_BIT_NV'
--
-- == See Also
--
-- 'DeviceDiagnosticsConfigCreateInfoNV',
-- 'DeviceDiagnosticsConfigFlagBitsNV', 'DeviceDiagnosticsConfigFlagsNV',
-- 'PhysicalDeviceDiagnosticsConfigFeaturesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_device_diagnostics_config Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_device_diagnostics_config  ( PhysicalDeviceDiagnosticsConfigFeaturesNV(..)
                                                          , DeviceDiagnosticsConfigCreateInfoNV(..)
                                                          , DeviceDiagnosticsConfigFlagsNV
                                                          , DeviceDiagnosticsConfigFlagBitsNV( DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV
                                                                                             , DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV
                                                                                             , DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV
                                                                                             , DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_ERROR_REPORTING_BIT_NV
                                                                                             , ..
                                                                                             )
                                                          , NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION
                                                          , pattern NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION
                                                          , NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME
                                                          , pattern NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV))
-- | VkPhysicalDeviceDiagnosticsConfigFeaturesNV - Structure describing the
-- device-generated diagnostic configuration features that can be supported
-- by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDiagnosticsConfigFeaturesNV' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDiagnosticsConfigFeaturesNV' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_diagnostics_config VK_NV_device_diagnostics_config>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDiagnosticsConfigFeaturesNV = PhysicalDeviceDiagnosticsConfigFeaturesNV
  { -- | #features-diagnosticsConfig# @diagnosticsConfig@ indicates whether the
    -- implementation supports the ability to configure diagnostic tools.
    diagnosticsConfig :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDiagnosticsConfigFeaturesNV)
#endif
deriving instance Show PhysicalDeviceDiagnosticsConfigFeaturesNV

instance ToCStruct PhysicalDeviceDiagnosticsConfigFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDiagnosticsConfigFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (diagnosticsConfig))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDiagnosticsConfigFeaturesNV where
  peekCStruct p = do
    diagnosticsConfig <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDiagnosticsConfigFeaturesNV
             (bool32ToBool diagnosticsConfig)

instance Storable PhysicalDeviceDiagnosticsConfigFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDiagnosticsConfigFeaturesNV where
  zero = PhysicalDeviceDiagnosticsConfigFeaturesNV
           zero


-- | VkDeviceDiagnosticsConfigCreateInfoNV - Specify diagnostics config for a
-- Vulkan device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_diagnostics_config VK_NV_device_diagnostics_config>,
-- 'DeviceDiagnosticsConfigFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceDiagnosticsConfigCreateInfoNV = DeviceDiagnosticsConfigCreateInfoNV
  { -- | @flags@ is a bitmask of 'DeviceDiagnosticsConfigFlagBitsNV' specifying
    -- additional parameters for configuring diagnostic tools.
    --
    -- #VUID-VkDeviceDiagnosticsConfigCreateInfoNV-flags-parameter# @flags@
    -- /must/ be a valid combination of 'DeviceDiagnosticsConfigFlagBitsNV'
    -- values
    flags :: DeviceDiagnosticsConfigFlagsNV }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceDiagnosticsConfigCreateInfoNV)
#endif
deriving instance Show DeviceDiagnosticsConfigCreateInfoNV

instance ToCStruct DeviceDiagnosticsConfigCreateInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceDiagnosticsConfigCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceDiagnosticsConfigFlagsNV)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DeviceDiagnosticsConfigCreateInfoNV where
  peekCStruct p = do
    flags <- peek @DeviceDiagnosticsConfigFlagsNV ((p `plusPtr` 16 :: Ptr DeviceDiagnosticsConfigFlagsNV))
    pure $ DeviceDiagnosticsConfigCreateInfoNV
             flags

instance Storable DeviceDiagnosticsConfigCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceDiagnosticsConfigCreateInfoNV where
  zero = DeviceDiagnosticsConfigCreateInfoNV
           zero


type DeviceDiagnosticsConfigFlagsNV = DeviceDiagnosticsConfigFlagBitsNV

-- | VkDeviceDiagnosticsConfigFlagBitsNV - Bitmask specifying diagnostics
-- flags
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_diagnostics_config VK_NV_device_diagnostics_config>,
-- 'DeviceDiagnosticsConfigFlagsNV'
newtype DeviceDiagnosticsConfigFlagBitsNV = DeviceDiagnosticsConfigFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV' enables the
-- generation of debug information for shaders.
pattern DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV = DeviceDiagnosticsConfigFlagBitsNV 0x00000001

-- | 'DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV' enables
-- driver side tracking of resources (images, buffers, etc.) used to
-- augment the device fault information.
pattern DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV = DeviceDiagnosticsConfigFlagBitsNV 0x00000002

-- | 'DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV' enables
-- automatic insertion of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#device-diagnostic-checkpoints diagnostic checkpoints>
-- for draw calls, dispatches, trace rays, and copies. The CPU call stack
-- at the time of the command will be associated as the marker data for the
-- automatically inserted checkpoints.
pattern DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV = DeviceDiagnosticsConfigFlagBitsNV 0x00000004

-- | 'DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_ERROR_REPORTING_BIT_NV' enables
-- shader error reporting.
pattern DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_ERROR_REPORTING_BIT_NV = DeviceDiagnosticsConfigFlagBitsNV 0x00000008

conNameDeviceDiagnosticsConfigFlagBitsNV :: String
conNameDeviceDiagnosticsConfigFlagBitsNV = "DeviceDiagnosticsConfigFlagBitsNV"

enumPrefixDeviceDiagnosticsConfigFlagBitsNV :: String
enumPrefixDeviceDiagnosticsConfigFlagBitsNV = "DEVICE_DIAGNOSTICS_CONFIG_ENABLE_"

showTableDeviceDiagnosticsConfigFlagBitsNV :: [(DeviceDiagnosticsConfigFlagBitsNV, String)]
showTableDeviceDiagnosticsConfigFlagBitsNV =
  [
    ( DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV
    , "SHADER_DEBUG_INFO_BIT_NV"
    )
  ,
    ( DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV
    , "RESOURCE_TRACKING_BIT_NV"
    )
  ,
    ( DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV
    , "AUTOMATIC_CHECKPOINTS_BIT_NV"
    )
  ,
    ( DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_ERROR_REPORTING_BIT_NV
    , "SHADER_ERROR_REPORTING_BIT_NV"
    )
  ]

instance Show DeviceDiagnosticsConfigFlagBitsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixDeviceDiagnosticsConfigFlagBitsNV
      showTableDeviceDiagnosticsConfigFlagBitsNV
      conNameDeviceDiagnosticsConfigFlagBitsNV
      (\(DeviceDiagnosticsConfigFlagBitsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DeviceDiagnosticsConfigFlagBitsNV where
  readPrec =
    enumReadPrec
      enumPrefixDeviceDiagnosticsConfigFlagBitsNV
      showTableDeviceDiagnosticsConfigFlagBitsNV
      conNameDeviceDiagnosticsConfigFlagBitsNV
      DeviceDiagnosticsConfigFlagBitsNV

type NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION"
pattern NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION = 2


type NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME = "VK_NV_device_diagnostics_config"

-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME"
pattern NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME = "VK_NV_device_diagnostics_config"

