{-# language CPP #-}
module Vulkan.Extensions.VK_NV_device_diagnostics_config  ( PhysicalDeviceDiagnosticsConfigFeaturesNV(..)
                                                          , DeviceDiagnosticsConfigCreateInfoNV(..)
                                                          , DeviceDiagnosticsConfigFlagBitsNV( DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV
                                                                                             , DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV
                                                                                             , DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV
                                                                                             , ..
                                                                                             )
                                                          , DeviceDiagnosticsConfigFlagsNV
                                                          , NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION
                                                          , pattern NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION
                                                          , NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME
                                                          , pattern NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME
                                                          ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV))
-- | VkPhysicalDeviceDiagnosticsConfigFeaturesNV - Structure describing the
-- device-generated diagnostic configuration features that can be supported
-- by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceDiagnosticsConfigFeaturesNV' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceDiagnosticsConfigFeaturesNV' structure is included
-- in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceDiagnosticsConfigFeaturesNV' /can/ also be used in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the
-- feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDiagnosticsConfigFeaturesNV = PhysicalDeviceDiagnosticsConfigFeaturesNV
  { -- | #features-features-diagnosticsConfig# @diagnosticsConfig@ indicates
    -- whether the implementation supports the ability to configure diagnostic
    -- tools.
    diagnosticsConfig :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDiagnosticsConfigFeaturesNV)
#endif
deriving instance Show PhysicalDeviceDiagnosticsConfigFeaturesNV

instance ToCStruct PhysicalDeviceDiagnosticsConfigFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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
-- 'DeviceDiagnosticsConfigFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceDiagnosticsConfigCreateInfoNV = DeviceDiagnosticsConfigCreateInfoNV
  { -- | @flags@ is a bitmask of 'DeviceDiagnosticsConfigFlagBitsNV' specifying
    -- addtional parameters for configuring diagnostic tools.
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
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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


-- | VkDeviceDiagnosticsConfigFlagBitsNV - Bitmask specifying diagnostics
-- flags
--
-- = See Also
--
-- 'DeviceDiagnosticsConfigFlagsNV'
newtype DeviceDiagnosticsConfigFlagBitsNV = DeviceDiagnosticsConfigFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV' enables the
-- generation of debug information for shaders.
pattern DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV = DeviceDiagnosticsConfigFlagBitsNV 0x00000001
-- | 'DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV' enables
-- driver side tracking of resources (images, buffers, etc.) used to
-- augment the device fault information.
pattern DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV = DeviceDiagnosticsConfigFlagBitsNV 0x00000002
-- | 'DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV' enables
-- automatic insertion of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#device-diagnostic-checkpoints diagnostic checkpoints>
-- for draw calls, dispatches, trace rays, and copies. The CPU call stack
-- at the time of the command will be associated as the marker data for the
-- automatically inserted checkpoints.
pattern DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV = DeviceDiagnosticsConfigFlagBitsNV 0x00000004

type DeviceDiagnosticsConfigFlagsNV = DeviceDiagnosticsConfigFlagBitsNV

instance Show DeviceDiagnosticsConfigFlagBitsNV where
  showsPrec p = \case
    DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV -> showString "DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV"
    DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV -> showString "DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV"
    DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV -> showString "DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV"
    DeviceDiagnosticsConfigFlagBitsNV x -> showParen (p >= 11) (showString "DeviceDiagnosticsConfigFlagBitsNV 0x" . showHex x)

instance Read DeviceDiagnosticsConfigFlagBitsNV where
  readPrec = parens (choose [("DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV", pure DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV)
                            , ("DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV", pure DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV)
                            , ("DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV", pure DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "DeviceDiagnosticsConfigFlagBitsNV")
                       v <- step readPrec
                       pure (DeviceDiagnosticsConfigFlagBitsNV v)))


type NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION"
pattern NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION = 1


type NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME = "VK_NV_device_diagnostics_config"

-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME"
pattern NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME = "VK_NV_device_diagnostics_config"

