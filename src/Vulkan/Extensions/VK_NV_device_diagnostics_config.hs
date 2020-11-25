{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_device_diagnostics_config"
module Vulkan.Extensions.VK_NV_device_diagnostics_config  ( PhysicalDeviceDiagnosticsConfigFeaturesNV(..)
                                                          , DeviceDiagnosticsConfigCreateInfoNV(..)
                                                          , DeviceDiagnosticsConfigFlagsNV
                                                          , DeviceDiagnosticsConfigFlagBitsNV( DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV
                                                                                             , DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV
                                                                                             , DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV
                                                                                             , ..
                                                                                             )
                                                          , NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION
                                                          , pattern NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION
                                                          , NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME
                                                          , pattern NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME
                                                          ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV))

-- No documentation found for TopLevel "VkPhysicalDeviceDiagnosticsConfigFeaturesNV"
data PhysicalDeviceDiagnosticsConfigFeaturesNV = PhysicalDeviceDiagnosticsConfigFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceDiagnosticsConfigFeaturesNV" "diagnosticsConfig"
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



-- No documentation found for TopLevel "VkDeviceDiagnosticsConfigCreateInfoNV"
data DeviceDiagnosticsConfigCreateInfoNV = DeviceDiagnosticsConfigCreateInfoNV
  { -- No documentation found for Nested "VkDeviceDiagnosticsConfigCreateInfoNV" "flags"
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


type DeviceDiagnosticsConfigFlagsNV = DeviceDiagnosticsConfigFlagBitsNV

-- No documentation found for TopLevel "VkDeviceDiagnosticsConfigFlagBitsNV"
newtype DeviceDiagnosticsConfigFlagBitsNV = DeviceDiagnosticsConfigFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDeviceDiagnosticsConfigFlagBitsNV" "VK_DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV"
pattern DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV     = DeviceDiagnosticsConfigFlagBitsNV 0x00000001
-- No documentation found for Nested "VkDeviceDiagnosticsConfigFlagBitsNV" "VK_DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV"
pattern DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV     = DeviceDiagnosticsConfigFlagBitsNV 0x00000002
-- No documentation found for Nested "VkDeviceDiagnosticsConfigFlagBitsNV" "VK_DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV"
pattern DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV = DeviceDiagnosticsConfigFlagBitsNV 0x00000004

conNameDeviceDiagnosticsConfigFlagBitsNV :: String
conNameDeviceDiagnosticsConfigFlagBitsNV = "DeviceDiagnosticsConfigFlagBitsNV"

enumPrefixDeviceDiagnosticsConfigFlagBitsNV :: String
enumPrefixDeviceDiagnosticsConfigFlagBitsNV = "DEVICE_DIAGNOSTICS_CONFIG_ENABLE_"

showTableDeviceDiagnosticsConfigFlagBitsNV :: [(DeviceDiagnosticsConfigFlagBitsNV, String)]
showTableDeviceDiagnosticsConfigFlagBitsNV =
  [ (DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV    , "SHADER_DEBUG_INFO_BIT_NV")
  , (DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV    , "RESOURCE_TRACKING_BIT_NV")
  , (DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV, "AUTOMATIC_CHECKPOINTS_BIT_NV")
  ]


instance Show DeviceDiagnosticsConfigFlagBitsNV where
showsPrec = enumShowsPrec enumPrefixDeviceDiagnosticsConfigFlagBitsNV
                          showTableDeviceDiagnosticsConfigFlagBitsNV
                          conNameDeviceDiagnosticsConfigFlagBitsNV
                          (\(DeviceDiagnosticsConfigFlagBitsNV x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DeviceDiagnosticsConfigFlagBitsNV where
  readPrec = enumReadPrec enumPrefixDeviceDiagnosticsConfigFlagBitsNV
                          showTableDeviceDiagnosticsConfigFlagBitsNV
                          conNameDeviceDiagnosticsConfigFlagBitsNV
                          DeviceDiagnosticsConfigFlagBitsNV


type NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION"
pattern NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION = 1


type NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME = "VK_NV_device_diagnostics_config"

-- No documentation found for TopLevel "VK_NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME"
pattern NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME = "VK_NV_device_diagnostics_config"

