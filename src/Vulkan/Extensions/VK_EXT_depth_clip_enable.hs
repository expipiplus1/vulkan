{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_depth_clip_enable"
module Vulkan.Extensions.VK_EXT_depth_clip_enable  ( PhysicalDeviceDepthClipEnableFeaturesEXT(..)
                                                   , PipelineRasterizationDepthClipStateCreateInfoEXT(..)
                                                   , PipelineRasterizationDepthClipStateCreateFlagsEXT(..)
                                                   , EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION
                                                   , pattern EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION
                                                   , EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME
                                                   , pattern EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceDepthClipEnableFeaturesEXT"
data PhysicalDeviceDepthClipEnableFeaturesEXT = PhysicalDeviceDepthClipEnableFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceDepthClipEnableFeaturesEXT" "depthClipEnable"
    depthClipEnable :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDepthClipEnableFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceDepthClipEnableFeaturesEXT

instance ToCStruct PhysicalDeviceDepthClipEnableFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDepthClipEnableFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (depthClipEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDepthClipEnableFeaturesEXT where
  peekCStruct p = do
    depthClipEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDepthClipEnableFeaturesEXT
             (bool32ToBool depthClipEnable)


instance Storable PhysicalDeviceDepthClipEnableFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDepthClipEnableFeaturesEXT where
  zero = PhysicalDeviceDepthClipEnableFeaturesEXT
           zero



-- No documentation found for TopLevel "VkPipelineRasterizationDepthClipStateCreateInfoEXT"
data PipelineRasterizationDepthClipStateCreateInfoEXT = PipelineRasterizationDepthClipStateCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineRasterizationDepthClipStateCreateInfoEXT" "flags"
    flags :: PipelineRasterizationDepthClipStateCreateFlagsEXT
  , -- No documentation found for Nested "VkPipelineRasterizationDepthClipStateCreateInfoEXT" "depthClipEnable"
    depthClipEnable :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRasterizationDepthClipStateCreateInfoEXT)
#endif
deriving instance Show PipelineRasterizationDepthClipStateCreateInfoEXT

instance ToCStruct PipelineRasterizationDepthClipStateCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationDepthClipStateCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRasterizationDepthClipStateCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (depthClipEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PipelineRasterizationDepthClipStateCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @PipelineRasterizationDepthClipStateCreateFlagsEXT ((p `plusPtr` 16 :: Ptr PipelineRasterizationDepthClipStateCreateFlagsEXT))
    depthClipEnable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PipelineRasterizationDepthClipStateCreateInfoEXT
             flags (bool32ToBool depthClipEnable)


instance Storable PipelineRasterizationDepthClipStateCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationDepthClipStateCreateInfoEXT where
  zero = PipelineRasterizationDepthClipStateCreateInfoEXT
           zero
           zero


-- No documentation found for TopLevel "VkPipelineRasterizationDepthClipStateCreateFlagsEXT"
newtype PipelineRasterizationDepthClipStateCreateFlagsEXT = PipelineRasterizationDepthClipStateCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineRasterizationDepthClipStateCreateFlagsEXT :: String
conNamePipelineRasterizationDepthClipStateCreateFlagsEXT = "PipelineRasterizationDepthClipStateCreateFlagsEXT"

enumPrefixPipelineRasterizationDepthClipStateCreateFlagsEXT :: String
enumPrefixPipelineRasterizationDepthClipStateCreateFlagsEXT = ""

showTablePipelineRasterizationDepthClipStateCreateFlagsEXT
  :: [(PipelineRasterizationDepthClipStateCreateFlagsEXT, String)]
showTablePipelineRasterizationDepthClipStateCreateFlagsEXT = []


instance Show PipelineRasterizationDepthClipStateCreateFlagsEXT where
showsPrec = enumShowsPrec enumPrefixPipelineRasterizationDepthClipStateCreateFlagsEXT
                          showTablePipelineRasterizationDepthClipStateCreateFlagsEXT
                          conNamePipelineRasterizationDepthClipStateCreateFlagsEXT
                          (\(PipelineRasterizationDepthClipStateCreateFlagsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineRasterizationDepthClipStateCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixPipelineRasterizationDepthClipStateCreateFlagsEXT
                          showTablePipelineRasterizationDepthClipStateCreateFlagsEXT
                          conNamePipelineRasterizationDepthClipStateCreateFlagsEXT
                          PipelineRasterizationDepthClipStateCreateFlagsEXT


type EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION"
pattern EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION = 1


type EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME = "VK_EXT_depth_clip_enable"

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME"
pattern EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME = "VK_EXT_depth_clip_enable"

