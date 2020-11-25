{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_blend_operation_advanced"
module Vulkan.Extensions.VK_EXT_blend_operation_advanced  ( PhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
                                                          , PhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
                                                          , PipelineColorBlendAdvancedStateCreateInfoEXT(..)
                                                          , BlendOverlapEXT( BLEND_OVERLAP_UNCORRELATED_EXT
                                                                           , BLEND_OVERLAP_DISJOINT_EXT
                                                                           , BLEND_OVERLAP_CONJOINT_EXT
                                                                           , ..
                                                                           )
                                                          , EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION
                                                          , pattern EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION
                                                          , EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
                                                          , pattern EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
                                                          ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.String (IsString)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT"
data PhysicalDeviceBlendOperationAdvancedFeaturesEXT = PhysicalDeviceBlendOperationAdvancedFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT" "advancedBlendCoherentOperations"
    advancedBlendCoherentOperations :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceBlendOperationAdvancedFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceBlendOperationAdvancedFeaturesEXT

instance ToCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceBlendOperationAdvancedFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (advancedBlendCoherentOperations))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  peekCStruct p = do
    advancedBlendCoherentOperations <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceBlendOperationAdvancedFeaturesEXT
             (bool32ToBool advancedBlendCoherentOperations)


instance Storable PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  zero = PhysicalDeviceBlendOperationAdvancedFeaturesEXT
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT"
data PhysicalDeviceBlendOperationAdvancedPropertiesEXT = PhysicalDeviceBlendOperationAdvancedPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendMaxColorAttachments"
    advancedBlendMaxColorAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendIndependentBlend"
    advancedBlendIndependentBlend :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendNonPremultipliedSrcColor"
    advancedBlendNonPremultipliedSrcColor :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendNonPremultipliedDstColor"
    advancedBlendNonPremultipliedDstColor :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendCorrelatedOverlap"
    advancedBlendCorrelatedOverlap :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendAllOperations"
    advancedBlendAllOperations :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceBlendOperationAdvancedPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceBlendOperationAdvancedPropertiesEXT

instance ToCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceBlendOperationAdvancedPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (advancedBlendMaxColorAttachments)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (advancedBlendIndependentBlend))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (advancedBlendNonPremultipliedSrcColor))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (advancedBlendNonPremultipliedDstColor))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (advancedBlendCorrelatedOverlap))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (advancedBlendAllOperations))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  peekCStruct p = do
    advancedBlendMaxColorAttachments <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    advancedBlendIndependentBlend <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    advancedBlendNonPremultipliedSrcColor <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    advancedBlendNonPremultipliedDstColor <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    advancedBlendCorrelatedOverlap <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    advancedBlendAllOperations <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    pure $ PhysicalDeviceBlendOperationAdvancedPropertiesEXT
             advancedBlendMaxColorAttachments (bool32ToBool advancedBlendIndependentBlend) (bool32ToBool advancedBlendNonPremultipliedSrcColor) (bool32ToBool advancedBlendNonPremultipliedDstColor) (bool32ToBool advancedBlendCorrelatedOverlap) (bool32ToBool advancedBlendAllOperations)


instance Storable PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  zero = PhysicalDeviceBlendOperationAdvancedPropertiesEXT
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPipelineColorBlendAdvancedStateCreateInfoEXT"
data PipelineColorBlendAdvancedStateCreateInfoEXT = PipelineColorBlendAdvancedStateCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineColorBlendAdvancedStateCreateInfoEXT" "srcPremultiplied"
    srcPremultiplied :: Bool
  , -- No documentation found for Nested "VkPipelineColorBlendAdvancedStateCreateInfoEXT" "dstPremultiplied"
    dstPremultiplied :: Bool
  , -- No documentation found for Nested "VkPipelineColorBlendAdvancedStateCreateInfoEXT" "blendOverlap"
    blendOverlap :: BlendOverlapEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineColorBlendAdvancedStateCreateInfoEXT)
#endif
deriving instance Show PipelineColorBlendAdvancedStateCreateInfoEXT

instance ToCStruct PipelineColorBlendAdvancedStateCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineColorBlendAdvancedStateCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (srcPremultiplied))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (dstPremultiplied))
    poke ((p `plusPtr` 24 :: Ptr BlendOverlapEXT)) (blendOverlap)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr BlendOverlapEXT)) (zero)
    f

instance FromCStruct PipelineColorBlendAdvancedStateCreateInfoEXT where
  peekCStruct p = do
    srcPremultiplied <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    dstPremultiplied <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    blendOverlap <- peek @BlendOverlapEXT ((p `plusPtr` 24 :: Ptr BlendOverlapEXT))
    pure $ PipelineColorBlendAdvancedStateCreateInfoEXT
             (bool32ToBool srcPremultiplied) (bool32ToBool dstPremultiplied) blendOverlap


instance Storable PipelineColorBlendAdvancedStateCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineColorBlendAdvancedStateCreateInfoEXT where
  zero = PipelineColorBlendAdvancedStateCreateInfoEXT
           zero
           zero
           zero


-- No documentation found for TopLevel "VkBlendOverlapEXT"
newtype BlendOverlapEXT = BlendOverlapEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkBlendOverlapEXT" "VK_BLEND_OVERLAP_UNCORRELATED_EXT"
pattern BLEND_OVERLAP_UNCORRELATED_EXT = BlendOverlapEXT 0
-- No documentation found for Nested "VkBlendOverlapEXT" "VK_BLEND_OVERLAP_DISJOINT_EXT"
pattern BLEND_OVERLAP_DISJOINT_EXT     = BlendOverlapEXT 1
-- No documentation found for Nested "VkBlendOverlapEXT" "VK_BLEND_OVERLAP_CONJOINT_EXT"
pattern BLEND_OVERLAP_CONJOINT_EXT     = BlendOverlapEXT 2
{-# complete BLEND_OVERLAP_UNCORRELATED_EXT,
             BLEND_OVERLAP_DISJOINT_EXT,
             BLEND_OVERLAP_CONJOINT_EXT :: BlendOverlapEXT #-}

conNameBlendOverlapEXT :: String
conNameBlendOverlapEXT = "BlendOverlapEXT"

enumPrefixBlendOverlapEXT :: String
enumPrefixBlendOverlapEXT = "BLEND_OVERLAP_"

showTableBlendOverlapEXT :: [(BlendOverlapEXT, String)]
showTableBlendOverlapEXT =
  [ (BLEND_OVERLAP_UNCORRELATED_EXT, "UNCORRELATED_EXT")
  , (BLEND_OVERLAP_DISJOINT_EXT    , "DISJOINT_EXT")
  , (BLEND_OVERLAP_CONJOINT_EXT    , "CONJOINT_EXT")
  ]


instance Show BlendOverlapEXT where
showsPrec = enumShowsPrec enumPrefixBlendOverlapEXT
                          showTableBlendOverlapEXT
                          conNameBlendOverlapEXT
                          (\(BlendOverlapEXT x) -> x)
                          (showsPrec 11)


instance Read BlendOverlapEXT where
  readPrec = enumReadPrec enumPrefixBlendOverlapEXT showTableBlendOverlapEXT conNameBlendOverlapEXT BlendOverlapEXT


type EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION"
pattern EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2


type EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME = "VK_EXT_blend_operation_advanced"

-- No documentation found for TopLevel "VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME"
pattern EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME = "VK_EXT_blend_operation_advanced"

