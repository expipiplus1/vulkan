{-# language CPP #-}
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

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.Core10.BaseType (Bool32)
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
-- | VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT - Structure describing
-- advanced blending features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceBlendOperationAdvancedFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceBlendOperationAdvancedFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceBlendOperationAdvancedFeaturesEXT' /can/ also be included
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable the features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceBlendOperationAdvancedFeaturesEXT = PhysicalDeviceBlendOperationAdvancedFeaturesEXT
  { -- | @advancedBlendCoherentOperations@ specifies whether blending using
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operations>
    -- is guaranteed to execute atomically and in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-order primitive order>.
    -- If this is 'Vulkan.Core10.BaseType.TRUE',
    -- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT'
    -- is treated the same as
    -- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COLOR_ATTACHMENT_READ_BIT',
    -- and advanced blending needs no additional synchronization over basic
    -- blending. If this is 'Vulkan.Core10.BaseType.FALSE', then memory
    -- dependencies are required to guarantee order between two advanced
    -- blending operations that occur on the same sample.
    advancedBlendCoherentOperations :: Bool }
  deriving (Typeable)
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


-- | VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT - Structure
-- describing advanced blending limits that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceBlendOperationAdvancedPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceBlendOperationAdvancedPropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceBlendOperationAdvancedPropertiesEXT = PhysicalDeviceBlendOperationAdvancedPropertiesEXT
  { -- | @advancedBlendMaxColorAttachments@ is one greater than the highest color
    -- attachment index that /can/ be used in a subpass, for a pipeline that
    -- uses an
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>.
    advancedBlendMaxColorAttachments :: Word32
  , -- | @advancedBlendIndependentBlend@ specifies whether advanced blend
    -- operations /can/ vary per-attachment.
    advancedBlendIndependentBlend :: Bool
  , -- | @advancedBlendNonPremultipliedSrcColor@ specifies whether the source
    -- color /can/ be treated as non-premultiplied. If this is
    -- 'Vulkan.Core10.BaseType.FALSE', then
    -- 'PipelineColorBlendAdvancedStateCreateInfoEXT'::@srcPremultiplied@
    -- /must/ be 'Vulkan.Core10.BaseType.TRUE'.
    advancedBlendNonPremultipliedSrcColor :: Bool
  , -- | @advancedBlendNonPremultipliedDstColor@ specifies whether the
    -- destination color /can/ be treated as non-premultiplied. If this is
    -- 'Vulkan.Core10.BaseType.FALSE', then
    -- 'PipelineColorBlendAdvancedStateCreateInfoEXT'::@dstPremultiplied@
    -- /must/ be 'Vulkan.Core10.BaseType.TRUE'.
    advancedBlendNonPremultipliedDstColor :: Bool
  , -- | @advancedBlendCorrelatedOverlap@ specifies whether the overlap mode
    -- /can/ be treated as correlated. If this is
    -- 'Vulkan.Core10.BaseType.FALSE', then
    -- 'PipelineColorBlendAdvancedStateCreateInfoEXT'::@blendOverlap@ /must/ be
    -- 'BLEND_OVERLAP_UNCORRELATED_EXT'.
    advancedBlendCorrelatedOverlap :: Bool
  , -- | @advancedBlendAllOperations@ specifies whether all advanced blend
    -- operation enums are supported. See the valid usage of
    -- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState'.
    advancedBlendAllOperations :: Bool
  }
  deriving (Typeable)
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


-- | VkPipelineColorBlendAdvancedStateCreateInfoEXT - Structure specifying
-- parameters that affect advanced blend operations
--
-- = Description
--
-- If this structure is not present, @srcPremultiplied@ and
-- @dstPremultiplied@ are both considered to be
-- 'Vulkan.Core10.BaseType.TRUE', and @blendOverlap@ is considered to be
-- 'BLEND_OVERLAP_UNCORRELATED_EXT'.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-advancedBlendNonPremultipliedSrcColor non-premultiplied source color>
--     property is not supported, @srcPremultiplied@ /must/ be
--     'Vulkan.Core10.BaseType.TRUE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-advancedBlendNonPremultipliedDstColor non-premultiplied destination color>
--     property is not supported, @dstPremultiplied@ /must/ be
--     'Vulkan.Core10.BaseType.TRUE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-advancedBlendCorrelatedOverlap correlated overlap>
--     property is not supported, @blendOverlap@ /must/ be
--     'BLEND_OVERLAP_UNCORRELATED_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT'
--
-- -   @blendOverlap@ /must/ be a valid 'BlendOverlapEXT' value
--
-- = See Also
--
-- 'BlendOverlapEXT', 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineColorBlendAdvancedStateCreateInfoEXT = PipelineColorBlendAdvancedStateCreateInfoEXT
  { -- | @srcPremultiplied@ specifies whether the source color of the blend
    -- operation is treated as premultiplied.
    srcPremultiplied :: Bool
  , -- | @dstPremultiplied@ specifies whether the destination color of the blend
    -- operation is treated as premultiplied.
    dstPremultiplied :: Bool
  , -- | @blendOverlap@ is a 'BlendOverlapEXT' value specifying how the source
    -- and destination sample’s coverage is correlated.
    blendOverlap :: BlendOverlapEXT
  }
  deriving (Typeable)
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


-- | VkBlendOverlapEXT - Enumerant specifying the blend overlap parameter
--
-- = Description
--
-- \'
--
-- +-----------------------------------+--------------------------------------------------------------------------------------+
-- | Overlap Mode                      | Weighting Equations                                                                  |
-- +===================================+======================================================================================+
-- | 'BLEND_OVERLAP_UNCORRELATED_EXT'  | \[                                              \begin{aligned}                      |
-- |                                   |                                                 p_0(A_s,A_d) & = A_sA_d \\           |
-- |                                   |                                                 p_1(A_s,A_d) & = A_s(1-A_d) \\       |
-- |                                   |                                                 p_2(A_s,A_d) & = A_d(1-A_s) \\       |
-- |                                   |                                               \end{aligned}\]                        |
-- +-----------------------------------+--------------------------------------------------------------------------------------+
-- | 'BLEND_OVERLAP_CONJOINT_EXT'      | \[                                              \begin{aligned}                      |
-- |                                   |                                                 p_0(A_s,A_d) & = min(A_s,A_d) \\     |
-- |                                   |                                                 p_1(A_s,A_d) & = max(A_s-A_d,0) \\   |
-- |                                   |                                                 p_2(A_s,A_d) & = max(A_d-A_s,0) \\   |
-- |                                   |                                               \end{aligned}\]                        |
-- +-----------------------------------+--------------------------------------------------------------------------------------+
-- | 'BLEND_OVERLAP_DISJOINT_EXT'      | \[                                              \begin{aligned}                      |
-- |                                   |                                                 p_0(A_s,A_d) & = max(A_s+A_d-1,0) \\ |
-- |                                   |                                                 p_1(A_s,A_d) & = min(A_s,1-A_d) \\   |
-- |                                   |                                                 p_2(A_s,A_d) & = min(A_d,1-A_s) \\   |
-- |                                   |                                               \end{aligned}\]                        |
-- +-----------------------------------+--------------------------------------------------------------------------------------+
--
-- Advanced Blend Overlap Modes
--
-- = See Also
--
-- 'PipelineColorBlendAdvancedStateCreateInfoEXT'
newtype BlendOverlapEXT = BlendOverlapEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'BLEND_OVERLAP_UNCORRELATED_EXT' specifies that there is no correlation
-- between the source and destination coverage.
pattern BLEND_OVERLAP_UNCORRELATED_EXT = BlendOverlapEXT 0
-- | 'BLEND_OVERLAP_DISJOINT_EXT' specifies that the source and destination
-- coverage are considered to have minimal overlap.
pattern BLEND_OVERLAP_DISJOINT_EXT = BlendOverlapEXT 1
-- | 'BLEND_OVERLAP_CONJOINT_EXT' specifies that the source and destination
-- coverage are considered to have maximal overlap.
pattern BLEND_OVERLAP_CONJOINT_EXT = BlendOverlapEXT 2
{-# complete BLEND_OVERLAP_UNCORRELATED_EXT,
             BLEND_OVERLAP_DISJOINT_EXT,
             BLEND_OVERLAP_CONJOINT_EXT :: BlendOverlapEXT #-}

instance Show BlendOverlapEXT where
  showsPrec p = \case
    BLEND_OVERLAP_UNCORRELATED_EXT -> showString "BLEND_OVERLAP_UNCORRELATED_EXT"
    BLEND_OVERLAP_DISJOINT_EXT -> showString "BLEND_OVERLAP_DISJOINT_EXT"
    BLEND_OVERLAP_CONJOINT_EXT -> showString "BLEND_OVERLAP_CONJOINT_EXT"
    BlendOverlapEXT x -> showParen (p >= 11) (showString "BlendOverlapEXT " . showsPrec 11 x)

instance Read BlendOverlapEXT where
  readPrec = parens (choose [("BLEND_OVERLAP_UNCORRELATED_EXT", pure BLEND_OVERLAP_UNCORRELATED_EXT)
                            , ("BLEND_OVERLAP_DISJOINT_EXT", pure BLEND_OVERLAP_DISJOINT_EXT)
                            , ("BLEND_OVERLAP_CONJOINT_EXT", pure BLEND_OVERLAP_CONJOINT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "BlendOverlapEXT")
                       v <- step readPrec
                       pure (BlendOverlapEXT v)))


type EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION"
pattern EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2


type EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME = "VK_EXT_blend_operation_advanced"

-- No documentation found for TopLevel "VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME"
pattern EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME = "VK_EXT_blend_operation_advanced"

