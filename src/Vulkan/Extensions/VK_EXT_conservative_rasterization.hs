{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_conservative_rasterization  ( PhysicalDeviceConservativeRasterizationPropertiesEXT(..)
                                                            , PipelineRasterizationConservativeStateCreateInfoEXT(..)
                                                            , PipelineRasterizationConservativeStateCreateFlagsEXT(..)
                                                            , ConservativeRasterizationModeEXT( CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT
                                                                                              , CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT
                                                                                              , CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT
                                                                                              , ..
                                                                                              )
                                                            , EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
                                                            , pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
                                                            , EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
                                                            , pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
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
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.BaseType (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT))
-- | VkPhysicalDeviceConservativeRasterizationPropertiesEXT - Structure
-- describing conservative raster properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the
-- 'PhysicalDeviceConservativeRasterizationPropertiesEXT' structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceConservativeRasterizationPropertiesEXT' structure
-- is included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits and properties.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceConservativeRasterizationPropertiesEXT = PhysicalDeviceConservativeRasterizationPropertiesEXT
  { -- | @primitiveOverestimationSize@ is the size in pixels the generating
    -- primitive is increased at each of its edges during conservative
    -- rasterization overestimation mode. Even with a size of 0.0, conservative
    -- rasterization overestimation rules still apply and if any part of the
    -- pixel rectangle is covered by the generating primitive, fragments are
    -- generated for the entire pixel. However implementations /may/ make the
    -- pixel coverage area even more conservative by increasing the size of the
    -- generating primitive.
    primitiveOverestimationSize :: Float
  , -- | @maxExtraPrimitiveOverestimationSize@ is the maximum size in pixels of
    -- extra overestimation the implementation supports in the pipeline state.
    -- A value of 0.0 means the implementation does not support any additional
    -- overestimation of the generating primitive during conservative
    -- rasterization. A value above 0.0 allows the application to further
    -- increase the size of the generating primitive during conservative
    -- rasterization overestimation.
    maxExtraPrimitiveOverestimationSize :: Float
  , -- | @extraPrimitiveOverestimationSizeGranularity@ is the granularity of
    -- extra overestimation that can be specified in the pipeline state between
    -- 0.0 and @maxExtraPrimitiveOverestimationSize@ inclusive. A value of 0.0
    -- means the implementation can use the smallest representable non-zero
    -- value in the screen space pixel fixed-point grid.
    extraPrimitiveOverestimationSizeGranularity :: Float
  , -- | @primitiveUnderestimation@ is true if the implementation supports the
    -- 'CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT' conservative
    -- rasterization mode in addition to
    -- 'CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT'. Otherwise the
    -- implementation only supports
    -- 'CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT'.
    primitiveUnderestimation :: Bool
  , -- | @conservativePointAndLineRasterization@ is true if the implementation
    -- supports conservative rasterization of point and line primitives as well
    -- as triangle primitives. Otherwise the implementation only supports
    -- triangle primitives.
    conservativePointAndLineRasterization :: Bool
  , -- | @degenerateTrianglesRasterized@ is false if the implementation culls
    -- primitives generated from triangles that become zero area after they are
    -- quantized to the fixed-point rasterization pixel grid.
    -- @degenerateTrianglesRasterized@ is true if these primitives are not
    -- culled and the provoking vertex attributes and depth value are used for
    -- the fragments. The primitive area calculation is done on the primitive
    -- generated from the clipped triangle if applicable. Zero area primitives
    -- are backfacing and the application /can/ enable backface culling if
    -- desired.
    degenerateTrianglesRasterized :: Bool
  , -- | @degenerateLinesRasterized@ is false if the implementation culls lines
    -- that become zero length after they are quantized to the fixed-point
    -- rasterization pixel grid. @degenerateLinesRasterized@ is true if zero
    -- length lines are not culled and the provoking vertex attributes and
    -- depth value are used for the fragments.
    degenerateLinesRasterized :: Bool
  , -- | @fullyCoveredFragmentShaderInputVariable@ is true if the implementation
    -- supports the SPIR-V builtin fragment shader input variable
    -- @FullyCoveredEXT@ which specifies that conservative rasterization is
    -- enabled and the fragment area is fully covered by the generating
    -- primitive.
    fullyCoveredFragmentShaderInputVariable :: Bool
  , -- | @conservativeRasterizationPostDepthCoverage@ is true if the
    -- implementation supports conservative rasterization with the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-fragment-earlytest-postdepthcoverage PostDepthCoverage>
    -- execution mode enabled. When supported the
    -- 'Vulkan.Core10.BaseType.SampleMask' built-in input variable will reflect
    -- the coverage after the early per-fragment depth and stencil tests are
    -- applied even when conservative rasterization is enabled. Otherwise
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-fragment-earlytest-postdepthcoverage PostDepthCoverage>
    -- execution mode /must/ not be used when conservative rasterization is
    -- enabled.
    conservativeRasterizationPostDepthCoverage :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceConservativeRasterizationPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceConservativeRasterizationPropertiesEXT

instance ToCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceConservativeRasterizationPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (primitiveOverestimationSize))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (maxExtraPrimitiveOverestimationSize))
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (extraPrimitiveOverestimationSizeGranularity))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (primitiveUnderestimation))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (conservativePointAndLineRasterization))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (degenerateTrianglesRasterized))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (degenerateLinesRasterized))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (fullyCoveredFragmentShaderInputVariable))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (conservativeRasterizationPostDepthCoverage))
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT where
  peekCStruct p = do
    primitiveOverestimationSize <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    maxExtraPrimitiveOverestimationSize <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    extraPrimitiveOverestimationSizeGranularity <- peek @CFloat ((p `plusPtr` 24 :: Ptr CFloat))
    primitiveUnderestimation <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    conservativePointAndLineRasterization <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    degenerateTrianglesRasterized <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    degenerateLinesRasterized <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    fullyCoveredFragmentShaderInputVariable <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    conservativeRasterizationPostDepthCoverage <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    pure $ PhysicalDeviceConservativeRasterizationPropertiesEXT
             ((\(CFloat a) -> a) primitiveOverestimationSize) ((\(CFloat a) -> a) maxExtraPrimitiveOverestimationSize) ((\(CFloat a) -> a) extraPrimitiveOverestimationSizeGranularity) (bool32ToBool primitiveUnderestimation) (bool32ToBool conservativePointAndLineRasterization) (bool32ToBool degenerateTrianglesRasterized) (bool32ToBool degenerateLinesRasterized) (bool32ToBool fullyCoveredFragmentShaderInputVariable) (bool32ToBool conservativeRasterizationPostDepthCoverage)

instance Storable PhysicalDeviceConservativeRasterizationPropertiesEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceConservativeRasterizationPropertiesEXT where
  zero = PhysicalDeviceConservativeRasterizationPropertiesEXT
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPipelineRasterizationConservativeStateCreateInfoEXT - Structure
-- specifying conservative raster state
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'ConservativeRasterizationModeEXT',
-- 'PipelineRasterizationConservativeStateCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRasterizationConservativeStateCreateInfoEXT = PipelineRasterizationConservativeStateCreateInfoEXT
  { -- | @flags@ is reserved for future use.
    --
    -- @flags@ /must/ be @0@
    flags :: PipelineRasterizationConservativeStateCreateFlagsEXT
  , -- | @conservativeRasterizationMode@ is the conservative rasterization mode
    -- to use.
    --
    -- @conservativeRasterizationMode@ /must/ be a valid
    -- 'ConservativeRasterizationModeEXT' value
    conservativeRasterizationMode :: ConservativeRasterizationModeEXT
  , -- | @extraPrimitiveOverestimationSize@ is the extra size in pixels to
    -- increase the generating primitive during conservative rasterization at
    -- each of its edges in @X@ and @Y@ equally in screen space beyond the base
    -- overestimation specified in
    -- 'PhysicalDeviceConservativeRasterizationPropertiesEXT'::@primitiveOverestimationSize@.
    --
    -- @extraPrimitiveOverestimationSize@ /must/ be in the range of @0.0@ to
    -- 'PhysicalDeviceConservativeRasterizationPropertiesEXT'::@maxExtraPrimitiveOverestimationSize@
    -- inclusive
    extraPrimitiveOverestimationSize :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRasterizationConservativeStateCreateInfoEXT)
#endif
deriving instance Show PipelineRasterizationConservativeStateCreateInfoEXT

instance ToCStruct PipelineRasterizationConservativeStateCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationConservativeStateCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRasterizationConservativeStateCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr ConservativeRasterizationModeEXT)) (conservativeRasterizationMode)
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (extraPrimitiveOverestimationSize))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr ConservativeRasterizationModeEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct PipelineRasterizationConservativeStateCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @PipelineRasterizationConservativeStateCreateFlagsEXT ((p `plusPtr` 16 :: Ptr PipelineRasterizationConservativeStateCreateFlagsEXT))
    conservativeRasterizationMode <- peek @ConservativeRasterizationModeEXT ((p `plusPtr` 20 :: Ptr ConservativeRasterizationModeEXT))
    extraPrimitiveOverestimationSize <- peek @CFloat ((p `plusPtr` 24 :: Ptr CFloat))
    pure $ PipelineRasterizationConservativeStateCreateInfoEXT
             flags conservativeRasterizationMode ((\(CFloat a) -> a) extraPrimitiveOverestimationSize)

instance Storable PipelineRasterizationConservativeStateCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationConservativeStateCreateInfoEXT where
  zero = PipelineRasterizationConservativeStateCreateInfoEXT
           zero
           zero
           zero


-- | VkPipelineRasterizationConservativeStateCreateFlagsEXT - Reserved for
-- future use
--
-- = Description
--
-- 'PipelineRasterizationConservativeStateCreateFlagsEXT' is a bitmask type
-- for setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'PipelineRasterizationConservativeStateCreateInfoEXT'
newtype PipelineRasterizationConservativeStateCreateFlagsEXT = PipelineRasterizationConservativeStateCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show PipelineRasterizationConservativeStateCreateFlagsEXT where
  showsPrec p = \case
    PipelineRasterizationConservativeStateCreateFlagsEXT x -> showParen (p >= 11) (showString "PipelineRasterizationConservativeStateCreateFlagsEXT 0x" . showHex x)

instance Read PipelineRasterizationConservativeStateCreateFlagsEXT where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineRasterizationConservativeStateCreateFlagsEXT")
                       v <- step readPrec
                       pure (PipelineRasterizationConservativeStateCreateFlagsEXT v)))


-- | VkConservativeRasterizationModeEXT - Specify the conservative
-- rasterization mode
--
-- = See Also
--
-- 'PipelineRasterizationConservativeStateCreateInfoEXT'
newtype ConservativeRasterizationModeEXT = ConservativeRasterizationModeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT' specifies that
-- conservative rasterization is disabled and rasterization proceeds as
-- normal.
pattern CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT = ConservativeRasterizationModeEXT 0
-- | 'CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT' specifies that
-- conservative rasterization is enabled in overestimation mode.
pattern CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT = ConservativeRasterizationModeEXT 1
-- | 'CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT' specifies that
-- conservative rasterization is enabled in underestimation mode.
pattern CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT = ConservativeRasterizationModeEXT 2
{-# complete CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT,
             CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT,
             CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT :: ConservativeRasterizationModeEXT #-}

instance Show ConservativeRasterizationModeEXT where
  showsPrec p = \case
    CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT -> showString "CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT"
    CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT -> showString "CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT"
    CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT -> showString "CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT"
    ConservativeRasterizationModeEXT x -> showParen (p >= 11) (showString "ConservativeRasterizationModeEXT " . showsPrec 11 x)

instance Read ConservativeRasterizationModeEXT where
  readPrec = parens (choose [("CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT", pure CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT)
                            , ("CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT", pure CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT)
                            , ("CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT", pure CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ConservativeRasterizationModeEXT")
                       v <- step readPrec
                       pure (ConservativeRasterizationModeEXT v)))


type EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION"
pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1


type EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_conservative_rasterization"

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME"
pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_conservative_rasterization"

