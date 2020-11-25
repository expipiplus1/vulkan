{-# language CPP #-}
-- | = Name
--
-- VK_EXT_conservative_rasterization - device extension
--
-- == VK_EXT_conservative_rasterization
--
-- [__Name String__]
--     @VK_EXT_conservative_rasterization@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     102
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
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_conservative_rasterization:%20&body=@pdaniell-nv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-06-09
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_fragment_fully_covered.html SPV_EXT_fragment_fully_covered>
--         if the
--         'PhysicalDeviceConservativeRasterizationPropertiesEXT'::@fullyCoveredFragmentShaderInputVariable@
--         feature is used.
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_post_depth_coverage.html SPV_KHR_post_depth_coverage>if
--         the
--         'PhysicalDeviceConservativeRasterizationPropertiesEXT'::@conservativeRasterizationPostDepthCoverage@
--         feature is used.
--
--     -   This extension provides API support for
--         <https://www.khronos.org/registry/OpenGL/extensions/NV/NV_conservative_raster_underestimation.txt GL_NV_conservative_raster_underestimation>
--         if the
--         'PhysicalDeviceConservativeRasterizationPropertiesEXT'::@fullyCoveredFragmentShaderInputVariable@
--         feature is used.
--
-- [__Contributors__]
--
--     -   Daniel Koch, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Slawomir Grajewski, Intel
--
--     -   Stu Smith, Imagination Technologies
--
-- == Description
--
-- This extension adds a new rasterization mode called conservative
-- rasterization. There are two modes of conservative rasterization;
-- overestimation and underestimation.
--
-- When overestimation is enabled, if any part of the primitive, including
-- its edges, covers any part of the rectangular pixel area, including its
-- sides, then a fragment is generated with all coverage samples turned on.
-- This extension allows for some variation in implementations by
-- accounting for differences in overestimation, where the generating
-- primitive size is increased at each of its edges by some sub-pixel
-- amount to further increase conservative pixel coverage. Implementations
-- can allow the application to specify an extra overestimation beyond the
-- base overestimation the implementation already does. It also allows
-- implementations to either cull degenerate primitives or rasterize them.
--
-- When underestimation is enabled, fragments are only generated if the
-- rectangular pixel area is fully covered by the generating primitive. If
-- supported by the implementation, when a pixel rectangle is fully covered
-- the fragment shader input variable builtin called FullyCoveredEXT is set
-- to true. The shader variable works in either overestimation or
-- underestimation mode.
--
-- Implementations can process degenerate triangles and lines by either
-- discarding them or generating conservative fragments for them.
-- Degenerate triangles are those that end up with zero area after the
-- rasterizer quantizes them to the fixed-point pixel grid. Degenerate
-- lines are those with zero length after quantization.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceConservativeRasterizationPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationConservativeStateCreateInfoEXT'
--
-- == New Enums
--
-- -   'ConservativeRasterizationModeEXT'
--
-- == New Bitmasks
--
-- -   'PipelineRasterizationConservativeStateCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME'
--
-- -   'EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1.1, 2020-09-06 (Piers Daniell)
--
--     -   Add missing SPIR-V and GLSL dependencies.
--
-- -   Revision 1, 2017-08-28 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'ConservativeRasterizationModeEXT',
-- 'PhysicalDeviceConservativeRasterizationPropertiesEXT',
-- 'PipelineRasterizationConservativeStateCreateFlagsEXT',
-- 'PipelineRasterizationConservativeStateCreateInfoEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conservative_rasterization Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceConservativeRasterizationPropertiesEXT = PhysicalDeviceConservativeRasterizationPropertiesEXT
  { -- | #limits-primitiveOverestimationSize# @primitiveOverestimationSize@ is
    -- the size in pixels the generating primitive is increased at each of its
    -- edges during conservative rasterization overestimation mode. Even with a
    -- size of 0.0, conservative rasterization overestimation rules still apply
    -- and if any part of the pixel rectangle is covered by the generating
    -- primitive, fragments are generated for the entire pixel. However
    -- implementations /may/ make the pixel coverage area even more
    -- conservative by increasing the size of the generating primitive.
    primitiveOverestimationSize :: Float
  , -- | #limits-maxExtraPrimitiveOverestimationSize#
    -- @maxExtraPrimitiveOverestimationSize@ is the maximum size in pixels of
    -- extra overestimation the implementation supports in the pipeline state.
    -- A value of 0.0 means the implementation does not support any additional
    -- overestimation of the generating primitive during conservative
    -- rasterization. A value above 0.0 allows the application to further
    -- increase the size of the generating primitive during conservative
    -- rasterization overestimation.
    maxExtraPrimitiveOverestimationSize :: Float
  , -- | #limits-extraPrimitiveOverestimationSizeGranularity#
    -- @extraPrimitiveOverestimationSizeGranularity@ is the granularity of
    -- extra overestimation that can be specified in the pipeline state between
    -- 0.0 and @maxExtraPrimitiveOverestimationSize@ inclusive. A value of 0.0
    -- means the implementation can use the smallest representable non-zero
    -- value in the screen space pixel fixed-point grid.
    extraPrimitiveOverestimationSizeGranularity :: Float
  , -- | #limits-primitiveUnderestimation# @primitiveUnderestimation@ is true if
    -- the implementation supports the
    -- 'CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT' conservative
    -- rasterization mode in addition to
    -- 'CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT'. Otherwise the
    -- implementation only supports
    -- 'CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT'.
    primitiveUnderestimation :: Bool
  , -- | #limits-conservativePointAndLineRasterization#
    -- @conservativePointAndLineRasterization@ is true if the implementation
    -- supports conservative rasterization of point and line primitives as well
    -- as triangle primitives. Otherwise the implementation only supports
    -- triangle primitives.
    conservativePointAndLineRasterization :: Bool
  , -- | #limits-degenerateTrianglesRasterized# @degenerateTrianglesRasterized@
    -- is false if the implementation culls primitives generated from triangles
    -- that become zero area after they are quantized to the fixed-point
    -- rasterization pixel grid. @degenerateTrianglesRasterized@ is true if
    -- these primitives are not culled and the provoking vertex attributes and
    -- depth value are used for the fragments. The primitive area calculation
    -- is done on the primitive generated from the clipped triangle if
    -- applicable. Zero area primitives are backfacing and the application
    -- /can/ enable backface culling if desired.
    degenerateTrianglesRasterized :: Bool
  , -- | #limits-degenerateLinesRasterized# @degenerateLinesRasterized@ is false
    -- if the implementation culls lines that become zero length after they are
    -- quantized to the fixed-point rasterization pixel grid.
    -- @degenerateLinesRasterized@ is true if zero length lines are not culled
    -- and the provoking vertex attributes and depth value are used for the
    -- fragments.
    degenerateLinesRasterized :: Bool
  , -- | #limits-fullyCoveredFragmentShaderInputVariable#
    -- @fullyCoveredFragmentShaderInputVariable@ is true if the implementation
    -- supports the SPIR-V builtin fragment shader input variable
    -- @FullyCoveredEXT@ which specifies that conservative rasterization is
    -- enabled and the fragment area is fully covered by the generating
    -- primitive.
    fullyCoveredFragmentShaderInputVariable :: Bool
  , -- | #limits-conservativeRasterizationPostDepthCoverage#
    -- @conservativeRasterizationPostDepthCoverage@ is true if the
    -- implementation supports conservative rasterization with the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-fragment-earlytest-postdepthcoverage PostDepthCoverage>
    -- execution mode enabled. When supported the
    -- 'Vulkan.Core10.FundamentalTypes.SampleMask' built-in input variable will
    -- reflect the coverage after the early per-fragment depth and stencil
    -- tests are applied even when conservative rasterization is enabled.
    -- Otherwise
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
    -- #VUID-VkPipelineRasterizationConservativeStateCreateInfoEXT-flags-zerobitmask#
    -- @flags@ /must/ be @0@
    flags :: PipelineRasterizationConservativeStateCreateFlagsEXT
  , -- | @conservativeRasterizationMode@ is the conservative rasterization mode
    -- to use.
    --
    -- #VUID-VkPipelineRasterizationConservativeStateCreateInfoEXT-conservativeRasterizationMode-parameter#
    -- @conservativeRasterizationMode@ /must/ be a valid
    -- 'ConservativeRasterizationModeEXT' value
    conservativeRasterizationMode :: ConservativeRasterizationModeEXT
  , -- | @extraPrimitiveOverestimationSize@ is the extra size in pixels to
    -- increase the generating primitive during conservative rasterization at
    -- each of its edges in @X@ and @Y@ equally in screen space beyond the base
    -- overestimation specified in
    -- 'PhysicalDeviceConservativeRasterizationPropertiesEXT'::@primitiveOverestimationSize@.
    --
    -- #VUID-VkPipelineRasterizationConservativeStateCreateInfoEXT-extraPrimitiveOverestimationSize-01769#
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
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineRasterizationConservativeStateCreateFlagsEXT :: String
conNamePipelineRasterizationConservativeStateCreateFlagsEXT = "PipelineRasterizationConservativeStateCreateFlagsEXT"

enumPrefixPipelineRasterizationConservativeStateCreateFlagsEXT :: String
enumPrefixPipelineRasterizationConservativeStateCreateFlagsEXT = ""

showTablePipelineRasterizationConservativeStateCreateFlagsEXT
  :: [(PipelineRasterizationConservativeStateCreateFlagsEXT, String)]
showTablePipelineRasterizationConservativeStateCreateFlagsEXT = []

instance Show PipelineRasterizationConservativeStateCreateFlagsEXT where
  showsPrec = enumShowsPrec enumPrefixPipelineRasterizationConservativeStateCreateFlagsEXT
                            showTablePipelineRasterizationConservativeStateCreateFlagsEXT
                            conNamePipelineRasterizationConservativeStateCreateFlagsEXT
                            (\(PipelineRasterizationConservativeStateCreateFlagsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineRasterizationConservativeStateCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixPipelineRasterizationConservativeStateCreateFlagsEXT
                          showTablePipelineRasterizationConservativeStateCreateFlagsEXT
                          conNamePipelineRasterizationConservativeStateCreateFlagsEXT
                          PipelineRasterizationConservativeStateCreateFlagsEXT


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
pattern CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT      = ConservativeRasterizationModeEXT 0
-- | 'CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT' specifies that
-- conservative rasterization is enabled in overestimation mode.
pattern CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT  = ConservativeRasterizationModeEXT 1
-- | 'CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT' specifies that
-- conservative rasterization is enabled in underestimation mode.
pattern CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT = ConservativeRasterizationModeEXT 2
{-# complete CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT,
             CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT,
             CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT :: ConservativeRasterizationModeEXT #-}

conNameConservativeRasterizationModeEXT :: String
conNameConservativeRasterizationModeEXT = "ConservativeRasterizationModeEXT"

enumPrefixConservativeRasterizationModeEXT :: String
enumPrefixConservativeRasterizationModeEXT = "CONSERVATIVE_RASTERIZATION_MODE_"

showTableConservativeRasterizationModeEXT :: [(ConservativeRasterizationModeEXT, String)]
showTableConservativeRasterizationModeEXT =
  [ (CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT     , "DISABLED_EXT")
  , (CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT , "OVERESTIMATE_EXT")
  , (CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT, "UNDERESTIMATE_EXT")
  ]

instance Show ConservativeRasterizationModeEXT where
  showsPrec = enumShowsPrec enumPrefixConservativeRasterizationModeEXT
                            showTableConservativeRasterizationModeEXT
                            conNameConservativeRasterizationModeEXT
                            (\(ConservativeRasterizationModeEXT x) -> x)
                            (showsPrec 11)

instance Read ConservativeRasterizationModeEXT where
  readPrec = enumReadPrec enumPrefixConservativeRasterizationModeEXT
                          showTableConservativeRasterizationModeEXT
                          conNameConservativeRasterizationModeEXT
                          ConservativeRasterizationModeEXT


type EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION"
pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1


type EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_conservative_rasterization"

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME"
pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_conservative_rasterization"

