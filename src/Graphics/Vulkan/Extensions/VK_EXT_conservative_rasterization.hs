{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization
  ( VkConservativeRasterizationModeEXT(..)
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT
  , VkPipelineRasterizationConservativeStateCreateFlagsEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  , VkPhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , VkPipelineRasterizationConservativeStateCreateInfoEXT(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , VkFlags
  )


-- ** VkConservativeRasterizationModeEXT

-- | VkConservativeRasterizationModeEXT - Specify the conservative
-- rasterization mode
--
-- = See Also
--
-- 'VkPipelineRasterizationConservativeStateCreateInfoEXT'
newtype VkConservativeRasterizationModeEXT = VkConservativeRasterizationModeEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkConservativeRasterizationModeEXT where
  showsPrec _ VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT = showString "VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT"
  showsPrec _ VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT = showString "VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT"
  showsPrec _ VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT = showString "VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT"
  showsPrec p (VkConservativeRasterizationModeEXT x) = showParen (p >= 11) (showString "VkConservativeRasterizationModeEXT " . showsPrec 11 x)

instance Read VkConservativeRasterizationModeEXT where
  readPrec = parens ( choose [ ("VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT",      pure VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT)
                             , ("VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT",  pure VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT)
                             , ("VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT", pure VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkConservativeRasterizationModeEXT")
                        v <- step readPrec
                        pure (VkConservativeRasterizationModeEXT v)
                        )
                    )

-- | @VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT@ specifies that
-- conservative rasterization is disabled and rasterization proceeds as
-- normal.
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT :: VkConservativeRasterizationModeEXT
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT = VkConservativeRasterizationModeEXT 0

-- | @VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT@ specifies that
-- conservative rasterization is enabled in overestimation mode.
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT :: VkConservativeRasterizationModeEXT
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT = VkConservativeRasterizationModeEXT 1

-- | @VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT@ specifies that
-- conservative rasterization is enabled in underestimation mode.
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT :: VkConservativeRasterizationModeEXT
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT = VkConservativeRasterizationModeEXT 2
-- ** VkPipelineRasterizationConservativeStateCreateFlagsEXT

-- | VkPipelineRasterizationConservativeStateCreateFlagsEXT - Reserved for
-- future use
--
-- = Description
--
-- @VkPipelineRasterizationConservativeStateCreateFlagsEXT@ is a bitmask
-- type for setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'VkPipelineRasterizationConservativeStateCreateInfoEXT'
newtype VkPipelineRasterizationConservativeStateCreateFlagsEXT = VkPipelineRasterizationConservativeStateCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineRasterizationConservativeStateCreateFlagsEXT where
  
  showsPrec p (VkPipelineRasterizationConservativeStateCreateFlagsEXT x) = showParen (p >= 11) (showString "VkPipelineRasterizationConservativeStateCreateFlagsEXT " . showsPrec 11 x)

instance Read VkPipelineRasterizationConservativeStateCreateFlagsEXT where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineRasterizationConservativeStateCreateFlagsEXT")
                        v <- step readPrec
                        pure (VkPipelineRasterizationConservativeStateCreateFlagsEXT v)
                        )
                    )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT = VkStructureType 1000101000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT = VkStructureType 1000101001
-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION"
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION :: Integral a => a
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME"
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_conservative_rasterization"
-- | VkPhysicalDeviceConservativeRasterizationPropertiesEXT - Structure
-- describing conservative raster properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the
-- @VkPhysicalDeviceConservativeRasterizationPropertiesEXT@ structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @primitiveOverestimationSize@ is the size in pixels the generating
--     primitive is increased at each of its edges during conservative
--     rasterization overestimation mode. Even with a size of 0.0,
--     conservative rasterization overestimation rules still apply and if
--     any part of the pixel rectangle is covered by the generating
--     primitive, fragments are generated for the entire pixel. However
--     implementations /may/ make the pixel coverage area even more
--     conservative by increasing the size of the generating primitive.
--
-- -   @maxExtraPrimitiveOverestimationSize@ is the maximum size in pixels
--     of extra overestimation the implementation supports in the pipeline
--     state. A value of 0.0 means the implementation does not support any
--     additional overestimation of the generating primitive during
--     conservative rasterization. A value above 0.0 allows the application
--     to further increase the size of the generating primitive during
--     conservative rasterization overestimation.
--
-- -   @extraPrimitiveOverestimationSizeGranularity@ is the granularity of
--     extra overestimation that can be specified in the pipeline state
--     between 0.0 and @maxExtraPrimitiveOverestimationSize@ inclusive. A
--     value of 0.0 means the implementation can use the smallest
--     representable non-zero value in the screen space pixel fixed-point
--     grid.
--
-- -   @primitiveUnderestimation@ is true if the implementation supports
--     the @VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT@
--     conservative rasterization mode in addition to
--     @VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT@. Otherwise the
--     implementation only supports
--     @VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT@.
--
-- -   @conservativePointAndLineRasterization@ is true if the
--     implementation supports conservative rasterization of point and line
--     primitives as well as triangle primitives. Otherwise the
--     implementation only supports triangle primitives.
--
-- -   @degenerateTrianglesRasterized@ is false if the implementation culls
--     primitives generated from triangles that become zero area after they
--     are quantized to the fixed-point rasterization pixel grid.
--     @degenerateTrianglesRasterized@ is true if these primitives are not
--     culled and the provoking vertex attributes and depth value are used
--     for the fragments. The primitive area calculation is done on the
--     primitive generated from the clipped triangle if applicable. Zero
--     area primitives are backfacing and the application /can/ enable
--     backface culling if desired.
--
-- -   @degenerateLinesRasterized@ is false if the implementation culls
--     lines that become zero length after they are quantized to the
--     fixed-point rasterization pixel grid. @degenerateLinesRasterized@ is
--     true if zero length lines are not culled and the provoking vertex
--     attributes and depth value are used for the fragments.
--
-- -   @fullyCoveredFragmentShaderInputVariable@ is true if the
--     implementation supports the SPIR-V builtin fragment shader input
--     variable FullyCoveredEXT which specifies that conservative
--     rasterization is enabled and the fragment pixel square is fully
--     covered by the generating primitive.
--
-- -   @conservativeRasterizationPostDepthCoverage@ is true if the
--     implementation supports conservative rasterization with the
--     [@PostDepthCoverage@](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#shaders-fragment-earlytest-postdepthcoverage)
--     execution mode enabled. When supported the @SampleMask@ built-in
--     input variable will reflect the coverage after the early
--     per-fragment depth and stencil tests are applied even when
--     conservative rasterization is enabled. Otherwise
--     [@PostDepthCoverage@](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#shaders-fragment-earlytest-postdepthcoverage)
--     execution mode /must/ not be used when conservative rasterization is
--     enabled.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT@
--
-- If the @VkPhysicalDeviceConservativeRasterizationPropertiesEXT@
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2KHR',
-- it is filled with the implementation-dependent limits and properties.
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceConservativeRasterizationPropertiesEXT = VkPhysicalDeviceConservativeRasterizationPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "primitiveOverestimationSize"
  vkPrimitiveOverestimationSize :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "maxExtraPrimitiveOverestimationSize"
  vkMaxExtraPrimitiveOverestimationSize :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "extraPrimitiveOverestimationSizeGranularity"
  vkExtraPrimitiveOverestimationSizeGranularity :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "primitiveUnderestimation"
  vkPrimitiveUnderestimation :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "conservativePointAndLineRasterization"
  vkConservativePointAndLineRasterization :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "degenerateTrianglesRasterized"
  vkDegenerateTrianglesRasterized :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "degenerateLinesRasterized"
  vkDegenerateLinesRasterized :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "fullyCoveredFragmentShaderInputVariable"
  vkFullyCoveredFragmentShaderInputVariable :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceConservativeRasterizationPropertiesEXT" "conservativeRasterizationPostDepthCoverage"
  vkConservativeRasterizationPostDepthCoverage :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceConservativeRasterizationPropertiesEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceConservativeRasterizationPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                                    <*> peek (ptr `plusPtr` 8)
                                                                    <*> peek (ptr `plusPtr` 16)
                                                                    <*> peek (ptr `plusPtr` 20)
                                                                    <*> peek (ptr `plusPtr` 24)
                                                                    <*> peek (ptr `plusPtr` 28)
                                                                    <*> peek (ptr `plusPtr` 32)
                                                                    <*> peek (ptr `plusPtr` 36)
                                                                    <*> peek (ptr `plusPtr` 40)
                                                                    <*> peek (ptr `plusPtr` 44)
                                                                    <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkPrimitiveOverestimationSize (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkMaxExtraPrimitiveOverestimationSize (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkExtraPrimitiveOverestimationSizeGranularity (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkPrimitiveUnderestimation (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 32) (vkConservativePointAndLineRasterization (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkDegenerateTrianglesRasterized (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 40) (vkDegenerateLinesRasterized (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 44) (vkFullyCoveredFragmentShaderInputVariable (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
                *> poke (ptr `plusPtr` 48) (vkConservativeRasterizationPostDepthCoverage (poked :: VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
-- | VkPipelineRasterizationConservativeStateCreateInfoEXT - Structure
-- specifying conservative raster state
--
-- == Valid Usage
--
-- -   @extraPrimitiveOverestimationSize@ /must/ be in the range of @0.0@
--     to
--     @VkPhysicalDeviceConservativeRasterizationPropertiesEXT@::@maxExtraPrimitiveOverestimationSize@
--     inclusive
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT@
--
-- -   @flags@ /must/ be @0@
--
-- -   @conservativeRasterizationMode@ /must/ be a valid
--     'VkConservativeRasterizationModeEXT' value
--
-- = See Also
--
-- 'VkConservativeRasterizationModeEXT',
-- 'VkPipelineRasterizationConservativeStateCreateFlagsEXT',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineRasterizationConservativeStateCreateInfoEXT = VkPipelineRasterizationConservativeStateCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineRasterizationConservativeStateCreateFlagsEXT
  , -- | @conservativeRasterizationMode@ is the conservative rasterization mode
  -- to use.
  vkConservativeRasterizationMode :: VkConservativeRasterizationModeEXT
  , -- | @extraPrimitiveOverestimationSize@ is the extra size in pixels to
  -- increase the generating primitive during conservative rasterization at
  -- each of its edges in @X@ and @Y@ equally in screen space beyond the base
  -- overestimation specified in
  -- @VkPhysicalDeviceConservativeRasterizationPropertiesEXT@::@primitiveOverestimationSize@.
  vkExtraPrimitiveOverestimationSize :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkPipelineRasterizationConservativeStateCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineRasterizationConservativeStateCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                                   <*> peek (ptr `plusPtr` 8)
                                                                   <*> peek (ptr `plusPtr` 16)
                                                                   <*> peek (ptr `plusPtr` 20)
                                                                   <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkConservativeRasterizationMode (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkExtraPrimitiveOverestimationSize (poked :: VkPipelineRasterizationConservativeStateCreateInfoEXT))
