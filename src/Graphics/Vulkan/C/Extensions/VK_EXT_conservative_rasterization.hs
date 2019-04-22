{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization
  ( VkConservativeRasterizationModeEXT(..)
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT
  , pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT
  , VkPhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , VkPipelineRasterizationConservativeStateCreateFlagsEXT(..)
  , VkPipelineRasterizationConservativeStateCreateInfoEXT(..)
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  , pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
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
  deriving (Eq, Ord, Storable, Zero)

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

-- | 'VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT' specifies that
-- conservative rasterization is disabled and rasterization proceeds as
-- normal.
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT :: VkConservativeRasterizationModeEXT
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT = VkConservativeRasterizationModeEXT 0

-- | 'VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT' specifies that
-- conservative rasterization is enabled in overestimation mode.
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT :: VkConservativeRasterizationModeEXT
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT = VkConservativeRasterizationModeEXT 1

-- | 'VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT' specifies that
-- conservative rasterization is enabled in underestimation mode.
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT :: VkConservativeRasterizationModeEXT
pattern VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT = VkConservativeRasterizationModeEXT 2

-- | VkPhysicalDeviceConservativeRasterizationPropertiesEXT - Structure
-- describing conservative raster properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the
-- 'VkPhysicalDeviceConservativeRasterizationPropertiesEXT' structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'VkPhysicalDeviceConservativeRasterizationPropertiesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits and properties.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPhysicalDeviceConservativeRasterizationPropertiesEXT = VkPhysicalDeviceConservativeRasterizationPropertiesEXT
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT'
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @primitiveOverestimationSize@ is the size in pixels the generating
  -- primitive is increased at each of its edges during conservative
  -- rasterization overestimation mode. Even with a size of 0.0, conservative
  -- rasterization overestimation rules still apply and if any part of the
  -- pixel rectangle is covered by the generating primitive, fragments are
  -- generated for the entire pixel. However implementations /may/ make the
  -- pixel coverage area even more conservative by increasing the size of the
  -- generating primitive.
  vkPrimitiveOverestimationSize :: CFloat
  , -- | @maxExtraPrimitiveOverestimationSize@ is the maximum size in pixels of
  -- extra overestimation the implementation supports in the pipeline state.
  -- A value of 0.0 means the implementation does not support any additional
  -- overestimation of the generating primitive during conservative
  -- rasterization. A value above 0.0 allows the application to further
  -- increase the size of the generating primitive during conservative
  -- rasterization overestimation.
  vkMaxExtraPrimitiveOverestimationSize :: CFloat
  , -- | @extraPrimitiveOverestimationSizeGranularity@ is the granularity of
  -- extra overestimation that can be specified in the pipeline state between
  -- 0.0 and @maxExtraPrimitiveOverestimationSize@ inclusive. A value of 0.0
  -- means the implementation can use the smallest representable non-zero
  -- value in the screen space pixel fixed-point grid.
  vkExtraPrimitiveOverestimationSizeGranularity :: CFloat
  , -- | @primitiveUnderestimation@ is true if the implementation supports the
  -- 'VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT' conservative
  -- rasterization mode in addition to
  -- 'VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT'. Otherwise the
  -- implementation only supports
  -- 'VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT'.
  vkPrimitiveUnderestimation :: VkBool32
  , -- | @conservativePointAndLineRasterization@ is true if the implementation
  -- supports conservative rasterization of point and line primitives as well
  -- as triangle primitives. Otherwise the implementation only supports
  -- triangle primitives.
  vkConservativePointAndLineRasterization :: VkBool32
  , -- | @degenerateTrianglesRasterized@ is false if the implementation culls
  -- primitives generated from triangles that become zero area after they are
  -- quantized to the fixed-point rasterization pixel grid.
  -- @degenerateTrianglesRasterized@ is true if these primitives are not
  -- culled and the provoking vertex attributes and depth value are used for
  -- the fragments. The primitive area calculation is done on the primitive
  -- generated from the clipped triangle if applicable. Zero area primitives
  -- are backfacing and the application /can/ enable backface culling if
  -- desired.
  vkDegenerateTrianglesRasterized :: VkBool32
  , -- | @degenerateLinesRasterized@ is false if the implementation culls lines
  -- that become zero length after they are quantized to the fixed-point
  -- rasterization pixel grid. @degenerateLinesRasterized@ is true if zero
  -- length lines are not culled and the provoking vertex attributes and
  -- depth value are used for the fragments.
  vkDegenerateLinesRasterized :: VkBool32
  , -- | @fullyCoveredFragmentShaderInputVariable@ is true if the implementation
  -- supports the SPIR-V builtin fragment shader input variable
  -- FullyCoveredEXT which specifies that conservative rasterization is
  -- enabled and the fragment area is fully covered by the generating
  -- primitive.
  vkFullyCoveredFragmentShaderInputVariable :: VkBool32
  , -- | @conservativeRasterizationPostDepthCoverage@ /must/ be false.
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

instance Zero VkPhysicalDeviceConservativeRasterizationPropertiesEXT where
  zero = VkPhysicalDeviceConservativeRasterizationPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
                                                                zero
                                                                zero
                                                                zero
                                                                zero
                                                                zero
                                                                zero
                                                                zero
                                                                zero
                                                                zero
                                                                zero

-- ** VkPipelineRasterizationConservativeStateCreateFlagsEXT

-- | VkPipelineRasterizationConservativeStateCreateFlagsEXT - Reserved for
-- future use
--
-- = Description
--
-- 'VkPipelineRasterizationConservativeStateCreateFlagsEXT' is a bitmask
-- type for setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'VkPipelineRasterizationConservativeStateCreateInfoEXT'
newtype VkPipelineRasterizationConservativeStateCreateFlagsEXT = VkPipelineRasterizationConservativeStateCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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



-- | VkPipelineRasterizationConservativeStateCreateInfoEXT - Structure
-- specifying conservative raster state
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkConservativeRasterizationModeEXT',
-- 'VkPipelineRasterizationConservativeStateCreateFlagsEXT',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPipelineRasterizationConservativeStateCreateInfoEXT = VkPipelineRasterizationConservativeStateCreateInfoEXT
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT'
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ /must/ be @0@
  vkFlags :: VkPipelineRasterizationConservativeStateCreateFlagsEXT
  , -- | @conservativeRasterizationMode@ /must/ be a valid
  -- 'VkConservativeRasterizationModeEXT' value
  vkConservativeRasterizationMode :: VkConservativeRasterizationModeEXT
  , -- | @extraPrimitiveOverestimationSize@ /must/ be in the range of @0.0@ to
  -- 'VkPhysicalDeviceConservativeRasterizationPropertiesEXT'::@maxExtraPrimitiveOverestimationSize@
  -- inclusive
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

instance Zero VkPipelineRasterizationConservativeStateCreateInfoEXT where
  zero = VkPipelineRasterizationConservativeStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
                                                               zero
                                                               zero
                                                               zero
                                                               zero

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME"
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_conservative_rasterization"

-- No documentation found for TopLevel "VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION"
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION :: Integral a => a
pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT = VkStructureType 1000101000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT = VkStructureType 1000101001
