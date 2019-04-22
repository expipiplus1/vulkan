{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( VkCoarseSampleLocationNV(..)
  , VkCoarseSampleOrderCustomNV(..)
  , VkCoarseSampleOrderTypeNV(..)
  , pattern VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV
  , pattern VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV
  , pattern VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV
  , pattern VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV
  , VkPhysicalDeviceShadingRateImageFeaturesNV(..)
  , VkPhysicalDeviceShadingRateImagePropertiesNV(..)
  , VkPipelineViewportCoarseSampleOrderStateCreateInfoNV(..)
  , VkPipelineViewportShadingRateImageStateCreateInfoNV(..)
  , VkShadingRatePaletteEntryNV(..)
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV
  , VkShadingRatePaletteNV(..)
  , FN_vkCmdBindShadingRateImageNV
  , PFN_vkCmdBindShadingRateImageNV
  , vkCmdBindShadingRateImageNV
  , FN_vkCmdSetCoarseSampleOrderNV
  , PFN_vkCmdSetCoarseSampleOrderNV
  , vkCmdSetCoarseSampleOrderNV
  , FN_vkCmdSetViewportShadingRatePaletteNV
  , PFN_vkCmdSetViewportShadingRatePaletteNV
  , vkCmdSetViewportShadingRatePaletteNV
  , pattern VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV
  , pattern VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV
  , pattern VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV
  , pattern VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV
  , pattern VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV
  , pattern VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME
  , pattern VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION
  , pattern VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
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
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageUsageFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkImageView
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkDynamicState(..)
  , VkExtent2D(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkCoarseSampleLocationNV - Structure specifying parameters controlling
-- shading rate image usage
--
-- == Valid Usage
--
-- = See Also
--
-- 'VkCoarseSampleOrderCustomNV'
data VkCoarseSampleLocationNV = VkCoarseSampleLocationNV
  { -- | @pixelX@ /must/ be less than the width (in pixels) of the fragment.
  vkPixelX :: Word32
  , -- | @pixelY@ /must/ be less than the height (in pixels) of the fragment.
  vkPixelY :: Word32
  , -- | @sample@ /must/ be less than the number of coverage samples in each
  -- pixel belonging to the fragment.
  vkSample :: Word32
  }
  deriving (Eq, Show)

instance Storable VkCoarseSampleLocationNV where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkCoarseSampleLocationNV <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPixelX (poked :: VkCoarseSampleLocationNV))
                *> poke (ptr `plusPtr` 4) (vkPixelY (poked :: VkCoarseSampleLocationNV))
                *> poke (ptr `plusPtr` 8) (vkSample (poked :: VkCoarseSampleLocationNV))

instance Zero VkCoarseSampleLocationNV where
  zero = VkCoarseSampleLocationNV zero
                                  zero
                                  zero

-- | VkCoarseSampleOrderCustomNV - Structure specifying parameters
-- controlling shading rate image usage
--
-- = Description
--
-- When using a custom sample ordering, element /i/ in @pSampleLocations@
-- specifies a specific pixel and per-pixel coverage sample number that
-- corresponds to the coverage sample numbered /i/ in the multi-pixel
-- fragment.
--
-- == Valid Usage
--
-- -   @shadingRate@ /must/ be a shading rate that generates fragments with
--     more than one pixel.
--
-- -   @sampleCount@ /must/ correspond to a sample count enumerated in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlags'
--     whose corresponding bit is set in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@framebufferNoAttachmentsSampleCounts@.
--
-- -   @sampleLocationCount@ /must/ be equal to the product of
--     @sampleCount@, the fragment width for @shadingRate@, and the
--     fragment height for @shadingRate@.
--
-- -   @sampleLocationCount@ /must/ be less than or equal to the value of
--     'VkPhysicalDeviceShadingRateImagePropertiesNV'::@shadingRateMaxCoarseSamples@.
--
-- -   The array @pSampleLocations@ /must/ contain exactly one entry for
--     every combination of valid values for @pixelX@, @pixelY@, and
--     @sample@ in the structure 'VkCoarseSampleOrderCustomNV'.
--
-- == Valid Usage (Implicit)
--
-- -   @shadingRate@ /must/ be a valid 'VkShadingRatePaletteEntryNV' value
--
-- -   @pSampleLocations@ /must/ be a valid pointer to an array of
--     @sampleLocationCount@ 'VkCoarseSampleLocationNV' structures
--
-- -   @sampleLocationCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'VkCoarseSampleLocationNV',
-- 'VkPipelineViewportCoarseSampleOrderStateCreateInfoNV',
-- 'VkShadingRatePaletteEntryNV', 'vkCmdSetCoarseSampleOrderNV'
data VkCoarseSampleOrderCustomNV = VkCoarseSampleOrderCustomNV
  { -- | @shadingRate@ is a shading rate palette entry that identifies the
  -- fragment width and height for the combination of fragment area and
  -- per-pixel coverage sample count to control.
  vkShadingRate :: VkShadingRatePaletteEntryNV
  , -- | @sampleCount@ identifies the per-pixel coverage sample count for the
  -- combination of fragment area and coverage sample count to control.
  vkSampleCount :: Word32
  , -- | @sampleLocationCount@ specifies the number of sample locations in the
  -- custom ordering.
  vkSampleLocationCount :: Word32
  , -- | @pSampleLocations@ is a pointer to an array of
  -- 'VkCoarseSampleOrderCustomNV' structures that specifies the location of
  -- each sample in the custom ordering.
  vkPSampleLocations :: Ptr VkCoarseSampleLocationNV
  }
  deriving (Eq, Show)

instance Storable VkCoarseSampleOrderCustomNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkCoarseSampleOrderCustomNV <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 4)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkShadingRate (poked :: VkCoarseSampleOrderCustomNV))
                *> poke (ptr `plusPtr` 4) (vkSampleCount (poked :: VkCoarseSampleOrderCustomNV))
                *> poke (ptr `plusPtr` 8) (vkSampleLocationCount (poked :: VkCoarseSampleOrderCustomNV))
                *> poke (ptr `plusPtr` 16) (vkPSampleLocations (poked :: VkCoarseSampleOrderCustomNV))

instance Zero VkCoarseSampleOrderCustomNV where
  zero = VkCoarseSampleOrderCustomNV zero
                                     zero
                                     zero
                                     zero

-- ** VkCoarseSampleOrderTypeNV

-- | VkCoarseSampleOrderTypeNV - Shading rate image sample ordering types
--
-- = See Also
--
-- 'VkPipelineViewportCoarseSampleOrderStateCreateInfoNV',
-- 'vkCmdSetCoarseSampleOrderNV'
newtype VkCoarseSampleOrderTypeNV = VkCoarseSampleOrderTypeNV Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkCoarseSampleOrderTypeNV where
  showsPrec _ VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV = showString "VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV"
  showsPrec _ VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV = showString "VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV"
  showsPrec _ VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV = showString "VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV"
  showsPrec _ VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV = showString "VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV"
  showsPrec p (VkCoarseSampleOrderTypeNV x) = showParen (p >= 11) (showString "VkCoarseSampleOrderTypeNV " . showsPrec 11 x)

instance Read VkCoarseSampleOrderTypeNV where
  readPrec = parens ( choose [ ("VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV",      pure VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV)
                             , ("VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV",       pure VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV)
                             , ("VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV",  pure VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV)
                             , ("VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV", pure VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCoarseSampleOrderTypeNV")
                        v <- step readPrec
                        pure (VkCoarseSampleOrderTypeNV v)
                        )
                    )

-- | 'VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV' specifies that coverage samples
-- will be ordered in an implementation-dependent manner.
pattern VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV :: VkCoarseSampleOrderTypeNV
pattern VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV = VkCoarseSampleOrderTypeNV 0

-- | 'VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV' specifies that coverage samples
-- will be ordered according to the array of custom orderings provided in
-- either the @pCustomSampleOrders@ member of
-- 'VkPipelineViewportCoarseSampleOrderStateCreateInfoNV' or the
-- @pCustomSampleOrders@ member of 'vkCmdSetCoarseSampleOrderNV'.
pattern VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV :: VkCoarseSampleOrderTypeNV
pattern VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV = VkCoarseSampleOrderTypeNV 1

-- | 'VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV' specifies that coverage
-- samples will be ordered sequentially, sorted first by pixel coordinate
-- (in row-major order) and then by coverage sample number.
pattern VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV :: VkCoarseSampleOrderTypeNV
pattern VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV = VkCoarseSampleOrderTypeNV 2

-- | 'VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV' specifies that coverage
-- samples will be ordered sequentially, sorted first by coverage sample
-- number and then by pixel coordinate (in row-major order).
pattern VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV :: VkCoarseSampleOrderTypeNV
pattern VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV = VkCoarseSampleOrderTypeNV 3

-- | VkPhysicalDeviceShadingRateImageFeaturesNV - Structure describing
-- shading rate image features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceShadingRateImageFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#primsrast-shading-rate-image Shading Rate Image>
-- for more information.
--
-- If the 'VkPhysicalDeviceShadingRateImageFeaturesNV' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'VkPhysicalDeviceShadingRateImageFeaturesNV' /can/ also be used in the
-- @pNext@ chain of 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to
-- enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPhysicalDeviceShadingRateImageFeaturesNV = VkPhysicalDeviceShadingRateImageFeaturesNV
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV'
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceShadingRateImageFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- | @shadingRateImage@ indicates that the implementation supports the use of
  -- a shading rate image to derive an effective shading rate for fragment
  -- processing. It also indicates that the implementation supports the
  -- @ShadingRateNV@ SPIR-V execution mode.
  vkShadingRateImage :: VkBool32
  , -- | @shadingRateCoarseSampleOrder@ indicates that the implementation
  -- supports a user-configurable ordering of coverage samples in fragments
  -- larger than one pixel.
  vkShadingRateCoarseSampleOrder :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceShadingRateImageFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceShadingRateImageFeaturesNV <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
                                                        <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceShadingRateImageFeaturesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceShadingRateImageFeaturesNV))
                *> poke (ptr `plusPtr` 16) (vkShadingRateImage (poked :: VkPhysicalDeviceShadingRateImageFeaturesNV))
                *> poke (ptr `plusPtr` 20) (vkShadingRateCoarseSampleOrder (poked :: VkPhysicalDeviceShadingRateImageFeaturesNV))

instance Zero VkPhysicalDeviceShadingRateImageFeaturesNV where
  zero = VkPhysicalDeviceShadingRateImageFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV
                                                    zero
                                                    zero
                                                    zero

-- | VkPhysicalDeviceShadingRateImagePropertiesNV - Structure describing
-- shading rate image limits that can be supported by an implementation
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceShadingRateImagePropertiesNV'
-- structure describe the following implementation-dependent properties
-- related to the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#primsrast-shading-rate-image shading rate image>
-- feature:
--
-- = Description
--
-- If the 'VkPhysicalDeviceShadingRateImagePropertiesNV' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPhysicalDeviceShadingRateImagePropertiesNV = VkPhysicalDeviceShadingRateImagePropertiesNV
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV'
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @shadingRateTexelSize@ indicates the width and height of the portion of
  -- the framebuffer corresponding to each texel in the shading rate image.
  vkShadingRateTexelSize :: VkExtent2D
  , -- | @shadingRatePaletteSize@ indicates the maximum number of palette entries
  -- supported for the shading rate image.
  vkShadingRatePaletteSize :: Word32
  , -- | @shadingRateMaxCoarseSamples@ specifies the maximum number of coverage
  -- samples supported in a single fragment. If the product of the fragment
  -- size derived from the base shading rate and the number of coverage
  -- samples per pixel exceeds this limit, the final shading rate will be
  -- adjusted so that its product does not exceed the limit.
  vkShadingRateMaxCoarseSamples :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceShadingRateImagePropertiesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceShadingRateImagePropertiesNV <$> peek (ptr `plusPtr` 0)
                                                          <*> peek (ptr `plusPtr` 8)
                                                          <*> peek (ptr `plusPtr` 16)
                                                          <*> peek (ptr `plusPtr` 24)
                                                          <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceShadingRateImagePropertiesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceShadingRateImagePropertiesNV))
                *> poke (ptr `plusPtr` 16) (vkShadingRateTexelSize (poked :: VkPhysicalDeviceShadingRateImagePropertiesNV))
                *> poke (ptr `plusPtr` 24) (vkShadingRatePaletteSize (poked :: VkPhysicalDeviceShadingRateImagePropertiesNV))
                *> poke (ptr `plusPtr` 28) (vkShadingRateMaxCoarseSamples (poked :: VkPhysicalDeviceShadingRateImagePropertiesNV))

instance Zero VkPhysicalDeviceShadingRateImagePropertiesNV where
  zero = VkPhysicalDeviceShadingRateImagePropertiesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV
                                                      zero
                                                      zero
                                                      zero
                                                      zero

-- | VkPipelineViewportCoarseSampleOrderStateCreateInfoNV - Structure
-- specifying parameters controlling sample order in coarse fragments
--
-- = Description
--
-- If this structure is not present, @sampleOrderType@ is considered to be
-- 'VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'.
--
-- If @sampleOrderType@ is 'VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV', the
-- coverage sample order used for any combination of fragment area and
-- coverage sample count not enumerated in @pCustomSampleOrders@ will be
-- identical to that used for 'VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'.
--
-- If the pipeline was created with
-- 'VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV', the contents of this
-- structure (if present) are ignored, and the coverage sample order is
-- instead specified by 'vkCmdSetCoarseSampleOrderNV'.
--
-- == Valid Usage
--
-- -   If @sampleOrderType@ is not 'VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV',
--     @customSamplerOrderCount@ /must/ be @0@
--
-- -   The array @pCustomSampleOrders@ /must/ not contain two structures
--     with matching values for both the @shadingRate@ and @sampleCount@
--     members.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV'
--
-- -   @sampleOrderType@ /must/ be a valid 'VkCoarseSampleOrderTypeNV'
--     value
--
-- -   If @customSampleOrderCount@ is not @0@, @pCustomSampleOrders@ /must/
--     be a valid pointer to an array of @customSampleOrderCount@ valid
--     'VkCoarseSampleOrderCustomNV' structures
--
-- = See Also
--
-- 'VkCoarseSampleOrderCustomNV', 'VkCoarseSampleOrderTypeNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPipelineViewportCoarseSampleOrderStateCreateInfoNV = VkPipelineViewportCoarseSampleOrderStateCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @sampleOrderType@ specifies the mechanism used to order coverage samples
  -- in fragments larger than one pixel.
  vkSampleOrderType :: VkCoarseSampleOrderTypeNV
  , -- | @customSampleOrderCount@ specifies the number of custom sample orderings
  -- to use when ordering coverage samples.
  vkCustomSampleOrderCount :: Word32
  , -- | @pCustomSampleOrders@ is a pointer to an array of
  -- 'VkCoarseSampleOrderCustomNV' structures, each of which specifies the
  -- coverage sample order for a single combination of fragment area and
  -- coverage sample count.
  vkPCustomSampleOrders :: Ptr VkCoarseSampleOrderCustomNV
  }
  deriving (Eq, Show)

instance Storable VkPipelineViewportCoarseSampleOrderStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineViewportCoarseSampleOrderStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                                  <*> peek (ptr `plusPtr` 8)
                                                                  <*> peek (ptr `plusPtr` 16)
                                                                  <*> peek (ptr `plusPtr` 20)
                                                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkSampleOrderType (poked :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkCustomSampleOrderCount (poked :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPCustomSampleOrders (poked :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))

instance Zero VkPipelineViewportCoarseSampleOrderStateCreateInfoNV where
  zero = VkPipelineViewportCoarseSampleOrderStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV
                                                              zero
                                                              zero
                                                              zero
                                                              zero

-- | VkPipelineViewportShadingRateImageStateCreateInfoNV - Structure
-- specifying parameters controlling shading rate image usage
--
-- = Description
--
-- If this structure is not present, @shadingRateImageEnable@ is considered
-- to be 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', and the shading rate
-- image and palettes are not used.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @0@ or @1@
--
-- -   @viewportCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@
--
-- -   If @shadingRateImageEnable@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @viewportCount@ /must/ be
--     equal to the @viewportCount@ member of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo'
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV',
--     @pShadingRatePalettes@ /must/ be a valid pointer to an array of
--     @viewportCount@ 'VkShadingRatePaletteNV' structures
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV'
--
-- -   If @viewportCount@ is not @0@, and @pShadingRatePalettes@ is not
--     @NULL@, @pShadingRatePalettes@ /must/ be a valid pointer to an array
--     of @viewportCount@ valid 'VkShadingRatePaletteNV' structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32', 'VkShadingRatePaletteNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPipelineViewportShadingRateImageStateCreateInfoNV = VkPipelineViewportShadingRateImageStateCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @shadingRateImageEnable@ specifies whether shading rate image and
  -- palettes are used during rasterization.
  vkShadingRateImageEnable :: VkBool32
  , -- | @viewportCount@ specifies the number of per-viewport palettes used to
  -- translate values stored in shading rate images.
  vkViewportCount :: Word32
  , -- | @pShadingRatePalettes@ is a pointer to an array of
  -- 'VkShadingRatePaletteNV' structures defining the palette for each
  -- viewport. If the shading rate palette state is dynamic, this member is
  -- ignored.
  vkPShadingRatePalettes :: Ptr VkShadingRatePaletteNV
  }
  deriving (Eq, Show)

instance Storable VkPipelineViewportShadingRateImageStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineViewportShadingRateImageStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                                 <*> peek (ptr `plusPtr` 8)
                                                                 <*> peek (ptr `plusPtr` 16)
                                                                 <*> peek (ptr `plusPtr` 20)
                                                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportShadingRateImageStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportShadingRateImageStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkShadingRateImageEnable (poked :: VkPipelineViewportShadingRateImageStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkViewportCount (poked :: VkPipelineViewportShadingRateImageStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPShadingRatePalettes (poked :: VkPipelineViewportShadingRateImageStateCreateInfoNV))

instance Zero VkPipelineViewportShadingRateImageStateCreateInfoNV where
  zero = VkPipelineViewportShadingRateImageStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV
                                                             zero
                                                             zero
                                                             zero
                                                             zero

-- ** VkShadingRatePaletteEntryNV

-- | VkShadingRatePaletteEntryNV - Shading rate image palette entry types
--
-- = Description
--
-- The following table indicates the width and height (in pixels) of each
-- fragment generated using the indicated shading rate, as well as the
-- maximum number of fragment shader invocations launched for each
-- fragment. When processing regions of a primitive that have a shading
-- rate of 'VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV', no fragments
-- will be generated in that region.
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | Shading Rate    | Width           | Height          | Invocations     |
-- > +=================+=================+=================+=================+
-- > | 'VK_SHADING_RAT | 0               | 0               | 0               |
-- > | E_PALETTE_ENTRY |                 |                 |                 |
-- > | _NO_INVOCATIONS |                 |                 |                 |
-- > | _NV'            |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'VK_SHADING_RAT | 1               | 1               | 16              |
-- > | E_PALETTE_ENTRY |                 |                 |                 |
-- > | _16_INVOCATIONS |                 |                 |                 |
-- > | _PER_PIXEL_NV'  |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'VK_SHADING_RAT | 1               | 1               | 8               |
-- > | E_PALETTE_ENTRY |                 |                 |                 |
-- > | _8_INVOCATIONS_ |                 |                 |                 |
-- > | PER_PIXEL_NV'   |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'VK_SHADING_RAT | 1               | 1               | 4               |
-- > | E_PALETTE_ENTRY |                 |                 |                 |
-- > | _4_INVOCATIONS_ |                 |                 |                 |
-- > | PER_PIXEL_NV'   |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'VK_SHADING_RAT | 1               | 1               | 2               |
-- > | E_PALETTE_ENTRY |                 |                 |                 |
-- > | _2_INVOCATIONS_ |                 |                 |                 |
-- > | PER_PIXEL_NV'   |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'VK_SHADING_RAT | 1               | 1               | 1               |
-- > | E_PALETTE_ENTRY |                 |                 |                 |
-- > | _1_INVOCATION_P |                 |                 |                 |
-- > | ER_PIXEL_NV'    |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'VK_SHADING_RAT | 2               | 1               | 1               |
-- > | E_PALETTE_ENTRY |                 |                 |                 |
-- > | _1_INVOCATION_P |                 |                 |                 |
-- > | ER_2X1_PIXELS_N |                 |                 |                 |
-- > | V'              |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'VK_SHADING_RAT | 1               | 2               | 1               |
-- > | E_PALETTE_ENTRY |                 |                 |                 |
-- > | _1_INVOCATION_P |                 |                 |                 |
-- > | ER_1X2_PIXELS_N |                 |                 |                 |
-- > | V'              |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'VK_SHADING_RAT | 2               | 2               | 1               |
-- > | E_PALETTE_ENTRY |                 |                 |                 |
-- > | _1_INVOCATION_P |                 |                 |                 |
-- > | ER_2X2_PIXELS_N |                 |                 |                 |
-- > | V'              |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'VK_SHADING_RAT | 4               | 2               | 1               |
-- > | E_PALETTE_ENTRY |                 |                 |                 |
-- > | _1_INVOCATION_P |                 |                 |                 |
-- > | ER_4X2_PIXELS_N |                 |                 |                 |
-- > | V'              |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'VK_SHADING_RAT | 2               | 4               | 1               |
-- > | E_PALETTE_ENTRY |                 |                 |                 |
-- > | _1_INVOCATION_P |                 |                 |                 |
-- > | ER_2X4_PIXELS_N |                 |                 |                 |
-- > | V'              |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'VK_SHADING_RAT | 4               | 4               | 1               |
-- > | E_PALETTE_ENTRY |                 |                 |                 |
-- > | _1_INVOCATION_P |                 |                 |                 |
-- > | ER_4X4_PIXELS_N |                 |                 |                 |
-- > | V'              |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'VkCoarseSampleOrderCustomNV', 'VkShadingRatePaletteNV'
newtype VkShadingRatePaletteEntryNV = VkShadingRatePaletteEntryNV Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkShadingRatePaletteEntryNV where
  showsPrec _ VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV = showString "VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV"
  showsPrec _ VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV = showString "VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV"
  showsPrec _ VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV = showString "VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV"
  showsPrec _ VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV = showString "VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV"
  showsPrec _ VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV = showString "VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV"
  showsPrec _ VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV = showString "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV"
  showsPrec _ VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV = showString "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV"
  showsPrec _ VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV = showString "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV"
  showsPrec _ VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV = showString "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV"
  showsPrec _ VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV = showString "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV"
  showsPrec _ VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV = showString "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV"
  showsPrec _ VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV = showString "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV"
  showsPrec p (VkShadingRatePaletteEntryNV x) = showParen (p >= 11) (showString "VkShadingRatePaletteEntryNV " . showsPrec 11 x)

instance Read VkShadingRatePaletteEntryNV where
  readPrec = parens ( choose [ ("VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV",              pure VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV)
                             , ("VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV",    pure VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV)
                             , ("VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV",     pure VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV)
                             , ("VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV",     pure VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV)
                             , ("VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV",     pure VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV)
                             , ("VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV",      pure VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV)
                             , ("VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV", pure VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV)
                             , ("VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV", pure VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV)
                             , ("VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV", pure VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV)
                             , ("VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV", pure VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV)
                             , ("VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV", pure VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV)
                             , ("VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV", pure VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkShadingRatePaletteEntryNV")
                        v <- step readPrec
                        pure (VkShadingRatePaletteEntryNV v)
                        )
                    )

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV"
pattern VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV :: VkShadingRatePaletteEntryNV
pattern VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV = VkShadingRatePaletteEntryNV 0

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV"
pattern VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV :: VkShadingRatePaletteEntryNV
pattern VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV = VkShadingRatePaletteEntryNV 1

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV"
pattern VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV :: VkShadingRatePaletteEntryNV
pattern VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV = VkShadingRatePaletteEntryNV 2

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV"
pattern VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV :: VkShadingRatePaletteEntryNV
pattern VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV = VkShadingRatePaletteEntryNV 3

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV"
pattern VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV :: VkShadingRatePaletteEntryNV
pattern VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV = VkShadingRatePaletteEntryNV 4

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV"
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV :: VkShadingRatePaletteEntryNV
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV = VkShadingRatePaletteEntryNV 5

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV"
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV :: VkShadingRatePaletteEntryNV
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV = VkShadingRatePaletteEntryNV 6

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV"
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV :: VkShadingRatePaletteEntryNV
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV = VkShadingRatePaletteEntryNV 7

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV"
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV :: VkShadingRatePaletteEntryNV
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV = VkShadingRatePaletteEntryNV 8

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV"
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV :: VkShadingRatePaletteEntryNV
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV = VkShadingRatePaletteEntryNV 9

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV"
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV :: VkShadingRatePaletteEntryNV
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV = VkShadingRatePaletteEntryNV 10

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV"
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV :: VkShadingRatePaletteEntryNV
pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV = VkShadingRatePaletteEntryNV 11

-- | VkShadingRatePaletteNV - Structure specifying a single shading rate
-- palette
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkPipelineViewportShadingRateImageStateCreateInfoNV',
-- 'VkShadingRatePaletteEntryNV', 'vkCmdSetViewportShadingRatePaletteNV'
data VkShadingRatePaletteNV = VkShadingRatePaletteNV
  { -- | @shadingRatePaletteEntryCount@ /must/ be greater than @0@
  vkShadingRatePaletteEntryCount :: Word32
  , -- | @pShadingRatePaletteEntries@ /must/ be a valid pointer to an array of
  -- @shadingRatePaletteEntryCount@ valid 'VkShadingRatePaletteEntryNV'
  -- values
  vkPShadingRatePaletteEntries :: Ptr VkShadingRatePaletteEntryNV
  }
  deriving (Eq, Show)

instance Storable VkShadingRatePaletteNV where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkShadingRatePaletteNV <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkShadingRatePaletteEntryCount (poked :: VkShadingRatePaletteNV))
                *> poke (ptr `plusPtr` 8) (vkPShadingRatePaletteEntries (poked :: VkShadingRatePaletteNV))

instance Zero VkShadingRatePaletteNV where
  zero = VkShadingRatePaletteNV zero
                                zero

-- | vkCmdBindShadingRateImageNV - Bind a shading rate image on a command
-- buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @imageView@ is an image view handle that specifies the shading rate
--     image. @imageView@ /may/ be set to
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', which is
--     equivalent to specifying a view of an image filled with zero values.
--
-- -   @imageLayout@ is the layout that the image subresources accessible
--     from @imageView@ will be in when the shading rate image is accessed.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature /must/ be enabled.
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', it /must/ be a
--     valid 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' handle of
--     type 'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY'.
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', it /must/ have
--     a format of 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8_UINT'.
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', it /must/ have
--     been created with a @usage@ value including
--     'VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @imageLayout@
--     /must/ match the actual
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' of each subresource
--     accessible from @imageView@ at the time the subresource is accessed.
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @imageLayout@
--     /must/ be 'VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV' or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @imageView@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' handle
--
-- -   @imageLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   Both of @commandBuffer@, and @imageView@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBindShadingRateImageNV" vkCmdBindShadingRateImageNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("imageView" ::: VkImageView) -> ("imageLayout" ::: VkImageLayout) -> IO ()
#else
vkCmdBindShadingRateImageNV :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("imageView" ::: VkImageView) -> ("imageLayout" ::: VkImageLayout) -> IO ()
vkCmdBindShadingRateImageNV deviceCmds = mkVkCmdBindShadingRateImageNV (pVkCmdBindShadingRateImageNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindShadingRateImageNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("imageView" ::: VkImageView) -> ("imageLayout" ::: VkImageLayout) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("imageView" ::: VkImageView) -> ("imageLayout" ::: VkImageLayout) -> IO ())
#endif

type FN_vkCmdBindShadingRateImageNV = ("commandBuffer" ::: VkCommandBuffer) -> ("imageView" ::: VkImageView) -> ("imageLayout" ::: VkImageLayout) -> IO ()
type PFN_vkCmdBindShadingRateImageNV = FunPtr FN_vkCmdBindShadingRateImageNV

-- | vkCmdSetCoarseSampleOrderNV - Set sample order for coarse fragments on a
-- command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @sampleOrderType@ specifies the mechanism used to order coverage
--     samples in fragments larger than one pixel.
--
-- -   @customSampleOrderCount@ specifies the number of custom sample
--     orderings to use when ordering coverage samples.
--
-- -   @pCustomSampleOrders@ is a pointer to an array of
--     'VkCoarseSampleOrderCustomNV' structures, each of which specifies
--     the coverage sample order for a single combination of fragment area
--     and coverage sample count.
--
-- = Description
--
-- If @sampleOrderType@ is 'VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV', the
-- coverage sample order used for any combination of fragment area and
-- coverage sample count not enumerated in @pCustomSampleOrders@ will be
-- identical to that used for 'VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'.
--
-- == Valid Usage
--
-- -   If @sampleOrderType@ is not 'VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV',
--     @customSamplerOrderCount@ /must/ be @0@
--
-- -   The array @pCustomSampleOrders@ /must/ not contain two structures
--     with matching values for both the @shadingRate@ and @sampleCount@
--     members.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @sampleOrderType@ /must/ be a valid 'VkCoarseSampleOrderTypeNV'
--     value
--
-- -   If @customSampleOrderCount@ is not @0@, @pCustomSampleOrders@ /must/
--     be a valid pointer to an array of @customSampleOrderCount@ valid
--     'VkCoarseSampleOrderCustomNV' structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'VkCoarseSampleOrderCustomNV', 'VkCoarseSampleOrderTypeNV',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetCoarseSampleOrderNV" vkCmdSetCoarseSampleOrderNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("sampleOrderType" ::: VkCoarseSampleOrderTypeNV) -> ("customSampleOrderCount" ::: Word32) -> ("pCustomSampleOrders" ::: Ptr VkCoarseSampleOrderCustomNV) -> IO ()
#else
vkCmdSetCoarseSampleOrderNV :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("sampleOrderType" ::: VkCoarseSampleOrderTypeNV) -> ("customSampleOrderCount" ::: Word32) -> ("pCustomSampleOrders" ::: Ptr VkCoarseSampleOrderCustomNV) -> IO ()
vkCmdSetCoarseSampleOrderNV deviceCmds = mkVkCmdSetCoarseSampleOrderNV (pVkCmdSetCoarseSampleOrderNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCoarseSampleOrderNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("sampleOrderType" ::: VkCoarseSampleOrderTypeNV) -> ("customSampleOrderCount" ::: Word32) -> ("pCustomSampleOrders" ::: Ptr VkCoarseSampleOrderCustomNV) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("sampleOrderType" ::: VkCoarseSampleOrderTypeNV) -> ("customSampleOrderCount" ::: Word32) -> ("pCustomSampleOrders" ::: Ptr VkCoarseSampleOrderCustomNV) -> IO ())
#endif

type FN_vkCmdSetCoarseSampleOrderNV = ("commandBuffer" ::: VkCommandBuffer) -> ("sampleOrderType" ::: VkCoarseSampleOrderTypeNV) -> ("customSampleOrderCount" ::: Word32) -> ("pCustomSampleOrders" ::: Ptr VkCoarseSampleOrderCustomNV) -> IO ()
type PFN_vkCmdSetCoarseSampleOrderNV = FunPtr FN_vkCmdSetCoarseSampleOrderNV

-- | vkCmdSetViewportShadingRatePaletteNV - Set shading rate image palettes
-- on a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @firstViewport@ is the index of the first viewport whose shading
--     rate palette is updated by the command.
--
-- -   @viewportCount@ is the number of viewports whose shading rate
--     palettes are updated by the command.
--
-- -   @pShadingRatePalettes@ is a pointer to an array of
--     'VkShadingRatePaletteNV' structures defining the palette for each
--     viewport.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature /must/ be enabled.
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV' dynamic state
--     enabled
--
-- -   @firstViewport@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstViewport@ and @viewportCount@ /must/ be between @1@
--     and
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @firstViewport@ /must/ be @0@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pShadingRatePalettes@ /must/ be a valid pointer to an array of
--     @viewportCount@ valid 'VkShadingRatePaletteNV' structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   @viewportCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'VkShadingRatePaletteNV'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetViewportShadingRatePaletteNV" vkCmdSetViewportShadingRatePaletteNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pShadingRatePalettes" ::: Ptr VkShadingRatePaletteNV) -> IO ()
#else
vkCmdSetViewportShadingRatePaletteNV :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pShadingRatePalettes" ::: Ptr VkShadingRatePaletteNV) -> IO ()
vkCmdSetViewportShadingRatePaletteNV deviceCmds = mkVkCmdSetViewportShadingRatePaletteNV (pVkCmdSetViewportShadingRatePaletteNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportShadingRatePaletteNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pShadingRatePalettes" ::: Ptr VkShadingRatePaletteNV) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pShadingRatePalettes" ::: Ptr VkShadingRatePaletteNV) -> IO ())
#endif

type FN_vkCmdSetViewportShadingRatePaletteNV = ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pShadingRatePalettes" ::: Ptr VkShadingRatePaletteNV) -> IO ()
type PFN_vkCmdSetViewportShadingRatePaletteNV = FunPtr FN_vkCmdSetViewportShadingRatePaletteNV

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV"
pattern VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV :: VkAccessFlagBits
pattern VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV = VkAccessFlagBits 0x00800000

-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV"
pattern VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV :: VkDynamicState
pattern VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV = VkDynamicState 1000164006

-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV"
pattern VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV :: VkDynamicState
pattern VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV = VkDynamicState 1000164004

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV"
pattern VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV :: VkImageLayout
pattern VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV = VkImageLayout 1000164003

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV"
pattern VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV = VkImageUsageFlagBits 0x00000100

-- No documentation found for TopLevel "VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME"
pattern VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME = "VK_NV_shading_rate_image"

-- No documentation found for TopLevel "VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION"
pattern VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION :: Integral a => a
pattern VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION = 3

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV"
pattern VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV = VkPipelineStageFlagBits 0x00400000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV = VkStructureType 1000164001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV = VkStructureType 1000164002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV = VkStructureType 1000164005

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV = VkStructureType 1000164000
