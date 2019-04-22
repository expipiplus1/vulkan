{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_NV_shading_rate_image
  ( withCStructCoarseSampleLocationNV
  , fromCStructCoarseSampleLocationNV
  , CoarseSampleLocationNV(..)
  , withCStructCoarseSampleOrderCustomNV
  , fromCStructCoarseSampleOrderCustomNV
  , CoarseSampleOrderCustomNV(..)
  , CoarseSampleOrderTypeNV
  , pattern COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV
  , pattern COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV
  , pattern COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV
  , pattern COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV
  , withCStructPhysicalDeviceShadingRateImageFeaturesNV
  , fromCStructPhysicalDeviceShadingRateImageFeaturesNV
  , PhysicalDeviceShadingRateImageFeaturesNV(..)
  , withCStructPhysicalDeviceShadingRateImagePropertiesNV
  , fromCStructPhysicalDeviceShadingRateImagePropertiesNV
  , PhysicalDeviceShadingRateImagePropertiesNV(..)
  , withCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV
  , fromCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV
  , PipelineViewportCoarseSampleOrderStateCreateInfoNV(..)
  , withCStructPipelineViewportShadingRateImageStateCreateInfoNV
  , fromCStructPipelineViewportShadingRateImageStateCreateInfoNV
  , PipelineViewportShadingRateImageStateCreateInfoNV(..)
  , ShadingRatePaletteEntryNV
  , pattern SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV
  , pattern SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV
  , pattern SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV
  , pattern SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV
  , pattern SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV
  , pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV
  , pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV
  , pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV
  , pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV
  , pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV
  , pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV
  , pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV
  , withCStructShadingRatePaletteNV
  , fromCStructShadingRatePaletteNV
  , ShadingRatePaletteNV(..)
  , cmdBindShadingRateImageNV
  , cmdSetCoarseSampleOrderNV
  , cmdSetViewportShadingRatePaletteNV
  , pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME
  , pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV
  , pattern IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV
  , pattern DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV
  , pattern ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV
  , pattern IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV
  , pattern PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV
  , pattern DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Function
  ( (&)
  )
import Data.Maybe
  ( maybe
  )
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( VkCoarseSampleLocationNV(..)
  , VkCoarseSampleOrderCustomNV(..)
  , VkCoarseSampleOrderTypeNV(..)
  , VkPhysicalDeviceShadingRateImageFeaturesNV(..)
  , VkPhysicalDeviceShadingRateImagePropertiesNV(..)
  , VkPipelineViewportCoarseSampleOrderStateCreateInfoNV(..)
  , VkPipelineViewportShadingRateImageStateCreateInfoNV(..)
  , VkShadingRatePaletteEntryNV(..)
  , VkShadingRatePaletteNV(..)
  , vkCmdBindShadingRateImageNV
  , vkCmdSetCoarseSampleOrderNV
  , vkCmdSetViewportShadingRatePaletteNV
  , pattern VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV
  , pattern VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV
  , pattern VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV
  , pattern VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV
  , pattern VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME
  , pattern VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV
  , pattern VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.Image
  ( ImageLayout
  )
import Graphics.Vulkan.Core10.ImageView
  ( ImageView
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , fromCStructExtent2D
  , withCStructExtent2D
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV
  )
import Graphics.Vulkan.Core10.Image
  ( pattern IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV
  )
import Graphics.Vulkan.Core10.Pipeline
  ( pattern DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV
  , pattern DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV
  )
import Graphics.Vulkan.Core10.Queue
  ( pattern PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV
  )



-- | VkCoarseSampleLocationNV - Structure specifying parameters controlling
-- shading rate image usage
--
-- == Valid Usage
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderCustomNV'
data CoarseSampleLocationNV = CoarseSampleLocationNV
  { -- No documentation found for Nested "CoarseSampleLocationNV" "pixelX"
  pixelX :: Word32
  , -- No documentation found for Nested "CoarseSampleLocationNV" "pixelY"
  pixelY :: Word32
  , -- No documentation found for Nested "CoarseSampleLocationNV" "sample"
  sample :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkCoarseSampleLocationNV' and
-- marshal a 'CoarseSampleLocationNV' into it. The 'VkCoarseSampleLocationNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCoarseSampleLocationNV :: CoarseSampleLocationNV -> (VkCoarseSampleLocationNV -> IO a) -> IO a
withCStructCoarseSampleLocationNV marshalled cont = cont (VkCoarseSampleLocationNV (pixelX (marshalled :: CoarseSampleLocationNV)) (pixelY (marshalled :: CoarseSampleLocationNV)) (sample (marshalled :: CoarseSampleLocationNV)))

-- | A function to read a 'VkCoarseSampleLocationNV' and all additional
-- structures in the pointer chain into a 'CoarseSampleLocationNV'.
fromCStructCoarseSampleLocationNV :: VkCoarseSampleLocationNV -> IO CoarseSampleLocationNV
fromCStructCoarseSampleLocationNV c = CoarseSampleLocationNV <$> pure (vkPixelX (c :: VkCoarseSampleLocationNV))
                                                             <*> pure (vkPixelY (c :: VkCoarseSampleLocationNV))
                                                             <*> pure (vkSample (c :: VkCoarseSampleLocationNV))

instance Zero CoarseSampleLocationNV where
  zero = CoarseSampleLocationNV zero
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
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkPhysicalDeviceShadingRateImagePropertiesNV'::@shadingRateMaxCoarseSamples@.
--
-- -   The array @pSampleLocations@ /must/ contain exactly one entry for
--     every combination of valid values for @pixelX@, @pixelY@, and
--     @sample@ in the structure
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderCustomNV'.
--
-- == Valid Usage (Implicit)
--
-- -   @shadingRate@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkShadingRatePaletteEntryNV'
--     value
--
-- -   @pSampleLocations@ /must/ be a valid pointer to an array of
--     @sampleLocationCount@
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleLocationNV'
--     structures
--
-- -   @sampleLocationCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleLocationNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkPipelineViewportCoarseSampleOrderStateCreateInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkShadingRatePaletteEntryNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.vkCmdSetCoarseSampleOrderNV'
data CoarseSampleOrderCustomNV = CoarseSampleOrderCustomNV
  { -- No documentation found for Nested "CoarseSampleOrderCustomNV" "shadingRate"
  shadingRate :: ShadingRatePaletteEntryNV
  , -- No documentation found for Nested "CoarseSampleOrderCustomNV" "sampleCount"
  sampleCount :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "CoarseSampleOrderCustomNV" "pSampleLocations"
  sampleLocations :: Vector CoarseSampleLocationNV
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkCoarseSampleOrderCustomNV' and
-- marshal a 'CoarseSampleOrderCustomNV' into it. The 'VkCoarseSampleOrderCustomNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCoarseSampleOrderCustomNV :: CoarseSampleOrderCustomNV -> (VkCoarseSampleOrderCustomNV -> IO a) -> IO a
withCStructCoarseSampleOrderCustomNV marshalled cont = withVec withCStructCoarseSampleLocationNV (sampleLocations (marshalled :: CoarseSampleOrderCustomNV)) (\pPSampleLocations -> cont (VkCoarseSampleOrderCustomNV (shadingRate (marshalled :: CoarseSampleOrderCustomNV)) (sampleCount (marshalled :: CoarseSampleOrderCustomNV)) (fromIntegral (Data.Vector.length (sampleLocations (marshalled :: CoarseSampleOrderCustomNV)))) pPSampleLocations))

-- | A function to read a 'VkCoarseSampleOrderCustomNV' and all additional
-- structures in the pointer chain into a 'CoarseSampleOrderCustomNV'.
fromCStructCoarseSampleOrderCustomNV :: VkCoarseSampleOrderCustomNV -> IO CoarseSampleOrderCustomNV
fromCStructCoarseSampleOrderCustomNV c = CoarseSampleOrderCustomNV <$> pure (vkShadingRate (c :: VkCoarseSampleOrderCustomNV))
                                                                   <*> pure (vkSampleCount (c :: VkCoarseSampleOrderCustomNV))
                                                                   -- Length valued member elided
                                                                   <*> (Data.Vector.generateM (fromIntegral (vkSampleLocationCount (c :: VkCoarseSampleOrderCustomNV))) (((fromCStructCoarseSampleLocationNV <=<) . peekElemOff) (vkPSampleLocations (c :: VkCoarseSampleOrderCustomNV))))

instance Zero CoarseSampleOrderCustomNV where
  zero = CoarseSampleOrderCustomNV zero
                                   zero
                                   Data.Vector.empty


-- | VkCoarseSampleOrderTypeNV - Shading rate image sample ordering types
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkPipelineViewportCoarseSampleOrderStateCreateInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.vkCmdSetCoarseSampleOrderNV'
type CoarseSampleOrderTypeNV = VkCoarseSampleOrderTypeNV


{-# complete COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV, COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV, COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV, COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV :: CoarseSampleOrderTypeNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'
-- specifies that coverage samples will be ordered in an
-- implementation-dependent manner.
pattern COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV :: (a ~ CoarseSampleOrderTypeNV) => a
pattern COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV = VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV'
-- specifies that coverage samples will be ordered according to the array
-- of custom orderings provided in either the @pCustomSampleOrders@ member
-- of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkPipelineViewportCoarseSampleOrderStateCreateInfoNV'
-- or the @pCustomSampleOrders@ member of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.vkCmdSetCoarseSampleOrderNV'.
pattern COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV :: (a ~ CoarseSampleOrderTypeNV) => a
pattern COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV = VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV'
-- specifies that coverage samples will be ordered sequentially, sorted
-- first by pixel coordinate (in row-major order) and then by coverage
-- sample number.
pattern COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV :: (a ~ CoarseSampleOrderTypeNV) => a
pattern COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV = VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV'
-- specifies that coverage samples will be ordered sequentially, sorted
-- first by coverage sample number and then by pixel coordinate (in
-- row-major order).
pattern COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV :: (a ~ CoarseSampleOrderTypeNV) => a
pattern COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV = VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV


-- | VkPhysicalDeviceShadingRateImageFeaturesNV - Structure describing
-- shading rate image features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkPhysicalDeviceShadingRateImageFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#primsrast-shading-rate-image Shading Rate Image>
-- for more information.
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkPhysicalDeviceShadingRateImageFeaturesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkPhysicalDeviceShadingRateImageFeaturesNV'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceShadingRateImageFeaturesNV = PhysicalDeviceShadingRateImageFeaturesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceShadingRateImageFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImageFeaturesNV" "shadingRateImage"
  shadingRateImage :: Bool
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImageFeaturesNV" "shadingRateCoarseSampleOrder"
  shadingRateCoarseSampleOrder :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceShadingRateImageFeaturesNV' and
-- marshal a 'PhysicalDeviceShadingRateImageFeaturesNV' into it. The 'VkPhysicalDeviceShadingRateImageFeaturesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceShadingRateImageFeaturesNV :: PhysicalDeviceShadingRateImageFeaturesNV -> (VkPhysicalDeviceShadingRateImageFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceShadingRateImageFeaturesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceShadingRateImageFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceShadingRateImageFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV pPNext (boolToBool32 (shadingRateImage (marshalled :: PhysicalDeviceShadingRateImageFeaturesNV))) (boolToBool32 (shadingRateCoarseSampleOrder (marshalled :: PhysicalDeviceShadingRateImageFeaturesNV)))))

-- | A function to read a 'VkPhysicalDeviceShadingRateImageFeaturesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceShadingRateImageFeaturesNV'.
fromCStructPhysicalDeviceShadingRateImageFeaturesNV :: VkPhysicalDeviceShadingRateImageFeaturesNV -> IO PhysicalDeviceShadingRateImageFeaturesNV
fromCStructPhysicalDeviceShadingRateImageFeaturesNV c = PhysicalDeviceShadingRateImageFeaturesNV <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceShadingRateImageFeaturesNV)))
                                                                                                 <*> pure (bool32ToBool (vkShadingRateImage (c :: VkPhysicalDeviceShadingRateImageFeaturesNV)))
                                                                                                 <*> pure (bool32ToBool (vkShadingRateCoarseSampleOrder (c :: VkPhysicalDeviceShadingRateImageFeaturesNV)))

instance Zero PhysicalDeviceShadingRateImageFeaturesNV where
  zero = PhysicalDeviceShadingRateImageFeaturesNV Nothing
                                                  False
                                                  False



-- | VkPhysicalDeviceShadingRateImagePropertiesNV - Structure describing
-- shading rate image limits that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkPhysicalDeviceShadingRateImagePropertiesNV'
-- structure describe the following implementation-dependent properties
-- related to the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#primsrast-shading-rate-image shading rate image>
-- feature:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkPhysicalDeviceShadingRateImagePropertiesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceShadingRateImagePropertiesNV = PhysicalDeviceShadingRateImagePropertiesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceShadingRateImagePropertiesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImagePropertiesNV" "shadingRateTexelSize"
  shadingRateTexelSize :: Extent2D
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImagePropertiesNV" "shadingRatePaletteSize"
  shadingRatePaletteSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImagePropertiesNV" "shadingRateMaxCoarseSamples"
  shadingRateMaxCoarseSamples :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceShadingRateImagePropertiesNV' and
-- marshal a 'PhysicalDeviceShadingRateImagePropertiesNV' into it. The 'VkPhysicalDeviceShadingRateImagePropertiesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceShadingRateImagePropertiesNV :: PhysicalDeviceShadingRateImagePropertiesNV -> (VkPhysicalDeviceShadingRateImagePropertiesNV -> IO a) -> IO a
withCStructPhysicalDeviceShadingRateImagePropertiesNV marshalled cont = withCStructExtent2D (shadingRateTexelSize (marshalled :: PhysicalDeviceShadingRateImagePropertiesNV)) (\shadingRateTexelSize'' -> maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceShadingRateImagePropertiesNV)) (\pPNext -> cont (VkPhysicalDeviceShadingRateImagePropertiesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV pPNext shadingRateTexelSize'' (shadingRatePaletteSize (marshalled :: PhysicalDeviceShadingRateImagePropertiesNV)) (shadingRateMaxCoarseSamples (marshalled :: PhysicalDeviceShadingRateImagePropertiesNV)))))

-- | A function to read a 'VkPhysicalDeviceShadingRateImagePropertiesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceShadingRateImagePropertiesNV'.
fromCStructPhysicalDeviceShadingRateImagePropertiesNV :: VkPhysicalDeviceShadingRateImagePropertiesNV -> IO PhysicalDeviceShadingRateImagePropertiesNV
fromCStructPhysicalDeviceShadingRateImagePropertiesNV c = PhysicalDeviceShadingRateImagePropertiesNV <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceShadingRateImagePropertiesNV)))
                                                                                                     <*> (fromCStructExtent2D (vkShadingRateTexelSize (c :: VkPhysicalDeviceShadingRateImagePropertiesNV)))
                                                                                                     <*> pure (vkShadingRatePaletteSize (c :: VkPhysicalDeviceShadingRateImagePropertiesNV))
                                                                                                     <*> pure (vkShadingRateMaxCoarseSamples (c :: VkPhysicalDeviceShadingRateImagePropertiesNV))

instance Zero PhysicalDeviceShadingRateImagePropertiesNV where
  zero = PhysicalDeviceShadingRateImagePropertiesNV Nothing
                                                    zero
                                                    zero
                                                    zero



-- | VkPipelineViewportCoarseSampleOrderStateCreateInfoNV - Structure
-- specifying parameters controlling sample order in coarse fragments
--
-- = Description
--
-- If this structure is not present, @sampleOrderType@ is considered to be
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'.
--
-- If @sampleOrderType@ is
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV',
-- the coverage sample order used for any combination of fragment area and
-- coverage sample count not enumerated in @pCustomSampleOrders@ will be
-- identical to that used for
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'.
--
-- If the pipeline was created with
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV',
-- the contents of this structure (if present) are ignored, and the
-- coverage sample order is instead specified by
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.vkCmdSetCoarseSampleOrderNV'.
--
-- == Valid Usage
--
-- -   If @sampleOrderType@ is not
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV',
--     @customSamplerOrderCount@ /must/ be @0@
--
-- -   The array @pCustomSampleOrders@ /must/ not contain two structures
--     with matching values for both the @shadingRate@ and @sampleCount@
--     members.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV'
--
-- -   @sampleOrderType@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderTypeNV'
--     value
--
-- -   If @customSampleOrderCount@ is not @0@, @pCustomSampleOrders@ /must/
--     be a valid pointer to an array of @customSampleOrderCount@ valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderCustomNV'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderCustomNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderTypeNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineViewportCoarseSampleOrderStateCreateInfoNV = PipelineViewportCoarseSampleOrderStateCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineViewportCoarseSampleOrderStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportCoarseSampleOrderStateCreateInfoNV" "sampleOrderType"
  sampleOrderType :: CoarseSampleOrderTypeNV
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineViewportCoarseSampleOrderStateCreateInfoNV" "pCustomSampleOrders"
  customSampleOrders :: Vector CoarseSampleOrderCustomNV
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineViewportCoarseSampleOrderStateCreateInfoNV' and
-- marshal a 'PipelineViewportCoarseSampleOrderStateCreateInfoNV' into it. The 'VkPipelineViewportCoarseSampleOrderStateCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV :: PipelineViewportCoarseSampleOrderStateCreateInfoNV -> (VkPipelineViewportCoarseSampleOrderStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV marshalled cont = withVec withCStructCoarseSampleOrderCustomNV (customSampleOrders (marshalled :: PipelineViewportCoarseSampleOrderStateCreateInfoNV)) (\pPCustomSampleOrders -> maybeWith withSomeVkStruct (next (marshalled :: PipelineViewportCoarseSampleOrderStateCreateInfoNV)) (\pPNext -> cont (VkPipelineViewportCoarseSampleOrderStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV pPNext (sampleOrderType (marshalled :: PipelineViewportCoarseSampleOrderStateCreateInfoNV)) (fromIntegral (Data.Vector.length (customSampleOrders (marshalled :: PipelineViewportCoarseSampleOrderStateCreateInfoNV)))) pPCustomSampleOrders)))

-- | A function to read a 'VkPipelineViewportCoarseSampleOrderStateCreateInfoNV' and all additional
-- structures in the pointer chain into a 'PipelineViewportCoarseSampleOrderStateCreateInfoNV'.
fromCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV -> IO PipelineViewportCoarseSampleOrderStateCreateInfoNV
fromCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV c = PipelineViewportCoarseSampleOrderStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV)))
                                                                                                                     <*> pure (vkSampleOrderType (c :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))
                                                                                                                     -- Length valued member elided
                                                                                                                     <*> (Data.Vector.generateM (fromIntegral (vkCustomSampleOrderCount (c :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))) (((fromCStructCoarseSampleOrderCustomNV <=<) . peekElemOff) (vkPCustomSampleOrders (c :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))))

instance Zero PipelineViewportCoarseSampleOrderStateCreateInfoNV where
  zero = PipelineViewportCoarseSampleOrderStateCreateInfoNV Nothing
                                                            zero
                                                            Data.Vector.empty



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
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV',
--     @pShadingRatePalettes@ /must/ be a valid pointer to an array of
--     @viewportCount@
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkShadingRatePaletteNV'
--     structures
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV'
--
-- -   If @viewportCount@ is not @0@, and @pShadingRatePalettes@ is not
--     @NULL@, @pShadingRatePalettes@ /must/ be a valid pointer to an array
--     of @viewportCount@ valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkShadingRatePaletteNV'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkShadingRatePaletteNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineViewportShadingRateImageStateCreateInfoNV = PipelineViewportShadingRateImageStateCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineViewportShadingRateImageStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportShadingRateImageStateCreateInfoNV" "shadingRateImageEnable"
  shadingRateImageEnable :: Bool
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineViewportShadingRateImageStateCreateInfoNV" "pShadingRatePalettes"
  shadingRatePalettes :: Maybe (Vector ShadingRatePaletteNV)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineViewportShadingRateImageStateCreateInfoNV' and
-- marshal a 'PipelineViewportShadingRateImageStateCreateInfoNV' into it. The 'VkPipelineViewportShadingRateImageStateCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineViewportShadingRateImageStateCreateInfoNV :: PipelineViewportShadingRateImageStateCreateInfoNV -> (VkPipelineViewportShadingRateImageStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineViewportShadingRateImageStateCreateInfoNV marshalled cont = maybeWith (withVec withCStructShadingRatePaletteNV) (shadingRatePalettes (marshalled :: PipelineViewportShadingRateImageStateCreateInfoNV)) (\pPShadingRatePalettes -> maybeWith withSomeVkStruct (next (marshalled :: PipelineViewportShadingRateImageStateCreateInfoNV)) (\pPNext -> cont (VkPipelineViewportShadingRateImageStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV pPNext (boolToBool32 (shadingRateImageEnable (marshalled :: PipelineViewportShadingRateImageStateCreateInfoNV))) (maybe 0 (fromIntegral . Data.Vector.length) (shadingRatePalettes (marshalled :: PipelineViewportShadingRateImageStateCreateInfoNV))) pPShadingRatePalettes)))

-- | A function to read a 'VkPipelineViewportShadingRateImageStateCreateInfoNV' and all additional
-- structures in the pointer chain into a 'PipelineViewportShadingRateImageStateCreateInfoNV'.
fromCStructPipelineViewportShadingRateImageStateCreateInfoNV :: VkPipelineViewportShadingRateImageStateCreateInfoNV -> IO PipelineViewportShadingRateImageStateCreateInfoNV
fromCStructPipelineViewportShadingRateImageStateCreateInfoNV c = PipelineViewportShadingRateImageStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineViewportShadingRateImageStateCreateInfoNV)))
                                                                                                                   <*> pure (bool32ToBool (vkShadingRateImageEnable (c :: VkPipelineViewportShadingRateImageStateCreateInfoNV)))
                                                                                                                   -- Optional length valued member elided
                                                                                                                   <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkViewportCount (c :: VkPipelineViewportShadingRateImageStateCreateInfoNV))) (((fromCStructShadingRatePaletteNV <=<) . peekElemOff) p)) (vkPShadingRatePalettes (c :: VkPipelineViewportShadingRateImageStateCreateInfoNV))

instance Zero PipelineViewportShadingRateImageStateCreateInfoNV where
  zero = PipelineViewportShadingRateImageStateCreateInfoNV Nothing
                                                           False
                                                           Nothing


-- | VkShadingRatePaletteEntryNV - Shading rate image palette entry types
--
-- = Description
--
-- The following table indicates the width and height (in pixels) of each
-- fragment generated using the indicated shading rate, as well as the
-- maximum number of fragment shader invocations launched for each
-- fragment. When processing regions of a primitive that have a shading
-- rate of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV',
-- no fragments will be generated in that region.
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | Shading Rate    | Width           | Height          | Invocations     |
-- > +=================+=================+=================+=================+
-- > | 'Graphics.Vulka | 0               | 0               | 0               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_NO_IN |                 |                 |                 |
-- > | VOCATIONS_NV'   |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 1               | 1               | 16              |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_16_IN |                 |                 |                 |
-- > | VOCATIONS_PER_P |                 |                 |                 |
-- > | IXEL_NV'        |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 1               | 1               | 8               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_8_INV |                 |                 |                 |
-- > | OCATIONS_PER_PI |                 |                 |                 |
-- > | XEL_NV'         |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 1               | 1               | 4               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_4_INV |                 |                 |                 |
-- > | OCATIONS_PER_PI |                 |                 |                 |
-- > | XEL_NV'         |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 1               | 1               | 2               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_2_INV |                 |                 |                 |
-- > | OCATIONS_PER_PI |                 |                 |                 |
-- > | XEL_NV'         |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 1               | 1               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_PIX |                 |                 |                 |
-- > | EL_NV'          |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 2               | 1               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_2X1 |                 |                 |                 |
-- > | _PIXELS_NV'     |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 1               | 2               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_1X2 |                 |                 |                 |
-- > | _PIXELS_NV'     |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 2               | 2               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_2X2 |                 |                 |                 |
-- > | _PIXELS_NV'     |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 4               | 2               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_4X2 |                 |                 |                 |
-- > | _PIXELS_NV'     |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 2               | 4               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_2X4 |                 |                 |                 |
-- > | _PIXELS_NV'     |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 4               | 4               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_4X4 |                 |                 |                 |
-- > | _PIXELS_NV'     |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderCustomNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkShadingRatePaletteNV'
type ShadingRatePaletteEntryNV = VkShadingRatePaletteEntryNV


{-# complete SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV, SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV, SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV, SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV, SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV, SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV, SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV, SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV, SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV, SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV, SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV, SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV :: ShadingRatePaletteEntryNV #-}


-- No documentation found for Nested "ShadingRatePaletteEntryNV" "SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV :: (a ~ ShadingRatePaletteEntryNV) => a
pattern SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV = VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV


-- No documentation found for Nested "ShadingRatePaletteEntryNV" "SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV :: (a ~ ShadingRatePaletteEntryNV) => a
pattern SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV = VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV


-- No documentation found for Nested "ShadingRatePaletteEntryNV" "SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV :: (a ~ ShadingRatePaletteEntryNV) => a
pattern SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV = VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV


-- No documentation found for Nested "ShadingRatePaletteEntryNV" "SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV :: (a ~ ShadingRatePaletteEntryNV) => a
pattern SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV = VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV


-- No documentation found for Nested "ShadingRatePaletteEntryNV" "SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV :: (a ~ ShadingRatePaletteEntryNV) => a
pattern SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV = VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV


-- No documentation found for Nested "ShadingRatePaletteEntryNV" "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV :: (a ~ ShadingRatePaletteEntryNV) => a
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV = VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV


-- No documentation found for Nested "ShadingRatePaletteEntryNV" "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV :: (a ~ ShadingRatePaletteEntryNV) => a
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV = VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV


-- No documentation found for Nested "ShadingRatePaletteEntryNV" "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV :: (a ~ ShadingRatePaletteEntryNV) => a
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV = VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV


-- No documentation found for Nested "ShadingRatePaletteEntryNV" "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV :: (a ~ ShadingRatePaletteEntryNV) => a
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV = VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV


-- No documentation found for Nested "ShadingRatePaletteEntryNV" "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV :: (a ~ ShadingRatePaletteEntryNV) => a
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV = VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV


-- No documentation found for Nested "ShadingRatePaletteEntryNV" "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV :: (a ~ ShadingRatePaletteEntryNV) => a
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV = VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV


-- No documentation found for Nested "ShadingRatePaletteEntryNV" "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV :: (a ~ ShadingRatePaletteEntryNV) => a
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV = VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV


-- | VkShadingRatePaletteNV - Structure specifying a single shading rate
-- palette
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkPipelineViewportShadingRateImageStateCreateInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkShadingRatePaletteEntryNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.vkCmdSetViewportShadingRatePaletteNV'
data ShadingRatePaletteNV = ShadingRatePaletteNV
  { -- Length valued member elided
  -- No documentation found for Nested "ShadingRatePaletteNV" "pShadingRatePaletteEntries"
  shadingRatePaletteEntries :: Vector ShadingRatePaletteEntryNV
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkShadingRatePaletteNV' and
-- marshal a 'ShadingRatePaletteNV' into it. The 'VkShadingRatePaletteNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructShadingRatePaletteNV :: ShadingRatePaletteNV -> (VkShadingRatePaletteNV -> IO a) -> IO a
withCStructShadingRatePaletteNV marshalled cont = withVec (&) (shadingRatePaletteEntries (marshalled :: ShadingRatePaletteNV)) (\pPShadingRatePaletteEntries -> cont (VkShadingRatePaletteNV (fromIntegral (Data.Vector.length (shadingRatePaletteEntries (marshalled :: ShadingRatePaletteNV)))) pPShadingRatePaletteEntries))

-- | A function to read a 'VkShadingRatePaletteNV' and all additional
-- structures in the pointer chain into a 'ShadingRatePaletteNV'.
fromCStructShadingRatePaletteNV :: VkShadingRatePaletteNV -> IO ShadingRatePaletteNV
fromCStructShadingRatePaletteNV c = ShadingRatePaletteNV <$> -- Length valued member elided
                                                         (Data.Vector.generateM (fromIntegral (vkShadingRatePaletteEntryCount (c :: VkShadingRatePaletteNV))) (peekElemOff (vkPShadingRatePaletteEntries (c :: VkShadingRatePaletteNV))))

instance Zero ShadingRatePaletteNV where
  zero = ShadingRatePaletteNV Data.Vector.empty



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
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @imageLayout@
--     /must/ match the actual
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' of each subresource
--     accessible from @imageView@ at the time the subresource is accessed.
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @imageLayout@
--     /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV'
--     or 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'.
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
cmdBindShadingRateImageNV :: CommandBuffer ->  ImageView ->  ImageLayout ->  IO ()
cmdBindShadingRateImageNV = \(CommandBuffer commandBuffer' commandTable) -> \imageView' -> \imageLayout' -> vkCmdBindShadingRateImageNV commandTable commandBuffer' imageView' imageLayout' *> (pure ())


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
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderCustomNV'
--     structures, each of which specifies the coverage sample order for a
--     single combination of fragment area and coverage sample count.
--
-- = Description
--
-- If @sampleOrderType@ is
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV',
-- the coverage sample order used for any combination of fragment area and
-- coverage sample count not enumerated in @pCustomSampleOrders@ will be
-- identical to that used for
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'.
--
-- == Valid Usage
--
-- -   If @sampleOrderType@ is not
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV',
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
-- -   @sampleOrderType@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderTypeNV'
--     value
--
-- -   If @customSampleOrderCount@ is not @0@, @pCustomSampleOrders@ /must/
--     be a valid pointer to an array of @customSampleOrderCount@ valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderCustomNV'
--     structures
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
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderCustomNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderTypeNV',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdSetCoarseSampleOrderNV :: CommandBuffer ->  CoarseSampleOrderTypeNV ->  Vector CoarseSampleOrderCustomNV ->  IO ()
cmdSetCoarseSampleOrderNV = \(CommandBuffer commandBuffer' commandTable) -> \sampleOrderType' -> \customSampleOrders' -> withVec withCStructCoarseSampleOrderCustomNV customSampleOrders' (\pCustomSampleOrders' -> vkCmdSetCoarseSampleOrderNV commandTable commandBuffer' sampleOrderType' (fromIntegral $ Data.Vector.length customSampleOrders') pCustomSampleOrders' *> (pure ()))


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
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkShadingRatePaletteNV'
--     structures defining the palette for each viewport.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature /must/ be enabled.
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled
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
--     @viewportCount@ valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkShadingRatePaletteNV'
--     structures
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
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkShadingRatePaletteNV'
cmdSetViewportShadingRatePaletteNV :: CommandBuffer ->  Word32 ->  Vector ShadingRatePaletteNV ->  IO ()
cmdSetViewportShadingRatePaletteNV = \(CommandBuffer commandBuffer' commandTable) -> \firstViewport' -> \shadingRatePalettes' -> withVec withCStructShadingRatePaletteNV shadingRatePalettes' (\pShadingRatePalettes' -> vkCmdSetViewportShadingRatePaletteNV commandTable commandBuffer' firstViewport' (fromIntegral $ Data.Vector.length shadingRatePalettes') pShadingRatePalettes' *> (pure ()))

-- No documentation found for TopLevel "VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME"
pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME = VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION"
pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION :: Integral a => a
pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION = VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION
