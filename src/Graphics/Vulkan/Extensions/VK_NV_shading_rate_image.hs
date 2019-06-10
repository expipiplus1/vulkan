{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_NV_shading_rate_image
  ( CoarseSampleLocationNV(..)
  , CoarseSampleOrderCustomNV(..)
  , CoarseSampleOrderTypeNV
  , pattern COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV
  , pattern COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV
  , pattern COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV
  , pattern COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDeviceShadingRateImageFeaturesNV(..)
  , PhysicalDeviceShadingRateImagePropertiesNV(..)
  , PipelineViewportCoarseSampleOrderStateCreateInfoNV(..)
  , PipelineViewportShadingRateImageStateCreateInfoNV(..)
#endif
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

import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( VkCoarseSampleOrderTypeNV(..)
  , VkShadingRatePaletteEntryNV(..)
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
  )
import Graphics.Vulkan.Core10.Image
  ( ImageLayout
  )
import Graphics.Vulkan.Core10.ImageView
  ( ImageView
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  )
#endif
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
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



-- No documentation found for TopLevel "VkCoarseSampleLocationNV"
data CoarseSampleLocationNV = CoarseSampleLocationNV
  { -- No documentation found for Nested "CoarseSampleLocationNV" "pixelX"
  pixelX :: Word32
  , -- No documentation found for Nested "CoarseSampleLocationNV" "pixelY"
  pixelY :: Word32
  , -- No documentation found for Nested "CoarseSampleLocationNV" "sample"
  sample :: Word32
  }
  deriving (Show, Eq)

instance Zero CoarseSampleLocationNV where
  zero = CoarseSampleLocationNV zero
                                zero
                                zero



-- No documentation found for TopLevel "VkCoarseSampleOrderCustomNV"
data CoarseSampleOrderCustomNV = CoarseSampleOrderCustomNV
  { -- No documentation found for Nested "CoarseSampleOrderCustomNV" "shadingRate"
  shadingRate :: ShadingRatePaletteEntryNV
  , -- No documentation found for Nested "CoarseSampleOrderCustomNV" "sampleCount"
  sampleCount :: Word32
  , -- No documentation found for Nested "CoarseSampleOrderCustomNV" "pSampleLocations"
  sampleLocations :: Vector CoarseSampleLocationNV
  }
  deriving (Show, Eq)

instance Zero CoarseSampleOrderCustomNV where
  zero = CoarseSampleOrderCustomNV zero
                                   zero
                                   mempty


-- No documentation found for TopLevel "CoarseSampleOrderTypeNV"
type CoarseSampleOrderTypeNV = VkCoarseSampleOrderTypeNV


{-# complete COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV, COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV, COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV, COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV :: CoarseSampleOrderTypeNV #-}


-- No documentation found for Nested "CoarseSampleOrderTypeNV" "COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV"
pattern COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV :: (a ~ CoarseSampleOrderTypeNV) => a
pattern COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV = VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV


-- No documentation found for Nested "CoarseSampleOrderTypeNV" "COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV"
pattern COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV :: (a ~ CoarseSampleOrderTypeNV) => a
pattern COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV = VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV


-- No documentation found for Nested "CoarseSampleOrderTypeNV" "COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV"
pattern COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV :: (a ~ CoarseSampleOrderTypeNV) => a
pattern COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV = VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV


-- No documentation found for Nested "CoarseSampleOrderTypeNV" "COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV"
pattern COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV :: (a ~ CoarseSampleOrderTypeNV) => a
pattern COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV = VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceShadingRateImageFeaturesNV"
data PhysicalDeviceShadingRateImageFeaturesNV = PhysicalDeviceShadingRateImageFeaturesNV
  { -- No documentation found for Nested "PhysicalDeviceShadingRateImageFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImageFeaturesNV" "shadingRateImage"
  shadingRateImage :: Bool
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImageFeaturesNV" "shadingRateCoarseSampleOrder"
  shadingRateCoarseSampleOrder :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceShadingRateImageFeaturesNV where
  zero = PhysicalDeviceShadingRateImageFeaturesNV Nothing
                                                  False
                                                  False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceShadingRateImagePropertiesNV"
data PhysicalDeviceShadingRateImagePropertiesNV = PhysicalDeviceShadingRateImagePropertiesNV
  { -- No documentation found for Nested "PhysicalDeviceShadingRateImagePropertiesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImagePropertiesNV" "shadingRateTexelSize"
  shadingRateTexelSize :: Extent2D
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImagePropertiesNV" "shadingRatePaletteSize"
  shadingRatePaletteSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImagePropertiesNV" "shadingRateMaxCoarseSamples"
  shadingRateMaxCoarseSamples :: Word32
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceShadingRateImagePropertiesNV where
  zero = PhysicalDeviceShadingRateImagePropertiesNV Nothing
                                                    zero
                                                    zero
                                                    zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineViewportCoarseSampleOrderStateCreateInfoNV"
data PipelineViewportCoarseSampleOrderStateCreateInfoNV = PipelineViewportCoarseSampleOrderStateCreateInfoNV
  { -- No documentation found for Nested "PipelineViewportCoarseSampleOrderStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportCoarseSampleOrderStateCreateInfoNV" "sampleOrderType"
  sampleOrderType :: CoarseSampleOrderTypeNV
  , -- No documentation found for Nested "PipelineViewportCoarseSampleOrderStateCreateInfoNV" "pCustomSampleOrders"
  customSampleOrders :: Vector CoarseSampleOrderCustomNV
  }
  deriving (Show, Eq)

instance Zero PipelineViewportCoarseSampleOrderStateCreateInfoNV where
  zero = PipelineViewportCoarseSampleOrderStateCreateInfoNV Nothing
                                                            zero
                                                            mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineViewportShadingRateImageStateCreateInfoNV"
data PipelineViewportShadingRateImageStateCreateInfoNV = PipelineViewportShadingRateImageStateCreateInfoNV
  { -- No documentation found for Nested "PipelineViewportShadingRateImageStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportShadingRateImageStateCreateInfoNV" "shadingRateImageEnable"
  shadingRateImageEnable :: Bool
  , -- No documentation found for Nested "PipelineViewportShadingRateImageStateCreateInfoNV" "pShadingRatePalettes"
  shadingRatePalettes :: Either Word32 (Vector ShadingRatePaletteNV)
  }
  deriving (Show, Eq)

instance Zero PipelineViewportShadingRateImageStateCreateInfoNV where
  zero = PipelineViewportShadingRateImageStateCreateInfoNV Nothing
                                                           False
                                                           (Left 0)

#endif

-- No documentation found for TopLevel "ShadingRatePaletteEntryNV"
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


-- No documentation found for TopLevel "VkShadingRatePaletteNV"
data ShadingRatePaletteNV = ShadingRatePaletteNV
  { -- No documentation found for Nested "ShadingRatePaletteNV" "pShadingRatePaletteEntries"
  shadingRatePaletteEntries :: Vector ShadingRatePaletteEntryNV
  }
  deriving (Show, Eq)

instance Zero ShadingRatePaletteNV where
  zero = ShadingRatePaletteNV mempty



-- No documentation found for TopLevel "vkCmdBindShadingRateImageNV"
cmdBindShadingRateImageNV :: CommandBuffer ->  ImageView ->  ImageLayout ->  IO ()
cmdBindShadingRateImageNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetCoarseSampleOrderNV"
cmdSetCoarseSampleOrderNV :: CommandBuffer ->  CoarseSampleOrderTypeNV ->  Vector CoarseSampleOrderCustomNV ->  IO ()
cmdSetCoarseSampleOrderNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetViewportShadingRatePaletteNV"
cmdSetViewportShadingRatePaletteNV :: CommandBuffer ->  Word32 ->  Vector ShadingRatePaletteNV ->  IO ()
cmdSetViewportShadingRatePaletteNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME"
pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME = VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION"
pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION :: Integral a => a
pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION = VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION
