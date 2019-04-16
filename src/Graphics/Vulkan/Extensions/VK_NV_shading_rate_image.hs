{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_shading_rate_image
  ( withCStructCoarseSampleLocationNV
  , fromCStructCoarseSampleLocationNV
  , CoarseSampleLocationNV(..)
  , withCStructCoarseSampleOrderCustomNV
  , fromCStructCoarseSampleOrderCustomNV
  , CoarseSampleOrderCustomNV(..)
  , CoarseSampleOrderTypeNV
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
  , withCStructShadingRatePaletteNV
  , fromCStructShadingRatePaletteNV
  , ShadingRatePaletteNV(..)
  , cmdBindShadingRateImageNV
  , cmdSetCoarseSampleOrderNV
  , cmdSetViewportShadingRatePaletteNV
  , pattern VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION
  , pattern VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV
  , pattern VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV
  , pattern VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV
  , pattern VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV
  , pattern VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV
  , pattern VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV
  , pattern VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV
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
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
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
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdBindShadingRateImageNV
  , cmdSetCoarseSampleOrderNV
  , cmdSetViewportShadingRatePaletteNV
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
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( pattern VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV
  , pattern VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV
  , pattern VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV
  , pattern VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV
  , pattern VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV
  , pattern VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME
  , pattern VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION
  , pattern VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV
  )


-- No documentation found for TopLevel "CoarseSampleLocationNV"
data CoarseSampleLocationNV = CoarseSampleLocationNV
  { -- No documentation found for Nested "CoarseSampleLocationNV" "pixelX"
  vkPixelX :: Word32
  , -- No documentation found for Nested "CoarseSampleLocationNV" "pixelY"
  vkPixelY :: Word32
  , -- No documentation found for Nested "CoarseSampleLocationNV" "sample"
  vkSample :: Word32
  }
  deriving (Show, Eq)
withCStructCoarseSampleLocationNV :: CoarseSampleLocationNV -> (VkCoarseSampleLocationNV -> IO a) -> IO a
withCStructCoarseSampleLocationNV from cont = cont (VkCoarseSampleLocationNV (vkPixelX (from :: CoarseSampleLocationNV)) (vkPixelY (from :: CoarseSampleLocationNV)) (vkSample (from :: CoarseSampleLocationNV)))
fromCStructCoarseSampleLocationNV :: VkCoarseSampleLocationNV -> IO CoarseSampleLocationNV
fromCStructCoarseSampleLocationNV c = CoarseSampleLocationNV <$> pure (vkPixelX (c :: VkCoarseSampleLocationNV))
                                                             <*> pure (vkPixelY (c :: VkCoarseSampleLocationNV))
                                                             <*> pure (vkSample (c :: VkCoarseSampleLocationNV))
-- No documentation found for TopLevel "CoarseSampleOrderCustomNV"
data CoarseSampleOrderCustomNV = CoarseSampleOrderCustomNV
  { -- No documentation found for Nested "CoarseSampleOrderCustomNV" "shadingRate"
  vkShadingRate :: ShadingRatePaletteEntryNV
  , -- No documentation found for Nested "CoarseSampleOrderCustomNV" "sampleCount"
  vkSampleCount :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "CoarseSampleOrderCustomNV" "pSampleLocations"
  vkPSampleLocations :: Vector CoarseSampleLocationNV
  }
  deriving (Show, Eq)
withCStructCoarseSampleOrderCustomNV :: CoarseSampleOrderCustomNV -> (VkCoarseSampleOrderCustomNV -> IO a) -> IO a
withCStructCoarseSampleOrderCustomNV from cont = withVec withCStructCoarseSampleLocationNV (vkPSampleLocations (from :: CoarseSampleOrderCustomNV)) (\pSampleLocations -> cont (VkCoarseSampleOrderCustomNV (vkShadingRate (from :: CoarseSampleOrderCustomNV)) (vkSampleCount (from :: CoarseSampleOrderCustomNV)) (fromIntegral (Data.Vector.length (vkPSampleLocations (from :: CoarseSampleOrderCustomNV)))) pSampleLocations))
fromCStructCoarseSampleOrderCustomNV :: VkCoarseSampleOrderCustomNV -> IO CoarseSampleOrderCustomNV
fromCStructCoarseSampleOrderCustomNV c = CoarseSampleOrderCustomNV <$> pure (vkShadingRate (c :: VkCoarseSampleOrderCustomNV))
                                                                   <*> pure (vkSampleCount (c :: VkCoarseSampleOrderCustomNV))
                                                                   -- Length valued member elided
                                                                   <*> (Data.Vector.generateM (fromIntegral (vkSampleLocationCount (c :: VkCoarseSampleOrderCustomNV))) (((fromCStructCoarseSampleLocationNV <=<) . peekElemOff) (vkPSampleLocations (c :: VkCoarseSampleOrderCustomNV))))
-- No documentation found for TopLevel "CoarseSampleOrderTypeNV"
type CoarseSampleOrderTypeNV = VkCoarseSampleOrderTypeNV
-- No documentation found for TopLevel "PhysicalDeviceShadingRateImageFeaturesNV"
data PhysicalDeviceShadingRateImageFeaturesNV = PhysicalDeviceShadingRateImageFeaturesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceShadingRateImageFeaturesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImageFeaturesNV" "shadingRateImage"
  vkShadingRateImage :: Bool
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImageFeaturesNV" "shadingRateCoarseSampleOrder"
  vkShadingRateCoarseSampleOrder :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceShadingRateImageFeaturesNV :: PhysicalDeviceShadingRateImageFeaturesNV -> (VkPhysicalDeviceShadingRateImageFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceShadingRateImageFeaturesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceShadingRateImageFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceShadingRateImageFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV pPNext (boolToBool32 (vkShadingRateImage (from :: PhysicalDeviceShadingRateImageFeaturesNV))) (boolToBool32 (vkShadingRateCoarseSampleOrder (from :: PhysicalDeviceShadingRateImageFeaturesNV)))))
fromCStructPhysicalDeviceShadingRateImageFeaturesNV :: VkPhysicalDeviceShadingRateImageFeaturesNV -> IO PhysicalDeviceShadingRateImageFeaturesNV
fromCStructPhysicalDeviceShadingRateImageFeaturesNV c = PhysicalDeviceShadingRateImageFeaturesNV <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceShadingRateImageFeaturesNV)))
                                                                                                 <*> pure (bool32ToBool (vkShadingRateImage (c :: VkPhysicalDeviceShadingRateImageFeaturesNV)))
                                                                                                 <*> pure (bool32ToBool (vkShadingRateCoarseSampleOrder (c :: VkPhysicalDeviceShadingRateImageFeaturesNV)))
-- No documentation found for TopLevel "PhysicalDeviceShadingRateImagePropertiesNV"
data PhysicalDeviceShadingRateImagePropertiesNV = PhysicalDeviceShadingRateImagePropertiesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceShadingRateImagePropertiesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImagePropertiesNV" "shadingRateTexelSize"
  vkShadingRateTexelSize :: Extent2D
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImagePropertiesNV" "shadingRatePaletteSize"
  vkShadingRatePaletteSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShadingRateImagePropertiesNV" "shadingRateMaxCoarseSamples"
  vkShadingRateMaxCoarseSamples :: Word32
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceShadingRateImagePropertiesNV :: PhysicalDeviceShadingRateImagePropertiesNV -> (VkPhysicalDeviceShadingRateImagePropertiesNV -> IO a) -> IO a
withCStructPhysicalDeviceShadingRateImagePropertiesNV from cont = withCStructExtent2D (vkShadingRateTexelSize (from :: PhysicalDeviceShadingRateImagePropertiesNV)) (\shadingRateTexelSize -> maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceShadingRateImagePropertiesNV)) (\pPNext -> cont (VkPhysicalDeviceShadingRateImagePropertiesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV pPNext shadingRateTexelSize (vkShadingRatePaletteSize (from :: PhysicalDeviceShadingRateImagePropertiesNV)) (vkShadingRateMaxCoarseSamples (from :: PhysicalDeviceShadingRateImagePropertiesNV)))))
fromCStructPhysicalDeviceShadingRateImagePropertiesNV :: VkPhysicalDeviceShadingRateImagePropertiesNV -> IO PhysicalDeviceShadingRateImagePropertiesNV
fromCStructPhysicalDeviceShadingRateImagePropertiesNV c = PhysicalDeviceShadingRateImagePropertiesNV <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceShadingRateImagePropertiesNV)))
                                                                                                     <*> (fromCStructExtent2D (vkShadingRateTexelSize (c :: VkPhysicalDeviceShadingRateImagePropertiesNV)))
                                                                                                     <*> pure (vkShadingRatePaletteSize (c :: VkPhysicalDeviceShadingRateImagePropertiesNV))
                                                                                                     <*> pure (vkShadingRateMaxCoarseSamples (c :: VkPhysicalDeviceShadingRateImagePropertiesNV))
-- No documentation found for TopLevel "PipelineViewportCoarseSampleOrderStateCreateInfoNV"
data PipelineViewportCoarseSampleOrderStateCreateInfoNV = PipelineViewportCoarseSampleOrderStateCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineViewportCoarseSampleOrderStateCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportCoarseSampleOrderStateCreateInfoNV" "sampleOrderType"
  vkSampleOrderType :: CoarseSampleOrderTypeNV
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineViewportCoarseSampleOrderStateCreateInfoNV" "pCustomSampleOrders"
  vkPCustomSampleOrders :: Vector CoarseSampleOrderCustomNV
  }
  deriving (Show, Eq)
withCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV :: PipelineViewportCoarseSampleOrderStateCreateInfoNV -> (VkPipelineViewportCoarseSampleOrderStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV from cont = withVec withCStructCoarseSampleOrderCustomNV (vkPCustomSampleOrders (from :: PipelineViewportCoarseSampleOrderStateCreateInfoNV)) (\pCustomSampleOrders -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineViewportCoarseSampleOrderStateCreateInfoNV)) (\pPNext -> cont (VkPipelineViewportCoarseSampleOrderStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV pPNext (vkSampleOrderType (from :: PipelineViewportCoarseSampleOrderStateCreateInfoNV)) (fromIntegral (Data.Vector.length (vkPCustomSampleOrders (from :: PipelineViewportCoarseSampleOrderStateCreateInfoNV)))) pCustomSampleOrders)))
fromCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV -> IO PipelineViewportCoarseSampleOrderStateCreateInfoNV
fromCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV c = PipelineViewportCoarseSampleOrderStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV)))
                                                                                                                     <*> pure (vkSampleOrderType (c :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))
                                                                                                                     -- Length valued member elided
                                                                                                                     <*> (Data.Vector.generateM (fromIntegral (vkCustomSampleOrderCount (c :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))) (((fromCStructCoarseSampleOrderCustomNV <=<) . peekElemOff) (vkPCustomSampleOrders (c :: VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))))
-- No documentation found for TopLevel "PipelineViewportShadingRateImageStateCreateInfoNV"
data PipelineViewportShadingRateImageStateCreateInfoNV = PipelineViewportShadingRateImageStateCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineViewportShadingRateImageStateCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportShadingRateImageStateCreateInfoNV" "shadingRateImageEnable"
  vkShadingRateImageEnable :: Bool
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineViewportShadingRateImageStateCreateInfoNV" "pShadingRatePalettes"
  vkPShadingRatePalettes :: Maybe (Vector ShadingRatePaletteNV)
  }
  deriving (Show, Eq)
withCStructPipelineViewportShadingRateImageStateCreateInfoNV :: PipelineViewportShadingRateImageStateCreateInfoNV -> (VkPipelineViewportShadingRateImageStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineViewportShadingRateImageStateCreateInfoNV from cont = maybeWith (withVec withCStructShadingRatePaletteNV) (vkPShadingRatePalettes (from :: PipelineViewportShadingRateImageStateCreateInfoNV)) (\pShadingRatePalettes -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineViewportShadingRateImageStateCreateInfoNV)) (\pPNext -> cont (VkPipelineViewportShadingRateImageStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV pPNext (boolToBool32 (vkShadingRateImageEnable (from :: PipelineViewportShadingRateImageStateCreateInfoNV))) (maybe 0 (fromIntegral . Data.Vector.length) (vkPShadingRatePalettes (from :: PipelineViewportShadingRateImageStateCreateInfoNV))) pShadingRatePalettes)))
fromCStructPipelineViewportShadingRateImageStateCreateInfoNV :: VkPipelineViewportShadingRateImageStateCreateInfoNV -> IO PipelineViewportShadingRateImageStateCreateInfoNV
fromCStructPipelineViewportShadingRateImageStateCreateInfoNV c = PipelineViewportShadingRateImageStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineViewportShadingRateImageStateCreateInfoNV)))
                                                                                                                   <*> pure (bool32ToBool (vkShadingRateImageEnable (c :: VkPipelineViewportShadingRateImageStateCreateInfoNV)))
                                                                                                                   -- Optional length valued member elided
                                                                                                                   <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkViewportCount (c :: VkPipelineViewportShadingRateImageStateCreateInfoNV))) (((fromCStructShadingRatePaletteNV <=<) . peekElemOff) p)) (vkPShadingRatePalettes (c :: VkPipelineViewportShadingRateImageStateCreateInfoNV))
-- No documentation found for TopLevel "ShadingRatePaletteEntryNV"
type ShadingRatePaletteEntryNV = VkShadingRatePaletteEntryNV
-- No documentation found for TopLevel "ShadingRatePaletteNV"
data ShadingRatePaletteNV = ShadingRatePaletteNV
  { -- Length valued member elided
  -- No documentation found for Nested "ShadingRatePaletteNV" "pShadingRatePaletteEntries"
  vkPShadingRatePaletteEntries :: Vector ShadingRatePaletteEntryNV
  }
  deriving (Show, Eq)
withCStructShadingRatePaletteNV :: ShadingRatePaletteNV -> (VkShadingRatePaletteNV -> IO a) -> IO a
withCStructShadingRatePaletteNV from cont = withVec (&) (vkPShadingRatePaletteEntries (from :: ShadingRatePaletteNV)) (\pShadingRatePaletteEntries -> cont (VkShadingRatePaletteNV (fromIntegral (Data.Vector.length (vkPShadingRatePaletteEntries (from :: ShadingRatePaletteNV)))) pShadingRatePaletteEntries))
fromCStructShadingRatePaletteNV :: VkShadingRatePaletteNV -> IO ShadingRatePaletteNV
fromCStructShadingRatePaletteNV c = ShadingRatePaletteNV <$> -- Length valued member elided
                                                         (Data.Vector.generateM (fromIntegral (vkShadingRatePaletteEntryCount (c :: VkShadingRatePaletteNV))) (peekElemOff (vkPShadingRatePaletteEntries (c :: VkShadingRatePaletteNV))))

-- | Wrapper for 'vkCmdBindShadingRateImageNV'
cmdBindShadingRateImageNV :: CommandBuffer ->  ImageView ->  ImageLayout ->  IO ()
cmdBindShadingRateImageNV = \(CommandBuffer commandBuffer commandTable) -> \imageView -> \imageLayout -> Graphics.Vulkan.C.Dynamic.cmdBindShadingRateImageNV commandTable commandBuffer imageView imageLayout *> (pure ())

-- | Wrapper for 'vkCmdSetCoarseSampleOrderNV'
cmdSetCoarseSampleOrderNV :: CommandBuffer ->  CoarseSampleOrderTypeNV ->  Vector CoarseSampleOrderCustomNV ->  IO (  )
cmdSetCoarseSampleOrderNV = \(CommandBuffer commandBuffer commandTable) -> \sampleOrderType -> \customSampleOrders -> withVec withCStructCoarseSampleOrderCustomNV customSampleOrders (\pCustomSampleOrders -> Graphics.Vulkan.C.Dynamic.cmdSetCoarseSampleOrderNV commandTable commandBuffer sampleOrderType (fromIntegral $ Data.Vector.length customSampleOrders) pCustomSampleOrders *> (pure ()))

-- | Wrapper for 'vkCmdSetViewportShadingRatePaletteNV'
cmdSetViewportShadingRatePaletteNV :: CommandBuffer ->  Word32 ->  Vector ShadingRatePaletteNV ->  IO ()
cmdSetViewportShadingRatePaletteNV = \(CommandBuffer commandBuffer commandTable) -> \firstViewport -> \shadingRatePalettes -> withVec withCStructShadingRatePaletteNV shadingRatePalettes (\pShadingRatePalettes -> Graphics.Vulkan.C.Dynamic.cmdSetViewportShadingRatePaletteNV commandTable commandBuffer firstViewport (fromIntegral $ Data.Vector.length shadingRatePalettes) pShadingRatePalettes *> (pure ()))
