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


-- No documentation found for TopLevel "VkCoarseSampleLocationNV"
data VkCoarseSampleLocationNV = VkCoarseSampleLocationNV
  { -- No documentation found for Nested "VkCoarseSampleLocationNV" "pixelX"
  vkPixelX :: Word32
  , -- No documentation found for Nested "VkCoarseSampleLocationNV" "pixelY"
  vkPixelY :: Word32
  , -- No documentation found for Nested "VkCoarseSampleLocationNV" "sample"
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

-- No documentation found for TopLevel "VkCoarseSampleOrderCustomNV"
data VkCoarseSampleOrderCustomNV = VkCoarseSampleOrderCustomNV
  { -- No documentation found for Nested "VkCoarseSampleOrderCustomNV" "shadingRate"
  vkShadingRate :: VkShadingRatePaletteEntryNV
  , -- No documentation found for Nested "VkCoarseSampleOrderCustomNV" "sampleCount"
  vkSampleCount :: Word32
  , -- No documentation found for Nested "VkCoarseSampleOrderCustomNV" "sampleLocationCount"
  vkSampleLocationCount :: Word32
  , -- No documentation found for Nested "VkCoarseSampleOrderCustomNV" "pSampleLocations"
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

-- No documentation found for TopLevel "VkCoarseSampleOrderTypeNV"
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

-- No documentation found for Nested "VkCoarseSampleOrderTypeNV" "VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV"
pattern VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV :: VkCoarseSampleOrderTypeNV
pattern VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV = VkCoarseSampleOrderTypeNV 0

-- No documentation found for Nested "VkCoarseSampleOrderTypeNV" "VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV"
pattern VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV :: VkCoarseSampleOrderTypeNV
pattern VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV = VkCoarseSampleOrderTypeNV 1

-- No documentation found for Nested "VkCoarseSampleOrderTypeNV" "VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV"
pattern VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV :: VkCoarseSampleOrderTypeNV
pattern VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV = VkCoarseSampleOrderTypeNV 2

-- No documentation found for Nested "VkCoarseSampleOrderTypeNV" "VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV"
pattern VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV :: VkCoarseSampleOrderTypeNV
pattern VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV = VkCoarseSampleOrderTypeNV 3

-- No documentation found for TopLevel "VkPhysicalDeviceShadingRateImageFeaturesNV"
data VkPhysicalDeviceShadingRateImageFeaturesNV = VkPhysicalDeviceShadingRateImageFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceShadingRateImageFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceShadingRateImageFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceShadingRateImageFeaturesNV" "shadingRateImage"
  vkShadingRateImage :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceShadingRateImageFeaturesNV" "shadingRateCoarseSampleOrder"
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

-- No documentation found for TopLevel "VkPhysicalDeviceShadingRateImagePropertiesNV"
data VkPhysicalDeviceShadingRateImagePropertiesNV = VkPhysicalDeviceShadingRateImagePropertiesNV
  { -- No documentation found for Nested "VkPhysicalDeviceShadingRateImagePropertiesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceShadingRateImagePropertiesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceShadingRateImagePropertiesNV" "shadingRateTexelSize"
  vkShadingRateTexelSize :: VkExtent2D
  , -- No documentation found for Nested "VkPhysicalDeviceShadingRateImagePropertiesNV" "shadingRatePaletteSize"
  vkShadingRatePaletteSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShadingRateImagePropertiesNV" "shadingRateMaxCoarseSamples"
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

-- No documentation found for TopLevel "VkPipelineViewportCoarseSampleOrderStateCreateInfoNV"
data VkPipelineViewportCoarseSampleOrderStateCreateInfoNV = VkPipelineViewportCoarseSampleOrderStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineViewportCoarseSampleOrderStateCreateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineViewportCoarseSampleOrderStateCreateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineViewportCoarseSampleOrderStateCreateInfoNV" "sampleOrderType"
  vkSampleOrderType :: VkCoarseSampleOrderTypeNV
  , -- No documentation found for Nested "VkPipelineViewportCoarseSampleOrderStateCreateInfoNV" "customSampleOrderCount"
  vkCustomSampleOrderCount :: Word32
  , -- No documentation found for Nested "VkPipelineViewportCoarseSampleOrderStateCreateInfoNV" "pCustomSampleOrders"
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

-- No documentation found for TopLevel "VkPipelineViewportShadingRateImageStateCreateInfoNV"
data VkPipelineViewportShadingRateImageStateCreateInfoNV = VkPipelineViewportShadingRateImageStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineViewportShadingRateImageStateCreateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineViewportShadingRateImageStateCreateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineViewportShadingRateImageStateCreateInfoNV" "shadingRateImageEnable"
  vkShadingRateImageEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineViewportShadingRateImageStateCreateInfoNV" "viewportCount"
  vkViewportCount :: Word32
  , -- No documentation found for Nested "VkPipelineViewportShadingRateImageStateCreateInfoNV" "pShadingRatePalettes"
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

-- No documentation found for TopLevel "VkShadingRatePaletteEntryNV"
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

-- No documentation found for TopLevel "VkShadingRatePaletteNV"
data VkShadingRatePaletteNV = VkShadingRatePaletteNV
  { -- No documentation found for Nested "VkShadingRatePaletteNV" "shadingRatePaletteEntryCount"
  vkShadingRatePaletteEntryCount :: Word32
  , -- No documentation found for Nested "VkShadingRatePaletteNV" "pShadingRatePaletteEntries"
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

-- No documentation found for TopLevel "vkCmdBindShadingRateImageNV"
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

-- No documentation found for TopLevel "vkCmdSetCoarseSampleOrderNV"
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

-- No documentation found for TopLevel "vkCmdSetViewportShadingRatePaletteNV"
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
