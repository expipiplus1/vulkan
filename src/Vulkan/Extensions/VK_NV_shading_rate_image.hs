{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_shading_rate_image"
module Vulkan.Extensions.VK_NV_shading_rate_image  ( cmdBindShadingRateImageNV
                                                   , cmdSetViewportShadingRatePaletteNV
                                                   , cmdSetCoarseSampleOrderNV
                                                   , ShadingRatePaletteNV(..)
                                                   , PipelineViewportShadingRateImageStateCreateInfoNV(..)
                                                   , PhysicalDeviceShadingRateImageFeaturesNV(..)
                                                   , PhysicalDeviceShadingRateImagePropertiesNV(..)
                                                   , CoarseSampleLocationNV(..)
                                                   , CoarseSampleOrderCustomNV(..)
                                                   , PipelineViewportCoarseSampleOrderStateCreateInfoNV(..)
                                                   , ShadingRatePaletteEntryNV( SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV
                                                                              , SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV
                                                                              , ..
                                                                              )
                                                   , CoarseSampleOrderTypeNV( COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV
                                                                            , COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV
                                                                            , COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV
                                                                            , COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV
                                                                            , ..
                                                                            )
                                                   , NV_SHADING_RATE_IMAGE_SPEC_VERSION
                                                   , pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION
                                                   , NV_SHADING_RATE_IMAGE_EXTENSION_NAME
                                                   , pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME
                                                   ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindShadingRateImageNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetCoarseSampleOrderNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewportShadingRatePaletteNV))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Vulkan.Core10.Handles (ImageView)
import Vulkan.Core10.Handles (ImageView(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindShadingRateImageNV
  :: FunPtr (Ptr CommandBuffer_T -> ImageView -> ImageLayout -> IO ()) -> Ptr CommandBuffer_T -> ImageView -> ImageLayout -> IO ()

-- No documentation found for TopLevel "vkCmdBindShadingRateImageNV"
cmdBindShadingRateImageNV :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkCmdBindShadingRateImageNV" "commandBuffer"
                             CommandBuffer
                          -> -- No documentation found for Nested "vkCmdBindShadingRateImageNV" "imageView"
                             ImageView
                          -> -- No documentation found for Nested "vkCmdBindShadingRateImageNV" "imageLayout"
                             ImageLayout
                          -> io ()
cmdBindShadingRateImageNV commandBuffer imageView imageLayout = liftIO $ do
  let vkCmdBindShadingRateImageNVPtr = pVkCmdBindShadingRateImageNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBindShadingRateImageNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindShadingRateImageNV is null" Nothing Nothing
  let vkCmdBindShadingRateImageNV' = mkVkCmdBindShadingRateImageNV vkCmdBindShadingRateImageNVPtr
  vkCmdBindShadingRateImageNV' (commandBufferHandle (commandBuffer)) (imageView) (imageLayout)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportShadingRatePaletteNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ShadingRatePaletteNV -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ShadingRatePaletteNV -> IO ()

-- No documentation found for TopLevel "vkCmdSetViewportShadingRatePaletteNV"
cmdSetViewportShadingRatePaletteNV :: forall io
                                    . (MonadIO io)
                                   => -- No documentation found for Nested "vkCmdSetViewportShadingRatePaletteNV" "commandBuffer"
                                      CommandBuffer
                                   -> -- No documentation found for Nested "vkCmdSetViewportShadingRatePaletteNV" "firstViewport"
                                      ("firstViewport" ::: Word32)
                                   -> -- No documentation found for Nested "vkCmdSetViewportShadingRatePaletteNV" "pShadingRatePalettes"
                                      ("shadingRatePalettes" ::: Vector ShadingRatePaletteNV)
                                   -> io ()
cmdSetViewportShadingRatePaletteNV commandBuffer firstViewport shadingRatePalettes = liftIO . evalContT $ do
  let vkCmdSetViewportShadingRatePaletteNVPtr = pVkCmdSetViewportShadingRatePaletteNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetViewportShadingRatePaletteNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetViewportShadingRatePaletteNV is null" Nothing Nothing
  let vkCmdSetViewportShadingRatePaletteNV' = mkVkCmdSetViewportShadingRatePaletteNV vkCmdSetViewportShadingRatePaletteNVPtr
  pPShadingRatePalettes <- ContT $ allocaBytesAligned @ShadingRatePaletteNV ((Data.Vector.length (shadingRatePalettes)) * 16) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPShadingRatePalettes `plusPtr` (16 * (i)) :: Ptr ShadingRatePaletteNV) (e) . ($ ())) (shadingRatePalettes)
  lift $ vkCmdSetViewportShadingRatePaletteNV' (commandBufferHandle (commandBuffer)) (firstViewport) ((fromIntegral (Data.Vector.length $ (shadingRatePalettes)) :: Word32)) (pPShadingRatePalettes)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCoarseSampleOrderNV
  :: FunPtr (Ptr CommandBuffer_T -> CoarseSampleOrderTypeNV -> Word32 -> Ptr CoarseSampleOrderCustomNV -> IO ()) -> Ptr CommandBuffer_T -> CoarseSampleOrderTypeNV -> Word32 -> Ptr CoarseSampleOrderCustomNV -> IO ()

-- No documentation found for TopLevel "vkCmdSetCoarseSampleOrderNV"
cmdSetCoarseSampleOrderNV :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkCmdSetCoarseSampleOrderNV" "commandBuffer"
                             CommandBuffer
                          -> -- No documentation found for Nested "vkCmdSetCoarseSampleOrderNV" "sampleOrderType"
                             CoarseSampleOrderTypeNV
                          -> -- No documentation found for Nested "vkCmdSetCoarseSampleOrderNV" "pCustomSampleOrders"
                             ("customSampleOrders" ::: Vector CoarseSampleOrderCustomNV)
                          -> io ()
cmdSetCoarseSampleOrderNV commandBuffer sampleOrderType customSampleOrders = liftIO . evalContT $ do
  let vkCmdSetCoarseSampleOrderNVPtr = pVkCmdSetCoarseSampleOrderNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetCoarseSampleOrderNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCoarseSampleOrderNV is null" Nothing Nothing
  let vkCmdSetCoarseSampleOrderNV' = mkVkCmdSetCoarseSampleOrderNV vkCmdSetCoarseSampleOrderNVPtr
  pPCustomSampleOrders <- ContT $ allocaBytesAligned @CoarseSampleOrderCustomNV ((Data.Vector.length (customSampleOrders)) * 24) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCustomSampleOrders `plusPtr` (24 * (i)) :: Ptr CoarseSampleOrderCustomNV) (e) . ($ ())) (customSampleOrders)
  lift $ vkCmdSetCoarseSampleOrderNV' (commandBufferHandle (commandBuffer)) (sampleOrderType) ((fromIntegral (Data.Vector.length $ (customSampleOrders)) :: Word32)) (pPCustomSampleOrders)
  pure $ ()



-- No documentation found for TopLevel "VkShadingRatePaletteNV"
data ShadingRatePaletteNV = ShadingRatePaletteNV
  { -- No documentation found for Nested "VkShadingRatePaletteNV" "pShadingRatePaletteEntries"
    shadingRatePaletteEntries :: Vector ShadingRatePaletteEntryNV }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShadingRatePaletteNV)
#endif
deriving instance Show ShadingRatePaletteNV

instance ToCStruct ShadingRatePaletteNV where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShadingRatePaletteNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (shadingRatePaletteEntries)) :: Word32))
    pPShadingRatePaletteEntries' <- ContT $ allocaBytesAligned @ShadingRatePaletteEntryNV ((Data.Vector.length (shadingRatePaletteEntries)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPShadingRatePaletteEntries' `plusPtr` (4 * (i)) :: Ptr ShadingRatePaletteEntryNV) (e)) (shadingRatePaletteEntries)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ShadingRatePaletteEntryNV))) (pPShadingRatePaletteEntries')
    lift $ f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    pPShadingRatePaletteEntries' <- ContT $ allocaBytesAligned @ShadingRatePaletteEntryNV ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPShadingRatePaletteEntries' `plusPtr` (4 * (i)) :: Ptr ShadingRatePaletteEntryNV) (e)) (mempty)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ShadingRatePaletteEntryNV))) (pPShadingRatePaletteEntries')
    lift $ f

instance FromCStruct ShadingRatePaletteNV where
  peekCStruct p = do
    shadingRatePaletteEntryCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pShadingRatePaletteEntries <- peek @(Ptr ShadingRatePaletteEntryNV) ((p `plusPtr` 8 :: Ptr (Ptr ShadingRatePaletteEntryNV)))
    pShadingRatePaletteEntries' <- generateM (fromIntegral shadingRatePaletteEntryCount) (\i -> peek @ShadingRatePaletteEntryNV ((pShadingRatePaletteEntries `advancePtrBytes` (4 * (i)) :: Ptr ShadingRatePaletteEntryNV)))
    pure $ ShadingRatePaletteNV
             pShadingRatePaletteEntries'

instance Zero ShadingRatePaletteNV where
  zero = ShadingRatePaletteNV
           mempty



-- No documentation found for TopLevel "VkPipelineViewportShadingRateImageStateCreateInfoNV"
data PipelineViewportShadingRateImageStateCreateInfoNV = PipelineViewportShadingRateImageStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineViewportShadingRateImageStateCreateInfoNV" "shadingRateImageEnable"
    shadingRateImageEnable :: Bool
  , -- No documentation found for Nested "VkPipelineViewportShadingRateImageStateCreateInfoNV" "pShadingRatePalettes"
    shadingRatePalettes :: Vector ShadingRatePaletteNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineViewportShadingRateImageStateCreateInfoNV)
#endif
deriving instance Show PipelineViewportShadingRateImageStateCreateInfoNV

instance ToCStruct PipelineViewportShadingRateImageStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportShadingRateImageStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shadingRateImageEnable))
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (shadingRatePalettes)) :: Word32))
    pPShadingRatePalettes' <- ContT $ allocaBytesAligned @ShadingRatePaletteNV ((Data.Vector.length (shadingRatePalettes)) * 16) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPShadingRatePalettes' `plusPtr` (16 * (i)) :: Ptr ShadingRatePaletteNV) (e) . ($ ())) (shadingRatePalettes)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ShadingRatePaletteNV))) (pPShadingRatePalettes')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    pPShadingRatePalettes' <- ContT $ allocaBytesAligned @ShadingRatePaletteNV ((Data.Vector.length (mempty)) * 16) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPShadingRatePalettes' `plusPtr` (16 * (i)) :: Ptr ShadingRatePaletteNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ShadingRatePaletteNV))) (pPShadingRatePalettes')
    lift $ f

instance FromCStruct PipelineViewportShadingRateImageStateCreateInfoNV where
  peekCStruct p = do
    shadingRateImageEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    viewportCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pShadingRatePalettes <- peek @(Ptr ShadingRatePaletteNV) ((p `plusPtr` 24 :: Ptr (Ptr ShadingRatePaletteNV)))
    pShadingRatePalettes' <- generateM (fromIntegral viewportCount) (\i -> peekCStruct @ShadingRatePaletteNV ((pShadingRatePalettes `advancePtrBytes` (16 * (i)) :: Ptr ShadingRatePaletteNV)))
    pure $ PipelineViewportShadingRateImageStateCreateInfoNV
             (bool32ToBool shadingRateImageEnable) pShadingRatePalettes'

instance Zero PipelineViewportShadingRateImageStateCreateInfoNV where
  zero = PipelineViewportShadingRateImageStateCreateInfoNV
           zero
           mempty



-- No documentation found for TopLevel "VkPhysicalDeviceShadingRateImageFeaturesNV"
data PhysicalDeviceShadingRateImageFeaturesNV = PhysicalDeviceShadingRateImageFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceShadingRateImageFeaturesNV" "shadingRateImage"
    shadingRateImage :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShadingRateImageFeaturesNV" "shadingRateCoarseSampleOrder"
    shadingRateCoarseSampleOrder :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShadingRateImageFeaturesNV)
#endif
deriving instance Show PhysicalDeviceShadingRateImageFeaturesNV

instance ToCStruct PhysicalDeviceShadingRateImageFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShadingRateImageFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shadingRateImage))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shadingRateCoarseSampleOrder))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShadingRateImageFeaturesNV where
  peekCStruct p = do
    shadingRateImage <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shadingRateCoarseSampleOrder <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceShadingRateImageFeaturesNV
             (bool32ToBool shadingRateImage) (bool32ToBool shadingRateCoarseSampleOrder)


instance Storable PhysicalDeviceShadingRateImageFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShadingRateImageFeaturesNV where
  zero = PhysicalDeviceShadingRateImageFeaturesNV
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceShadingRateImagePropertiesNV"
data PhysicalDeviceShadingRateImagePropertiesNV = PhysicalDeviceShadingRateImagePropertiesNV
  { -- No documentation found for Nested "VkPhysicalDeviceShadingRateImagePropertiesNV" "shadingRateTexelSize"
    shadingRateTexelSize :: Extent2D
  , -- No documentation found for Nested "VkPhysicalDeviceShadingRateImagePropertiesNV" "shadingRatePaletteSize"
    shadingRatePaletteSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShadingRateImagePropertiesNV" "shadingRateMaxCoarseSamples"
    shadingRateMaxCoarseSamples :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShadingRateImagePropertiesNV)
#endif
deriving instance Show PhysicalDeviceShadingRateImagePropertiesNV

instance ToCStruct PhysicalDeviceShadingRateImagePropertiesNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShadingRateImagePropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (shadingRateTexelSize)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (shadingRatePaletteSize)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (shadingRateMaxCoarseSamples)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceShadingRateImagePropertiesNV where
  peekCStruct p = do
    shadingRateTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    shadingRatePaletteSize <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    shadingRateMaxCoarseSamples <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ PhysicalDeviceShadingRateImagePropertiesNV
             shadingRateTexelSize shadingRatePaletteSize shadingRateMaxCoarseSamples


instance Storable PhysicalDeviceShadingRateImagePropertiesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShadingRateImagePropertiesNV where
  zero = PhysicalDeviceShadingRateImagePropertiesNV
           zero
           zero
           zero



-- No documentation found for TopLevel "VkCoarseSampleLocationNV"
data CoarseSampleLocationNV = CoarseSampleLocationNV
  { -- No documentation found for Nested "VkCoarseSampleLocationNV" "pixelX"
    pixelX :: Word32
  , -- No documentation found for Nested "VkCoarseSampleLocationNV" "pixelY"
    pixelY :: Word32
  , -- No documentation found for Nested "VkCoarseSampleLocationNV" "sample"
    sample :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CoarseSampleLocationNV)
#endif
deriving instance Show CoarseSampleLocationNV

instance ToCStruct CoarseSampleLocationNV where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CoarseSampleLocationNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (pixelX)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (pixelY)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (sample)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct CoarseSampleLocationNV where
  peekCStruct p = do
    pixelX <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pixelY <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    sample <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ CoarseSampleLocationNV
             pixelX pixelY sample


instance Storable CoarseSampleLocationNV where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CoarseSampleLocationNV where
  zero = CoarseSampleLocationNV
           zero
           zero
           zero



-- No documentation found for TopLevel "VkCoarseSampleOrderCustomNV"
data CoarseSampleOrderCustomNV = CoarseSampleOrderCustomNV
  { -- No documentation found for Nested "VkCoarseSampleOrderCustomNV" "shadingRate"
    shadingRate :: ShadingRatePaletteEntryNV
  , -- No documentation found for Nested "VkCoarseSampleOrderCustomNV" "sampleCount"
    sampleCount :: Word32
  , -- No documentation found for Nested "VkCoarseSampleOrderCustomNV" "pSampleLocations"
    sampleLocations :: Vector CoarseSampleLocationNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CoarseSampleOrderCustomNV)
#endif
deriving instance Show CoarseSampleOrderCustomNV

instance ToCStruct CoarseSampleOrderCustomNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CoarseSampleOrderCustomNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr ShadingRatePaletteEntryNV)) (shadingRate)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (sampleCount)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (sampleLocations)) :: Word32))
    pPSampleLocations' <- ContT $ allocaBytesAligned @CoarseSampleLocationNV ((Data.Vector.length (sampleLocations)) * 12) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSampleLocations' `plusPtr` (12 * (i)) :: Ptr CoarseSampleLocationNV) (e)) (sampleLocations)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CoarseSampleLocationNV))) (pPSampleLocations')
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr ShadingRatePaletteEntryNV)) (zero)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    pPSampleLocations' <- ContT $ allocaBytesAligned @CoarseSampleLocationNV ((Data.Vector.length (mempty)) * 12) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSampleLocations' `plusPtr` (12 * (i)) :: Ptr CoarseSampleLocationNV) (e)) (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CoarseSampleLocationNV))) (pPSampleLocations')
    lift $ f

instance FromCStruct CoarseSampleOrderCustomNV where
  peekCStruct p = do
    shadingRate <- peek @ShadingRatePaletteEntryNV ((p `plusPtr` 0 :: Ptr ShadingRatePaletteEntryNV))
    sampleCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    sampleLocationCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pSampleLocations <- peek @(Ptr CoarseSampleLocationNV) ((p `plusPtr` 16 :: Ptr (Ptr CoarseSampleLocationNV)))
    pSampleLocations' <- generateM (fromIntegral sampleLocationCount) (\i -> peekCStruct @CoarseSampleLocationNV ((pSampleLocations `advancePtrBytes` (12 * (i)) :: Ptr CoarseSampleLocationNV)))
    pure $ CoarseSampleOrderCustomNV
             shadingRate sampleCount pSampleLocations'

instance Zero CoarseSampleOrderCustomNV where
  zero = CoarseSampleOrderCustomNV
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkPipelineViewportCoarseSampleOrderStateCreateInfoNV"
data PipelineViewportCoarseSampleOrderStateCreateInfoNV = PipelineViewportCoarseSampleOrderStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineViewportCoarseSampleOrderStateCreateInfoNV" "sampleOrderType"
    sampleOrderType :: CoarseSampleOrderTypeNV
  , -- No documentation found for Nested "VkPipelineViewportCoarseSampleOrderStateCreateInfoNV" "pCustomSampleOrders"
    customSampleOrders :: Vector CoarseSampleOrderCustomNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineViewportCoarseSampleOrderStateCreateInfoNV)
#endif
deriving instance Show PipelineViewportCoarseSampleOrderStateCreateInfoNV

instance ToCStruct PipelineViewportCoarseSampleOrderStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportCoarseSampleOrderStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CoarseSampleOrderTypeNV)) (sampleOrderType)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (customSampleOrders)) :: Word32))
    pPCustomSampleOrders' <- ContT $ allocaBytesAligned @CoarseSampleOrderCustomNV ((Data.Vector.length (customSampleOrders)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCustomSampleOrders' `plusPtr` (24 * (i)) :: Ptr CoarseSampleOrderCustomNV) (e) . ($ ())) (customSampleOrders)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CoarseSampleOrderCustomNV))) (pPCustomSampleOrders')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CoarseSampleOrderTypeNV)) (zero)
    pPCustomSampleOrders' <- ContT $ allocaBytesAligned @CoarseSampleOrderCustomNV ((Data.Vector.length (mempty)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCustomSampleOrders' `plusPtr` (24 * (i)) :: Ptr CoarseSampleOrderCustomNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CoarseSampleOrderCustomNV))) (pPCustomSampleOrders')
    lift $ f

instance FromCStruct PipelineViewportCoarseSampleOrderStateCreateInfoNV where
  peekCStruct p = do
    sampleOrderType <- peek @CoarseSampleOrderTypeNV ((p `plusPtr` 16 :: Ptr CoarseSampleOrderTypeNV))
    customSampleOrderCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pCustomSampleOrders <- peek @(Ptr CoarseSampleOrderCustomNV) ((p `plusPtr` 24 :: Ptr (Ptr CoarseSampleOrderCustomNV)))
    pCustomSampleOrders' <- generateM (fromIntegral customSampleOrderCount) (\i -> peekCStruct @CoarseSampleOrderCustomNV ((pCustomSampleOrders `advancePtrBytes` (24 * (i)) :: Ptr CoarseSampleOrderCustomNV)))
    pure $ PipelineViewportCoarseSampleOrderStateCreateInfoNV
             sampleOrderType pCustomSampleOrders'

instance Zero PipelineViewportCoarseSampleOrderStateCreateInfoNV where
  zero = PipelineViewportCoarseSampleOrderStateCreateInfoNV
           zero
           mempty


-- No documentation found for TopLevel "VkShadingRatePaletteEntryNV"
newtype ShadingRatePaletteEntryNV = ShadingRatePaletteEntryNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV              = ShadingRatePaletteEntryNV 0
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV    = ShadingRatePaletteEntryNV 1
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV     = ShadingRatePaletteEntryNV 2
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV     = ShadingRatePaletteEntryNV 3
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV     = ShadingRatePaletteEntryNV 4
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV      = ShadingRatePaletteEntryNV 5
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV = ShadingRatePaletteEntryNV 6
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV = ShadingRatePaletteEntryNV 7
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV = ShadingRatePaletteEntryNV 8
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV = ShadingRatePaletteEntryNV 9
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV = ShadingRatePaletteEntryNV 10
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV = ShadingRatePaletteEntryNV 11
{-# complete SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV,
             SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV,
             SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV,
             SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV,
             SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV,
             SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV :: ShadingRatePaletteEntryNV #-}

conNameShadingRatePaletteEntryNV :: String
conNameShadingRatePaletteEntryNV = "ShadingRatePaletteEntryNV"

enumPrefixShadingRatePaletteEntryNV :: String
enumPrefixShadingRatePaletteEntryNV = "SHADING_RATE_PALETTE_ENTRY_"

showTableShadingRatePaletteEntryNV :: [(ShadingRatePaletteEntryNV, String)]
showTableShadingRatePaletteEntryNV =
  [ (SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV             , "NO_INVOCATIONS_NV")
  , (SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV   , "16_INVOCATIONS_PER_PIXEL_NV")
  , (SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV    , "8_INVOCATIONS_PER_PIXEL_NV")
  , (SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV    , "4_INVOCATIONS_PER_PIXEL_NV")
  , (SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV    , "2_INVOCATIONS_PER_PIXEL_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV     , "1_INVOCATION_PER_PIXEL_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV, "1_INVOCATION_PER_2X1_PIXELS_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV, "1_INVOCATION_PER_1X2_PIXELS_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV, "1_INVOCATION_PER_2X2_PIXELS_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV, "1_INVOCATION_PER_4X2_PIXELS_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV, "1_INVOCATION_PER_2X4_PIXELS_NV")
  , (SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV, "1_INVOCATION_PER_4X4_PIXELS_NV")
  ]


instance Show ShadingRatePaletteEntryNV where
showsPrec = enumShowsPrec enumPrefixShadingRatePaletteEntryNV
                          showTableShadingRatePaletteEntryNV
                          conNameShadingRatePaletteEntryNV
                          (\(ShadingRatePaletteEntryNV x) -> x)
                          (showsPrec 11)


instance Read ShadingRatePaletteEntryNV where
  readPrec = enumReadPrec enumPrefixShadingRatePaletteEntryNV
                          showTableShadingRatePaletteEntryNV
                          conNameShadingRatePaletteEntryNV
                          ShadingRatePaletteEntryNV


-- No documentation found for TopLevel "VkCoarseSampleOrderTypeNV"
newtype CoarseSampleOrderTypeNV = CoarseSampleOrderTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkCoarseSampleOrderTypeNV" "VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV"
pattern COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV      = CoarseSampleOrderTypeNV 0
-- No documentation found for Nested "VkCoarseSampleOrderTypeNV" "VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV"
pattern COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV       = CoarseSampleOrderTypeNV 1
-- No documentation found for Nested "VkCoarseSampleOrderTypeNV" "VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV"
pattern COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV  = CoarseSampleOrderTypeNV 2
-- No documentation found for Nested "VkCoarseSampleOrderTypeNV" "VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV"
pattern COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV = CoarseSampleOrderTypeNV 3
{-# complete COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV,
             COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV,
             COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV,
             COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV :: CoarseSampleOrderTypeNV #-}

conNameCoarseSampleOrderTypeNV :: String
conNameCoarseSampleOrderTypeNV = "CoarseSampleOrderTypeNV"

enumPrefixCoarseSampleOrderTypeNV :: String
enumPrefixCoarseSampleOrderTypeNV = "COARSE_SAMPLE_ORDER_TYPE_"

showTableCoarseSampleOrderTypeNV :: [(CoarseSampleOrderTypeNV, String)]
showTableCoarseSampleOrderTypeNV =
  [ (COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV     , "DEFAULT_NV")
  , (COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV      , "CUSTOM_NV")
  , (COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV , "PIXEL_MAJOR_NV")
  , (COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV, "SAMPLE_MAJOR_NV")
  ]


instance Show CoarseSampleOrderTypeNV where
showsPrec = enumShowsPrec enumPrefixCoarseSampleOrderTypeNV
                          showTableCoarseSampleOrderTypeNV
                          conNameCoarseSampleOrderTypeNV
                          (\(CoarseSampleOrderTypeNV x) -> x)
                          (showsPrec 11)


instance Read CoarseSampleOrderTypeNV where
  readPrec = enumReadPrec enumPrefixCoarseSampleOrderTypeNV
                          showTableCoarseSampleOrderTypeNV
                          conNameCoarseSampleOrderTypeNV
                          CoarseSampleOrderTypeNV


type NV_SHADING_RATE_IMAGE_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION"
pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION = 3


type NV_SHADING_RATE_IMAGE_EXTENSION_NAME = "VK_NV_shading_rate_image"

-- No documentation found for TopLevel "VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME"
pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME = "VK_NV_shading_rate_image"

