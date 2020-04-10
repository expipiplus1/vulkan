{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_shading_rate_image  ( cmdBindShadingRateImageNV
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

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.Either (Either)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.Handles (CommandBuffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer_T)
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdBindShadingRateImageNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetCoarseSampleOrderNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewportShadingRatePaletteNV))
import Graphics.Vulkan.Core10.SharedTypes (Extent2D)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Graphics.Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Graphics.Vulkan.Core10.Handles (ImageView)
import Graphics.Vulkan.Core10.Handles (ImageView(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindShadingRateImageNV
  :: FunPtr (Ptr CommandBuffer_T -> ImageView -> ImageLayout -> IO ()) -> Ptr CommandBuffer_T -> ImageView -> ImageLayout -> IO ()

-- | vkCmdBindShadingRateImageNV - Bind a shading rate image on a command
-- buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @imageView@ is an image view handle specifying the shading rate
--     image. @imageView@ /may/ be set to
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', which is
--     equivalent to specifying a view of an image filled with zero values.
--
-- -   @imageLayout@ is the layout that the image subresources accessible
--     from @imageView@ will be in when the shading rate image is accessed.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature /must/ be enabled.
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ be a
--     valid 'Graphics.Vulkan.Core10.Handles.ImageView' handle of type
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'.
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ have a
--     format of 'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R8_UINT'.
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ have
--     been created with a @usage@ value including
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @imageLayout@
--     /must/ match the actual
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' of each
--     subresource accessible from @imageView@ at the time the subresource
--     is accessed.
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @imageLayout@
--     /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV'
--     or 'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   If @imageView@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@
--     /must/ be a valid 'Graphics.Vulkan.Core10.Handles.ImageView' handle
--
-- -   @imageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   Both of @commandBuffer@, and @imageView@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Graphics.Vulkan.Core10.Handles.ImageView'
cmdBindShadingRateImageNV :: CommandBuffer -> ImageView -> ImageLayout -> IO ()
cmdBindShadingRateImageNV commandBuffer imageView imageLayout = do
  let vkCmdBindShadingRateImageNV' = mkVkCmdBindShadingRateImageNV (pVkCmdBindShadingRateImageNV (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdBindShadingRateImageNV' (commandBufferHandle (commandBuffer)) (imageView) (imageLayout)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportShadingRatePaletteNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ShadingRatePaletteNV -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ShadingRatePaletteNV -> IO ()

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
--     'ShadingRatePaletteNV' structures defining the palette for each
--     viewport.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature /must/ be enabled.
--
-- -   @firstViewport@ /must/ be less than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstViewport@ and @viewportCount@ /must/ be between @1@
--     and
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @firstViewport@ /must/ be @0@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pShadingRatePalettes@ /must/ be a valid pointer to an array of
--     @viewportCount@ valid 'ShadingRatePaletteNV' structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   @viewportCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer', 'ShadingRatePaletteNV'
cmdSetViewportShadingRatePaletteNV :: CommandBuffer -> ("firstViewport" ::: Word32) -> ("shadingRatePalettes" ::: Vector ShadingRatePaletteNV) -> IO ()
cmdSetViewportShadingRatePaletteNV commandBuffer firstViewport shadingRatePalettes = evalContT $ do
  let vkCmdSetViewportShadingRatePaletteNV' = mkVkCmdSetViewportShadingRatePaletteNV (pVkCmdSetViewportShadingRatePaletteNV (deviceCmds (commandBuffer :: CommandBuffer)))
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
--     'CoarseSampleOrderCustomNV' structures, each of which specifies the
--     coverage sample order for a single combination of fragment area and
--     coverage sample count.
--
-- = Description
--
-- If @sampleOrderType@ is 'COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV', the
-- coverage sample order used for any combination of fragment area and
-- coverage sample count not enumerated in @pCustomSampleOrders@ will be
-- identical to that used for 'COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'.
--
-- == Valid Usage
--
-- -   If @sampleOrderType@ is not 'COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV',
--     @customSamplerOrderCount@ /must/ be @0@
--
-- -   The array @pCustomSampleOrders@ /must/ not contain two structures
--     with matching values for both the @shadingRate@ and @sampleCount@
--     members.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @sampleOrderType@ /must/ be a valid 'CoarseSampleOrderTypeNV' value
--
-- -   If @customSampleOrderCount@ is not @0@, @pCustomSampleOrders@ /must/
--     be a valid pointer to an array of @customSampleOrderCount@ valid
--     'CoarseSampleOrderCustomNV' structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'CoarseSampleOrderCustomNV', 'CoarseSampleOrderTypeNV',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdSetCoarseSampleOrderNV :: CommandBuffer -> CoarseSampleOrderTypeNV -> ("customSampleOrders" ::: Vector CoarseSampleOrderCustomNV) -> IO ()
cmdSetCoarseSampleOrderNV commandBuffer sampleOrderType customSampleOrders = evalContT $ do
  let vkCmdSetCoarseSampleOrderNV' = mkVkCmdSetCoarseSampleOrderNV (pVkCmdSetCoarseSampleOrderNV (deviceCmds (commandBuffer :: CommandBuffer)))
  pPCustomSampleOrders <- ContT $ allocaBytesAligned @CoarseSampleOrderCustomNV ((Data.Vector.length (customSampleOrders)) * 24) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCustomSampleOrders `plusPtr` (24 * (i)) :: Ptr CoarseSampleOrderCustomNV) (e) . ($ ())) (customSampleOrders)
  lift $ vkCmdSetCoarseSampleOrderNV' (commandBufferHandle (commandBuffer)) (sampleOrderType) ((fromIntegral (Data.Vector.length $ (customSampleOrders)) :: Word32)) (pPCustomSampleOrders)
  pure $ ()


-- | VkShadingRatePaletteNV - Structure specifying a single shading rate
-- palette
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PipelineViewportShadingRateImageStateCreateInfoNV',
-- 'ShadingRatePaletteEntryNV', 'cmdSetViewportShadingRatePaletteNV'
data ShadingRatePaletteNV = ShadingRatePaletteNV
  { -- | @pShadingRatePaletteEntries@ /must/ be a valid pointer to an array of
    -- @shadingRatePaletteEntryCount@ valid 'ShadingRatePaletteEntryNV' values
    shadingRatePaletteEntries :: Vector ShadingRatePaletteEntryNV }
  deriving (Typeable)
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


-- | VkPipelineViewportShadingRateImageStateCreateInfoNV - Structure
-- specifying parameters controlling shading rate image usage
--
-- = Description
--
-- If this structure is not present, @shadingRateImageEnable@ is considered
-- to be 'Graphics.Vulkan.Core10.BaseType.FALSE', and the shading rate
-- image and palettes are not used.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @0@ or @1@
--
-- -   @viewportCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@
--
-- -   If @shadingRateImageEnable@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', @viewportCount@ /must/ be
--     equal to the @viewportCount@ member of
--     'Graphics.Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV',
--     @pShadingRatePalettes@ /must/ be a valid pointer to an array of
--     @viewportCount@ 'ShadingRatePaletteNV' structures
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV'
--
-- -   If @viewportCount@ is not @0@, and @pShadingRatePalettes@ is not
--     @NULL@, @pShadingRatePalettes@ /must/ be a valid pointer to an array
--     of @viewportCount@ valid 'ShadingRatePaletteNV' structures
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32', 'ShadingRatePaletteNV',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineViewportShadingRateImageStateCreateInfoNV = PipelineViewportShadingRateImageStateCreateInfoNV
  { -- | @shadingRateImageEnable@ specifies whether shading rate image and
    -- palettes are used during rasterization.
    shadingRateImageEnable :: Bool
  , -- | @pShadingRatePalettes@ is a pointer to an array of
    -- 'ShadingRatePaletteNV' structures defining the palette for each
    -- viewport. If the shading rate palette state is dynamic, this member is
    -- ignored.
    shadingRatePalettes :: Either Word32 (Vector ShadingRatePaletteNV)
  }
  deriving (Typeable)
deriving instance Show PipelineViewportShadingRateImageStateCreateInfoNV

instance ToCStruct PipelineViewportShadingRateImageStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportShadingRateImageStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shadingRateImageEnable))
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (either id (fromIntegral . Data.Vector.length) (shadingRatePalettes)) :: Word32))
    pShadingRatePalettes'' <- case (shadingRatePalettes) of
      Left _ -> pure nullPtr
      Right v -> do
        pPShadingRatePalettes' <- ContT $ allocaBytesAligned @ShadingRatePaletteNV ((Data.Vector.length (v)) * 16) 8
        Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPShadingRatePalettes' `plusPtr` (16 * (i)) :: Ptr ShadingRatePaletteNV) (e) . ($ ())) (v)
        pure $ pPShadingRatePalettes'
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ShadingRatePaletteNV))) pShadingRatePalettes''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PipelineViewportShadingRateImageStateCreateInfoNV where
  peekCStruct p = do
    shadingRateImageEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    viewportCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pShadingRatePalettes <- peek @(Ptr ShadingRatePaletteNV) ((p `plusPtr` 24 :: Ptr (Ptr ShadingRatePaletteNV)))
    pShadingRatePalettes' <- maybePeek (\j -> generateM (fromIntegral viewportCount) (\i -> peekCStruct @ShadingRatePaletteNV (((j) `advancePtrBytes` (16 * (i)) :: Ptr ShadingRatePaletteNV)))) pShadingRatePalettes
    let pShadingRatePalettes'' = maybe (Left viewportCount) Right pShadingRatePalettes'
    pure $ PipelineViewportShadingRateImageStateCreateInfoNV
             (bool32ToBool shadingRateImageEnable) pShadingRatePalettes''

instance Zero PipelineViewportShadingRateImageStateCreateInfoNV where
  zero = PipelineViewportShadingRateImageStateCreateInfoNV
           zero
           (Left 0)


-- | VkPhysicalDeviceShadingRateImageFeaturesNV - Structure describing
-- shading rate image features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShadingRateImageFeaturesNV' structure
-- describe the following features:
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-shading-rate-image Shading Rate Image>
-- for more information.
--
-- If the 'PhysicalDeviceShadingRateImageFeaturesNV' structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceShadingRateImageFeaturesNV' /can/ also be included in the
-- @pNext@ chain of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShadingRateImageFeaturesNV = PhysicalDeviceShadingRateImageFeaturesNV
  { -- | @shadingRateImage@ indicates that the implementation supports the use of
    -- a shading rate image to derive an effective shading rate for fragment
    -- processing. It also indicates that the implementation supports the
    -- @ShadingRateNV@ SPIR-V execution mode.
    shadingRateImage :: Bool
  , -- | @shadingRateCoarseSampleOrder@ indicates that the implementation
    -- supports a user-configurable ordering of coverage samples in fragments
    -- larger than one pixel.
    shadingRateCoarseSampleOrder :: Bool
  }
  deriving (Typeable)
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


-- | VkPhysicalDeviceShadingRateImagePropertiesNV - Structure describing
-- shading rate image limits that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShadingRateImagePropertiesNV'
-- structure describe the following implementation-dependent properties
-- related to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-shading-rate-image shading rate image>
-- feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShadingRateImagePropertiesNV' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SharedTypes.Extent2D',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShadingRateImagePropertiesNV = PhysicalDeviceShadingRateImagePropertiesNV
  { -- | @shadingRateTexelSize@ indicates the width and height of the portion of
    -- the framebuffer corresponding to each texel in the shading rate image.
    shadingRateTexelSize :: Extent2D
  , -- | @shadingRatePaletteSize@ indicates the maximum number of palette entries
    -- supported for the shading rate image.
    shadingRatePaletteSize :: Word32
  , -- | @shadingRateMaxCoarseSamples@ specifies the maximum number of coverage
    -- samples supported in a single fragment. If the product of the fragment
    -- size derived from the base shading rate and the number of coverage
    -- samples per pixel exceeds this limit, the final shading rate will be
    -- adjusted so that its product does not exceed the limit.
    shadingRateMaxCoarseSamples :: Word32
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceShadingRateImagePropertiesNV

instance ToCStruct PhysicalDeviceShadingRateImagePropertiesNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShadingRateImagePropertiesNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Extent2D)) (shadingRateTexelSize) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (shadingRatePaletteSize)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (shadingRateMaxCoarseSamples)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Extent2D)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    lift $ f

instance FromCStruct PhysicalDeviceShadingRateImagePropertiesNV where
  peekCStruct p = do
    shadingRateTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    shadingRatePaletteSize <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    shadingRateMaxCoarseSamples <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ PhysicalDeviceShadingRateImagePropertiesNV
             shadingRateTexelSize shadingRatePaletteSize shadingRateMaxCoarseSamples

instance Zero PhysicalDeviceShadingRateImagePropertiesNV where
  zero = PhysicalDeviceShadingRateImagePropertiesNV
           zero
           zero
           zero


-- | VkCoarseSampleLocationNV - Structure specifying parameters controlling
-- shading rate image usage
--
-- == Valid Usage
--
-- = See Also
--
-- 'CoarseSampleOrderCustomNV'
data CoarseSampleLocationNV = CoarseSampleLocationNV
  { -- | @pixelX@ /must/ be less than the width (in pixels) of the fragment.
    pixelX :: Word32
  , -- | @pixelY@ /must/ be less than the height (in pixels) of the fragment.
    pixelY :: Word32
  , -- | @sample@ /must/ be less than the number of coverage samples in each
    -- pixel belonging to the fragment.
    sample :: Word32
  }
  deriving (Typeable)
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
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlags'
--     whose corresponding bit is set in
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@framebufferNoAttachmentsSampleCounts@.
--
-- -   @sampleLocationCount@ /must/ be equal to the product of
--     @sampleCount@, the fragment width for @shadingRate@, and the
--     fragment height for @shadingRate@.
--
-- -   @sampleLocationCount@ /must/ be less than or equal to the value of
--     'PhysicalDeviceShadingRateImagePropertiesNV'::@shadingRateMaxCoarseSamples@.
--
-- -   The array @pSampleLocations@ /must/ contain exactly one entry for
--     every combination of valid values for @pixelX@, @pixelY@, and
--     @sample@ in the structure 'CoarseSampleOrderCustomNV'.
--
-- == Valid Usage (Implicit)
--
-- -   @shadingRate@ /must/ be a valid 'ShadingRatePaletteEntryNV' value
--
-- -   @pSampleLocations@ /must/ be a valid pointer to an array of
--     @sampleLocationCount@ 'CoarseSampleLocationNV' structures
--
-- -   @sampleLocationCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'CoarseSampleLocationNV',
-- 'PipelineViewportCoarseSampleOrderStateCreateInfoNV',
-- 'ShadingRatePaletteEntryNV', 'cmdSetCoarseSampleOrderNV'
data CoarseSampleOrderCustomNV = CoarseSampleOrderCustomNV
  { -- | @shadingRate@ is a shading rate palette entry that identifies the
    -- fragment width and height for the combination of fragment area and
    -- per-pixel coverage sample count to control.
    shadingRate :: ShadingRatePaletteEntryNV
  , -- | @sampleCount@ identifies the per-pixel coverage sample count for the
    -- combination of fragment area and coverage sample count to control.
    sampleCount :: Word32
  , -- | @pSampleLocations@ is a pointer to an array of
    -- 'CoarseSampleOrderCustomNV' structures specifying the location of each
    -- sample in the custom ordering.
    sampleLocations :: Vector CoarseSampleLocationNV
  }
  deriving (Typeable)
deriving instance Show CoarseSampleOrderCustomNV

instance ToCStruct CoarseSampleOrderCustomNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CoarseSampleOrderCustomNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr ShadingRatePaletteEntryNV)) (shadingRate)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (sampleCount)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (sampleLocations)) :: Word32))
    pPSampleLocations' <- ContT $ allocaBytesAligned @CoarseSampleLocationNV ((Data.Vector.length (sampleLocations)) * 12) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPSampleLocations' `plusPtr` (12 * (i)) :: Ptr CoarseSampleLocationNV) (e) . ($ ())) (sampleLocations)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CoarseSampleLocationNV))) (pPSampleLocations')
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr ShadingRatePaletteEntryNV)) (zero)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    pPSampleLocations' <- ContT $ allocaBytesAligned @CoarseSampleLocationNV ((Data.Vector.length (mempty)) * 12) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPSampleLocations' `plusPtr` (12 * (i)) :: Ptr CoarseSampleLocationNV) (e) . ($ ())) (mempty)
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


-- | VkPipelineViewportCoarseSampleOrderStateCreateInfoNV - Structure
-- specifying parameters controlling sample order in coarse fragments
--
-- = Description
--
-- If this structure is not present, @sampleOrderType@ is considered to be
-- 'COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'.
--
-- If @sampleOrderType@ is 'COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV', the
-- coverage sample order used for any combination of fragment area and
-- coverage sample count not enumerated in @pCustomSampleOrders@ will be
-- identical to that used for 'COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV'.
--
-- If the pipeline was created with
-- 'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV',
-- the contents of this structure (if present) are ignored, and the
-- coverage sample order is instead specified by
-- 'cmdSetCoarseSampleOrderNV'.
--
-- == Valid Usage
--
-- -   If @sampleOrderType@ is not 'COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV',
--     @customSamplerOrderCount@ /must/ be @0@
--
-- -   The array @pCustomSampleOrders@ /must/ not contain two structures
--     with matching values for both the @shadingRate@ and @sampleCount@
--     members.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV'
--
-- -   @sampleOrderType@ /must/ be a valid 'CoarseSampleOrderTypeNV' value
--
-- -   If @customSampleOrderCount@ is not @0@, @pCustomSampleOrders@ /must/
--     be a valid pointer to an array of @customSampleOrderCount@ valid
--     'CoarseSampleOrderCustomNV' structures
--
-- = See Also
--
-- 'CoarseSampleOrderCustomNV', 'CoarseSampleOrderTypeNV',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineViewportCoarseSampleOrderStateCreateInfoNV = PipelineViewportCoarseSampleOrderStateCreateInfoNV
  { -- | @sampleOrderType@ specifies the mechanism used to order coverage samples
    -- in fragments larger than one pixel.
    sampleOrderType :: CoarseSampleOrderTypeNV
  , -- | @pCustomSampleOrders@ is a pointer to an array of
    -- @customSampleOrderCount@ 'CoarseSampleOrderCustomNV' structures, each of
    -- which specifies the coverage sample order for a single combination of
    -- fragment area and coverage sample count.
    customSampleOrders :: Vector CoarseSampleOrderCustomNV
  }
  deriving (Typeable)
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


-- | VkShadingRatePaletteEntryNV - Shading rate image palette entry types
--
-- = Description
--
-- The following table indicates the width and height (in pixels) of each
-- fragment generated using the indicated shading rate, as well as the
-- maximum number of fragment shader invocations launched for each
-- fragment. When processing regions of a primitive that have a shading
-- rate of 'SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV', no fragments
-- will be generated in that region.
--
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | Shading Rate                                                | Width           | Height          | Invocations     |
-- +=============================================================+=================+=================+=================+
-- | 'SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV'              | 0               | 0               | 0               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV'    | 1               | 1               | 16              |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV'     | 1               | 1               | 8               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV'     | 1               | 1               | 4               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV'     | 1               | 1               | 2               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV'      | 1               | 1               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV' | 2               | 1               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV' | 1               | 2               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV' | 2               | 2               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV' | 4               | 2               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV' | 2               | 4               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
-- | 'SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV' | 4               | 4               | 1               |
-- +-------------------------------------------------------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'CoarseSampleOrderCustomNV', 'ShadingRatePaletteNV'
newtype ShadingRatePaletteEntryNV = ShadingRatePaletteEntryNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV"
pattern SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV = ShadingRatePaletteEntryNV 0
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV = ShadingRatePaletteEntryNV 1
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV = ShadingRatePaletteEntryNV 2
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV = ShadingRatePaletteEntryNV 3
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV = ShadingRatePaletteEntryNV 4
-- No documentation found for Nested "VkShadingRatePaletteEntryNV" "VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV"
pattern SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV = ShadingRatePaletteEntryNV 5
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

instance Show ShadingRatePaletteEntryNV where
  showsPrec p = \case
    SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV -> showString "SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV"
    SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV -> showString "SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV"
    SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV -> showString "SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV"
    SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV -> showString "SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV"
    SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV -> showString "SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV"
    SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV -> showString "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV"
    SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV -> showString "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV"
    SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV -> showString "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV"
    SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV -> showString "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV"
    SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV -> showString "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV"
    SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV -> showString "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV"
    SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV -> showString "SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV"
    ShadingRatePaletteEntryNV x -> showParen (p >= 11) (showString "ShadingRatePaletteEntryNV " . showsPrec 11 x)

instance Read ShadingRatePaletteEntryNV where
  readPrec = parens (choose [("SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV", pure SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV)
                            , ("SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV", pure SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV)
                            , ("SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV", pure SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV)
                            , ("SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV", pure SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV)
                            , ("SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV", pure SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV)
                            , ("SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV", pure SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV)
                            , ("SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV", pure SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV)
                            , ("SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV", pure SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV)
                            , ("SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV", pure SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV)
                            , ("SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV", pure SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV)
                            , ("SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV", pure SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV)
                            , ("SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV", pure SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "ShadingRatePaletteEntryNV")
                       v <- step readPrec
                       pure (ShadingRatePaletteEntryNV v)))


-- | VkCoarseSampleOrderTypeNV - Shading rate image sample ordering types
--
-- = See Also
--
-- 'PipelineViewportCoarseSampleOrderStateCreateInfoNV',
-- 'cmdSetCoarseSampleOrderNV'
newtype CoarseSampleOrderTypeNV = CoarseSampleOrderTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV' specifies that coverage samples
-- will be ordered in an implementation-dependent manner.
pattern COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV = CoarseSampleOrderTypeNV 0
-- | 'COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV' specifies that coverage samples
-- will be ordered according to the array of custom orderings provided in
-- either the @pCustomSampleOrders@ member of
-- 'PipelineViewportCoarseSampleOrderStateCreateInfoNV' or the
-- @pCustomSampleOrders@ member of 'cmdSetCoarseSampleOrderNV'.
pattern COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV = CoarseSampleOrderTypeNV 1
-- | 'COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV' specifies that coverage
-- samples will be ordered sequentially, sorted first by pixel coordinate
-- (in row-major order) and then by coverage sample number.
pattern COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV = CoarseSampleOrderTypeNV 2
-- | 'COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV' specifies that coverage
-- samples will be ordered sequentially, sorted first by coverage sample
-- number and then by pixel coordinate (in row-major order).
pattern COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV = CoarseSampleOrderTypeNV 3
{-# complete COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV,
             COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV,
             COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV,
             COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV :: CoarseSampleOrderTypeNV #-}

instance Show CoarseSampleOrderTypeNV where
  showsPrec p = \case
    COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV -> showString "COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV"
    COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV -> showString "COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV"
    COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV -> showString "COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV"
    COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV -> showString "COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV"
    CoarseSampleOrderTypeNV x -> showParen (p >= 11) (showString "CoarseSampleOrderTypeNV " . showsPrec 11 x)

instance Read CoarseSampleOrderTypeNV where
  readPrec = parens (choose [("COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV", pure COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV)
                            , ("COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV", pure COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV)
                            , ("COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV", pure COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV)
                            , ("COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV", pure COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "CoarseSampleOrderTypeNV")
                       v <- step readPrec
                       pure (CoarseSampleOrderTypeNV v)))


type NV_SHADING_RATE_IMAGE_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION"
pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SHADING_RATE_IMAGE_SPEC_VERSION = 3


type NV_SHADING_RATE_IMAGE_EXTENSION_NAME = "VK_NV_shading_rate_image"

-- No documentation found for TopLevel "VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME"
pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME = "VK_NV_shading_rate_image"

