{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_sample_locations  ( cmdSetSampleLocationsEXT
                                                           , getPhysicalDeviceMultisamplePropertiesEXT
                                                           , SampleLocationEXT(..)
                                                           , SampleLocationsInfoEXT(..)
                                                           , AttachmentSampleLocationsEXT(..)
                                                           , SubpassSampleLocationsEXT(..)
                                                           , RenderPassSampleLocationsBeginInfoEXT(..)
                                                           , PipelineSampleLocationsStateCreateInfoEXT(..)
                                                           , PhysicalDeviceSampleLocationsPropertiesEXT(..)
                                                           , MultisamplePropertiesEXT(..)
                                                           , EXT_SAMPLE_LOCATIONS_SPEC_VERSION
                                                           , pattern EXT_SAMPLE_LOCATIONS_SPEC_VERSION
                                                           , EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
                                                           , pattern EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
                                                           ) where

import Graphics.Vulkan.CStruct.Utils (FixedArray)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.CStruct.Utils (lowerArrayPtr)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.Handles (CommandBuffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer_T)
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetSampleLocationsEXT))
import Graphics.Vulkan.Core10.SharedTypes (Extent2D)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceMultisamplePropertiesEXT))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice_T)
import Graphics.Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Graphics.Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits(..))
import Graphics.Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlags)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetSampleLocationsEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr SampleLocationsInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr SampleLocationsInfoEXT -> IO ()

-- | vkCmdSetSampleLocationsEXT - Set the dynamic sample locations state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pSampleLocationsInfo@ is the sample locations state to set.
--
-- == Valid Usage
--
-- -   The @sampleLocationsPerPixel@ member of @pSampleLocationsInfo@
--     /must/ equal the @rasterizationSamples@ member of the
--     'Graphics.Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'
--     structure the bound graphics pipeline has been created with
--
-- -   If
--     'PhysicalDeviceSampleLocationsPropertiesEXT'::@variableSampleLocations@
--     is 'Graphics.Vulkan.Core10.BaseType.FALSE' then the current render
--     pass /must/ have been begun by specifying a
--     'RenderPassSampleLocationsBeginInfoEXT' structure whose
--     @pPostSubpassSampleLocations@ member contains an element with a
--     @subpassIndex@ matching the current subpass index and the
--     @sampleLocationsInfo@ member of that element /must/ match the sample
--     locations state pointed to by @pSampleLocationsInfo@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pSampleLocationsInfo@ /must/ be a valid pointer to a valid
--     'SampleLocationsInfoEXT' structure
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
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer', 'SampleLocationsInfoEXT'
cmdSetSampleLocationsEXT :: forall io . MonadIO io => CommandBuffer -> SampleLocationsInfoEXT -> io ()
cmdSetSampleLocationsEXT commandBuffer sampleLocationsInfo = liftIO . evalContT $ do
  let vkCmdSetSampleLocationsEXT' = mkVkCmdSetSampleLocationsEXT (pVkCmdSetSampleLocationsEXT (deviceCmds (commandBuffer :: CommandBuffer)))
  pSampleLocationsInfo <- ContT $ withCStruct (sampleLocationsInfo)
  lift $ vkCmdSetSampleLocationsEXT' (commandBufferHandle (commandBuffer)) pSampleLocationsInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceMultisamplePropertiesEXT
  :: FunPtr (Ptr PhysicalDevice_T -> SampleCountFlagBits -> Ptr MultisamplePropertiesEXT -> IO ()) -> Ptr PhysicalDevice_T -> SampleCountFlagBits -> Ptr MultisamplePropertiesEXT -> IO ()

-- | vkGetPhysicalDeviceMultisamplePropertiesEXT - Report sample count
-- specific multisampling capabilities of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     additional multisampling capabilities.
--
-- -   @samples@ is the sample count to query the capabilities for.
--
-- -   @pMultisampleProperties@ is a pointer to a
--     'MultisamplePropertiesEXT' structure in which information about the
--     additional multisampling capabilities specific to the sample count
--     is returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'MultisamplePropertiesEXT',
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice',
-- 'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
getPhysicalDeviceMultisamplePropertiesEXT :: forall io . MonadIO io => PhysicalDevice -> ("samples" ::: SampleCountFlagBits) -> io (MultisamplePropertiesEXT)
getPhysicalDeviceMultisamplePropertiesEXT physicalDevice samples = liftIO . evalContT $ do
  let vkGetPhysicalDeviceMultisamplePropertiesEXT' = mkVkGetPhysicalDeviceMultisamplePropertiesEXT (pVkGetPhysicalDeviceMultisamplePropertiesEXT (instanceCmds (physicalDevice :: PhysicalDevice)))
  pPMultisampleProperties <- ContT (withZeroCStruct @MultisamplePropertiesEXT)
  lift $ vkGetPhysicalDeviceMultisamplePropertiesEXT' (physicalDeviceHandle (physicalDevice)) (samples) (pPMultisampleProperties)
  pMultisampleProperties <- lift $ peekCStruct @MultisamplePropertiesEXT pPMultisampleProperties
  pure $ (pMultisampleProperties)


-- | VkSampleLocationEXT - Structure specifying the coordinates of a sample
-- location
--
-- = Description
--
-- The domain space of the sample location coordinates has an upper-left
-- origin within the pixel in framebuffer space.
--
-- The values specified in a 'SampleLocationEXT' structure are always
-- clamped to the implementation-dependent sample location coordinate range
-- [@sampleLocationCoordinateRange@[0],@sampleLocationCoordinateRange@[1]]
-- that /can/ be queried by adding a
-- 'PhysicalDeviceSampleLocationsPropertiesEXT' structure to the @pNext@
-- chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'.
--
-- = See Also
--
-- 'SampleLocationsInfoEXT'
data SampleLocationEXT = SampleLocationEXT
  { -- | @x@ is the horizontal coordinate of the sample’s location.
    x :: Float
  , -- | @y@ is the vertical coordinate of the sample’s location.
    y :: Float
  }
  deriving (Typeable)
deriving instance Show SampleLocationEXT

instance ToCStruct SampleLocationEXT where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SampleLocationEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (x))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (y))
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct SampleLocationEXT where
  peekCStruct p = do
    x <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    y <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    pure $ SampleLocationEXT
             ((\(CFloat a) -> a) x) ((\(CFloat a) -> a) y)

instance Storable SampleLocationEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SampleLocationEXT where
  zero = SampleLocationEXT
           zero
           zero


-- | VkSampleLocationsInfoEXT - Structure specifying a set of sample
-- locations
--
-- = Description
--
-- This structure /can/ be used either to specify the sample locations to
-- be used for rendering or to specify the set of sample locations an image
-- subresource has been last rendered with for the purposes of layout
-- transitions of depth\/stencil images created with
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'.
--
-- The sample locations in @pSampleLocations@ specify
-- @sampleLocationsPerPixel@ number of sample locations for each pixel in
-- the grid of the size specified in @sampleLocationGridSize@. The sample
-- location for sample i at the pixel grid location (x,y) is taken from
-- @pSampleLocations@[(x + y * @sampleLocationGridSize.width@) *
-- @sampleLocationsPerPixel@ + i].
--
-- If the render pass has a fragment density map, the implementation will
-- choose the sample locations for the fragment and the contents of
-- @pSampleLocations@ /may/ be ignored.
--
-- == Valid Usage
--
-- -   @sampleLocationsPerPixel@ /must/ be a bit value that is set in
--     'PhysicalDeviceSampleLocationsPropertiesEXT'::@sampleLocationSampleCounts@
--
-- -   @sampleLocationsCount@ /must/ equal @sampleLocationsPerPixel@ ×
--     @sampleLocationGridSize.width@ × @sampleLocationGridSize.height@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT'
--
-- -   If @sampleLocationsPerPixel@ is not @0@, @sampleLocationsPerPixel@
--     /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
--     value
--
-- -   If @sampleLocationsCount@ is not @0@, @pSampleLocations@ /must/ be a
--     valid pointer to an array of @sampleLocationsCount@
--     'SampleLocationEXT' structures
--
-- = See Also
--
-- 'AttachmentSampleLocationsEXT',
-- 'Graphics.Vulkan.Core10.SharedTypes.Extent2D',
-- 'PipelineSampleLocationsStateCreateInfoEXT',
-- 'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'SampleLocationEXT',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'SubpassSampleLocationsEXT', 'cmdSetSampleLocationsEXT'
data SampleLocationsInfoEXT = SampleLocationsInfoEXT
  { -- | @sampleLocationsPerPixel@ is a
    -- 'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
    -- specifying the number of sample locations per pixel.
    sampleLocationsPerPixel :: SampleCountFlagBits
  , -- | @sampleLocationGridSize@ is the size of the sample location grid to
    -- select custom sample locations for.
    sampleLocationGridSize :: Extent2D
  , -- | @pSampleLocations@ is a pointer to an array of @sampleLocationsCount@
    -- 'SampleLocationEXT' structures.
    sampleLocations :: Vector SampleLocationEXT
  }
  deriving (Typeable)
deriving instance Show SampleLocationsInfoEXT

instance ToCStruct SampleLocationsInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SampleLocationsInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr SampleCountFlagBits)) (sampleLocationsPerPixel)
    ContT $ pokeCStruct ((p `plusPtr` 20 :: Ptr Extent2D)) (sampleLocationGridSize) . ($ ())
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (sampleLocations)) :: Word32))
    pPSampleLocations' <- ContT $ allocaBytesAligned @SampleLocationEXT ((Data.Vector.length (sampleLocations)) * 8) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPSampleLocations' `plusPtr` (8 * (i)) :: Ptr SampleLocationEXT) (e) . ($ ())) (sampleLocations)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr SampleLocationEXT))) (pPSampleLocations')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 20 :: Ptr Extent2D)) (zero) . ($ ())
    pPSampleLocations' <- ContT $ allocaBytesAligned @SampleLocationEXT ((Data.Vector.length (mempty)) * 8) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPSampleLocations' `plusPtr` (8 * (i)) :: Ptr SampleLocationEXT) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr SampleLocationEXT))) (pPSampleLocations')
    lift $ f

instance FromCStruct SampleLocationsInfoEXT where
  peekCStruct p = do
    sampleLocationsPerPixel <- peek @SampleCountFlagBits ((p `plusPtr` 16 :: Ptr SampleCountFlagBits))
    sampleLocationGridSize <- peekCStruct @Extent2D ((p `plusPtr` 20 :: Ptr Extent2D))
    sampleLocationsCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pSampleLocations <- peek @(Ptr SampleLocationEXT) ((p `plusPtr` 32 :: Ptr (Ptr SampleLocationEXT)))
    pSampleLocations' <- generateM (fromIntegral sampleLocationsCount) (\i -> peekCStruct @SampleLocationEXT ((pSampleLocations `advancePtrBytes` (8 * (i)) :: Ptr SampleLocationEXT)))
    pure $ SampleLocationsInfoEXT
             sampleLocationsPerPixel sampleLocationGridSize pSampleLocations'

instance Zero SampleLocationsInfoEXT where
  zero = SampleLocationsInfoEXT
           zero
           zero
           mempty


-- | VkAttachmentSampleLocationsEXT - Structure specifying the sample
-- locations state to use in the initial layout transition of attachments
--
-- = Description
--
-- If the image referenced by the framebuffer attachment at index
-- @attachmentIndex@ was not created with
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
-- then the values specified in @sampleLocationsInfo@ are ignored.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'RenderPassSampleLocationsBeginInfoEXT', 'SampleLocationsInfoEXT'
data AttachmentSampleLocationsEXT = AttachmentSampleLocationsEXT
  { -- | @attachmentIndex@ /must/ be less than the @attachmentCount@ specified in
    -- 'Graphics.Vulkan.Core10.Pass.RenderPassCreateInfo' the render pass
    -- specified by
    -- 'Graphics.Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderPass@
    -- was created with
    attachmentIndex :: Word32
  , -- | @sampleLocationsInfo@ /must/ be a valid 'SampleLocationsInfoEXT'
    -- structure
    sampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Typeable)
deriving instance Show AttachmentSampleLocationsEXT

instance ToCStruct AttachmentSampleLocationsEXT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentSampleLocationsEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (attachmentIndex)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr SampleLocationsInfoEXT)) (sampleLocationsInfo) . ($ ())
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr SampleLocationsInfoEXT)) (zero) . ($ ())
    lift $ f

instance FromCStruct AttachmentSampleLocationsEXT where
  peekCStruct p = do
    attachmentIndex <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    sampleLocationsInfo <- peekCStruct @SampleLocationsInfoEXT ((p `plusPtr` 8 :: Ptr SampleLocationsInfoEXT))
    pure $ AttachmentSampleLocationsEXT
             attachmentIndex sampleLocationsInfo

instance Zero AttachmentSampleLocationsEXT where
  zero = AttachmentSampleLocationsEXT
           zero
           zero


-- | VkSubpassSampleLocationsEXT - Structure specifying the sample locations
-- state to use for layout transitions of attachments performed after a
-- given subpass
--
-- = Description
--
-- If the image referenced by the depth\/stencil attachment used in the
-- subpass identified by @subpassIndex@ was not created with
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
-- or if the subpass does not use a depth\/stencil attachment, and
-- 'PhysicalDeviceSampleLocationsPropertiesEXT'::@variableSampleLocations@
-- is 'Graphics.Vulkan.Core10.BaseType.TRUE' then the values specified in
-- @sampleLocationsInfo@ are ignored.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'RenderPassSampleLocationsBeginInfoEXT', 'SampleLocationsInfoEXT'
data SubpassSampleLocationsEXT = SubpassSampleLocationsEXT
  { -- | @subpassIndex@ /must/ be less than the @subpassCount@ specified in
    -- 'Graphics.Vulkan.Core10.Pass.RenderPassCreateInfo' the render pass
    -- specified by
    -- 'Graphics.Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderPass@
    -- was created with
    subpassIndex :: Word32
  , -- | @sampleLocationsInfo@ /must/ be a valid 'SampleLocationsInfoEXT'
    -- structure
    sampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Typeable)
deriving instance Show SubpassSampleLocationsEXT

instance ToCStruct SubpassSampleLocationsEXT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassSampleLocationsEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (subpassIndex)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr SampleLocationsInfoEXT)) (sampleLocationsInfo) . ($ ())
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr SampleLocationsInfoEXT)) (zero) . ($ ())
    lift $ f

instance FromCStruct SubpassSampleLocationsEXT where
  peekCStruct p = do
    subpassIndex <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    sampleLocationsInfo <- peekCStruct @SampleLocationsInfoEXT ((p `plusPtr` 8 :: Ptr SampleLocationsInfoEXT))
    pure $ SubpassSampleLocationsEXT
             subpassIndex sampleLocationsInfo

instance Zero SubpassSampleLocationsEXT where
  zero = SubpassSampleLocationsEXT
           zero
           zero


-- | VkRenderPassSampleLocationsBeginInfoEXT - Structure specifying sample
-- locations to use for the layout transition of custom sample locations
-- compatible depth\/stencil attachments
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT'
--
-- -   If @attachmentInitialSampleLocationsCount@ is not @0@,
--     @pAttachmentInitialSampleLocations@ /must/ be a valid pointer to an
--     array of @attachmentInitialSampleLocationsCount@ valid
--     'AttachmentSampleLocationsEXT' structures
--
-- -   If @postSubpassSampleLocationsCount@ is not @0@,
--     @pPostSubpassSampleLocations@ /must/ be a valid pointer to an array
--     of @postSubpassSampleLocationsCount@ valid
--     'SubpassSampleLocationsEXT' structures
--
-- = See Also
--
-- 'AttachmentSampleLocationsEXT',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'SubpassSampleLocationsEXT'
data RenderPassSampleLocationsBeginInfoEXT = RenderPassSampleLocationsBeginInfoEXT
  { -- | @pAttachmentInitialSampleLocations@ is a pointer to an array of
    -- @attachmentInitialSampleLocationsCount@ 'AttachmentSampleLocationsEXT'
    -- structures specifying the attachment indices and their corresponding
    -- sample location state. Each element of
    -- @pAttachmentInitialSampleLocations@ /can/ specify the sample location
    -- state to use in the automatic layout transition performed to transition
    -- a depth\/stencil attachment from the initial layout of the attachment to
    -- the image layout specified for the attachment in the first subpass using
    -- it.
    attachmentInitialSampleLocations :: Vector AttachmentSampleLocationsEXT
  , -- | @pPostSubpassSampleLocations@ is a pointer to an array of
    -- @postSubpassSampleLocationsCount@ 'SubpassSampleLocationsEXT' structures
    -- specifying the subpass indices and their corresponding sample location
    -- state. Each element of @pPostSubpassSampleLocations@ /can/ specify the
    -- sample location state to use in the automatic layout transition
    -- performed to transition the depth\/stencil attachment used by the
    -- specified subpass to the image layout specified in a dependent subpass
    -- or to the final layout of the attachment in case the specified subpass
    -- is the last subpass using that attachment. In addition, if
    -- 'PhysicalDeviceSampleLocationsPropertiesEXT'::@variableSampleLocations@
    -- is 'Graphics.Vulkan.Core10.BaseType.FALSE', each element of
    -- @pPostSubpassSampleLocations@ /must/ specify the sample location state
    -- that matches the sample locations used by all pipelines that will be
    -- bound to a command buffer during the specified subpass. If
    -- @variableSampleLocations@ is 'Graphics.Vulkan.Core10.BaseType.TRUE', the
    -- sample locations used for rasterization do not depend on
    -- @pPostSubpassSampleLocations@.
    postSubpassSampleLocations :: Vector SubpassSampleLocationsEXT
  }
  deriving (Typeable)
deriving instance Show RenderPassSampleLocationsBeginInfoEXT

instance ToCStruct RenderPassSampleLocationsBeginInfoEXT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassSampleLocationsBeginInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (attachmentInitialSampleLocations)) :: Word32))
    pPAttachmentInitialSampleLocations' <- ContT $ allocaBytesAligned @AttachmentSampleLocationsEXT ((Data.Vector.length (attachmentInitialSampleLocations)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachmentInitialSampleLocations' `plusPtr` (48 * (i)) :: Ptr AttachmentSampleLocationsEXT) (e) . ($ ())) (attachmentInitialSampleLocations)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AttachmentSampleLocationsEXT))) (pPAttachmentInitialSampleLocations')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (postSubpassSampleLocations)) :: Word32))
    pPPostSubpassSampleLocations' <- ContT $ allocaBytesAligned @SubpassSampleLocationsEXT ((Data.Vector.length (postSubpassSampleLocations)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPPostSubpassSampleLocations' `plusPtr` (48 * (i)) :: Ptr SubpassSampleLocationsEXT) (e) . ($ ())) (postSubpassSampleLocations)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SubpassSampleLocationsEXT))) (pPPostSubpassSampleLocations')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPAttachmentInitialSampleLocations' <- ContT $ allocaBytesAligned @AttachmentSampleLocationsEXT ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachmentInitialSampleLocations' `plusPtr` (48 * (i)) :: Ptr AttachmentSampleLocationsEXT) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AttachmentSampleLocationsEXT))) (pPAttachmentInitialSampleLocations')
    pPPostSubpassSampleLocations' <- ContT $ allocaBytesAligned @SubpassSampleLocationsEXT ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPPostSubpassSampleLocations' `plusPtr` (48 * (i)) :: Ptr SubpassSampleLocationsEXT) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SubpassSampleLocationsEXT))) (pPPostSubpassSampleLocations')
    lift $ f

instance FromCStruct RenderPassSampleLocationsBeginInfoEXT where
  peekCStruct p = do
    attachmentInitialSampleLocationsCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAttachmentInitialSampleLocations <- peek @(Ptr AttachmentSampleLocationsEXT) ((p `plusPtr` 24 :: Ptr (Ptr AttachmentSampleLocationsEXT)))
    pAttachmentInitialSampleLocations' <- generateM (fromIntegral attachmentInitialSampleLocationsCount) (\i -> peekCStruct @AttachmentSampleLocationsEXT ((pAttachmentInitialSampleLocations `advancePtrBytes` (48 * (i)) :: Ptr AttachmentSampleLocationsEXT)))
    postSubpassSampleLocationsCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pPostSubpassSampleLocations <- peek @(Ptr SubpassSampleLocationsEXT) ((p `plusPtr` 40 :: Ptr (Ptr SubpassSampleLocationsEXT)))
    pPostSubpassSampleLocations' <- generateM (fromIntegral postSubpassSampleLocationsCount) (\i -> peekCStruct @SubpassSampleLocationsEXT ((pPostSubpassSampleLocations `advancePtrBytes` (48 * (i)) :: Ptr SubpassSampleLocationsEXT)))
    pure $ RenderPassSampleLocationsBeginInfoEXT
             pAttachmentInitialSampleLocations' pPostSubpassSampleLocations'

instance Zero RenderPassSampleLocationsBeginInfoEXT where
  zero = RenderPassSampleLocationsBeginInfoEXT
           mempty
           mempty


-- | VkPipelineSampleLocationsStateCreateInfoEXT - Structure specifying
-- sample locations for a pipeline
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32', 'SampleLocationsInfoEXT',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineSampleLocationsStateCreateInfoEXT = PipelineSampleLocationsStateCreateInfoEXT
  { -- | @sampleLocationsEnable@ controls whether custom sample locations are
    -- used. If @sampleLocationsEnable@ is
    -- 'Graphics.Vulkan.Core10.BaseType.FALSE', the default sample locations
    -- are used and the values specified in @sampleLocationsInfo@ are ignored.
    sampleLocationsEnable :: Bool
  , -- | @sampleLocationsInfo@ /must/ be a valid 'SampleLocationsInfoEXT'
    -- structure
    sampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Typeable)
deriving instance Show PipelineSampleLocationsStateCreateInfoEXT

instance ToCStruct PipelineSampleLocationsStateCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineSampleLocationsStateCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (sampleLocationsEnable))
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr SampleLocationsInfoEXT)) (sampleLocationsInfo) . ($ ())
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr SampleLocationsInfoEXT)) (zero) . ($ ())
    lift $ f

instance FromCStruct PipelineSampleLocationsStateCreateInfoEXT where
  peekCStruct p = do
    sampleLocationsEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    sampleLocationsInfo <- peekCStruct @SampleLocationsInfoEXT ((p `plusPtr` 24 :: Ptr SampleLocationsInfoEXT))
    pure $ PipelineSampleLocationsStateCreateInfoEXT
             (bool32ToBool sampleLocationsEnable) sampleLocationsInfo

instance Zero PipelineSampleLocationsStateCreateInfoEXT where
  zero = PipelineSampleLocationsStateCreateInfoEXT
           zero
           zero


-- | VkPhysicalDeviceSampleLocationsPropertiesEXT - Structure describing
-- sample location limits that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceSampleLocationsPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceSampleLocationsPropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.SharedTypes.Extent2D',
-- 'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSampleLocationsPropertiesEXT = PhysicalDeviceSampleLocationsPropertiesEXT
  { -- | @sampleLocationSampleCounts@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
    -- indicating the sample counts supporting custom sample locations.
    sampleLocationSampleCounts :: SampleCountFlags
  , -- | @maxSampleLocationGridSize@ is the maximum size of the pixel grid in
    -- which sample locations /can/ vary that is supported for all sample
    -- counts in @sampleLocationSampleCounts@.
    maxSampleLocationGridSize :: Extent2D
  , -- | @sampleLocationCoordinateRange@[2] is the range of supported sample
    -- location coordinates.
    sampleLocationCoordinateRange :: (Float, Float)
  , -- | @sampleLocationSubPixelBits@ is the number of bits of subpixel precision
    -- for sample locations.
    sampleLocationSubPixelBits :: Word32
  , -- | @variableSampleLocations@ specifies whether the sample locations used by
    -- all pipelines that will be bound to a command buffer during a subpass
    -- /must/ match. If set to 'Graphics.Vulkan.Core10.BaseType.TRUE', the
    -- implementation supports variable sample locations in a subpass. If set
    -- to 'Graphics.Vulkan.Core10.BaseType.FALSE', then the sample locations
    -- /must/ stay constant in each subpass.
    variableSampleLocations :: Bool
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceSampleLocationsPropertiesEXT

instance ToCStruct PhysicalDeviceSampleLocationsPropertiesEXT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSampleLocationsPropertiesEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr SampleCountFlags)) (sampleLocationSampleCounts)
    ContT $ pokeCStruct ((p `plusPtr` 20 :: Ptr Extent2D)) (maxSampleLocationGridSize) . ($ ())
    let pSampleLocationCoordinateRange' = lowerArrayPtr ((p `plusPtr` 28 :: Ptr (FixedArray 2 CFloat)))
    lift $ case (sampleLocationCoordinateRange) of
      (e0, e1) -> do
        poke (pSampleLocationCoordinateRange' :: Ptr CFloat) (CFloat (e0))
        poke (pSampleLocationCoordinateRange' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (sampleLocationSubPixelBits)
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (variableSampleLocations))
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr SampleCountFlags)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 20 :: Ptr Extent2D)) (zero) . ($ ())
    let pSampleLocationCoordinateRange' = lowerArrayPtr ((p `plusPtr` 28 :: Ptr (FixedArray 2 CFloat)))
    lift $ case ((zero, zero)) of
      (e0, e1) -> do
        poke (pSampleLocationCoordinateRange' :: Ptr CFloat) (CFloat (e0))
        poke (pSampleLocationCoordinateRange' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance FromCStruct PhysicalDeviceSampleLocationsPropertiesEXT where
  peekCStruct p = do
    sampleLocationSampleCounts <- peek @SampleCountFlags ((p `plusPtr` 16 :: Ptr SampleCountFlags))
    maxSampleLocationGridSize <- peekCStruct @Extent2D ((p `plusPtr` 20 :: Ptr Extent2D))
    let psampleLocationCoordinateRange = lowerArrayPtr @CFloat ((p `plusPtr` 28 :: Ptr (FixedArray 2 CFloat)))
    sampleLocationCoordinateRange0 <- peek @CFloat ((psampleLocationCoordinateRange `advancePtrBytes` 0 :: Ptr CFloat))
    sampleLocationCoordinateRange1 <- peek @CFloat ((psampleLocationCoordinateRange `advancePtrBytes` 4 :: Ptr CFloat))
    sampleLocationSubPixelBits <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    variableSampleLocations <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    pure $ PhysicalDeviceSampleLocationsPropertiesEXT
             sampleLocationSampleCounts maxSampleLocationGridSize ((((\(CFloat a) -> a) sampleLocationCoordinateRange0), ((\(CFloat a) -> a) sampleLocationCoordinateRange1))) sampleLocationSubPixelBits (bool32ToBool variableSampleLocations)

instance Zero PhysicalDeviceSampleLocationsPropertiesEXT where
  zero = PhysicalDeviceSampleLocationsPropertiesEXT
           zero
           zero
           (zero, zero)
           zero
           zero


-- | VkMultisamplePropertiesEXT - Structure returning information about
-- sample count specific additional multisampling capabilities
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SharedTypes.Extent2D',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceMultisamplePropertiesEXT'
data MultisamplePropertiesEXT = MultisamplePropertiesEXT
  { -- | @maxSampleLocationGridSize@ is the maximum size of the pixel grid in
    -- which sample locations /can/ vary.
    maxSampleLocationGridSize :: Extent2D }
  deriving (Typeable)
deriving instance Show MultisamplePropertiesEXT

instance ToCStruct MultisamplePropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MultisamplePropertiesEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Extent2D)) (maxSampleLocationGridSize) . ($ ())
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Extent2D)) (zero) . ($ ())
    lift $ f

instance FromCStruct MultisamplePropertiesEXT where
  peekCStruct p = do
    maxSampleLocationGridSize <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    pure $ MultisamplePropertiesEXT
             maxSampleLocationGridSize

instance Zero MultisamplePropertiesEXT where
  zero = MultisamplePropertiesEXT
           zero


type EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION"
pattern EXT_SAMPLE_LOCATIONS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1


type EXT_SAMPLE_LOCATIONS_EXTENSION_NAME = "VK_EXT_sample_locations"

-- No documentation found for TopLevel "VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME"
pattern EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SAMPLE_LOCATIONS_EXTENSION_NAME = "VK_EXT_sample_locations"

