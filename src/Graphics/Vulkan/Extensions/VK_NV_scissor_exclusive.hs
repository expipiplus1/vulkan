{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive  ( cmdSetExclusiveScissorNV
                                                           , PhysicalDeviceExclusiveScissorFeaturesNV(..)
                                                           , PipelineViewportExclusiveScissorStateCreateInfoNV(..)
                                                           , NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
                                                           , pattern NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
                                                           , NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
                                                           , pattern NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
                                                           ) where

import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Either (Either)
import Data.String (IsString)
import Data.Typeable (Typeable)
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
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.Handles (CommandBuffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer_T)
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetExclusiveScissorNV))
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.CommandBufferBuilding (Rect2D)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetExclusiveScissorNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()

-- | vkCmdSetExclusiveScissorNV - Set the dynamic exclusive scissor
-- rectangles on a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @firstExclusiveScissor@ is the index of the first exclusive scissor
--     rectangle whose state is updated by the command.
--
-- -   @exclusiveScissorCount@ is the number of exclusive scissor
--     rectangles updated by the command.
--
-- -   @pExclusiveScissors@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D' structures
--     defining exclusive scissor rectangles.
--
-- = Description
--
-- The scissor rectangles taken from element i of @pExclusiveScissors@
-- replace the current state for the scissor index @firstExclusiveScissor@
-- + i, for i in [0, @exclusiveScissorCount@).
--
-- Each scissor rectangle is described by a
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D' structure, with
-- the @offset.x@ and @offset.y@ values determining the upper left corner
-- of the scissor rectangle, and the @extent.width@ and @extent.height@
-- values determining the size in pixels.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-exclusiveScissor exclusive scissor>
--     feature /must/ be enabled.
--
-- -   @firstExclusiveScissor@ /must/ be less than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstExclusiveScissor@ and @exclusiveScissorCount@
--     /must/ be between @1@ and
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @firstExclusiveScissor@ /must/ be @0@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @exclusiveScissorCount@ /must/ be @1@
--
-- -   The @x@ and @y@ members of @offset@ in each member of
--     @pExclusiveScissors@ /must/ be greater than or equal to @0@
--
-- -   Evaluation of (@offset.x@ + @extent.width@) for each member of
--     @pExclusiveScissors@ /must/ not cause a signed integer addition
--     overflow
--
-- -   Evaluation of (@offset.y@ + @extent.height@) for each member of
--     @pExclusiveScissors@ /must/ not cause a signed integer addition
--     overflow
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pExclusiveScissors@ /must/ be a valid pointer to an array of
--     @exclusiveScissorCount@
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D' structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   @exclusiveScissorCount@ /must/ be greater than @0@
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
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D'
cmdSetExclusiveScissorNV :: forall io . MonadIO io => CommandBuffer -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissors" ::: Vector Rect2D) -> io ()
cmdSetExclusiveScissorNV commandBuffer firstExclusiveScissor exclusiveScissors = liftIO . evalContT $ do
  let vkCmdSetExclusiveScissorNV' = mkVkCmdSetExclusiveScissorNV (pVkCmdSetExclusiveScissorNV (deviceCmds (commandBuffer :: CommandBuffer)))
  pPExclusiveScissors <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (exclusiveScissors)) * 16) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPExclusiveScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e) . ($ ())) (exclusiveScissors)
  lift $ vkCmdSetExclusiveScissorNV' (commandBufferHandle (commandBuffer)) (firstExclusiveScissor) ((fromIntegral (Data.Vector.length $ (exclusiveScissors)) :: Word32)) (pPExclusiveScissors)
  pure $ ()


-- | VkPhysicalDeviceExclusiveScissorFeaturesNV - Structure describing
-- exclusive scissor features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceExclusiveScissorFeaturesNV' structure
-- describe the following features:
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-exclusive-scissor Exclusive Scissor Test>
-- for more information.
--
-- If the 'PhysicalDeviceExclusiveScissorFeaturesNV' structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceExclusiveScissorFeaturesNV' /can/ also be included in the
-- @pNext@ chain of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable the feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExclusiveScissorFeaturesNV = PhysicalDeviceExclusiveScissorFeaturesNV
  { -- | @exclusiveScissor@ indicates that the implementation supports the
    -- exclusive scissor test.
    exclusiveScissor :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceExclusiveScissorFeaturesNV

instance ToCStruct PhysicalDeviceExclusiveScissorFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExclusiveScissorFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (exclusiveScissor))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExclusiveScissorFeaturesNV where
  peekCStruct p = do
    exclusiveScissor <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceExclusiveScissorFeaturesNV
             (bool32ToBool exclusiveScissor)

instance Storable PhysicalDeviceExclusiveScissorFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExclusiveScissorFeaturesNV where
  zero = PhysicalDeviceExclusiveScissorFeaturesNV
           zero


-- | VkPipelineViewportExclusiveScissorStateCreateInfoNV - Structure
-- specifying parameters controlling exclusive scissor testing
--
-- = Description
--
-- If this structure is not present, @exclusiveScissorCount@ is considered
-- to be @0@ and the exclusive scissor test is disabled.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @exclusiveScissorCount@ /must/ be @0@ or @1@
--
-- -   @exclusiveScissorCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@
--
-- -   @exclusiveScissorCount@ /must/ be @0@ or identical to the
--     @viewportCount@ member of
--     'Graphics.Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV'
--     and @exclusiveScissorCount@ is not @0@, @pExclusiveScissors@ /must/
--     be a valid pointer to an array of @exclusiveScissorCount@
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D' structures
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV'
--
-- -   If @exclusiveScissorCount@ is not @0@, and @pExclusiveScissors@ is
--     not @NULL@, @pExclusiveScissors@ /must/ be a valid pointer to an
--     array of @exclusiveScissorCount@
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D' structures
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineViewportExclusiveScissorStateCreateInfoNV = PipelineViewportExclusiveScissorStateCreateInfoNV
  { -- | @pExclusiveScissors@ is a pointer to an array of
    -- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D' structures
    -- defining exclusive scissor rectangles. If the exclusive scissor state is
    -- dynamic, this member is ignored.
    exclusiveScissors :: Either Word32 (Vector Rect2D) }
  deriving (Typeable)
deriving instance Show PipelineViewportExclusiveScissorStateCreateInfoNV

instance ToCStruct PipelineViewportExclusiveScissorStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportExclusiveScissorStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (either id (fromIntegral . Data.Vector.length) (exclusiveScissors)) :: Word32))
    pExclusiveScissors'' <- case (exclusiveScissors) of
      Left _ -> pure nullPtr
      Right v -> do
        pPExclusiveScissors' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (v)) * 16) 4
        Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPExclusiveScissors' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e) . ($ ())) (v)
        pure $ pPExclusiveScissors'
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Rect2D))) pExclusiveScissors''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineViewportExclusiveScissorStateCreateInfoNV where
  peekCStruct p = do
    exclusiveScissorCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pExclusiveScissors <- peek @(Ptr Rect2D) ((p `plusPtr` 24 :: Ptr (Ptr Rect2D)))
    pExclusiveScissors' <- maybePeek (\j -> generateM (fromIntegral exclusiveScissorCount) (\i -> peekCStruct @Rect2D (((j) `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))) pExclusiveScissors
    let pExclusiveScissors'' = maybe (Left exclusiveScissorCount) Right pExclusiveScissors'
    pure $ PipelineViewportExclusiveScissorStateCreateInfoNV
             pExclusiveScissors''

instance Zero PipelineViewportExclusiveScissorStateCreateInfoNV where
  zero = PipelineViewportExclusiveScissorStateCreateInfoNV
           (Left 0)


type NV_SCISSOR_EXCLUSIVE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION"
pattern NV_SCISSOR_EXCLUSIVE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SCISSOR_EXCLUSIVE_SPEC_VERSION = 1


type NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME = "VK_NV_scissor_exclusive"

-- No documentation found for TopLevel "VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME"
pattern NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME = "VK_NV_scissor_exclusive"

