{-# language CPP #-}
module Vulkan.Extensions.VK_NV_clip_space_w_scaling  ( cmdSetViewportWScalingNV
                                                     , ViewportWScalingNV(..)
                                                     , PipelineViewportWScalingStateCreateInfoNV(..)
                                                     , NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
                                                     , pattern NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
                                                     , NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
                                                     , pattern NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
                                                     ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewportWScalingNV))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportWScalingNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ViewportWScalingNV -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ViewportWScalingNV -> IO ()

-- | vkCmdSetViewportWScalingNV - Set the viewport W scaling on a command
-- buffer
--
-- = Description
--
-- The viewport parameters taken from element i of @pViewportWScalings@
-- replace the current state for the viewport index @firstViewport@ + i,
-- for i in [0, @viewportCount@).
--
-- == Valid Usage
--
-- -   @firstViewport@ /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstViewport@ and @viewportCount@ /must/ be between @1@
--     and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pViewportWScalings@ /must/ be a valid pointer to an array of
--     @viewportCount@ 'ViewportWScalingNV' structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   @viewportCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
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
-- 'Vulkan.Core10.Handles.CommandBuffer', 'ViewportWScalingNV'
cmdSetViewportWScalingNV :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @firstViewport@ is the index of the first viewport whose parameters are
                            -- updated by the command.
                            ("firstViewport" ::: Word32)
                         -> -- | @pViewportWScalings@ is a pointer to an array of 'ViewportWScalingNV'
                            -- structures specifying viewport parameters.
                            ("viewportWScalings" ::: Vector ViewportWScalingNV)
                         -> io ()
cmdSetViewportWScalingNV commandBuffer firstViewport viewportWScalings = liftIO . evalContT $ do
  let vkCmdSetViewportWScalingNVPtr = pVkCmdSetViewportWScalingNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetViewportWScalingNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetViewportWScalingNV is null" Nothing Nothing
  let vkCmdSetViewportWScalingNV' = mkVkCmdSetViewportWScalingNV vkCmdSetViewportWScalingNVPtr
  pPViewportWScalings <- ContT $ allocaBytesAligned @ViewportWScalingNV ((Data.Vector.length (viewportWScalings)) * 8) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPViewportWScalings `plusPtr` (8 * (i)) :: Ptr ViewportWScalingNV) (e) . ($ ())) (viewportWScalings)
  lift $ vkCmdSetViewportWScalingNV' (commandBufferHandle (commandBuffer)) (firstViewport) ((fromIntegral (Data.Vector.length $ (viewportWScalings)) :: Word32)) (pPViewportWScalings)
  pure $ ()


-- | VkViewportWScalingNV - Structure specifying a viewport
--
-- = See Also
--
-- 'PipelineViewportWScalingStateCreateInfoNV', 'cmdSetViewportWScalingNV'
data ViewportWScalingNV = ViewportWScalingNV
  { -- | @xcoeff@ and @ycoeff@ are the viewport’s W scaling factor for x and y
    -- respectively.
    xcoeff :: Float
  , -- No documentation found for Nested "VkViewportWScalingNV" "ycoeff"
    ycoeff :: Float
  }
  deriving (Typeable)
deriving instance Show ViewportWScalingNV

instance ToCStruct ViewportWScalingNV where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ViewportWScalingNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (xcoeff))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (ycoeff))
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct ViewportWScalingNV where
  peekCStruct p = do
    xcoeff <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    ycoeff <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    pure $ ViewportWScalingNV
             ((\(CFloat a) -> a) xcoeff) ((\(CFloat a) -> a) ycoeff)

instance Storable ViewportWScalingNV where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ViewportWScalingNV where
  zero = ViewportWScalingNV
           zero
           zero


-- | VkPipelineViewportWScalingStateCreateInfoNV - Structure specifying
-- parameters of a newly created pipeline viewport W scaling state
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'ViewportWScalingNV'
data PipelineViewportWScalingStateCreateInfoNV = PipelineViewportWScalingStateCreateInfoNV
  { -- | @viewportWScalingEnable@ controls whether viewport __W__ scaling is
    -- enabled.
    viewportWScalingEnable :: Bool
  , -- | @viewportCount@ is the number of viewports used by __W__ scaling, and
    -- /must/ match the number of viewports in the pipeline if viewport __W__
    -- scaling is enabled.
    --
    -- @viewportCount@ /must/ be greater than @0@
    viewportCount :: Word32
  , -- | @pViewportWScalings@ is a pointer to an array of 'ViewportWScalingNV'
    -- structures defining the __W__ scaling parameters for the corresponding
    -- viewports. If the viewport __W__ scaling state is dynamic, this member
    -- is ignored.
    viewportWScalings :: Vector ViewportWScalingNV
  }
  deriving (Typeable)
deriving instance Show PipelineViewportWScalingStateCreateInfoNV

instance ToCStruct PipelineViewportWScalingStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportWScalingStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (viewportWScalingEnable))
    let pViewportWScalingsLength = Data.Vector.length $ (viewportWScalings)
    viewportCount'' <- lift $ if (viewportCount) == 0
      then pure $ fromIntegral pViewportWScalingsLength
      else do
        unless (fromIntegral pViewportWScalingsLength == (viewportCount) || pViewportWScalingsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pViewportWScalings must be empty or have 'viewportCount' elements" Nothing Nothing
        pure (viewportCount)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (viewportCount'')
    pViewportWScalings'' <- if Data.Vector.null (viewportWScalings)
      then pure nullPtr
      else do
        pPViewportWScalings <- ContT $ allocaBytesAligned @ViewportWScalingNV (((Data.Vector.length (viewportWScalings))) * 8) 4
        Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPViewportWScalings `plusPtr` (8 * (i)) :: Ptr ViewportWScalingNV) (e) . ($ ())) ((viewportWScalings))
        pure $ pPViewportWScalings
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ViewportWScalingNV))) pViewportWScalings''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PipelineViewportWScalingStateCreateInfoNV where
  peekCStruct p = do
    viewportWScalingEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    viewportCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pViewportWScalings <- peek @(Ptr ViewportWScalingNV) ((p `plusPtr` 24 :: Ptr (Ptr ViewportWScalingNV)))
    let pViewportWScalingsLength = if pViewportWScalings == nullPtr then 0 else (fromIntegral viewportCount)
    pViewportWScalings' <- generateM pViewportWScalingsLength (\i -> peekCStruct @ViewportWScalingNV ((pViewportWScalings `advancePtrBytes` (8 * (i)) :: Ptr ViewportWScalingNV)))
    pure $ PipelineViewportWScalingStateCreateInfoNV
             (bool32ToBool viewportWScalingEnable) viewportCount pViewportWScalings'

instance Zero PipelineViewportWScalingStateCreateInfoNV where
  zero = PipelineViewportWScalingStateCreateInfoNV
           zero
           zero
           mempty


type NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION"
pattern NV_CLIP_SPACE_W_SCALING_SPEC_VERSION :: forall a . Integral a => a
pattern NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1


type NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME = "VK_NV_clip_space_w_scaling"

-- No documentation found for TopLevel "VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME"
pattern NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME = "VK_NV_clip_space_w_scaling"

