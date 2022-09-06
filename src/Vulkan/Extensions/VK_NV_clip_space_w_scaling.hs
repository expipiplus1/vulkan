{-# language CPP #-}
-- | = Name
--
-- VK_NV_clip_space_w_scaling - device extension
--
-- == VK_NV_clip_space_w_scaling
--
-- [__Name String__]
--     @VK_NV_clip_space_w_scaling@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     88
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Contact__]
--
--     -   Eric Werness
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_clip_space_w_scaling] @ewerness-nv%0A<<Here describe the issue or question you have about the VK_NV_clip_space_w_scaling extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-02-15
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Kedarnath Thangudu, NVIDIA
--
-- == Description
--
-- Virtual Reality (VR) applications often involve a post-processing step
-- to apply a “barrel” distortion to the rendered image to correct the
-- “pincushion” distortion introduced by the optics in a VR device. The
-- barrel distorted image has lower resolution along the edges compared to
-- the center. Since the original image is rendered at high resolution,
-- which is uniform across the complete image, a lot of pixels towards the
-- edges do not make it to the final post-processed image.
--
-- This extension provides a mechanism to render VR scenes at a non-uniform
-- resolution, in particular a resolution that falls linearly from the
-- center towards the edges. This is achieved by scaling the w coordinate
-- of the vertices in the clip space before perspective divide. The clip
-- space w coordinate of the vertices /can/ be offset as of a function of x
-- and y coordinates as follows:
--
-- w\' = w + Ax + By
--
-- In the intended use case for viewport position scaling, an application
-- should use a set of four viewports, one for each of the four quadrants
-- of a Cartesian coordinate system. Each viewport is set to the dimension
-- of the image, but is scissored to the quadrant it represents. The
-- application should specify A and B coefficients of the w-scaling
-- equation above, that have the same value, but different signs, for each
-- of the viewports. The signs of A and B should match the signs of x and y
-- for the quadrant that they represent such that the value of w\' will
-- always be greater than or equal to the original w value for the entire
-- image. Since the offset to w, (Ax + By), is always positive, and
-- increases with the absolute values of x and y, the effective resolution
-- will fall off linearly from the center of the image to its edges.
--
-- == New Commands
--
-- -   'cmdSetViewportWScalingNV'
--
-- == New Structures
--
-- -   'ViewportWScalingNV'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo':
--
--     -   'PipelineViewportWScalingStateCreateInfoNV'
--
-- == New Enum Constants
--
-- -   'NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME'
--
-- -   'NV_CLIP_SPACE_W_SCALING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV'
--
-- == Issues
--
-- 1) Is the pipeline struct name too long?
--
-- __RESOLVED__: It fits with the naming convention.
--
-- 2) Separate W scaling section or fold into coordinate transformations?
--
-- __RESOLVED__: Leaving it as its own section for now.
--
-- == Examples
--
-- > VkViewport viewports[4];
-- > VkRect2D scissors[4];
-- > VkViewportWScalingNV scalings[4];
-- >
-- > for (int i = 0; i < 4; i++) {
-- >     int x = (i & 2) ? 0 : currentWindowWidth / 2;
-- >     int y = (i & 1) ? 0 : currentWindowHeight / 2;
-- >
-- >     viewports[i].x = 0;
-- >     viewports[i].y = 0;
-- >     viewports[i].width = currentWindowWidth;
-- >     viewports[i].height = currentWindowHeight;
-- >     viewports[i].minDepth = 0.0f;
-- >     viewports[i].maxDepth = 1.0f;
-- >
-- >     scissors[i].offset.x = x;
-- >     scissors[i].offset.y = y;
-- >     scissors[i].extent.width = currentWindowWidth/2;
-- >     scissors[i].extent.height = currentWindowHeight/2;
-- >
-- >     const float factor = 0.15;
-- >     scalings[i].xcoeff = ((i & 2) ? -1.0 : 1.0) * factor;
-- >     scalings[i].ycoeff = ((i & 1) ? -1.0 : 1.0) * factor;
-- > }
-- >
-- > VkPipelineViewportWScalingStateCreateInfoNV vpWScalingStateInfo = { VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV };
-- >
-- > vpWScalingStateInfo.viewportWScalingEnable = VK_TRUE;
-- > vpWScalingStateInfo.viewportCount = 4;
-- > vpWScalingStateInfo.pViewportWScalings = &scalings[0];
-- >
-- > VkPipelineViewportStateCreateInfo vpStateInfo = { VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO };
-- > vpStateInfo.viewportCount = 4;
-- > vpStateInfo.pViewports = &viewports[0];
-- > vpStateInfo.scissorCount = 4;
-- > vpStateInfo.pScissors = &scissors[0];
-- > vpStateInfo.pNext = &vpWScalingStateInfo;
--
-- Example shader to read from a w-scaled texture:
--
-- > // Vertex Shader
-- > // Draw a triangle that covers the whole screen
-- > const vec4 positions[3] = vec4[3](vec4(-1, -1, 0, 1),
-- >                                   vec4( 3, -1, 0, 1),
-- >                                   vec4(-1,  3, 0, 1));
-- > out vec2 uv;
-- > void main()
-- > {
-- >     vec4 pos = positions[ gl_VertexID ];
-- >     gl_Position = pos;
-- >     uv = pos.xy;
-- > }
-- >
-- > // Fragment Shader
-- > uniform sampler2D tex;
-- > uniform float xcoeff;
-- > uniform float ycoeff;
-- > out vec4 Color;
-- > in vec2 uv;
-- >
-- > void main()
-- > {
-- >     // Handle uv as if upper right quadrant
-- >     vec2 uvabs = abs(uv);
-- >
-- >     // unscale: transform w-scaled image into an unscaled image
-- >     //   scale: transform unscaled image int a w-scaled image
-- >     float unscale = 1.0 / (1 + xcoeff * uvabs.x + xcoeff * uvabs.y);
-- >     //float scale = 1.0 / (1 - xcoeff * uvabs.x - xcoeff * uvabs.y);
-- >
-- >     vec2 P = vec2(unscale * uvabs.x, unscale * uvabs.y);
-- >
-- >     // Go back to the right quadrant
-- >     P *= sign(uv);
-- >
-- >     Color = texture(tex, P * 0.5 + 0.5);
-- > }
--
-- == Version History
--
-- -   Revision 1, 2017-02-15 (Eric Werness)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PipelineViewportWScalingStateCreateInfoNV', 'ViewportWScalingNV',
-- 'cmdSetViewportWScalingNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_clip_space_w_scaling Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_clip_space_w_scaling  ( cmdSetViewportWScalingNV
                                                     , ViewportWScalingNV(..)
                                                     , PipelineViewportWScalingStateCreateInfoNV(..)
                                                     , NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
                                                     , pattern NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
                                                     , NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
                                                     , pattern NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
                                                     ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
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
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewportWScalingNV))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportWScalingNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ViewportWScalingNV -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ViewportWScalingNV -> IO ()

-- | vkCmdSetViewportWScalingNV - Set the viewport W scaling dynamically for
-- a command buffer
--
-- = Description
--
-- The viewport parameters taken from element i of @pViewportWScalings@
-- replace the current state for the viewport index @firstViewport@ + i,
-- for i in [0, @viewportCount@).
--
-- This command sets the viewport __W__ scaling for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'PipelineViewportWScalingStateCreateInfoNV'::@pViewportWScalings@ values
-- used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetViewportWScalingNV-firstViewport-01324# The sum of
--     @firstViewport@ and @viewportCount@ /must/ be between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetViewportWScalingNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetViewportWScalingNV-pViewportWScalings-parameter#
--     @pViewportWScalings@ /must/ be a valid pointer to an array of
--     @viewportCount@ 'ViewportWScalingNV' structures
--
-- -   #VUID-vkCmdSetViewportWScalingNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetViewportWScalingNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetViewportWScalingNV-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdSetViewportWScalingNV-viewportCount-arraylength#
--     @viewportCount@ /must/ be greater than @0@
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_clip_space_w_scaling VK_NV_clip_space_w_scaling>,
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
  let vkCmdSetViewportWScalingNVPtr = pVkCmdSetViewportWScalingNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetViewportWScalingNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetViewportWScalingNV is null" Nothing Nothing
  let vkCmdSetViewportWScalingNV' = mkVkCmdSetViewportWScalingNV vkCmdSetViewportWScalingNVPtr
  pPViewportWScalings <- ContT $ allocaBytes @ViewportWScalingNV ((Data.Vector.length (viewportWScalings)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPViewportWScalings `plusPtr` (8 * (i)) :: Ptr ViewportWScalingNV) (e)) (viewportWScalings)
  lift $ traceAroundEvent "vkCmdSetViewportWScalingNV" (vkCmdSetViewportWScalingNV' (commandBufferHandle (commandBuffer)) (firstViewport) ((fromIntegral (Data.Vector.length $ (viewportWScalings)) :: Word32)) (pPViewportWScalings))
  pure $ ()


-- | VkViewportWScalingNV - Structure specifying a viewport
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_clip_space_w_scaling VK_NV_clip_space_w_scaling>,
-- 'PipelineViewportWScalingStateCreateInfoNV', 'cmdSetViewportWScalingNV'
data ViewportWScalingNV = ViewportWScalingNV
  { -- | @xcoeff@ and @ycoeff@ are the viewport’s W scaling factor for x and y
    -- respectively.
    xcoeff :: Float
  , -- No documentation found for Nested "VkViewportWScalingNV" "ycoeff"
    ycoeff :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ViewportWScalingNV)
#endif
deriving instance Show ViewportWScalingNV

instance ToCStruct ViewportWScalingNV where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
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
             (coerce @CFloat @Float xcoeff) (coerce @CFloat @Float ycoeff)

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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_clip_space_w_scaling VK_NV_clip_space_w_scaling>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'ViewportWScalingNV'
data PipelineViewportWScalingStateCreateInfoNV = PipelineViewportWScalingStateCreateInfoNV
  { -- | @viewportWScalingEnable@ controls whether viewport __W__ scaling is
    -- enabled.
    viewportWScalingEnable :: Bool
  , -- | @viewportCount@ is the number of viewports used by __W__ scaling, and
    -- /must/ match the number of viewports in the pipeline if viewport __W__
    -- scaling is enabled.
    --
    -- #VUID-VkPipelineViewportWScalingStateCreateInfoNV-viewportCount-arraylength#
    -- @viewportCount@ /must/ be greater than @0@
    viewportCount :: Word32
  , -- | @pViewportWScalings@ is a pointer to an array of 'ViewportWScalingNV'
    -- structures defining the __W__ scaling parameters for the corresponding
    -- viewports. If the viewport __W__ scaling state is dynamic, this member
    -- is ignored.
    viewportWScalings :: Vector ViewportWScalingNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineViewportWScalingStateCreateInfoNV)
#endif
deriving instance Show PipelineViewportWScalingStateCreateInfoNV

instance ToCStruct PipelineViewportWScalingStateCreateInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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
        pPViewportWScalings <- ContT $ allocaBytes @ViewportWScalingNV (((Data.Vector.length (viewportWScalings))) * 8)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPViewportWScalings `plusPtr` (8 * (i)) :: Ptr ViewportWScalingNV) (e)) ((viewportWScalings))
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

