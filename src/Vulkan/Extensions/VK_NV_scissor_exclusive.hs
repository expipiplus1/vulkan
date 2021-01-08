{-# language CPP #-}
-- | = Name
--
-- VK_NV_scissor_exclusive - device extension
--
-- == VK_NV_scissor_exclusive
--
-- [__Name String__]
--     @VK_NV_scissor_exclusive@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     206
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Pat Brown
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_scissor_exclusive:%20&body=@nvpbrown%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-07-31
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--     None
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension adds support for an exclusive scissor test to Vulkan. The
-- exclusive scissor test behaves like the scissor test, except that the
-- exclusive scissor test fails for pixels inside the corresponding
-- rectangle and passes for pixels outside the rectangle. If the same
-- rectangle is used for both the scissor and exclusive scissor tests, the
-- exclusive scissor test will pass if and only if the scissor test fails.
--
-- == New Commands
--
-- -   'cmdSetExclusiveScissorNV'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExclusiveScissorFeaturesNV'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo':
--
--     -   'PipelineViewportExclusiveScissorStateCreateInfoNV'
--
-- == New Enum Constants
--
-- -   'NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME'
--
-- -   'NV_SCISSOR_EXCLUSIVE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV'
--
-- == Issues
--
-- 1) For the scissor test, the viewport state must be created with a
-- matching number of scissor and viewport rectangles. Should we have the
-- same requirement for exclusive scissors?
--
-- __RESOLVED__: For exclusive scissors, we relax this requirement and
-- allow an exclusive scissor rectangle count that is either zero or equal
-- to the number of viewport rectangles. If you pass in an exclusive
-- scissor count of zero, the exclusive scissor test is treated as
-- disabled.
--
-- == Version History
--
-- -   Revision 1, 2018-07-31 (Pat Brown)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceExclusiveScissorFeaturesNV',
-- 'PipelineViewportExclusiveScissorStateCreateInfoNV',
-- 'cmdSetExclusiveScissorNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_scissor_exclusive Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_scissor_exclusive  ( cmdSetExclusiveScissorNV
                                                  , PhysicalDeviceExclusiveScissorFeaturesNV(..)
                                                  , PipelineViewportExclusiveScissorStateCreateInfoNV(..)
                                                  , NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
                                                  , pattern NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
                                                  , NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
                                                  , pattern NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
                                                  ) where

import Vulkan.Internal.Utils (traceAroundEvent)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetExclusiveScissorNV))
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetExclusiveScissorNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()

-- | vkCmdSetExclusiveScissorNV - Set the dynamic exclusive scissor
-- rectangles on a command buffer
--
-- = Description
--
-- The scissor rectangles taken from element i of @pExclusiveScissors@
-- replace the current state for the scissor index @firstExclusiveScissor@
-- + i, for i in [0, @exclusiveScissorCount@).
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetExclusiveScissorNV-None-02031# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-exclusiveScissor exclusive scissor>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetExclusiveScissorNV-firstExclusiveScissor-02034# The
--     sum of @firstExclusiveScissor@ and @exclusiveScissorCount@ /must/ be
--     between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   #VUID-vkCmdSetExclusiveScissorNV-firstExclusiveScissor-02035# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @firstExclusiveScissor@ /must/ be @0@
--
-- -   #VUID-vkCmdSetExclusiveScissorNV-exclusiveScissorCount-02036# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @exclusiveScissorCount@ /must/ be @1@
--
-- -   #VUID-vkCmdSetExclusiveScissorNV-x-02037# The @x@ and @y@ members of
--     @offset@ in each member of @pExclusiveScissors@ /must/ be greater
--     than or equal to @0@
--
-- -   #VUID-vkCmdSetExclusiveScissorNV-offset-02038# Evaluation of
--     (@offset.x@ + @extent.width@) for each member of
--     @pExclusiveScissors@ /must/ not cause a signed integer addition
--     overflow
--
-- -   #VUID-vkCmdSetExclusiveScissorNV-offset-02039# Evaluation of
--     (@offset.y@ + @extent.height@) for each member of
--     @pExclusiveScissors@ /must/ not cause a signed integer addition
--     overflow
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetExclusiveScissorNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetExclusiveScissorNV-pExclusiveScissors-parameter#
--     @pExclusiveScissors@ /must/ be a valid pointer to an array of
--     @exclusiveScissorCount@ 'Vulkan.Core10.FundamentalTypes.Rect2D'
--     structures
--
-- -   #VUID-vkCmdSetExclusiveScissorNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetExclusiveScissorNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetExclusiveScissorNV-exclusiveScissorCount-arraylength#
--     @exclusiveScissorCount@ /must/ be greater than @0@
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
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.Rect2D'
cmdSetExclusiveScissorNV :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @firstExclusiveScissor@ is the index of the first exclusive scissor
                            -- rectangle whose state is updated by the command.
                            ("firstExclusiveScissor" ::: Word32)
                         -> -- | @pExclusiveScissors@ is a pointer to an array of
                            -- 'Vulkan.Core10.FundamentalTypes.Rect2D' structures defining exclusive
                            -- scissor rectangles.
                            ("exclusiveScissors" ::: Vector Rect2D)
                         -> io ()
cmdSetExclusiveScissorNV commandBuffer firstExclusiveScissor exclusiveScissors = liftIO . evalContT $ do
  let vkCmdSetExclusiveScissorNVPtr = pVkCmdSetExclusiveScissorNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetExclusiveScissorNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetExclusiveScissorNV is null" Nothing Nothing
  let vkCmdSetExclusiveScissorNV' = mkVkCmdSetExclusiveScissorNV vkCmdSetExclusiveScissorNVPtr
  pPExclusiveScissors <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (exclusiveScissors)) * 16) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPExclusiveScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (exclusiveScissors)
  lift $ traceAroundEvent "vkCmdSetExclusiveScissorNV" (vkCmdSetExclusiveScissorNV' (commandBufferHandle (commandBuffer)) (firstExclusiveScissor) ((fromIntegral (Data.Vector.length $ (exclusiveScissors)) :: Word32)) (pPExclusiveScissors))
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
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceExclusiveScissorFeaturesNV' /can/ also be included in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the
-- feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExclusiveScissorFeaturesNV = PhysicalDeviceExclusiveScissorFeaturesNV
  { -- | #features-exclusiveScissor# @exclusiveScissor@ indicates that the
    -- implementation supports the exclusive scissor test.
    exclusiveScissor :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExclusiveScissorFeaturesNV)
#endif
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
-- If the
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV'
-- dynamic state is enabled for a pipeline, the @pExclusiveScissors@ member
-- is ignored.
--
-- When this structure is included in the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo', it defines
-- parameters of the exclusive scissor test. If this structure is not
-- included in the @pNext@ chain, it is equivalent to specifying this
-- structure with a @exclusiveScissorCount@ of @0@.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineViewportExclusiveScissorStateCreateInfoNV-exclusiveScissorCount-02027#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @exclusiveScissorCount@ /must/ be @0@ or @1@
--
-- -   #VUID-VkPipelineViewportExclusiveScissorStateCreateInfoNV-exclusiveScissorCount-02028#
--     @exclusiveScissorCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@
--
-- -   #VUID-VkPipelineViewportExclusiveScissorStateCreateInfoNV-exclusiveScissorCount-02029#
--     @exclusiveScissorCount@ /must/ be @0@ or greater than or equal to
--     the @viewportCount@ member of
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineViewportExclusiveScissorStateCreateInfoNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Rect2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineViewportExclusiveScissorStateCreateInfoNV = PipelineViewportExclusiveScissorStateCreateInfoNV
  { -- | @pExclusiveScissors@ is a pointer to an array of
    -- 'Vulkan.Core10.FundamentalTypes.Rect2D' structures defining exclusive
    -- scissor rectangles.
    exclusiveScissors :: Vector Rect2D }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineViewportExclusiveScissorStateCreateInfoNV)
#endif
deriving instance Show PipelineViewportExclusiveScissorStateCreateInfoNV

instance ToCStruct PipelineViewportExclusiveScissorStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportExclusiveScissorStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (exclusiveScissors)) :: Word32))
    pPExclusiveScissors' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (exclusiveScissors)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPExclusiveScissors' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (exclusiveScissors)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Rect2D))) (pPExclusiveScissors')
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
    pExclusiveScissors' <- generateM (fromIntegral exclusiveScissorCount) (\i -> peekCStruct @Rect2D ((pExclusiveScissors `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
    pure $ PipelineViewportExclusiveScissorStateCreateInfoNV
             pExclusiveScissors'

instance Zero PipelineViewportExclusiveScissorStateCreateInfoNV where
  zero = PipelineViewportExclusiveScissorStateCreateInfoNV
           mempty


type NV_SCISSOR_EXCLUSIVE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION"
pattern NV_SCISSOR_EXCLUSIVE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SCISSOR_EXCLUSIVE_SPEC_VERSION = 1


type NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME = "VK_NV_scissor_exclusive"

-- No documentation found for TopLevel "VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME"
pattern NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME = "VK_NV_scissor_exclusive"

