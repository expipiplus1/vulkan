{-# language CPP #-}
-- | = Name
--
-- VK_EXT_color_write_enable - device extension
--
-- == VK_EXT_color_write_enable
--
-- [__Name String__]
--     @VK_EXT_color_write_enable@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     382
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Sharif Elcott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_color_write_enable] @selcott%0A<<Here describe the issue or question you have about the VK_EXT_color_write_enable extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-02-25
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Sharif Elcott, Google
--
--     -   Tobias Hector, AMD
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension allows for selectively enabling and disabling writes to
-- output color attachments via a pipeline dynamic state.
--
-- The intended use cases for this new state are mostly identical to those
-- of colorWriteMask, such as selectively disabling writes to avoid
-- feedback loops between subpasses or bandwidth savings for unused
-- outputs. By making the state dynamic, one additional benefit is the
-- ability to reduce pipeline counts and pipeline switching via shaders
-- that write a superset of the desired data of which subsets are selected
-- dynamically. The reason for a new state, colorWriteEnable, rather than
-- making colorWriteMask dynamic is that, on many implementations, the more
-- flexible per-component semantics of the colorWriteMask state cannot be
-- made dynamic in a performant manner.
--
-- == New Commands
--
-- -   'cmdSetColorWriteEnableEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceColorWriteEnableFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo':
--
--     -   'PipelineColorWriteCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_COLOR_WRITE_ENABLE_EXTENSION_NAME'
--
-- -   'EXT_COLOR_WRITE_ENABLE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COLOR_WRITE_ENABLE_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COLOR_WRITE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2020-01-25 (Sharif Elcott)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceColorWriteEnableFeaturesEXT',
-- 'PipelineColorWriteCreateInfoEXT', 'cmdSetColorWriteEnableEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_color_write_enable Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_color_write_enable  ( cmdSetColorWriteEnableEXT
                                                    , PhysicalDeviceColorWriteEnableFeaturesEXT(..)
                                                    , PipelineColorWriteCreateInfoEXT(..)
                                                    , EXT_COLOR_WRITE_ENABLE_SPEC_VERSION
                                                    , pattern EXT_COLOR_WRITE_ENABLE_SPEC_VERSION
                                                    , EXT_COLOR_WRITE_ENABLE_EXTENSION_NAME
                                                    , pattern EXT_COLOR_WRITE_ENABLE_EXTENSION_NAME
                                                    ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
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
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetColorWriteEnableEXT))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COLOR_WRITE_ENABLE_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COLOR_WRITE_CREATE_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetColorWriteEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Bool32 -> IO ()

-- | vkCmdSetColorWriteEnableEXT - Enable or disable writes to a color
-- attachment dynamically for a command buffer
--
-- = Description
--
-- This command sets the color write enables for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'PipelineColorWriteCreateInfoEXT'::@pColorWriteEnables@ values used to
-- create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetColorWriteEnableEXT-None-04803# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-colorWriteEnable colorWriteEnable>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetColorWriteEnableEXT-attachmentCount-06656#
--     @attachmentCount@ /must/ be less than or equal to the
--     @maxColorAttachments@ member of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetColorWriteEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetColorWriteEnableEXT-pColorWriteEnables-parameter#
--     @pColorWriteEnables@ /must/ be a valid pointer to an array of
--     @attachmentCount@ 'Vulkan.Core10.FundamentalTypes.Bool32' values
--
-- -   #VUID-vkCmdSetColorWriteEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetColorWriteEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetColorWriteEnableEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdSetColorWriteEnableEXT-attachmentCount-arraylength#
--     @attachmentCount@ /must/ be greater than @0@
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_color_write_enable VK_EXT_color_write_enable>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetColorWriteEnableEXT :: forall io
                           . (MonadIO io)
                          => -- | @commandBuffer@ is the command buffer into which the command will be
                             -- recorded.
                             CommandBuffer
                          -> -- | @pColorWriteEnables@ is a pointer to an array of per target attachment
                             -- boolean values specifying whether color writes are enabled for the given
                             -- attachment.
                             ("colorWriteEnables" ::: Vector Bool)
                          -> io ()
cmdSetColorWriteEnableEXT commandBuffer colorWriteEnables = liftIO . evalContT $ do
  let vkCmdSetColorWriteEnableEXTPtr = pVkCmdSetColorWriteEnableEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetColorWriteEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetColorWriteEnableEXT is null" Nothing Nothing
  let vkCmdSetColorWriteEnableEXT' = mkVkCmdSetColorWriteEnableEXT vkCmdSetColorWriteEnableEXTPtr
  pPColorWriteEnables <- ContT $ allocaBytes @Bool32 ((Data.Vector.length (colorWriteEnables)) * 4)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPColorWriteEnables `plusPtr` (4 * (i)) :: Ptr Bool32) (boolToBool32 (e))) (colorWriteEnables)
  lift $ traceAroundEvent "vkCmdSetColorWriteEnableEXT" (vkCmdSetColorWriteEnableEXT' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (colorWriteEnables)) :: Word32)) (pPColorWriteEnables))
  pure $ ()


-- | VkPhysicalDeviceColorWriteEnableFeaturesEXT - Structure describing
-- whether writes to color attachments can be enabled and disabled
-- dynamically
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceColorWriteEnableFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceColorWriteEnableFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_color_write_enable VK_EXT_color_write_enable>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceColorWriteEnableFeaturesEXT = PhysicalDeviceColorWriteEnableFeaturesEXT
  { -- | #features-colorWriteEnable# @colorWriteEnable@ indicates that the
    -- implementation supports the dynamic state
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'.
    colorWriteEnable :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceColorWriteEnableFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceColorWriteEnableFeaturesEXT

instance ToCStruct PhysicalDeviceColorWriteEnableFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceColorWriteEnableFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COLOR_WRITE_ENABLE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (colorWriteEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COLOR_WRITE_ENABLE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceColorWriteEnableFeaturesEXT where
  peekCStruct p = do
    colorWriteEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceColorWriteEnableFeaturesEXT
             (bool32ToBool colorWriteEnable)

instance Storable PhysicalDeviceColorWriteEnableFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceColorWriteEnableFeaturesEXT where
  zero = PhysicalDeviceColorWriteEnableFeaturesEXT
           zero


-- | VkPipelineColorWriteCreateInfoEXT - Structure specifying color write
-- state of a newly created pipeline
--
-- = Description
--
-- When this structure is included in the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo', it defines
-- per-attachment color write state. If this structure is not included in
-- the @pNext@ chain, it is equivalent to specifying this structure with
-- @attachmentCount@ equal to the @attachmentCount@ member of
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo', and
-- @pColorWriteEnables@ pointing to an array of as many
-- 'Vulkan.Core10.FundamentalTypes.TRUE' values.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-colorWriteEnable colorWriteEnable>
-- feature is not enabled on the device, all
-- 'Vulkan.Core10.FundamentalTypes.Bool32' elements in the
-- @pColorWriteEnables@ array /must/ be
-- 'Vulkan.Core10.FundamentalTypes.TRUE'.
--
-- Color Write Enable interacts with the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#framebuffer-color-write-mask Color Write Mask>
-- as follows:
--
-- -   If @colorWriteEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     writes to the attachment are determined by the @colorWriteMask@.
--
-- -   If @colorWriteEnable@ is 'Vulkan.Core10.FundamentalTypes.FALSE', the
--     @colorWriteMask@ is ignored and writes to all components of the
--     attachment are disabled. This is equivalent to specifying a
--     @colorWriteMask@ of 0.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineColorWriteCreateInfoEXT-pAttachments-04801# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-colorWriteEnable colorWriteEnable>
--     feature is not enabled, all elements of @pColorWriteEnables@ /must/
--     be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkPipelineColorWriteCreateInfoEXT-attachmentCount-04802#
--     @attachmentCount@ /must/ be equal to the @attachmentCount@ member of
--     the 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'
--     structure specified during pipeline creation
--
-- -   #VUID-VkPipelineColorWriteCreateInfoEXT-attachmentCount-06655#
--     @attachmentCount@ /must/ be less than or equal to the
--     @maxColorAttachments@ member of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineColorWriteCreateInfoEXT-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COLOR_WRITE_CREATE_INFO_EXT'
--
-- -   #VUID-VkPipelineColorWriteCreateInfoEXT-pColorWriteEnables-parameter#
--     If @attachmentCount@ is not @0@, @pColorWriteEnables@ /must/ be a
--     valid pointer to an array of @attachmentCount@
--     'Vulkan.Core10.FundamentalTypes.Bool32' values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_color_write_enable VK_EXT_color_write_enable>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineColorWriteCreateInfoEXT = PipelineColorWriteCreateInfoEXT
  { -- | @pColorWriteEnables@ is a pointer to an array of per target attachment
    -- boolean values specifying whether color writes are enabled for the given
    -- attachment.
    colorWriteEnables :: Vector Bool }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineColorWriteCreateInfoEXT)
#endif
deriving instance Show PipelineColorWriteCreateInfoEXT

instance ToCStruct PipelineColorWriteCreateInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineColorWriteCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COLOR_WRITE_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorWriteEnables)) :: Word32))
    pPColorWriteEnables' <- ContT $ allocaBytes @Bool32 ((Data.Vector.length (colorWriteEnables)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorWriteEnables' `plusPtr` (4 * (i)) :: Ptr Bool32) (boolToBool32 (e))) (colorWriteEnables)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Bool32))) (pPColorWriteEnables')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COLOR_WRITE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineColorWriteCreateInfoEXT where
  peekCStruct p = do
    attachmentCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pColorWriteEnables <- peek @(Ptr Bool32) ((p `plusPtr` 24 :: Ptr (Ptr Bool32)))
    pColorWriteEnables' <- generateM (fromIntegral attachmentCount) (\i -> do
      pColorWriteEnablesElem <- peek @Bool32 ((pColorWriteEnables `advancePtrBytes` (4 * (i)) :: Ptr Bool32))
      pure $ bool32ToBool pColorWriteEnablesElem)
    pure $ PipelineColorWriteCreateInfoEXT
             pColorWriteEnables'

instance Zero PipelineColorWriteCreateInfoEXT where
  zero = PipelineColorWriteCreateInfoEXT
           mempty


type EXT_COLOR_WRITE_ENABLE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_COLOR_WRITE_ENABLE_SPEC_VERSION"
pattern EXT_COLOR_WRITE_ENABLE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_COLOR_WRITE_ENABLE_SPEC_VERSION = 1


type EXT_COLOR_WRITE_ENABLE_EXTENSION_NAME = "VK_EXT_color_write_enable"

-- No documentation found for TopLevel "VK_EXT_COLOR_WRITE_ENABLE_EXTENSION_NAME"
pattern EXT_COLOR_WRITE_ENABLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_COLOR_WRITE_ENABLE_EXTENSION_NAME = "VK_EXT_color_write_enable"

