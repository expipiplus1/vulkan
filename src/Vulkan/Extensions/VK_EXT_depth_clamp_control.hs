{-# language CPP #-}
-- | = Name
--
-- VK_EXT_depth_clamp_control - device extension
--
-- = VK_EXT_depth_clamp_control
--
-- [__Name String__]
--     @VK_EXT_depth_clamp_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     583
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Jules Blok
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_depth_clamp_control] @jules%0A*Here describe the issue or question you have about the VK_EXT_depth_clamp_control extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_depth_clamp_control.adoc VK_EXT_depth_clamp_control>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-07-15
--
-- [__Contributors__]
--
--     -   Jules Blok, Independent
--
-- == Description
--
-- This extension allows the application to control the viewport depth
-- clamp range separately from the viewport @minDepth@ and @maxDepth@. This
-- gives the ability for the application to restrict depth values to an
-- application-defined range rather than the viewport depth range or the
-- range defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clamp_zero_one VK_EXT_depth_clamp_zero_one>
-- extension.
--
-- It can be used to set a smaller or larger clamping range than the
-- viewport depth range without affecting the depth mapping of the viewport
-- transform. Another possible use of this extension is to restrict depth
-- values beyond the viewport depth range to a clamping range other than
-- the [0, 1] range defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clamp_zero_one VK_EXT_depth_clamp_zero_one>
-- extension.
--
-- == New Commands
--
-- -   'cmdSetDepthClampRangeEXT'
--
-- == New Structures
--
-- -   'DepthClampRangeEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDepthClampControlFeaturesEXT'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo':
--
--     -   'PipelineViewportDepthClampControlCreateInfoEXT'
--
-- == New Enums
--
-- -   'DepthClampModeEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEPTH_CLAMP_CONTROL_EXTENSION_NAME'
--
-- -   'EXT_DEPTH_CLAMP_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_RANGE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_CONTROL_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLAMP_CONTROL_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) Should the depth clamp range be a per-viewport parameter?
--
-- __RESOLVED__: No. Because the depth clamp range was previously defined
-- to be equal to the viewport depth range, conformant runtimes are already
-- handling the depth clamp range as a per-viewport parameter. However
-- because of complexities from interactions with multiple viewports a
-- per-viewport clamp range is left to a future extensions if a use case
-- arises.
--
-- 2) Should this pipeline state be dynamic?
--
-- __RESOLVED__: Yes. Since the viewport depth range can already be a
-- dynamic state conformant runtimes are already able to handle the depth
-- clamp range as a dynamic state.
--
-- 3) Can the depth clamp range be ignored when depth clamping is disabled?
--
-- __RESOLVED__: Yes. This extension overrides the clamping range used only
-- when depth clamping is enabled. The alternative would be highly
-- unintuitive. As a consequence the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clip_enable VK_EXT_depth_clip_enable>
-- extension is required if depth clipping is desired in combination with
-- this extension.
--
-- == Version History
--
-- -   Revision 1, 2024-02-13 (Jules Blok)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_depth_clamp_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_depth_clamp_control  ( cmdSetDepthClampRangeEXT
                                                     , PhysicalDeviceDepthClampControlFeaturesEXT(..)
                                                     , PipelineViewportDepthClampControlCreateInfoEXT(..)
                                                     , DepthClampRangeEXT(..)
                                                     , DepthClampModeEXT( DEPTH_CLAMP_MODE_VIEWPORT_RANGE_EXT
                                                                        , DEPTH_CLAMP_MODE_USER_DEFINED_RANGE_EXT
                                                                        , ..
                                                                        )
                                                     , EXT_DEPTH_CLAMP_CONTROL_SPEC_VERSION
                                                     , pattern EXT_DEPTH_CLAMP_CONTROL_SPEC_VERSION
                                                     , EXT_DEPTH_CLAMP_CONTROL_EXTENSION_NAME
                                                     , pattern EXT_DEPTH_CLAMP_CONTROL_EXTENSION_NAME
                                                     ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (maybePeek)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
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
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthClampRangeEXT))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_CONTROL_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLAMP_CONTROL_CREATE_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthClampRangeEXT
  :: FunPtr (Ptr CommandBuffer_T -> DepthClampModeEXT -> Ptr DepthClampRangeEXT -> IO ()) -> Ptr CommandBuffer_T -> DepthClampModeEXT -> Ptr DepthClampRangeEXT -> IO ()

-- | vkCmdSetDepthClampRangeEXT - Set the viewport depth clamp range
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the viewport depth clamp range for subsequent drawing
-- commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_RANGE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'PipelineViewportDepthClampControlCreateInfoEXT'::@depthClampMode@ value
-- used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDepthClampRangeEXT-pDepthClampRange-09647# If
--     @depthClampMode@ is 'DEPTH_CLAMP_MODE_USER_DEFINED_RANGE_EXT', then
--     @pDepthClampRange@ must be a valid pointer to a valid
--     'DepthClampRangeEXT' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthClampRangeEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthClampRangeEXT-depthClampMode-parameter#
--     @depthClampMode@ /must/ be a valid 'DepthClampModeEXT' value
--
-- -   #VUID-vkCmdSetDepthClampRangeEXT-pDepthClampRange-parameter# If
--     @pDepthClampRange@ is not @NULL@, @pDepthClampRange@ /must/ be a
--     valid pointer to a valid 'DepthClampRangeEXT' structure
--
-- -   #VUID-vkCmdSetDepthClampRangeEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthClampRangeEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetDepthClampRangeEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clamp_control VK_EXT_depth_clamp_control>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DepthClampModeEXT',
-- 'DepthClampRangeEXT'
cmdSetDepthClampRangeEXT :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @depthClampMode@ determines how the clamp range is determined for each
                            -- viewport.
                            DepthClampModeEXT
                         -> -- | @pDepthClampRange@ sets the depth clamp range for all viewports if
                            -- @depthClampMode@ is 'DEPTH_CLAMP_MODE_USER_DEFINED_RANGE_EXT'.
                            ("depthClampRange" ::: Maybe DepthClampRangeEXT)
                         -> io ()
cmdSetDepthClampRangeEXT commandBuffer
                           depthClampMode
                           depthClampRange = liftIO . evalContT $ do
  let vkCmdSetDepthClampRangeEXTPtr = pVkCmdSetDepthClampRangeEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetDepthClampRangeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthClampRangeEXT is null" Nothing Nothing
  let vkCmdSetDepthClampRangeEXT' = mkVkCmdSetDepthClampRangeEXT vkCmdSetDepthClampRangeEXTPtr
  pDepthClampRange <- case (depthClampRange) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkCmdSetDepthClampRangeEXT" (vkCmdSetDepthClampRangeEXT'
                                                          (commandBufferHandle (commandBuffer))
                                                          (depthClampMode)
                                                          pDepthClampRange)
  pure $ ()


-- | VkPhysicalDeviceDepthClampControlFeaturesEXT - Structure describing
-- additional depth clamp control supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDepthClampControlFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDepthClampControlFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clamp_control VK_EXT_depth_clamp_control>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDepthClampControlFeaturesEXT = PhysicalDeviceDepthClampControlFeaturesEXT
  { -- | #features-depthClampControl# @depthClampControl@ indicates that the
    -- implementation supports setting
    -- 'PipelineViewportDepthClampControlCreateInfoEXT'::@depthClampMode@ to
    -- 'DEPTH_CLAMP_MODE_USER_DEFINED_RANGE_EXT'.
    depthClampControl :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDepthClampControlFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceDepthClampControlFeaturesEXT

instance ToCStruct PhysicalDeviceDepthClampControlFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDepthClampControlFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (depthClampControl))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDepthClampControlFeaturesEXT where
  peekCStruct p = do
    depthClampControl <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDepthClampControlFeaturesEXT
             (bool32ToBool depthClampControl)

instance Storable PhysicalDeviceDepthClampControlFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDepthClampControlFeaturesEXT where
  zero = PhysicalDeviceDepthClampControlFeaturesEXT
           zero


-- | VkPipelineViewportDepthClampControlCreateInfoEXT - Structure specifying
-- parameters of a newly created pipeline depth clamp control state
--
-- = Description
--
-- This structure extends
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo' and specifies
-- the depth clamp range used in the pipeline. If this structure is not
-- provided in the next chain then @depthClampMode@ defaults to
-- 'DEPTH_CLAMP_MODE_VIEWPORT_RANGE_EXT'.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineViewportDepthClampControlCreateInfoEXT-pDepthClampRange-09646#
--     If @depthClampMode@ is 'DEPTH_CLAMP_MODE_USER_DEFINED_RANGE_EXT',
--     and the pipeline is not created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_RANGE_EXT',
--     then @pDepthClampRange@ /must/ be a valid pointer to a valid
--     'DepthClampRangeEXT' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineViewportDepthClampControlCreateInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLAMP_CONTROL_CREATE_INFO_EXT'
--
-- -   #VUID-VkPipelineViewportDepthClampControlCreateInfoEXT-depthClampMode-parameter#
--     @depthClampMode@ /must/ be a valid 'DepthClampModeEXT' value
--
-- -   #VUID-VkPipelineViewportDepthClampControlCreateInfoEXT-pDepthClampRange-parameter#
--     If @pDepthClampRange@ is not @NULL@, @pDepthClampRange@ /must/ be a
--     valid pointer to a valid 'DepthClampRangeEXT' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clamp_control VK_EXT_depth_clamp_control>,
-- 'DepthClampModeEXT', 'DepthClampRangeEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineViewportDepthClampControlCreateInfoEXT = PipelineViewportDepthClampControlCreateInfoEXT
  { -- | @depthClampMode@ determines how the clamp range is determined for each
    -- viewport.
    depthClampMode :: DepthClampModeEXT
  , -- | @pDepthClampRange@ sets the depth clamp range for all viewports if
    -- @depthClampMode@ is 'DEPTH_CLAMP_MODE_USER_DEFINED_RANGE_EXT'.
    depthClampRange :: Maybe DepthClampRangeEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineViewportDepthClampControlCreateInfoEXT)
#endif
deriving instance Show PipelineViewportDepthClampControlCreateInfoEXT

instance ToCStruct PipelineViewportDepthClampControlCreateInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportDepthClampControlCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLAMP_CONTROL_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DepthClampModeEXT)) (depthClampMode)
    pDepthClampRange'' <- case (depthClampRange) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DepthClampRangeEXT))) pDepthClampRange''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLAMP_CONTROL_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DepthClampModeEXT)) (zero)
    f

instance FromCStruct PipelineViewportDepthClampControlCreateInfoEXT where
  peekCStruct p = do
    depthClampMode <- peek @DepthClampModeEXT ((p `plusPtr` 16 :: Ptr DepthClampModeEXT))
    pDepthClampRange <- peek @(Ptr DepthClampRangeEXT) ((p `plusPtr` 24 :: Ptr (Ptr DepthClampRangeEXT)))
    pDepthClampRange' <- maybePeek (\j -> peekCStruct @DepthClampRangeEXT (j)) pDepthClampRange
    pure $ PipelineViewportDepthClampControlCreateInfoEXT
             depthClampMode pDepthClampRange'

instance Zero PipelineViewportDepthClampControlCreateInfoEXT where
  zero = PipelineViewportDepthClampControlCreateInfoEXT
           zero
           Nothing


-- | VkDepthClampRangeEXT - Structure specifying a depth clamp range
--
-- == Valid Usage
--
-- -   #VUID-VkDepthClampRangeEXT-pDepthClampRange-00999# @minDepthClamp@
--     /must/ be less than or equal to @maxDepthClamp@
--
-- -   #VUID-VkDepthClampRangeEXT-pDepthClampRange-09648# If the
--     @VK_EXT_depth_range_unrestricted@ extension is not enabled,
--     @minDepthClamp@ /must/ be greater than or equal to @0.0@
--
-- -   #VUID-VkDepthClampRangeEXT-pDepthClampRange-09649# If the
--     @VK_EXT_depth_range_unrestricted@ extension is not enabled,
--     @maxDepthClamp@ /must/ be less than or equal to @1.0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clamp_control VK_EXT_depth_clamp_control>,
-- 'PipelineViewportDepthClampControlCreateInfoEXT',
-- 'cmdSetDepthClampRangeEXT'
data DepthClampRangeEXT = DepthClampRangeEXT
  { -- | @minDepthClamp@ sets zmin in the depth clamp range of the viewport.
    minDepthClamp :: Float
  , -- | @maxDepthClamp@ sets zmax in the depth clamp range of the viewport.
    maxDepthClamp :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DepthClampRangeEXT)
#endif
deriving instance Show DepthClampRangeEXT

instance ToCStruct DepthClampRangeEXT where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DepthClampRangeEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (minDepthClamp))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (maxDepthClamp))
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct DepthClampRangeEXT where
  peekCStruct p = do
    minDepthClamp <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    maxDepthClamp <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    pure $ DepthClampRangeEXT
             (coerce @CFloat @Float minDepthClamp)
             (coerce @CFloat @Float maxDepthClamp)

instance Storable DepthClampRangeEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DepthClampRangeEXT where
  zero = DepthClampRangeEXT
           zero
           zero


-- | VkDepthClampModeEXT - Modes that determine the depth clamp range
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clamp_control VK_EXT_depth_clamp_control>,
-- 'PipelineViewportDepthClampControlCreateInfoEXT',
-- 'cmdSetDepthClampRangeEXT'
newtype DepthClampModeEXT = DepthClampModeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DEPTH_CLAMP_MODE_VIEWPORT_RANGE_EXT' specifies that the depth clamp
-- range follows the viewport depth range. The depth clamp range of each
-- viewport will implicitly be set to zmin = min(n,f) and zmax = max(n,f),
-- where n and f are the @minDepth@ and @maxDepth@ depth range values of
-- the viewport.
pattern DEPTH_CLAMP_MODE_VIEWPORT_RANGE_EXT = DepthClampModeEXT 0

-- | 'DEPTH_CLAMP_MODE_USER_DEFINED_RANGE_EXT' specifies that a single
-- user-defined depth clamp range will be used for all viewports. The
-- user-defined depth clamp range is defined by the @minDepthClamp@ and
-- @maxDepthClamp@ members of 'DepthClampRangeEXT'.
pattern DEPTH_CLAMP_MODE_USER_DEFINED_RANGE_EXT = DepthClampModeEXT 1

{-# COMPLETE
  DEPTH_CLAMP_MODE_VIEWPORT_RANGE_EXT
  , DEPTH_CLAMP_MODE_USER_DEFINED_RANGE_EXT ::
    DepthClampModeEXT
  #-}

conNameDepthClampModeEXT :: String
conNameDepthClampModeEXT = "DepthClampModeEXT"

enumPrefixDepthClampModeEXT :: String
enumPrefixDepthClampModeEXT = "DEPTH_CLAMP_MODE_"

showTableDepthClampModeEXT :: [(DepthClampModeEXT, String)]
showTableDepthClampModeEXT =
  [
    ( DEPTH_CLAMP_MODE_VIEWPORT_RANGE_EXT
    , "VIEWPORT_RANGE_EXT"
    )
  ,
    ( DEPTH_CLAMP_MODE_USER_DEFINED_RANGE_EXT
    , "USER_DEFINED_RANGE_EXT"
    )
  ]

instance Show DepthClampModeEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixDepthClampModeEXT
      showTableDepthClampModeEXT
      conNameDepthClampModeEXT
      (\(DepthClampModeEXT x) -> x)
      (showsPrec 11)

instance Read DepthClampModeEXT where
  readPrec =
    enumReadPrec
      enumPrefixDepthClampModeEXT
      showTableDepthClampModeEXT
      conNameDepthClampModeEXT
      DepthClampModeEXT

type EXT_DEPTH_CLAMP_CONTROL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLAMP_CONTROL_SPEC_VERSION"
pattern EXT_DEPTH_CLAMP_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEPTH_CLAMP_CONTROL_SPEC_VERSION = 1


type EXT_DEPTH_CLAMP_CONTROL_EXTENSION_NAME = "VK_EXT_depth_clamp_control"

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLAMP_CONTROL_EXTENSION_NAME"
pattern EXT_DEPTH_CLAMP_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEPTH_CLAMP_CONTROL_EXTENSION_NAME = "VK_EXT_depth_clamp_control"

