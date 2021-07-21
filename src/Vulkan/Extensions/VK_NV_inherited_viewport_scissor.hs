{-# language CPP #-}
-- | = Name
--
-- VK_NV_inherited_viewport_scissor - device extension
--
-- == VK_NV_inherited_viewport_scissor
--
-- [__Name String__]
--     @VK_NV_inherited_viewport_scissor@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     279
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   David Zhao Akeley
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_inherited_viewport_scissor:%20&body=@akeley98%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-02-04
--
-- [__Contributors__]
--
--     -   David Zhao Akeley, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
-- == Description
--
-- This extension adds the ability for a secondary command buffer to
-- inherit the dynamic viewport and scissor state from a primary command
-- buffer, or a previous secondary command buffer executed within the same
-- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' call. It
-- addresses a frequent scenario in applications that deal with window
-- resizing and want to improve utilization of re-usable secondary command
-- buffers. The functionality is provided through
-- 'CommandBufferInheritanceViewportScissorInfoNV'. Viewport inheritance is
-- effectively limited to the 2D rectangle; secondary command buffers must
-- re-specify the inherited depth range values.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'CommandBufferInheritanceViewportScissorInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceInheritedViewportScissorFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_INHERITED_VIEWPORT_SCISSOR_EXTENSION_NAME'
--
-- -   'NV_INHERITED_VIEWPORT_SCISSOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_VIEWPORT_SCISSOR_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_INHERITED_VIEWPORT_SCISSOR_FEATURES_NV'
--
-- == Issues
--
-- (1) Why are viewport depth values configured in the
-- 'CommandBufferInheritanceViewportScissorInfoNV' struct, rather than by a
-- @vkCmd…​@ function?
--
-- __DISCUSSION__:
--
-- We considered both adding a new @vkCmdSetViewportDepthNV@ function, and
-- modifying 'Vulkan.Core10.CommandBufferBuilding.cmdSetViewport' to ignore
-- the @x@, @y@, @width@, and @height@ values when called with a secondary
-- command buffer that activates this extension.
--
-- The primary design considerations for this extension are debuggability
-- and easy integration into existing applications. The main issue with
-- adding a new @vkCmdSetViewportDepthNV@ function is reducing
-- ease-of-integration. A new function pointer will have to be loaded, but
-- more importantly, a new function would require changes to be supported
-- in graphics debuggers; this would delay widespread adoption of the
-- extension.
--
-- The proposal to modify
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetViewport' would avoid these
-- issues. However, we expect that the intent of applications using this
-- extension is to have the viewport values used for drawing exactly match
-- the inherited values; thus, it would be better for debuggability if no
-- function for modifying the viewport depth alone is provided. By
-- specifying viewport depth values when starting secondary command buffer
-- recording, and requiring the specified depth values to match the
-- inherited depth values, we allow for validation layers that flag depth
-- changes as errors.
--
-- This design also better matches the hardware model. In fact, there is no
-- need to re-execute a depth-setting command. The graphics device retains
-- the viewport depth state; it is the CPU-side state of
-- 'Vulkan.Core10.Handles.CommandBuffer' that must be re-initialized.
--
-- (2) Why are viewport depth values specified as a partial
-- 'Vulkan.Core10.Pipeline.Viewport' struct, rather than a leaner
-- depth-only struct?
--
-- __DISCUSSION__:
--
-- We considered adding a new @VkViewportDepthNV@ struct containing only
-- @minDepth@ and @maxDepth@. However, as application developers would need
-- to maintain both a @VK_NV_inherited_viewport_scissor@ code path and a
-- fallback code path (at least in the short term), we ultimately chose to
-- continue using the existing 'Vulkan.Core10.Pipeline.Viewport' structure.
-- Doing so would allow application developers to reuse the same
-- 'Vulkan.Core10.Pipeline.Viewport' array for both code paths, rather than
-- constructing separate @VkViewportDepthNV@ and
-- 'Vulkan.Core10.Pipeline.Viewport' arrays for each code path.
--
-- == Version History
--
-- -   Revision 1, 2020-02-04 (David Zhao Akeley)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'CommandBufferInheritanceViewportScissorInfoNV',
-- 'PhysicalDeviceInheritedViewportScissorFeaturesNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_inherited_viewport_scissor Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_inherited_viewport_scissor  ( PhysicalDeviceInheritedViewportScissorFeaturesNV(..)
                                                           , CommandBufferInheritanceViewportScissorInfoNV(..)
                                                           , NV_INHERITED_VIEWPORT_SCISSOR_SPEC_VERSION
                                                           , pattern NV_INHERITED_VIEWPORT_SCISSOR_SPEC_VERSION
                                                           , NV_INHERITED_VIEWPORT_SCISSOR_EXTENSION_NAME
                                                           , pattern NV_INHERITED_VIEWPORT_SCISSOR_EXTENSION_NAME
                                                           ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Pipeline (Viewport)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_VIEWPORT_SCISSOR_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INHERITED_VIEWPORT_SCISSOR_FEATURES_NV))
-- | VkPhysicalDeviceInheritedViewportScissorFeaturesNV - Structure
-- describing the viewport scissor inheritance behavior for an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceInheritedViewportScissorFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceInheritedViewportScissorFeaturesNV' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceInheritedViewportScissorFeaturesNV = PhysicalDeviceInheritedViewportScissorFeaturesNV
  { -- | #features-inheritedViewportScissor2D# @inheritedViewportScissor2D@
    -- indicates whether secondary command buffers can inherit most of the
    -- dynamic state affected by
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT' or
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR', from a primary
    -- command buffer.
    inheritedViewportScissor2D :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceInheritedViewportScissorFeaturesNV)
#endif
deriving instance Show PhysicalDeviceInheritedViewportScissorFeaturesNV

instance ToCStruct PhysicalDeviceInheritedViewportScissorFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceInheritedViewportScissorFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INHERITED_VIEWPORT_SCISSOR_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (inheritedViewportScissor2D))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INHERITED_VIEWPORT_SCISSOR_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceInheritedViewportScissorFeaturesNV where
  peekCStruct p = do
    inheritedViewportScissor2D <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceInheritedViewportScissorFeaturesNV
             (bool32ToBool inheritedViewportScissor2D)

instance Storable PhysicalDeviceInheritedViewportScissorFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceInheritedViewportScissorFeaturesNV where
  zero = PhysicalDeviceInheritedViewportScissorFeaturesNV
           zero


-- | VkCommandBufferInheritanceViewportScissorInfoNV - Structure specifying
-- command buffer inheritance information
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' includes a
-- 'CommandBufferInheritanceViewportScissorInfoNV' structure, then that
-- structure controls whether a command buffer /can/ inherit the following
-- state from other command buffers:
--
-- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR'
--
-- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--
-- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT'
--
-- as well as the following state, with restrictions on inherited depth
-- values and viewport count:
--
-- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT'
--
-- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--
-- If @viewportScissor2D@ is 'Vulkan.Core10.FundamentalTypes.FALSE', then
-- the command buffer does not inherit the listed dynamic state, and
-- /should/ set this state itself. If this structure is not present, the
-- behavior is as if @viewportScissor2D@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- If @viewportScissor2D@ is 'Vulkan.Core10.FundamentalTypes.TRUE', then
-- the listed dynamic state is inherited, and the command buffer /must/ not
-- set this state, except that the viewport and scissor count /may/ be set
-- by binding a graphics pipeline that does not specify this state as
-- dynamic.
--
-- Note
--
-- Due to this restriction, applications /should/ ensure either all or none
-- of the graphics pipelines bound in this secondary command buffer use
-- dynamic viewport\/scissor counts.
--
-- When the command buffer is executed as part of a the execution of a
-- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' command, the
-- inherited state (if enabled) is determined by the following procedure,
-- performed separately for each dynamic state, and separately for each
-- value for dynamic state that consists of multiple values (e.g. multiple
-- viewports).
--
-- -   With \(i\) being the index of the executed command buffer in the
--     @pCommandBuffers@ array of
--     'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands', if
--     \(i > 0\) and any secondary command buffer from index \(0\) to
--     \(i-1\) modifies the state, the inherited state is provisionally set
--     to the final value set by the last such secondary command buffer.
--     Binding a graphics pipeline that defines the state statically is
--     equivalent to setting the state to an undefined value.
--
-- -   Otherwise, the tentatative inherited state is that of the primary
--     command buffer at the point the
--     'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' command was
--     recorded; if the state is undefined, then so is the provisional
--     inherited state.
--
-- -   If the provisional inherited state is an undefined value, then the
--     state is not inherited.
--
-- -   If the provisional inherited state is a viewport, with \(n\) being
--     its viewport index, then if \(n \ge\) @viewportDepthCount@, or if
--     either 'Vulkan.Core10.Pipeline.Viewport'::@minDepth@ or
--     'Vulkan.Core10.Pipeline.Viewport'::@maxDepth@ are not equal to the
--     respective values of the \(n^{th}\) element of @pViewportDepths@,
--     then the state is not inherited.
--
-- -   If the provisional inherited state passes both checks, then it
--     becomes the actual inherited state.
--
-- Note
--
-- There is no support for inheriting dynamic state from a secondary
-- command buffer executed as part of a different
-- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' command.
--
-- == Valid Usage
--
-- -   #VUID-VkCommandBufferInheritanceViewportScissorInfoNV-viewportScissor2D-04782#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-inheritedViewportScissor2D inherited viewport scissor>
--     feature is not enabled, @viewportScissor2D@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkCommandBufferInheritanceViewportScissorInfoNV-viewportScissor2D-04783#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled and @viewportScissor2D@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', then @viewportDepthCount@
--     /must/ be @1@
--
-- -   #VUID-VkCommandBufferInheritanceViewportScissorInfoNV-viewportScissor2D-04784#
--     If @viewportScissor2D@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then @viewportDepthCount@ /must/ be greater than @0@
--
-- -   #VUID-VkCommandBufferInheritanceViewportScissorInfoNV-viewportScissor2D-04785#
--     If @viewportScissor2D@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then @pViewportDepths@ /must/ be a valid pointer to an array of
--     @viewportDepthCount@ valid 'Vulkan.Core10.Pipeline.Viewport'
--     structures, except any requirements on @x@, @y@, @width@, and
--     @height@ do not apply.
--
-- -   #VUID-VkCommandBufferInheritanceViewportScissorInfoNV-viewportScissor2D-04786#
--     If @viewportScissor2D@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then the command buffer /must/ be recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCommandBufferInheritanceViewportScissorInfoNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_VIEWPORT_SCISSOR_INFO_NV'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.Pipeline.Viewport'
data CommandBufferInheritanceViewportScissorInfoNV = CommandBufferInheritanceViewportScissorInfoNV
  { -- | @viewportScissor2D@ specifies whether the listed dynamic state is
    -- inherited.
    viewportScissor2D :: Bool
  , -- | @viewportDepthCount@ specifies the maximum number of viewports to
    -- inherit. When @viewportScissor2D@ is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', the behavior is as if this value
    -- is zero.
    viewportDepthCount :: Word32
  , -- | @pViewportDepths@ is a pointer to a 'Vulkan.Core10.Pipeline.Viewport'
    -- structure specifying the expected depth range for each inherited
    -- viewport.
    viewportDepths :: Viewport
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandBufferInheritanceViewportScissorInfoNV)
#endif
deriving instance Show CommandBufferInheritanceViewportScissorInfoNV

instance ToCStruct CommandBufferInheritanceViewportScissorInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferInheritanceViewportScissorInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_VIEWPORT_SCISSOR_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (viewportScissor2D))
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (viewportDepthCount)
    pViewportDepths'' <- ContT $ withCStruct (viewportDepths)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Viewport))) pViewportDepths''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_VIEWPORT_SCISSOR_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    pViewportDepths'' <- ContT $ withCStruct (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Viewport))) pViewportDepths''
    lift $ f

instance FromCStruct CommandBufferInheritanceViewportScissorInfoNV where
  peekCStruct p = do
    viewportScissor2D <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    viewportDepthCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pViewportDepths <- peekCStruct @Viewport =<< peek ((p `plusPtr` 24 :: Ptr (Ptr Viewport)))
    pure $ CommandBufferInheritanceViewportScissorInfoNV
             (bool32ToBool viewportScissor2D) viewportDepthCount pViewportDepths

instance Zero CommandBufferInheritanceViewportScissorInfoNV where
  zero = CommandBufferInheritanceViewportScissorInfoNV
           zero
           zero
           zero


type NV_INHERITED_VIEWPORT_SCISSOR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_INHERITED_VIEWPORT_SCISSOR_SPEC_VERSION"
pattern NV_INHERITED_VIEWPORT_SCISSOR_SPEC_VERSION :: forall a . Integral a => a
pattern NV_INHERITED_VIEWPORT_SCISSOR_SPEC_VERSION = 1


type NV_INHERITED_VIEWPORT_SCISSOR_EXTENSION_NAME = "VK_NV_inherited_viewport_scissor"

-- No documentation found for TopLevel "VK_NV_INHERITED_VIEWPORT_SCISSOR_EXTENSION_NAME"
pattern NV_INHERITED_VIEWPORT_SCISSOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_INHERITED_VIEWPORT_SCISSOR_EXTENSION_NAME = "VK_NV_inherited_viewport_scissor"

