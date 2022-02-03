{-# language CPP #-}
-- | = Name
--
-- VK_EXT_extended_dynamic_state - device extension
--
-- == VK_EXT_extended_dynamic_state
--
-- [__Name String__]
--     @VK_EXT_extended_dynamic_state@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     268
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
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_extended_dynamic_state] @pdaniell-nv%0A<<Here describe the issue or question you have about the VK_EXT_extended_dynamic_state extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-12-09
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Dan Ginsburg, Valve Corporation
--
--     -   Graeme Leese, Broadcom
--
--     -   Hans-Kristian Arntzen, Valve Corporation
--
--     -   Jan-Harald Fredriksen, Arm Limited
--
--     -   Jason Ekstrand, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jesse Hall, Google
--
--     -   Philip Rebohle, Valve Corporation
--
--     -   Stuart Smith, Imagination Technologies
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension adds some more dynamic state to support applications that
-- need to reduce the number of pipeline state objects they compile and
-- bind.
--
-- == New Commands
--
-- -   'cmdBindVertexBuffers2EXT'
--
-- -   'cmdSetCullModeEXT'
--
-- -   'cmdSetDepthBoundsTestEnableEXT'
--
-- -   'cmdSetDepthCompareOpEXT'
--
-- -   'cmdSetDepthTestEnableEXT'
--
-- -   'cmdSetDepthWriteEnableEXT'
--
-- -   'cmdSetFrontFaceEXT'
--
-- -   'cmdSetPrimitiveTopologyEXT'
--
-- -   'cmdSetScissorWithCountEXT'
--
-- -   'cmdSetStencilOpEXT'
--
-- -   'cmdSetStencilTestEnableEXT'
--
-- -   'cmdSetViewportWithCountEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExtendedDynamicStateFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME'
--
-- -   'EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'DYNAMIC_STATE_CULL_MODE_EXT'
--
--     -   'DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT'
--
--     -   'DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT'
--
--     -   'DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT'
--
--     -   'DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT'
--
--     -   'DYNAMIC_STATE_FRONT_FACE_EXT'
--
--     -   'DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--
--     -   'DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--
--     -   'DYNAMIC_STATE_STENCIL_OP_EXT'
--
--     -   'DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT'
--
--     -   'DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--
--     -   'DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- This extension has been partially promoted. All dynamic state enumerants
-- and entry points in this extension are included in core Vulkan 1.3, with
-- the EXT suffix omitted. The feature structure is not promoted. Extension
-- interfaces that were promoted remain available as aliases of the core
-- functionality.
--
-- == Version History
--
-- -   Revision 1, 2019-12-09 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceExtendedDynamicStateFeaturesEXT',
-- 'cmdBindVertexBuffers2EXT', 'cmdSetCullModeEXT',
-- 'cmdSetDepthBoundsTestEnableEXT', 'cmdSetDepthCompareOpEXT',
-- 'cmdSetDepthTestEnableEXT', 'cmdSetDepthWriteEnableEXT',
-- 'cmdSetFrontFaceEXT', 'cmdSetPrimitiveTopologyEXT',
-- 'cmdSetScissorWithCountEXT', 'cmdSetStencilOpEXT',
-- 'cmdSetStencilTestEnableEXT', 'cmdSetViewportWithCountEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_extended_dynamic_state  ( pattern DYNAMIC_STATE_CULL_MODE_EXT
                                                        , pattern DYNAMIC_STATE_FRONT_FACE_EXT
                                                        , pattern DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT
                                                        , pattern DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT
                                                        , pattern DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT
                                                        , pattern DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT
                                                        , pattern DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT
                                                        , pattern DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT
                                                        , pattern DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT
                                                        , pattern DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT
                                                        , pattern DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT
                                                        , pattern DYNAMIC_STATE_STENCIL_OP_EXT
                                                        , cmdSetCullModeEXT
                                                        , cmdSetFrontFaceEXT
                                                        , cmdSetPrimitiveTopologyEXT
                                                        , cmdSetViewportWithCountEXT
                                                        , cmdSetScissorWithCountEXT
                                                        , cmdBindVertexBuffers2EXT
                                                        , cmdSetDepthTestEnableEXT
                                                        , cmdSetDepthWriteEnableEXT
                                                        , cmdSetDepthCompareOpEXT
                                                        , cmdSetDepthBoundsTestEnableEXT
                                                        , cmdSetStencilTestEnableEXT
                                                        , cmdSetStencilOpEXT
                                                        , PhysicalDeviceExtendedDynamicStateFeaturesEXT(..)
                                                        , EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION
                                                        , pattern EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION
                                                        , EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME
                                                        , pattern EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME
                                                        ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state (cmdBindVertexBuffers2)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state (cmdSetCullMode)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state (cmdSetDepthBoundsTestEnable)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state (cmdSetDepthCompareOp)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state (cmdSetDepthTestEnable)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state (cmdSetDepthWriteEnable)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state (cmdSetFrontFace)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state (cmdSetPrimitiveTopology)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state (cmdSetScissorWithCount)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state (cmdSetStencilOp)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state (cmdSetStencilTestEnable)
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state (cmdSetViewportWithCount)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_CULL_MODE))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_DEPTH_COMPARE_OP))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_DEPTH_TEST_ENABLE))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_DEPTH_WRITE_ENABLE))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_FRONT_FACE))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_PRIMITIVE_TOPOLOGY))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_SCISSOR_WITH_COUNT))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_STENCIL_OP))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_STENCIL_TEST_ENABLE))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE))
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_VIEWPORT_WITH_COUNT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT))
-- No documentation found for TopLevel "VK_DYNAMIC_STATE_CULL_MODE_EXT"
pattern DYNAMIC_STATE_CULL_MODE_EXT = DYNAMIC_STATE_CULL_MODE


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_FRONT_FACE_EXT"
pattern DYNAMIC_STATE_FRONT_FACE_EXT = DYNAMIC_STATE_FRONT_FACE


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT"
pattern DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT = DYNAMIC_STATE_PRIMITIVE_TOPOLOGY


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT"
pattern DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT = DYNAMIC_STATE_VIEWPORT_WITH_COUNT


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT"
pattern DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT = DYNAMIC_STATE_SCISSOR_WITH_COUNT


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT"
pattern DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT = DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT"
pattern DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT = DYNAMIC_STATE_DEPTH_TEST_ENABLE


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT"
pattern DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT = DYNAMIC_STATE_DEPTH_WRITE_ENABLE


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT"
pattern DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT = DYNAMIC_STATE_DEPTH_COMPARE_OP


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT"
pattern DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT = DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT"
pattern DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT = DYNAMIC_STATE_STENCIL_TEST_ENABLE


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_STENCIL_OP_EXT"
pattern DYNAMIC_STATE_STENCIL_OP_EXT = DYNAMIC_STATE_STENCIL_OP


-- No documentation found for TopLevel "vkCmdSetCullModeEXT"
cmdSetCullModeEXT = cmdSetCullMode


-- No documentation found for TopLevel "vkCmdSetFrontFaceEXT"
cmdSetFrontFaceEXT = cmdSetFrontFace


-- No documentation found for TopLevel "vkCmdSetPrimitiveTopologyEXT"
cmdSetPrimitiveTopologyEXT = cmdSetPrimitiveTopology


-- No documentation found for TopLevel "vkCmdSetViewportWithCountEXT"
cmdSetViewportWithCountEXT = cmdSetViewportWithCount


-- No documentation found for TopLevel "vkCmdSetScissorWithCountEXT"
cmdSetScissorWithCountEXT = cmdSetScissorWithCount


-- No documentation found for TopLevel "vkCmdBindVertexBuffers2EXT"
cmdBindVertexBuffers2EXT = cmdBindVertexBuffers2


-- No documentation found for TopLevel "vkCmdSetDepthTestEnableEXT"
cmdSetDepthTestEnableEXT = cmdSetDepthTestEnable


-- No documentation found for TopLevel "vkCmdSetDepthWriteEnableEXT"
cmdSetDepthWriteEnableEXT = cmdSetDepthWriteEnable


-- No documentation found for TopLevel "vkCmdSetDepthCompareOpEXT"
cmdSetDepthCompareOpEXT = cmdSetDepthCompareOp


-- No documentation found for TopLevel "vkCmdSetDepthBoundsTestEnableEXT"
cmdSetDepthBoundsTestEnableEXT = cmdSetDepthBoundsTestEnable


-- No documentation found for TopLevel "vkCmdSetStencilTestEnableEXT"
cmdSetStencilTestEnableEXT = cmdSetStencilTestEnable


-- No documentation found for TopLevel "vkCmdSetStencilOpEXT"
cmdSetStencilOpEXT = cmdSetStencilOp


-- | VkPhysicalDeviceExtendedDynamicStateFeaturesEXT - Structure describing
-- what extended dynamic state can be used
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceExtendedDynamicStateFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceExtendedDynamicStateFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExtendedDynamicStateFeaturesEXT = PhysicalDeviceExtendedDynamicStateFeaturesEXT
  { -- | #features-extendedDynamicState# @extendedDynamicState@ indicates that
    -- the implementation supports the following dynamic states:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CULL_MODE'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRONT_FACE'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE'
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP'
    extendedDynamicState :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExtendedDynamicStateFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceExtendedDynamicStateFeaturesEXT

instance ToCStruct PhysicalDeviceExtendedDynamicStateFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExtendedDynamicStateFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExtendedDynamicStateFeaturesEXT where
  peekCStruct p = do
    extendedDynamicState <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceExtendedDynamicStateFeaturesEXT
             (bool32ToBool extendedDynamicState)

instance Storable PhysicalDeviceExtendedDynamicStateFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExtendedDynamicStateFeaturesEXT where
  zero = PhysicalDeviceExtendedDynamicStateFeaturesEXT
           zero


type EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION"
pattern EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION = 1


type EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME = "VK_EXT_extended_dynamic_state"

-- No documentation found for TopLevel "VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME"
pattern EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME = "VK_EXT_extended_dynamic_state"

