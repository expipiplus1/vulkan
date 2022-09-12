{-# language CPP #-}
-- | = Name
--
-- VK_EXT_depth_clip_control - device extension
--
-- == VK_EXT_depth_clip_control
--
-- [__Name String__]
--     @VK_EXT_depth_clip_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     356
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
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_depth_clip_control] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_depth_clip_control extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-11-09
--
-- [__Contributors__]
--
--     -   Spencer Fricke, Samsung Electronics
--
--     -   Shahbaz Youssefi, Google
--
--     -   Ralph Potter, Samsung Electronics
--
-- == Description
--
-- This extension allows the application to use the OpenGL depth range in
-- NDC, i.e. with depth in range [-1, 1], as opposed to Vulkan’s default of
-- [0, 1]. The purpose of this extension is to allow efficient layering of
-- OpenGL over Vulkan, by avoiding emulation in the pre-rasterization
-- shader stages. This emulation, which effectively duplicates gl_Position
-- but with a different depth value, costs ALU and consumes shader output
-- components that the implementation may not have to spare to meet OpenGL
-- minimum requirements.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDepthClipControlFeaturesEXT'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo':
--
--     -   'PipelineViewportDepthClipControlCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEPTH_CLIP_CONTROL_EXTENSION_NAME'
--
-- -   'EXT_DEPTH_CLIP_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_CONTROL_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLIP_CONTROL_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) Should this extension include an origin control option to match
-- GL_LOWER_LEFT found in ARB_clip_control?
--
-- __RESOLVED__: No. The fix for porting over the origin is a simple y-axis
-- flip. The depth clip control is a much harder problem to solve than what
-- this extension is aimed to solve. Adding an equivalent to GL_LOWER_LEFT
-- would require more testing.
--
-- 2) Should this pipeline state be dynamic?
--
-- __RESOLVED__: Yes. The purpose of this extension is to emulate the
-- OpenGL depth range, which is expected to be globally fixed (in case of
-- OpenGL ES) or very infrequently changed (with @glClipControl@ in
-- OpenGL).
--
-- 3) Should the control provided in this extension be an enum that could
-- be extended in the future?
--
-- __RESOLVED__: No. It is highly unlikely that the depth range is changed
-- to anything other than [0, 1] in the future. Should that happen a new
-- extension will be required to extend such an enum, and that extension
-- might as well add a new struct to chain to
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@pNext@
-- instead.
--
-- == Version History
--
-- -   Revision 0, 2020-10-01 (Spencer Fricke)
--
--     -   Internal revisions
--
-- -   Revision 1, 2020-11-26 (Shahbaz Youssefi)
--
--     -   Language fixes
--
-- == See Also
--
-- 'PhysicalDeviceDepthClipControlFeaturesEXT',
-- 'PipelineViewportDepthClipControlCreateInfoEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_depth_clip_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_depth_clip_control  ( PhysicalDeviceDepthClipControlFeaturesEXT(..)
                                                    , PipelineViewportDepthClipControlCreateInfoEXT(..)
                                                    , EXT_DEPTH_CLIP_CONTROL_SPEC_VERSION
                                                    , pattern EXT_DEPTH_CLIP_CONTROL_SPEC_VERSION
                                                    , EXT_DEPTH_CLIP_CONTROL_EXTENSION_NAME
                                                    , pattern EXT_DEPTH_CLIP_CONTROL_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_CONTROL_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLIP_CONTROL_CREATE_INFO_EXT))
-- | VkPhysicalDeviceDepthClipControlFeaturesEXT - Structure describing
-- additional depth clip control supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDepthClipControlFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDepthClipControlFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clip_control VK_EXT_depth_clip_control>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDepthClipControlFeaturesEXT = PhysicalDeviceDepthClipControlFeaturesEXT
  { -- | #features-depthClipControl# @depthClipControl@ indicates that the
    -- implementation supports setting
    -- 'PipelineViewportDepthClipControlCreateInfoEXT'::@negativeOneToOne@ to
    -- 'Vulkan.Core10.FundamentalTypes.TRUE'.
    depthClipControl :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDepthClipControlFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceDepthClipControlFeaturesEXT

instance ToCStruct PhysicalDeviceDepthClipControlFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDepthClipControlFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (depthClipControl))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDepthClipControlFeaturesEXT where
  peekCStruct p = do
    depthClipControl <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDepthClipControlFeaturesEXT
             (bool32ToBool depthClipControl)

instance Storable PhysicalDeviceDepthClipControlFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDepthClipControlFeaturesEXT where
  zero = PhysicalDeviceDepthClipControlFeaturesEXT
           zero


-- | VkPipelineViewportDepthClipControlCreateInfoEXT - Structure specifying
-- parameters of a newly created pipeline depth clip control state
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineViewportDepthClipControlCreateInfoEXT-negativeOneToOne-06470#
--     If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-depthClipControl depthClipControl>
--     is not enabled, @negativeOneToOne@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineViewportDepthClipControlCreateInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLIP_CONTROL_CREATE_INFO_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clip_control VK_EXT_depth_clip_control>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineViewportDepthClipControlCreateInfoEXT = PipelineViewportDepthClipControlCreateInfoEXT
  { -- | @negativeOneToOne@ sets the zm in the /view volume/ to -wc
    negativeOneToOne :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineViewportDepthClipControlCreateInfoEXT)
#endif
deriving instance Show PipelineViewportDepthClipControlCreateInfoEXT

instance ToCStruct PipelineViewportDepthClipControlCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportDepthClipControlCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLIP_CONTROL_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (negativeOneToOne))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLIP_CONTROL_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PipelineViewportDepthClipControlCreateInfoEXT where
  peekCStruct p = do
    negativeOneToOne <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PipelineViewportDepthClipControlCreateInfoEXT
             (bool32ToBool negativeOneToOne)

instance Storable PipelineViewportDepthClipControlCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineViewportDepthClipControlCreateInfoEXT where
  zero = PipelineViewportDepthClipControlCreateInfoEXT
           zero


type EXT_DEPTH_CLIP_CONTROL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLIP_CONTROL_SPEC_VERSION"
pattern EXT_DEPTH_CLIP_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEPTH_CLIP_CONTROL_SPEC_VERSION = 1


type EXT_DEPTH_CLIP_CONTROL_EXTENSION_NAME = "VK_EXT_depth_clip_control"

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLIP_CONTROL_EXTENSION_NAME"
pattern EXT_DEPTH_CLIP_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEPTH_CLIP_CONTROL_EXTENSION_NAME = "VK_EXT_depth_clip_control"

