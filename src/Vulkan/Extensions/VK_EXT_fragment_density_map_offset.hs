{-# language CPP #-}
-- | = Name
--
-- VK_EXT_fragment_density_map_offset - device extension
--
-- = VK_EXT_fragment_density_map_offset
--
-- [__Name String__]
--     @VK_EXT_fragment_density_map_offset@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     620
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
--
-- [__Contact__]
--
--     -   Connor Abbott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_fragment_density_map_offset] @cwabbott0%0A*Here describe the issue or question you have about the VK_EXT_fragment_density_map_offset extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_fragment_density_map_offset.adoc VK_EXT_fragment_density_map_offset>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-02-14
--
-- [__Contributors__]
--
--     -   Connor Abbott, Valve Corporation
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Jonathan Wicks, Qualcomm Technologies, Inc.
--
--     -   Jonathan Tinkham, Qualcomm Technologies, Inc.
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
--     -   Manan Katwala, Qualcomm Technologies, Inc.
--
--     -   Mike Blumenkrantz, Valve Corporation
--
-- == Description
--
-- This extension allows an application to specify offsets to a fragment
-- density map attachment, changing the framebuffer location where density
-- values are applied to without having to regenerate the fragment density
-- map.
--
-- == New Commands
--
-- -   'cmdEndRendering2EXT'
--
-- == New Structures
--
-- -   'RenderingEndInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassEndInfo',
--     'Vulkan.Extensions.VK_KHR_maintenance10.RenderingEndInfoKHR':
--
--     -   'RenderPassFragmentDensityMapOffsetEndInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME'
--
-- -   'EXT_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_EXT'
--
--     -   'STRUCTURE_TYPE_RENDERING_END_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2025-02-14 (Connor Abbott)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_fragment_density_map_offset Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_fragment_density_map_offset  ( pattern STRUCTURE_TYPE_RENDERING_END_INFO_EXT
                                                             , cmdEndRendering2EXT
                                                             , PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT(..)
                                                             , PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT(..)
                                                             , RenderPassFragmentDensityMapOffsetEndInfoEXT(..)
                                                             , RenderingEndInfoEXT
                                                             , EXT_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION
                                                             , pattern EXT_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION
                                                             , EXT_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME
                                                             , pattern EXT_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME
                                                             , RenderingEndInfoKHR(..)
                                                             , cmdEndRendering2KHR
                                                             ) where

import Foreign.Marshal.Alloc (allocaBytes)
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
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Extensions.VK_KHR_maintenance10 (cmdEndRendering2KHR)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Offset2D)
import Vulkan.Extensions.VK_KHR_maintenance10 (RenderingEndInfoKHR)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_END_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_EXT))
import Vulkan.Extensions.VK_KHR_maintenance10 (cmdEndRendering2KHR)
import Vulkan.Extensions.VK_KHR_maintenance10 (RenderingEndInfoKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_RENDERING_END_INFO_EXT"
pattern STRUCTURE_TYPE_RENDERING_END_INFO_EXT = STRUCTURE_TYPE_RENDERING_END_INFO_KHR


-- No documentation found for TopLevel "vkCmdEndRendering2EXT"
cmdEndRendering2EXT = cmdEndRendering2KHR


-- | VkPhysicalDeviceFragmentDensityMapOffsetFeaturesEXT - Structure
-- describing fragment density map offset features that can be supported by
-- an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map_offset VK_EXT_fragment_density_map_offset>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_fragment_density_map_offset VK_QCOM_fragment_density_map_offset>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT = PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT
  { -- | #features-fragmentDensityMapOffset# @fragmentDensityMapOffset@ specifies
    -- whether the implementation supports
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-fragmentdensitymapoffsets fragment density map offsets>
    fragmentDensityMapOffset :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fragmentDensityMapOffset))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT where
  peekCStruct p = do
    fragmentDensityMapOffset <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT
             (bool32ToBool fragmentDensityMapOffset)

instance Storable PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT where
  zero = PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT
           zero


-- | VkPhysicalDeviceFragmentDensityMapOffsetPropertiesEXT - Structure
-- describing fragment density map offset properties that can be supported
-- by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map_offset VK_EXT_fragment_density_map_offset>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_fragment_density_map_offset VK_QCOM_fragment_density_map_offset>,
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT = PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT
  { -- | #limits-fragmentdensityoffsetgranularity#
    -- @fragmentDensityOffsetGranularity@ is the granularity for
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-fragmentdensitymapoffsets fragment density offsets>.
    fragmentDensityOffsetGranularity :: Extent2D }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (fragmentDensityOffsetGranularity)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT where
  peekCStruct p = do
    fragmentDensityOffsetGranularity <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    pure $ PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT
             fragmentDensityOffsetGranularity

instance Storable PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT where
  zero = PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT
           zero


-- | VkRenderPassFragmentDensityMapOffsetEndInfoEXT - Structure specifying
-- fragment density map offset subpass end information
--
-- = Description
--
-- The array elements are given per @layer@ as defined by
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragmentdensitymap-fetch-density-value Fetch Density Value>,
-- where index = layer. Each (x,y) offset is in framebuffer pixels and
-- shifts the fetch of the fragment density map by that amount. Offsets can
-- be positive or negative.
--
-- If neither the
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassEndInfo'::@pNext@
-- chain for the last subpass of a render pass nor the
-- 'RenderingEndInfoEXT'::@pNext@ chain of a dynamic render pass include
-- 'RenderPassFragmentDensityMapOffsetEndInfoEXT', or if
-- @fragmentDensityOffsetCount@ is zero, then the offset (0,0) is used for
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragmentdensitymap-fetch-density-value Fetch Density Value>.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-fragmentDensityMapOffsets-06503#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-fragmentDensityMapOffset fragmentDensityMapOffset>
--     feature is not enabled or fragment density map is not enabled in the
--     render pass, @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-fragmentDensityMapAttachment-06504#
--     If
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.RenderPassFragmentDensityMapCreateInfoEXT'::@fragmentDensityMapAttachment@
--     is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and was not
--     created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT',
--     @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-pDepthStencilAttachment-06505#
--     If the depth or stencil attachments for the render pass are used and
--     were not created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT',
--     @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-pInputAttachments-06506#
--     If any used input attachments for the render pass were not created
--     with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT',
--     @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-pColorAttachments-06507#
--     If any used color attachments for the render pass were not created
--     with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT',
--     @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-pResolveAttachments-06508#
--     If any used resolve attachments for the render pass were not created
--     with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT',
--     @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-pPreserveAttachments-06509#
--     If any used preserve attachments for the render pass were not
--     created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT',
--     @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-fragmentDensityOffsetCount-06510#
--     If @fragmentDensityOffsetCount@ is not @0@ and multiview is enabled
--     for the render pass, @fragmentDensityOffsetCount@ /must/ equal the
--     @layerCount@ that was specified in creating the fragment density map
--     attachment view
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-fragmentDensityOffsetCount-06511#
--     If @fragmentDensityOffsetCount@ is not @0@ and multiview is not
--     enabled for the render pass, @fragmentDensityOffsetCount@ /must/
--     equal @1@
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-x-06512# The
--     @x@ component of each element of @pFragmentDensityOffsets@ /must/ be
--     an integer multiple of @fragmentDensityOffsetGranularity.width@
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-y-06513# The
--     @y@ component of each element of @pFragmentDensityOffsets@ /must/ be
--     an integer multiple of @fragmentDensityOffsetGranularity.height@
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-pFragmentDensityOffsets-10730#
--     Each element of @pFragmentDensityOffsets@ must be identical for
--     every 'Vulkan.Extensions.VK_KHR_maintenance10.cmdEndRendering2KHR'
--     call made in a render pass
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_EXT'
--
-- -   #VUID-VkRenderPassFragmentDensityMapOffsetEndInfoEXT-pFragmentDensityOffsets-parameter#
--     If @fragmentDensityOffsetCount@ is not @0@,
--     @pFragmentDensityOffsets@ /must/ be a valid pointer to an array of
--     @fragmentDensityOffsetCount@
--     'Vulkan.Core10.FundamentalTypes.Offset2D' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map_offset VK_EXT_fragment_density_map_offset>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_fragment_density_map_offset VK_QCOM_fragment_density_map_offset>,
-- 'Vulkan.Core10.FundamentalTypes.Offset2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderPassFragmentDensityMapOffsetEndInfoEXT = RenderPassFragmentDensityMapOffsetEndInfoEXT
  { -- | @pFragmentDensityOffsets@ is a pointer to an array of
    -- 'Vulkan.Core10.FundamentalTypes.Offset2D' structs, each of which
    -- describes the offset per layer.
    fragmentDensityOffsets :: Vector Offset2D }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassFragmentDensityMapOffsetEndInfoEXT)
#endif
deriving instance Show RenderPassFragmentDensityMapOffsetEndInfoEXT

instance ToCStruct RenderPassFragmentDensityMapOffsetEndInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassFragmentDensityMapOffsetEndInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (fragmentDensityOffsets)) :: Word32))
    pPFragmentDensityOffsets' <- ContT $ allocaBytes @Offset2D ((Data.Vector.length (fragmentDensityOffsets)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPFragmentDensityOffsets' `plusPtr` (8 * (i)) :: Ptr Offset2D) (e)) (fragmentDensityOffsets)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Offset2D))) (pPFragmentDensityOffsets')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct RenderPassFragmentDensityMapOffsetEndInfoEXT where
  peekCStruct p = do
    fragmentDensityOffsetCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pFragmentDensityOffsets <- peek @(Ptr Offset2D) ((p `plusPtr` 24 :: Ptr (Ptr Offset2D)))
    pFragmentDensityOffsets' <- generateM (fromIntegral fragmentDensityOffsetCount) (\i -> peekCStruct @Offset2D ((pFragmentDensityOffsets `advancePtrBytes` (8 * (i)) :: Ptr Offset2D)))
    pure $ RenderPassFragmentDensityMapOffsetEndInfoEXT
             pFragmentDensityOffsets'

instance Zero RenderPassFragmentDensityMapOffsetEndInfoEXT where
  zero = RenderPassFragmentDensityMapOffsetEndInfoEXT
           mempty


-- No documentation found for TopLevel "VkRenderingEndInfoEXT"
type RenderingEndInfoEXT = RenderingEndInfoKHR


type EXT_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION"
pattern EXT_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION = 1


type EXT_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME = "VK_EXT_fragment_density_map_offset"

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME"
pattern EXT_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME = "VK_EXT_fragment_density_map_offset"

