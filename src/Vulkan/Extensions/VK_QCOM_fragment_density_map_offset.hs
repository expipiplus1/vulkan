{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_fragment_density_map_offset - device extension
--
-- == VK_QCOM_fragment_density_map_offset
--
-- [__Name String__]
--     @VK_QCOM_fragment_density_map_offset@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     426
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
--     -   Requires @VK_EXT_fragment_density_map@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_fragment_density_map_offset] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_fragment_density_map_offset extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-09-03
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Jonathan Wicks, Qualcomm Technologies, Inc.
--
--     -   Jonathan Tinkham, Qualcomm Technologies, Inc.
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension allows an application to specify offsets to a fragment
-- density map attachment, changing the framebuffer location where density
-- values are applied to without having to regenerate the fragment density
-- map.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassEndInfo':
--
--     -   'SubpassFragmentDensityMapOffsetEndInfoQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME'
--
-- -   'QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2021-09-03 (Matthew Netsch)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM',
-- 'PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM',
-- 'SubpassFragmentDensityMapOffsetEndInfoQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_fragment_density_map_offset Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_fragment_density_map_offset  ( PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM(..)
                                                              , PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM(..)
                                                              , SubpassFragmentDensityMapOffsetEndInfoQCOM(..)
                                                              , QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION
                                                              , pattern QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION
                                                              , QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME
                                                              , pattern QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Offset2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_QCOM))
-- | VkPhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM - Structure
-- describing fragment density map offset features that can be supported by
-- an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_fragment_density_map_offset VK_QCOM_fragment_density_map_offset>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM = PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM" "fragmentDensityMapOffset"
    fragmentDensityMapOffset :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM

instance ToCStruct PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fragmentDensityMapOffset))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM where
  peekCStruct p = do
    fragmentDensityMapOffset <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM
             (bool32ToBool fragmentDensityMapOffset)

instance Storable PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM where
  zero = PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM
           zero


-- | VkPhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM - Structure
-- describing fragment density map offset properties that can be supported
-- by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM' structure
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_fragment_density_map_offset VK_QCOM_fragment_density_map_offset>,
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM = PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM
  { -- | #limits-fragmentdensityoffsetgranularity#
    -- @fragmentDensityOffsetGranularity@ is the granularity for
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-fragmentdensitymapoffsets fragment density offsets>.
    fragmentDensityOffsetGranularity :: Extent2D }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM

instance ToCStruct PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (fragmentDensityOffsetGranularity)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM where
  peekCStruct p = do
    fragmentDensityOffsetGranularity <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    pure $ PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM
             fragmentDensityOffsetGranularity

instance Storable PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM where
  zero = PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM
           zero


-- | VkSubpassFragmentDensityMapOffsetEndInfoQCOM - Structure specifying
-- fragment density map offset subpass end information
--
-- = Description
--
-- The array elements are given per @layer@ as defined by
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragmentdensitymap-fetch-density-value Fetch Density Value>,
-- where index = layer. Each (x,y) offset is in framebuffer pixels and
-- shifts the fetch of the fragment density map by that amount. Offsets can
-- be positive or negative.
--
-- Offset values specified for any subpass that is not the last subpass in
-- the render pass are ignored. If the
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassEndInfo'::@pNext@
-- chain for the last subpass of a renderpass does not include
-- 'SubpassFragmentDensityMapOffsetEndInfoQCOM', or if
-- @fragmentDensityOffsetCount@ is zero, then the offset (0,0) is used for
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragmentdensitymap-fetch-density-value Fetch Density Value>.
--
-- == Valid Usage
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-fragmentDensityMapOffsets-06503#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-fragmentDensityMapOffsets fragmentDensityMapOffsets>
--     feature is not enabled or fragment density map is not enabled in the
--     render pass, @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-fragmentDensityMapAttachment-06504#
--     If
--     'Vulkan.Core10.Pass.SubpassDescription'::@fragmentDensityMapAttachment@
--     is not is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and was
--     not created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_QCOM',
--     @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-pDepthStencilAttachment-06505#
--     If
--     'Vulkan.Core10.Pass.SubpassDescription'::@pDepthStencilAttachment@
--     is not is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and was
--     not created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_QCOM',
--     @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-pInputAttachments-06506#
--     If any element of
--     'Vulkan.Core10.Pass.SubpassDescription'::@pInputAttachments@ is not
--     is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and was not
--     created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_QCOM',
--     @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-pColorAttachments-06507#
--     If any element of
--     'Vulkan.Core10.Pass.SubpassDescription'::@pColorAttachments@ is not
--     is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and was not
--     created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_QCOM',
--     @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-pResolveAttachments-06508#
--     If any element of
--     'Vulkan.Core10.Pass.SubpassDescription'::@pResolveAttachments@ is
--     not is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and was
--     not created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_QCOM',
--     @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-pPreserveAttachments-06509#
--     If any element of
--     'Vulkan.Core10.Pass.SubpassDescription'::@pPreserveAttachments@ is
--     not is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and was
--     not created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_QCOM',
--     @fragmentDensityOffsetCount@ /must/ equal @0@
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-fragmentDensityOffsetCount-06510#
--     If @fragmentDensityOffsetCount@ is not @0@ and multiview is enabled
--     for the render pass, @fragmentDensityOffsetCount@ /must/ equal the
--     @layerCount@ that was specified in creating the fragment density map
--     attachment view
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-fragmentDensityOffsetCount-06511#
--     If @fragmentDensityOffsetCount@ is not @0@ and multiview is not
--     enabled for the render pass, @fragmentDensityOffsetCount@ /must/
--     equal @1@
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-x-06512# The @x@
--     component of each element of @pFragmentDensityOffsets@ /must/ be an
--     integer multiple of @fragmentDensityOffsetGranularity.width@
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-y-06513# The @y@
--     component of each element of @pFragmentDensityOffsets@ /must/ be an
--     integer multiple of @fragmentDensityOffsetGranularity.height@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_QCOM'
--
-- -   #VUID-VkSubpassFragmentDensityMapOffsetEndInfoQCOM-pFragmentDensityOffsets-parameter#
--     If @fragmentDensityOffsetCount@ is not @0@,
--     @pFragmentDensityOffsets@ /must/ be a valid pointer to an array of
--     @fragmentDensityOffsetCount@
--     'Vulkan.Core10.FundamentalTypes.Offset2D' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_fragment_density_map_offset VK_QCOM_fragment_density_map_offset>,
-- 'Vulkan.Core10.FundamentalTypes.Offset2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SubpassFragmentDensityMapOffsetEndInfoQCOM = SubpassFragmentDensityMapOffsetEndInfoQCOM
  { -- | @pFragmentDensityOffsets@ is a pointer to an array of
    -- 'Vulkan.Core10.FundamentalTypes.Offset2D' structs, each of which
    -- describes the offset per layer.
    fragmentDensityOffsets :: Vector Offset2D }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassFragmentDensityMapOffsetEndInfoQCOM)
#endif
deriving instance Show SubpassFragmentDensityMapOffsetEndInfoQCOM

instance ToCStruct SubpassFragmentDensityMapOffsetEndInfoQCOM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassFragmentDensityMapOffsetEndInfoQCOM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_QCOM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (fragmentDensityOffsets)) :: Word32))
    pPFragmentDensityOffsets' <- ContT $ allocaBytes @Offset2D ((Data.Vector.length (fragmentDensityOffsets)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPFragmentDensityOffsets' `plusPtr` (8 * (i)) :: Ptr Offset2D) (e)) (fragmentDensityOffsets)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Offset2D))) (pPFragmentDensityOffsets')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SubpassFragmentDensityMapOffsetEndInfoQCOM where
  peekCStruct p = do
    fragmentDensityOffsetCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pFragmentDensityOffsets <- peek @(Ptr Offset2D) ((p `plusPtr` 24 :: Ptr (Ptr Offset2D)))
    pFragmentDensityOffsets' <- generateM (fromIntegral fragmentDensityOffsetCount) (\i -> peekCStruct @Offset2D ((pFragmentDensityOffsets `advancePtrBytes` (8 * (i)) :: Ptr Offset2D)))
    pure $ SubpassFragmentDensityMapOffsetEndInfoQCOM
             pFragmentDensityOffsets'

instance Zero SubpassFragmentDensityMapOffsetEndInfoQCOM where
  zero = SubpassFragmentDensityMapOffsetEndInfoQCOM
           mempty


type QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION"
pattern QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION = 1


type QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME = "VK_QCOM_fragment_density_map_offset"

-- No documentation found for TopLevel "VK_QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME"
pattern QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME = "VK_QCOM_fragment_density_map_offset"

