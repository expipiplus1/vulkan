{-# language CPP #-}
-- | = Name
--
-- VK_EXT_subpass_merge_feedback - device extension
--
-- == VK_EXT_subpass_merge_feedback
--
-- [__Name String__]
--     @VK_EXT_subpass_merge_feedback@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     459
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Ting Wei
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_subpass_merge_feedback] @catweiting%0A*Here describe the issue or question you have about the VK_EXT_subpass_merge_feedback extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_subpass_merge_feedback.adoc VK_EXT_subpass_merge_feedback>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-05-24
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Jorg Wagner, Arm
--
--     -   Ting Wei, Arm
--
-- == Description
--
-- This extension adds a mechanism to provide feedback to an application
-- about whether the subpasses specified on render pass creation are merged
-- by the implementation. Additionally, it provides a control to enable or
-- disable subpass merging in the render pass.
--
-- == New Structures
--
-- -   'RenderPassCreationFeedbackInfoEXT'
--
-- -   'RenderPassSubpassFeedbackInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSubpassMergeFeedbackFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2':
--
--     -   'RenderPassCreationFeedbackCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2',
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2':
--
--     -   'RenderPassCreationControlEXT'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2':
--
--     -   'RenderPassSubpassFeedbackCreateInfoEXT'
--
-- == New Enums
--
-- -   'SubpassMergeStatusEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SUBPASS_MERGE_FEEDBACK_EXTENSION_NAME'
--
-- -   'EXT_SUBPASS_MERGE_FEEDBACK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_MERGE_FEEDBACK_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_CREATION_CONTROL_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_CREATION_FEEDBACK_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_SUBPASS_FEEDBACK_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-03-10
--
--     -   Initial draft.
--
-- -   Revision 2, 2022-05-24
--
--     -   Fix structextends and constness issues.
--
-- == See Also
--
-- 'PhysicalDeviceSubpassMergeFeedbackFeaturesEXT',
-- 'RenderPassCreationControlEXT',
-- 'RenderPassCreationFeedbackCreateInfoEXT',
-- 'RenderPassCreationFeedbackInfoEXT',
-- 'RenderPassSubpassFeedbackCreateInfoEXT',
-- 'RenderPassSubpassFeedbackInfoEXT', 'SubpassMergeStatusEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_subpass_merge_feedback Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_subpass_merge_feedback  ( RenderPassCreationControlEXT(..)
                                                        , RenderPassCreationFeedbackInfoEXT(..)
                                                        , RenderPassCreationFeedbackCreateInfoEXT(..)
                                                        , RenderPassSubpassFeedbackInfoEXT(..)
                                                        , RenderPassSubpassFeedbackCreateInfoEXT(..)
                                                        , PhysicalDeviceSubpassMergeFeedbackFeaturesEXT(..)
                                                        , SubpassMergeStatusEXT( SUBPASS_MERGE_STATUS_MERGED_EXT
                                                                               , SUBPASS_MERGE_STATUS_DISALLOWED_EXT
                                                                               , SUBPASS_MERGE_STATUS_NOT_MERGED_SIDE_EFFECTS_EXT
                                                                               , SUBPASS_MERGE_STATUS_NOT_MERGED_SAMPLES_MISMATCH_EXT
                                                                               , SUBPASS_MERGE_STATUS_NOT_MERGED_VIEWS_MISMATCH_EXT
                                                                               , SUBPASS_MERGE_STATUS_NOT_MERGED_ALIASING_EXT
                                                                               , SUBPASS_MERGE_STATUS_NOT_MERGED_DEPENDENCIES_EXT
                                                                               , SUBPASS_MERGE_STATUS_NOT_MERGED_INCOMPATIBLE_INPUT_ATTACHMENT_EXT
                                                                               , SUBPASS_MERGE_STATUS_NOT_MERGED_TOO_MANY_ATTACHMENTS_EXT
                                                                               , SUBPASS_MERGE_STATUS_NOT_MERGED_INSUFFICIENT_STORAGE_EXT
                                                                               , SUBPASS_MERGE_STATUS_NOT_MERGED_DEPTH_STENCIL_COUNT_EXT
                                                                               , SUBPASS_MERGE_STATUS_NOT_MERGED_RESOLVE_ATTACHMENT_REUSE_EXT
                                                                               , SUBPASS_MERGE_STATUS_NOT_MERGED_SINGLE_SUBPASS_EXT
                                                                               , SUBPASS_MERGE_STATUS_NOT_MERGED_UNSPECIFIED_EXT
                                                                               , ..
                                                                               )
                                                        , EXT_SUBPASS_MERGE_FEEDBACK_SPEC_VERSION
                                                        , pattern EXT_SUBPASS_MERGE_FEEDBACK_SPEC_VERSION
                                                        , EXT_SUBPASS_MERGE_FEEDBACK_EXTENSION_NAME
                                                        , pattern EXT_SUBPASS_MERGE_FEEDBACK_EXTENSION_NAME
                                                        ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.ByteString (packCString)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_MERGE_FEEDBACK_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_CREATION_CONTROL_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_CREATION_FEEDBACK_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_SUBPASS_FEEDBACK_CREATE_INFO_EXT))
-- | VkRenderPassCreationControlEXT - Control about the creation of render
-- pass or subpass
--
-- = Description
--
-- If a 'RenderPassCreationControlEXT' structure is included in the @pNext@
-- chain of
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2'
-- and its value of @disallowMerging@ is
-- 'Vulkan.Core10.FundamentalTypes.TRUE', the implementation will disable
-- subpass merging for the entire render pass. If a
-- 'RenderPassCreationControlEXT' structure is included in the @pNext@
-- chain of
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2'
-- and its value of @disallowMerging@ is
-- 'Vulkan.Core10.FundamentalTypes.TRUE', the implementation will disable
-- merging the described subpass with previous subpasses in the render
-- pass.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_subpass_merge_feedback VK_EXT_subpass_merge_feedback>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.createRenderPass2'
data RenderPassCreationControlEXT = RenderPassCreationControlEXT
  { -- | @disallowMerging@ is a boolean value indicating whether subpass merging
    -- will be disabled.
    disallowMerging :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassCreationControlEXT)
#endif
deriving instance Show RenderPassCreationControlEXT

instance ToCStruct RenderPassCreationControlEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassCreationControlEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_CREATION_CONTROL_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (disallowMerging))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_CREATION_CONTROL_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct RenderPassCreationControlEXT where
  peekCStruct p = do
    disallowMerging <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ RenderPassCreationControlEXT
             (bool32ToBool disallowMerging)

instance Storable RenderPassCreationControlEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderPassCreationControlEXT where
  zero = RenderPassCreationControlEXT
           zero


-- | VkRenderPassCreationFeedbackInfoEXT - Feedback about the creation of a
-- render pass
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_subpass_merge_feedback VK_EXT_subpass_merge_feedback>,
-- 'RenderPassCreationFeedbackCreateInfoEXT'
data RenderPassCreationFeedbackInfoEXT = RenderPassCreationFeedbackInfoEXT
  { -- | @postMergeSubpassCount@ is the subpass count after merge.
    postMergeSubpassCount :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassCreationFeedbackInfoEXT)
#endif
deriving instance Show RenderPassCreationFeedbackInfoEXT

instance ToCStruct RenderPassCreationFeedbackInfoEXT where
  withCStruct x f = allocaBytes 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassCreationFeedbackInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (postMergeSubpassCount)
    f
  cStructSize = 4
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    f

instance FromCStruct RenderPassCreationFeedbackInfoEXT where
  peekCStruct p = do
    postMergeSubpassCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pure $ RenderPassCreationFeedbackInfoEXT
             postMergeSubpassCount

instance Storable RenderPassCreationFeedbackInfoEXT where
  sizeOf ~_ = 4
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderPassCreationFeedbackInfoEXT where
  zero = RenderPassCreationFeedbackInfoEXT
           zero


-- | VkRenderPassCreationFeedbackCreateInfoEXT - Request feedback about the
-- creation of render pass
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_subpass_merge_feedback VK_EXT_subpass_merge_feedback>,
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2',
-- 'RenderPassCreationControlEXT', 'RenderPassCreationFeedbackInfoEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.createRenderPass2'
data RenderPassCreationFeedbackCreateInfoEXT = RenderPassCreationFeedbackCreateInfoEXT
  { -- | @pRenderPassFeedback@ is a pointer to a
    -- 'RenderPassCreationFeedbackInfoEXT' structure in which feedback is
    -- returned.
    --
    -- #VUID-VkRenderPassCreationFeedbackCreateInfoEXT-pRenderPassFeedback-parameter#
    -- @pRenderPassFeedback@ /must/ be a valid pointer to a
    -- 'RenderPassCreationFeedbackInfoEXT' structure
    renderPassFeedback :: Ptr RenderPassCreationFeedbackInfoEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassCreationFeedbackCreateInfoEXT)
#endif
deriving instance Show RenderPassCreationFeedbackCreateInfoEXT

instance ToCStruct RenderPassCreationFeedbackCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassCreationFeedbackCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_CREATION_FEEDBACK_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr RenderPassCreationFeedbackInfoEXT))) (renderPassFeedback)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_CREATION_FEEDBACK_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr RenderPassCreationFeedbackInfoEXT))) (zero)
    f

instance FromCStruct RenderPassCreationFeedbackCreateInfoEXT where
  peekCStruct p = do
    pRenderPassFeedback <- peek @(Ptr RenderPassCreationFeedbackInfoEXT) ((p `plusPtr` 16 :: Ptr (Ptr RenderPassCreationFeedbackInfoEXT)))
    pure $ RenderPassCreationFeedbackCreateInfoEXT
             pRenderPassFeedback

instance Storable RenderPassCreationFeedbackCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderPassCreationFeedbackCreateInfoEXT where
  zero = RenderPassCreationFeedbackCreateInfoEXT
           zero


-- | VkRenderPassSubpassFeedbackInfoEXT - Feedback about the creation of
-- subpass
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_subpass_merge_feedback VK_EXT_subpass_merge_feedback>,
-- 'RenderPassSubpassFeedbackCreateInfoEXT', 'SubpassMergeStatusEXT'
data RenderPassSubpassFeedbackInfoEXT = RenderPassSubpassFeedbackInfoEXT
  { -- | @subpassMergeStatus@ is a 'SubpassMergeStatusEXT' value specifying
    -- information about whether the subpass is merged with previous subpass
    -- and the reason why it is not merged.
    subpassMergeStatus :: SubpassMergeStatusEXT
  , -- | @description@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@ containing a
    -- null-terminated UTF-8 string which provides additional details.
    description :: ByteString
  , -- | @postMergeIndex@ is the subpass index after the subpass merging.
    postMergeIndex :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassSubpassFeedbackInfoEXT)
#endif
deriving instance Show RenderPassSubpassFeedbackInfoEXT

instance ToCStruct RenderPassSubpassFeedbackInfoEXT where
  withCStruct x f = allocaBytes 264 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassSubpassFeedbackInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr SubpassMergeStatusEXT)) (subpassMergeStatus)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 4 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (postMergeIndex)
    f
  cStructSize = 264
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr SubpassMergeStatusEXT)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 4 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (zero)
    f

instance FromCStruct RenderPassSubpassFeedbackInfoEXT where
  peekCStruct p = do
    subpassMergeStatus <- peek @SubpassMergeStatusEXT ((p `plusPtr` 0 :: Ptr SubpassMergeStatusEXT))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 4 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    postMergeIndex <- peek @Word32 ((p `plusPtr` 260 :: Ptr Word32))
    pure $ RenderPassSubpassFeedbackInfoEXT
             subpassMergeStatus description postMergeIndex

instance Storable RenderPassSubpassFeedbackInfoEXT where
  sizeOf ~_ = 264
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderPassSubpassFeedbackInfoEXT where
  zero = RenderPassSubpassFeedbackInfoEXT
           zero
           mempty
           zero


-- | VkRenderPassSubpassFeedbackCreateInfoEXT - Request for feedback about
-- the creation of subpass
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_subpass_merge_feedback VK_EXT_subpass_merge_feedback>,
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2',
-- 'RenderPassCreationControlEXT', 'RenderPassSubpassFeedbackInfoEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.createRenderPass2'
data RenderPassSubpassFeedbackCreateInfoEXT = RenderPassSubpassFeedbackCreateInfoEXT
  { -- | @pSubpassFeedback@ is a pointer to a 'RenderPassSubpassFeedbackInfoEXT'
    -- structure in which feedback is returned.
    --
    -- #VUID-VkRenderPassSubpassFeedbackCreateInfoEXT-pSubpassFeedback-parameter#
    -- @pSubpassFeedback@ /must/ be a valid pointer to a
    -- 'RenderPassSubpassFeedbackInfoEXT' structure
    subpassFeedback :: Ptr RenderPassSubpassFeedbackInfoEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassSubpassFeedbackCreateInfoEXT)
#endif
deriving instance Show RenderPassSubpassFeedbackCreateInfoEXT

instance ToCStruct RenderPassSubpassFeedbackCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassSubpassFeedbackCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_SUBPASS_FEEDBACK_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr RenderPassSubpassFeedbackInfoEXT))) (subpassFeedback)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_SUBPASS_FEEDBACK_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr RenderPassSubpassFeedbackInfoEXT))) (zero)
    f

instance FromCStruct RenderPassSubpassFeedbackCreateInfoEXT where
  peekCStruct p = do
    pSubpassFeedback <- peek @(Ptr RenderPassSubpassFeedbackInfoEXT) ((p `plusPtr` 16 :: Ptr (Ptr RenderPassSubpassFeedbackInfoEXT)))
    pure $ RenderPassSubpassFeedbackCreateInfoEXT
             pSubpassFeedback

instance Storable RenderPassSubpassFeedbackCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderPassSubpassFeedbackCreateInfoEXT where
  zero = RenderPassSubpassFeedbackCreateInfoEXT
           zero


-- | VkPhysicalDeviceSubpassMergeFeedbackFeaturesEXT - Structure describing
-- whether subpass merging feedback can be supported by the implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceSubpassMergeFeedbackFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceSubpassMergeFeedbackFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_subpass_merge_feedback VK_EXT_subpass_merge_feedback>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSubpassMergeFeedbackFeaturesEXT = PhysicalDeviceSubpassMergeFeedbackFeaturesEXT
  { -- | #features-subpassMergeFeedback# @subpassMergeFeedback@ indicates whether
    -- the implementation supports feedback of subpass merging.
    subpassMergeFeedback :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSubpassMergeFeedbackFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceSubpassMergeFeedbackFeaturesEXT

instance ToCStruct PhysicalDeviceSubpassMergeFeedbackFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSubpassMergeFeedbackFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_MERGE_FEEDBACK_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (subpassMergeFeedback))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_MERGE_FEEDBACK_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSubpassMergeFeedbackFeaturesEXT where
  peekCStruct p = do
    subpassMergeFeedback <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceSubpassMergeFeedbackFeaturesEXT
             (bool32ToBool subpassMergeFeedback)

instance Storable PhysicalDeviceSubpassMergeFeedbackFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSubpassMergeFeedbackFeaturesEXT where
  zero = PhysicalDeviceSubpassMergeFeedbackFeaturesEXT
           zero


-- | VkSubpassMergeStatusEXT - Specify a subpass merging status
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_subpass_merge_feedback VK_EXT_subpass_merge_feedback>,
-- 'RenderPassSubpassFeedbackInfoEXT'
newtype SubpassMergeStatusEXT = SubpassMergeStatusEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SUBPASS_MERGE_STATUS_MERGED_EXT' specifies the subpass is merged with a
-- previous subpass.
pattern SUBPASS_MERGE_STATUS_MERGED_EXT = SubpassMergeStatusEXT 0

-- | 'SUBPASS_MERGE_STATUS_DISALLOWED_EXT' specifies the subpass is
-- disallowed to merge with previous subpass. If the render pass does not
-- allow subpass merging, then all subpass statuses are set to this value.
-- If a subpass description does not allow subpass merging, then only that
-- subpass’s status is set to this value.
pattern SUBPASS_MERGE_STATUS_DISALLOWED_EXT = SubpassMergeStatusEXT 1

-- | 'SUBPASS_MERGE_STATUS_NOT_MERGED_SIDE_EFFECTS_EXT' specifies the subpass
-- is not merged because it contains side effects.
pattern SUBPASS_MERGE_STATUS_NOT_MERGED_SIDE_EFFECTS_EXT = SubpassMergeStatusEXT 2

-- | 'SUBPASS_MERGE_STATUS_NOT_MERGED_SAMPLES_MISMATCH_EXT' specifies the
-- subpass is not merged because sample count is not compatible with
-- previous subpass.
pattern SUBPASS_MERGE_STATUS_NOT_MERGED_SAMPLES_MISMATCH_EXT = SubpassMergeStatusEXT 3

-- | 'SUBPASS_MERGE_STATUS_NOT_MERGED_VIEWS_MISMATCH_EXT' specifies the
-- subpass is not merged because view masks do not match with previous
-- subpass.
pattern SUBPASS_MERGE_STATUS_NOT_MERGED_VIEWS_MISMATCH_EXT = SubpassMergeStatusEXT 4

-- | 'SUBPASS_MERGE_STATUS_NOT_MERGED_ALIASING_EXT' specifies the subpass is
-- not merged because of attachments aliasing between them.
pattern SUBPASS_MERGE_STATUS_NOT_MERGED_ALIASING_EXT = SubpassMergeStatusEXT 5

-- | 'SUBPASS_MERGE_STATUS_NOT_MERGED_DEPENDENCIES_EXT' specifies the subpass
-- is not merged because subpass dependencies do not allow merging.
pattern SUBPASS_MERGE_STATUS_NOT_MERGED_DEPENDENCIES_EXT = SubpassMergeStatusEXT 6

-- | 'SUBPASS_MERGE_STATUS_NOT_MERGED_INCOMPATIBLE_INPUT_ATTACHMENT_EXT'
-- specifies the subpass is not merged because input attachment is not a
-- color attachment from previous subpass or the formats are incompatible.
pattern SUBPASS_MERGE_STATUS_NOT_MERGED_INCOMPATIBLE_INPUT_ATTACHMENT_EXT = SubpassMergeStatusEXT 7

-- | 'SUBPASS_MERGE_STATUS_NOT_MERGED_TOO_MANY_ATTACHMENTS_EXT' specifies the
-- subpass is not merged because of too many attachments.
pattern SUBPASS_MERGE_STATUS_NOT_MERGED_TOO_MANY_ATTACHMENTS_EXT = SubpassMergeStatusEXT 8

-- | 'SUBPASS_MERGE_STATUS_NOT_MERGED_INSUFFICIENT_STORAGE_EXT' specifies the
-- subpass is not merged because of insufficient memory.
pattern SUBPASS_MERGE_STATUS_NOT_MERGED_INSUFFICIENT_STORAGE_EXT = SubpassMergeStatusEXT 9

-- | 'SUBPASS_MERGE_STATUS_NOT_MERGED_DEPTH_STENCIL_COUNT_EXT' specifies the
-- subpass is not merged because of too many depth\/stencil attachments.
pattern SUBPASS_MERGE_STATUS_NOT_MERGED_DEPTH_STENCIL_COUNT_EXT = SubpassMergeStatusEXT 10

-- | 'SUBPASS_MERGE_STATUS_NOT_MERGED_RESOLVE_ATTACHMENT_REUSE_EXT' specifies
-- the subpass is not merged because a resolve attachment is reused as an
-- input attachment in a subsequent subpass.
pattern SUBPASS_MERGE_STATUS_NOT_MERGED_RESOLVE_ATTACHMENT_REUSE_EXT = SubpassMergeStatusEXT 11

-- | 'SUBPASS_MERGE_STATUS_NOT_MERGED_SINGLE_SUBPASS_EXT' specifies the
-- subpass is not merged because the render pass has only one subpass.
pattern SUBPASS_MERGE_STATUS_NOT_MERGED_SINGLE_SUBPASS_EXT = SubpassMergeStatusEXT 12

-- | 'SUBPASS_MERGE_STATUS_NOT_MERGED_UNSPECIFIED_EXT' specifies other
-- reasons why subpass is not merged. It is also the recommended default
-- value that should be reported when a subpass is not merged and when no
-- other value is appropriate.
pattern SUBPASS_MERGE_STATUS_NOT_MERGED_UNSPECIFIED_EXT = SubpassMergeStatusEXT 13

{-# COMPLETE
  SUBPASS_MERGE_STATUS_MERGED_EXT
  , SUBPASS_MERGE_STATUS_DISALLOWED_EXT
  , SUBPASS_MERGE_STATUS_NOT_MERGED_SIDE_EFFECTS_EXT
  , SUBPASS_MERGE_STATUS_NOT_MERGED_SAMPLES_MISMATCH_EXT
  , SUBPASS_MERGE_STATUS_NOT_MERGED_VIEWS_MISMATCH_EXT
  , SUBPASS_MERGE_STATUS_NOT_MERGED_ALIASING_EXT
  , SUBPASS_MERGE_STATUS_NOT_MERGED_DEPENDENCIES_EXT
  , SUBPASS_MERGE_STATUS_NOT_MERGED_INCOMPATIBLE_INPUT_ATTACHMENT_EXT
  , SUBPASS_MERGE_STATUS_NOT_MERGED_TOO_MANY_ATTACHMENTS_EXT
  , SUBPASS_MERGE_STATUS_NOT_MERGED_INSUFFICIENT_STORAGE_EXT
  , SUBPASS_MERGE_STATUS_NOT_MERGED_DEPTH_STENCIL_COUNT_EXT
  , SUBPASS_MERGE_STATUS_NOT_MERGED_RESOLVE_ATTACHMENT_REUSE_EXT
  , SUBPASS_MERGE_STATUS_NOT_MERGED_SINGLE_SUBPASS_EXT
  , SUBPASS_MERGE_STATUS_NOT_MERGED_UNSPECIFIED_EXT ::
    SubpassMergeStatusEXT
  #-}

conNameSubpassMergeStatusEXT :: String
conNameSubpassMergeStatusEXT = "SubpassMergeStatusEXT"

enumPrefixSubpassMergeStatusEXT :: String
enumPrefixSubpassMergeStatusEXT = "SUBPASS_MERGE_STATUS_"

showTableSubpassMergeStatusEXT :: [(SubpassMergeStatusEXT, String)]
showTableSubpassMergeStatusEXT =
  [
    ( SUBPASS_MERGE_STATUS_MERGED_EXT
    , "MERGED_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_DISALLOWED_EXT
    , "DISALLOWED_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_NOT_MERGED_SIDE_EFFECTS_EXT
    , "NOT_MERGED_SIDE_EFFECTS_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_NOT_MERGED_SAMPLES_MISMATCH_EXT
    , "NOT_MERGED_SAMPLES_MISMATCH_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_NOT_MERGED_VIEWS_MISMATCH_EXT
    , "NOT_MERGED_VIEWS_MISMATCH_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_NOT_MERGED_ALIASING_EXT
    , "NOT_MERGED_ALIASING_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_NOT_MERGED_DEPENDENCIES_EXT
    , "NOT_MERGED_DEPENDENCIES_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_NOT_MERGED_INCOMPATIBLE_INPUT_ATTACHMENT_EXT
    , "NOT_MERGED_INCOMPATIBLE_INPUT_ATTACHMENT_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_NOT_MERGED_TOO_MANY_ATTACHMENTS_EXT
    , "NOT_MERGED_TOO_MANY_ATTACHMENTS_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_NOT_MERGED_INSUFFICIENT_STORAGE_EXT
    , "NOT_MERGED_INSUFFICIENT_STORAGE_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_NOT_MERGED_DEPTH_STENCIL_COUNT_EXT
    , "NOT_MERGED_DEPTH_STENCIL_COUNT_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_NOT_MERGED_RESOLVE_ATTACHMENT_REUSE_EXT
    , "NOT_MERGED_RESOLVE_ATTACHMENT_REUSE_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_NOT_MERGED_SINGLE_SUBPASS_EXT
    , "NOT_MERGED_SINGLE_SUBPASS_EXT"
    )
  ,
    ( SUBPASS_MERGE_STATUS_NOT_MERGED_UNSPECIFIED_EXT
    , "NOT_MERGED_UNSPECIFIED_EXT"
    )
  ]

instance Show SubpassMergeStatusEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixSubpassMergeStatusEXT
      showTableSubpassMergeStatusEXT
      conNameSubpassMergeStatusEXT
      (\(SubpassMergeStatusEXT x) -> x)
      (showsPrec 11)

instance Read SubpassMergeStatusEXT where
  readPrec =
    enumReadPrec
      enumPrefixSubpassMergeStatusEXT
      showTableSubpassMergeStatusEXT
      conNameSubpassMergeStatusEXT
      SubpassMergeStatusEXT

type EXT_SUBPASS_MERGE_FEEDBACK_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_SUBPASS_MERGE_FEEDBACK_SPEC_VERSION"
pattern EXT_SUBPASS_MERGE_FEEDBACK_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SUBPASS_MERGE_FEEDBACK_SPEC_VERSION = 2


type EXT_SUBPASS_MERGE_FEEDBACK_EXTENSION_NAME = "VK_EXT_subpass_merge_feedback"

-- No documentation found for TopLevel "VK_EXT_SUBPASS_MERGE_FEEDBACK_EXTENSION_NAME"
pattern EXT_SUBPASS_MERGE_FEEDBACK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SUBPASS_MERGE_FEEDBACK_EXTENSION_NAME = "VK_EXT_subpass_merge_feedback"

