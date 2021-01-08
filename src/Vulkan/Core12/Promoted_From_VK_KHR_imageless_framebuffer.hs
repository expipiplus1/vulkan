{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_imageless_framebuffer"
module Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer  ( PhysicalDeviceImagelessFramebufferFeatures(..)
                                                                 , FramebufferAttachmentsCreateInfo(..)
                                                                 , FramebufferAttachmentImageInfo(..)
                                                                 , RenderPassAttachmentBeginInfo(..)
                                                                 , StructureType(..)
                                                                 , FramebufferCreateFlagBits(..)
                                                                 , FramebufferCreateFlags
                                                                 ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Handles (ImageView)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO))
import Vulkan.Core10.Enums.FramebufferCreateFlagBits (FramebufferCreateFlagBits(..))
import Vulkan.Core10.Enums.FramebufferCreateFlagBits (FramebufferCreateFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceImagelessFramebufferFeatures - Structure indicating
-- support for imageless framebuffers
--
-- = Members
--
-- The members of the 'PhysicalDeviceImagelessFramebufferFeatures'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceImagelessFramebufferFeatures' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceImagelessFramebufferFeatures' /can/ also be included in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- this feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImagelessFramebufferFeatures = PhysicalDeviceImagelessFramebufferFeatures
  { -- | #extension-features-imagelessFramebuffer# @imagelessFramebuffer@
    -- indicates that the implementation supports specifying the image view for
    -- attachments at render pass begin time via
    -- 'RenderPassAttachmentBeginInfo'.
    imagelessFramebuffer :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImagelessFramebufferFeatures)
#endif
deriving instance Show PhysicalDeviceImagelessFramebufferFeatures

instance ToCStruct PhysicalDeviceImagelessFramebufferFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImagelessFramebufferFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (imagelessFramebuffer))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImagelessFramebufferFeatures where
  peekCStruct p = do
    imagelessFramebuffer <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceImagelessFramebufferFeatures
             (bool32ToBool imagelessFramebuffer)

instance Storable PhysicalDeviceImagelessFramebufferFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImagelessFramebufferFeatures where
  zero = PhysicalDeviceImagelessFramebufferFeatures
           zero


-- | VkFramebufferAttachmentsCreateInfo - Structure specifying parameters of
-- images that will be used with a framebuffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkFramebufferAttachmentsCreateInfo-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO'
--
-- -   #VUID-VkFramebufferAttachmentsCreateInfo-pAttachmentImageInfos-parameter#
--     If @attachmentImageInfoCount@ is not @0@, @pAttachmentImageInfos@
--     /must/ be a valid pointer to an array of @attachmentImageInfoCount@
--     valid 'FramebufferAttachmentImageInfo' structures
--
-- = See Also
--
-- 'FramebufferAttachmentImageInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data FramebufferAttachmentsCreateInfo = FramebufferAttachmentsCreateInfo
  { -- | @pAttachmentImageInfos@ is a pointer to an array of
    -- 'FramebufferAttachmentImageInfo' instances, each of which describes a
    -- number of parameters of the corresponding attachment in a render pass
    -- instance.
    attachmentImageInfos :: Vector FramebufferAttachmentImageInfo }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FramebufferAttachmentsCreateInfo)
#endif
deriving instance Show FramebufferAttachmentsCreateInfo

instance ToCStruct FramebufferAttachmentsCreateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FramebufferAttachmentsCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (attachmentImageInfos)) :: Word32))
    pPAttachmentImageInfos' <- ContT $ allocaBytesAligned @FramebufferAttachmentImageInfo ((Data.Vector.length (attachmentImageInfos)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachmentImageInfos' `plusPtr` (48 * (i)) :: Ptr FramebufferAttachmentImageInfo) (e) . ($ ())) (attachmentImageInfos)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr FramebufferAttachmentImageInfo))) (pPAttachmentImageInfos')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct FramebufferAttachmentsCreateInfo where
  peekCStruct p = do
    attachmentImageInfoCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAttachmentImageInfos <- peek @(Ptr FramebufferAttachmentImageInfo) ((p `plusPtr` 24 :: Ptr (Ptr FramebufferAttachmentImageInfo)))
    pAttachmentImageInfos' <- generateM (fromIntegral attachmentImageInfoCount) (\i -> peekCStruct @FramebufferAttachmentImageInfo ((pAttachmentImageInfos `advancePtrBytes` (48 * (i)) :: Ptr FramebufferAttachmentImageInfo)))
    pure $ FramebufferAttachmentsCreateInfo
             pAttachmentImageInfos'

instance Zero FramebufferAttachmentsCreateInfo where
  zero = FramebufferAttachmentsCreateInfo
           mempty


-- | VkFramebufferAttachmentImageInfo - Structure specifying parameters of an
-- image that will be used with a framebuffer
--
-- = Description
--
-- Images that /can/ be used with the framebuffer when beginning a render
-- pass, as specified by 'RenderPassAttachmentBeginInfo', /must/ be created
-- with parameters that are identical to those specified here.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkFramebufferAttachmentImageInfo-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO'
--
-- -   #VUID-VkFramebufferAttachmentImageInfo-pNext-pNext# @pNext@ /must/
--     be @NULL@
--
-- -   #VUID-VkFramebufferAttachmentImageInfo-flags-parameter# @flags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits' values
--
-- -   #VUID-VkFramebufferAttachmentImageInfo-usage-parameter# @usage@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' values
--
-- -   #VUID-VkFramebufferAttachmentImageInfo-usage-requiredbitmask#
--     @usage@ /must/ not be @0@
--
-- -   #VUID-VkFramebufferAttachmentImageInfo-pViewFormats-parameter# If
--     @viewFormatCount@ is not @0@, @pViewFormats@ /must/ be a valid
--     pointer to an array of @viewFormatCount@ valid
--     'Vulkan.Core10.Enums.Format.Format' values
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.Format.Format', 'FramebufferAttachmentsCreateInfo',
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlags',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data FramebufferAttachmentImageInfo = FramebufferAttachmentImageInfo
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits', matching
    -- the value of 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ used to
    -- create an image that will be used with this framebuffer.
    flags :: ImageCreateFlags
  , -- | @usage@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits', matching
    -- the value of 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ used to
    -- create an image used with this framebuffer.
    usage :: ImageUsageFlags
  , -- | @width@ is the width of the image view used for rendering.
    width :: Word32
  , -- | @height@ is the height of the image view used for rendering.
    height :: Word32
  , -- No documentation found for Nested "VkFramebufferAttachmentImageInfo" "layerCount"
    layerCount :: Word32
  , -- | @pViewFormats@ is an array which lists of all formats which /can/ be
    -- used when creating views of the image, matching the value of
    -- 'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::pViewFormats
    -- used to create an image used with this framebuffer.
    viewFormats :: Vector Format
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FramebufferAttachmentImageInfo)
#endif
deriving instance Show FramebufferAttachmentImageInfo

instance ToCStruct FramebufferAttachmentImageInfo where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FramebufferAttachmentImageInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageUsageFlags)) (usage)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (width)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (height)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (layerCount)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (viewFormats)) :: Word32))
    pPViewFormats' <- ContT $ allocaBytesAligned @Format ((Data.Vector.length (viewFormats)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewFormats' `plusPtr` (4 * (i)) :: Ptr Format) (e)) (viewFormats)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Format))) (pPViewFormats')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr ImageUsageFlags)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct FramebufferAttachmentImageInfo where
  peekCStruct p = do
    flags <- peek @ImageCreateFlags ((p `plusPtr` 16 :: Ptr ImageCreateFlags))
    usage <- peek @ImageUsageFlags ((p `plusPtr` 20 :: Ptr ImageUsageFlags))
    width <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    layerCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    viewFormatCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pViewFormats <- peek @(Ptr Format) ((p `plusPtr` 40 :: Ptr (Ptr Format)))
    pViewFormats' <- generateM (fromIntegral viewFormatCount) (\i -> peek @Format ((pViewFormats `advancePtrBytes` (4 * (i)) :: Ptr Format)))
    pure $ FramebufferAttachmentImageInfo
             flags usage width height layerCount pViewFormats'

instance Zero FramebufferAttachmentImageInfo where
  zero = FramebufferAttachmentImageInfo
           zero
           zero
           zero
           zero
           zero
           mempty


-- | VkRenderPassAttachmentBeginInfo - Structure specifying images to be used
-- as framebuffer attachments
--
-- == Valid Usage
--
-- -   #VUID-VkRenderPassAttachmentBeginInfo-pAttachments-03218# Each
--     element of @pAttachments@ /must/ only specify a single mip level
--
-- -   #VUID-VkRenderPassAttachmentBeginInfo-pAttachments-03219# Each
--     element of @pAttachments@ /must/ have been created with the identity
--     swizzle
--
-- -   #VUID-VkRenderPassAttachmentBeginInfo-pAttachments-04114# Each
--     element of @pAttachments@ /must/ have been created with
--     'Vulkan.Core10.ImageView.ImageViewCreateInfo'::@viewType@ not equal
--     to 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderPassAttachmentBeginInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO'
--
-- -   #VUID-VkRenderPassAttachmentBeginInfo-pAttachments-parameter# If
--     @attachmentCount@ is not @0@, @pAttachments@ /must/ be a valid
--     pointer to an array of @attachmentCount@ valid
--     'Vulkan.Core10.Handles.ImageView' handles
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.ImageView',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderPassAttachmentBeginInfo = RenderPassAttachmentBeginInfo
  { -- | @pAttachments@ is a pointer to an array of
    -- 'Vulkan.Core10.Handles.ImageView' handles, each of which will be used as
    -- the corresponding attachment in the render pass instance.
    attachments :: Vector ImageView }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassAttachmentBeginInfo)
#endif
deriving instance Show RenderPassAttachmentBeginInfo

instance ToCStruct RenderPassAttachmentBeginInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassAttachmentBeginInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (attachments)) :: Word32))
    pPAttachments' <- ContT $ allocaBytesAligned @ImageView ((Data.Vector.length (attachments)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAttachments' `plusPtr` (8 * (i)) :: Ptr ImageView) (e)) (attachments)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ImageView))) (pPAttachments')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct RenderPassAttachmentBeginInfo where
  peekCStruct p = do
    attachmentCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAttachments <- peek @(Ptr ImageView) ((p `plusPtr` 24 :: Ptr (Ptr ImageView)))
    pAttachments' <- generateM (fromIntegral attachmentCount) (\i -> peek @ImageView ((pAttachments `advancePtrBytes` (8 * (i)) :: Ptr ImageView)))
    pure $ RenderPassAttachmentBeginInfo
             pAttachments'

instance Zero RenderPassAttachmentBeginInfo where
  zero = RenderPassAttachmentBeginInfo
           mempty

