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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Handles (ImageView)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO))
import Vulkan.Core10.Enums.FramebufferCreateFlagBits (FramebufferCreateFlagBits(..))
import Vulkan.Core10.Enums.FramebufferCreateFlagBits (FramebufferCreateFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkPhysicalDeviceImagelessFramebufferFeatures"
data PhysicalDeviceImagelessFramebufferFeatures = PhysicalDeviceImagelessFramebufferFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceImagelessFramebufferFeatures" "imagelessFramebuffer"
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



-- No documentation found for TopLevel "VkFramebufferAttachmentsCreateInfo"
data FramebufferAttachmentsCreateInfo = FramebufferAttachmentsCreateInfo
  { -- No documentation found for Nested "VkFramebufferAttachmentsCreateInfo" "pAttachmentImageInfos"
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
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPAttachmentImageInfos' <- ContT $ allocaBytesAligned @FramebufferAttachmentImageInfo ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachmentImageInfos' `plusPtr` (48 * (i)) :: Ptr FramebufferAttachmentImageInfo) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr FramebufferAttachmentImageInfo))) (pPAttachmentImageInfos')
    lift $ f

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



-- No documentation found for TopLevel "VkFramebufferAttachmentImageInfo"
data FramebufferAttachmentImageInfo = FramebufferAttachmentImageInfo
  { -- No documentation found for Nested "VkFramebufferAttachmentImageInfo" "flags"
    flags :: ImageCreateFlags
  , -- No documentation found for Nested "VkFramebufferAttachmentImageInfo" "usage"
    usage :: ImageUsageFlags
  , -- No documentation found for Nested "VkFramebufferAttachmentImageInfo" "width"
    width :: Word32
  , -- No documentation found for Nested "VkFramebufferAttachmentImageInfo" "height"
    height :: Word32
  , -- No documentation found for Nested "VkFramebufferAttachmentImageInfo" "layerCount"
    layerCount :: Word32
  , -- No documentation found for Nested "VkFramebufferAttachmentImageInfo" "pViewFormats"
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
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageUsageFlags)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    pPViewFormats' <- ContT $ allocaBytesAligned @Format ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewFormats' `plusPtr` (4 * (i)) :: Ptr Format) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Format))) (pPViewFormats')
    lift $ f

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



-- No documentation found for TopLevel "VkRenderPassAttachmentBeginInfo"
data RenderPassAttachmentBeginInfo = RenderPassAttachmentBeginInfo
  { -- No documentation found for Nested "VkRenderPassAttachmentBeginInfo" "pAttachments"
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
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPAttachments' <- ContT $ allocaBytesAligned @ImageView ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAttachments' `plusPtr` (8 * (i)) :: Ptr ImageView) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ImageView))) (pPAttachments')
    lift $ f

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

