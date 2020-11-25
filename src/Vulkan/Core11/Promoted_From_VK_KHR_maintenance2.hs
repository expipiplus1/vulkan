{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_maintenance2"
module Vulkan.Core11.Promoted_From_VK_KHR_maintenance2  ( InputAttachmentAspectReference(..)
                                                        , RenderPassInputAttachmentAspectCreateInfo(..)
                                                        , PhysicalDevicePointClippingProperties(..)
                                                        , ImageViewUsageCreateInfo(..)
                                                        , PipelineTessellationDomainOriginStateCreateInfo(..)
                                                        , ImageLayout(..)
                                                        , StructureType(..)
                                                        , ImageCreateFlagBits(..)
                                                        , ImageCreateFlags
                                                        , PointClippingBehavior(..)
                                                        , TessellationDomainOrigin(..)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core11.Enums.PointClippingBehavior (PointClippingBehavior)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core11.Enums.TessellationDomainOrigin (TessellationDomainOrigin)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Vulkan.Core11.Enums.PointClippingBehavior (PointClippingBehavior(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core11.Enums.TessellationDomainOrigin (TessellationDomainOrigin(..))

-- No documentation found for TopLevel "VkInputAttachmentAspectReference"
data InputAttachmentAspectReference = InputAttachmentAspectReference
  { -- No documentation found for Nested "VkInputAttachmentAspectReference" "subpass"
    subpass :: Word32
  , -- No documentation found for Nested "VkInputAttachmentAspectReference" "inputAttachmentIndex"
    inputAttachmentIndex :: Word32
  , -- No documentation found for Nested "VkInputAttachmentAspectReference" "aspectMask"
    aspectMask :: ImageAspectFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (InputAttachmentAspectReference)
#endif
deriving instance Show InputAttachmentAspectReference

instance ToCStruct InputAttachmentAspectReference where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p InputAttachmentAspectReference{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (subpass)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (inputAttachmentIndex)
    poke ((p `plusPtr` 8 :: Ptr ImageAspectFlags)) (aspectMask)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr ImageAspectFlags)) (zero)
    f

instance FromCStruct InputAttachmentAspectReference where
  peekCStruct p = do
    subpass <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    inputAttachmentIndex <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 8 :: Ptr ImageAspectFlags))
    pure $ InputAttachmentAspectReference
             subpass inputAttachmentIndex aspectMask


instance Storable InputAttachmentAspectReference where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero InputAttachmentAspectReference where
  zero = InputAttachmentAspectReference
           zero
           zero
           zero



-- No documentation found for TopLevel "VkRenderPassInputAttachmentAspectCreateInfo"
data RenderPassInputAttachmentAspectCreateInfo = RenderPassInputAttachmentAspectCreateInfo
  { -- No documentation found for Nested "VkRenderPassInputAttachmentAspectCreateInfo" "pAspectReferences"
    aspectReferences :: Vector InputAttachmentAspectReference }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassInputAttachmentAspectCreateInfo)
#endif
deriving instance Show RenderPassInputAttachmentAspectCreateInfo

instance ToCStruct RenderPassInputAttachmentAspectCreateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassInputAttachmentAspectCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (aspectReferences)) :: Word32))
    pPAspectReferences' <- ContT $ allocaBytesAligned @InputAttachmentAspectReference ((Data.Vector.length (aspectReferences)) * 12) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAspectReferences' `plusPtr` (12 * (i)) :: Ptr InputAttachmentAspectReference) (e)) (aspectReferences)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr InputAttachmentAspectReference))) (pPAspectReferences')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPAspectReferences' <- ContT $ allocaBytesAligned @InputAttachmentAspectReference ((Data.Vector.length (mempty)) * 12) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAspectReferences' `plusPtr` (12 * (i)) :: Ptr InputAttachmentAspectReference) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr InputAttachmentAspectReference))) (pPAspectReferences')
    lift $ f

instance FromCStruct RenderPassInputAttachmentAspectCreateInfo where
  peekCStruct p = do
    aspectReferenceCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAspectReferences <- peek @(Ptr InputAttachmentAspectReference) ((p `plusPtr` 24 :: Ptr (Ptr InputAttachmentAspectReference)))
    pAspectReferences' <- generateM (fromIntegral aspectReferenceCount) (\i -> peekCStruct @InputAttachmentAspectReference ((pAspectReferences `advancePtrBytes` (12 * (i)) :: Ptr InputAttachmentAspectReference)))
    pure $ RenderPassInputAttachmentAspectCreateInfo
             pAspectReferences'

instance Zero RenderPassInputAttachmentAspectCreateInfo where
  zero = RenderPassInputAttachmentAspectCreateInfo
           mempty



-- No documentation found for TopLevel "VkPhysicalDevicePointClippingProperties"
data PhysicalDevicePointClippingProperties = PhysicalDevicePointClippingProperties
  { -- No documentation found for Nested "VkPhysicalDevicePointClippingProperties" "pointClippingBehavior"
    pointClippingBehavior :: PointClippingBehavior }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePointClippingProperties)
#endif
deriving instance Show PhysicalDevicePointClippingProperties

instance ToCStruct PhysicalDevicePointClippingProperties where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePointClippingProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PointClippingBehavior)) (pointClippingBehavior)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PointClippingBehavior)) (zero)
    f

instance FromCStruct PhysicalDevicePointClippingProperties where
  peekCStruct p = do
    pointClippingBehavior <- peek @PointClippingBehavior ((p `plusPtr` 16 :: Ptr PointClippingBehavior))
    pure $ PhysicalDevicePointClippingProperties
             pointClippingBehavior


instance Storable PhysicalDevicePointClippingProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePointClippingProperties where
  zero = PhysicalDevicePointClippingProperties
           zero



-- No documentation found for TopLevel "VkImageViewUsageCreateInfo"
data ImageViewUsageCreateInfo = ImageViewUsageCreateInfo
  { -- No documentation found for Nested "VkImageViewUsageCreateInfo" "usage"
    usage :: ImageUsageFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewUsageCreateInfo)
#endif
deriving instance Show ImageViewUsageCreateInfo

instance ToCStruct ImageViewUsageCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageViewUsageCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageUsageFlags)) (usage)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageUsageFlags)) (zero)
    f

instance FromCStruct ImageViewUsageCreateInfo where
  peekCStruct p = do
    usage <- peek @ImageUsageFlags ((p `plusPtr` 16 :: Ptr ImageUsageFlags))
    pure $ ImageViewUsageCreateInfo
             usage


instance Storable ImageViewUsageCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageViewUsageCreateInfo where
  zero = ImageViewUsageCreateInfo
           zero



-- No documentation found for TopLevel "VkPipelineTessellationDomainOriginStateCreateInfo"
data PipelineTessellationDomainOriginStateCreateInfo = PipelineTessellationDomainOriginStateCreateInfo
  { -- No documentation found for Nested "VkPipelineTessellationDomainOriginStateCreateInfo" "domainOrigin"
    domainOrigin :: TessellationDomainOrigin }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineTessellationDomainOriginStateCreateInfo)
#endif
deriving instance Show PipelineTessellationDomainOriginStateCreateInfo

instance ToCStruct PipelineTessellationDomainOriginStateCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineTessellationDomainOriginStateCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TessellationDomainOrigin)) (domainOrigin)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TessellationDomainOrigin)) (zero)
    f

instance FromCStruct PipelineTessellationDomainOriginStateCreateInfo where
  peekCStruct p = do
    domainOrigin <- peek @TessellationDomainOrigin ((p `plusPtr` 16 :: Ptr TessellationDomainOrigin))
    pure $ PipelineTessellationDomainOriginStateCreateInfo
             domainOrigin


instance Storable PipelineTessellationDomainOriginStateCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineTessellationDomainOriginStateCreateInfo where
  zero = PipelineTessellationDomainOriginStateCreateInfo
           zero

