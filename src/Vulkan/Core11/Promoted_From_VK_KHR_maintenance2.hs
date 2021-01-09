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
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core11.Enums.PointClippingBehavior (PointClippingBehavior)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core11.Enums.TessellationDomainOrigin (TessellationDomainOrigin)
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
-- | VkInputAttachmentAspectReference - Structure specifying a subpass\/input
-- attachment pair and an aspect mask that /can/ be read.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- 'RenderPassInputAttachmentAspectCreateInfo'
data InputAttachmentAspectReference = InputAttachmentAspectReference
  { -- | @subpass@ is an index into the @pSubpasses@ array of the parent
    -- 'Vulkan.Core10.Pass.RenderPassCreateInfo' structure.
    subpass :: Word32
  , -- | @inputAttachmentIndex@ is an index into the @pInputAttachments@ of the
    -- specified subpass.
    inputAttachmentIndex :: Word32
  , -- | @aspectMask@ is a mask of which aspect(s) /can/ be accessed within the
    -- specified subpass.
    --
    -- #VUID-VkInputAttachmentAspectReference-aspectMask-01964# @aspectMask@
    -- /must/ not include
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_METADATA_BIT'
    --
    -- #VUID-VkInputAttachmentAspectReference-aspectMask-02250# @aspectMask@
    -- /must/ not include @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ for any
    -- index @i@
    --
    -- #VUID-VkInputAttachmentAspectReference-aspectMask-parameter#
    -- @aspectMask@ /must/ be a valid combination of
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' values
    --
    -- #VUID-VkInputAttachmentAspectReference-aspectMask-requiredbitmask#
    -- @aspectMask@ /must/ not be @0@
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


-- | VkRenderPassInputAttachmentAspectCreateInfo - Structure specifying, for
-- a given subpass\/input attachment pair, which aspect /can/ be read.
--
-- = Description
--
-- An application /can/ access any aspect of an input attachment that does
-- not have a specified aspect mask in the @pAspectReferences@ array.
-- Otherwise, an application /must/ not access aspect(s) of an input
-- attachment other than those in its specified aspect mask.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'InputAttachmentAspectReference',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderPassInputAttachmentAspectCreateInfo = RenderPassInputAttachmentAspectCreateInfo
  { -- | @pAspectReferences@ is a pointer to an array of @aspectReferenceCount@
    -- 'InputAttachmentAspectReference' structures containing a mask describing
    -- which aspect(s) /can/ be accessed for a given input attachment within a
    -- given subpass.
    --
    -- #VUID-VkRenderPassInputAttachmentAspectCreateInfo-pAspectReferences-parameter#
    -- @pAspectReferences@ /must/ be a valid pointer to an array of
    -- @aspectReferenceCount@ valid 'InputAttachmentAspectReference' structures
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
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

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


-- | VkPhysicalDevicePointClippingProperties - Structure describing the point
-- clipping behavior supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDevicePointClippingProperties' structure
-- describe the following implementation-dependent limit:
--
-- = Description
--
-- If the 'PhysicalDevicePointClippingProperties' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.PointClippingBehavior.PointClippingBehavior',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePointClippingProperties = PhysicalDevicePointClippingProperties
  { -- | #extension-limits-pointClipping# @pointClippingBehavior@ is a
    -- 'Vulkan.Core11.Enums.PointClippingBehavior.PointClippingBehavior' value
    -- specifying the point clipping behavior supported by the implementation.
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


-- | VkImageViewUsageCreateInfo - Specify the intended usage of an image view
--
-- = Description
--
-- When this structure is chained to
-- 'Vulkan.Core10.ImageView.ImageViewCreateInfo' the @usage@ field
-- overrides the implicit @usage@ parameter inherited from image creation
-- time and its value is used instead for the purposes of determining the
-- valid usage conditions of 'Vulkan.Core10.ImageView.ImageViewCreateInfo'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageViewUsageCreateInfo = ImageViewUsageCreateInfo
  { -- | @usage@ is a bitmask describing the allowed usages of the image view.
    -- See 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' for a
    -- description of the supported bits.
    --
    -- #VUID-VkImageViewUsageCreateInfo-usage-parameter# @usage@ /must/ be a
    -- valid combination of
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' values
    --
    -- #VUID-VkImageViewUsageCreateInfo-usage-requiredbitmask# @usage@ /must/
    -- not be @0@
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


-- | VkPipelineTessellationDomainOriginStateCreateInfo - Structure specifying
-- the orientation of the tessellation domain
--
-- = Description
--
-- If the 'PipelineTessellationDomainOriginStateCreateInfo' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo', it
-- controls the origin of the tessellation domain. If this structure is not
-- present, it is as if @domainOrigin@ were
-- 'Vulkan.Core11.Enums.TessellationDomainOrigin.TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core11.Enums.TessellationDomainOrigin.TessellationDomainOrigin'
data PipelineTessellationDomainOriginStateCreateInfo = PipelineTessellationDomainOriginStateCreateInfo
  { -- | @domainOrigin@ is a
    -- 'Vulkan.Core11.Enums.TessellationDomainOrigin.TessellationDomainOrigin'
    -- value controlling the origin of the tessellation domain space.
    --
    -- #VUID-VkPipelineTessellationDomainOriginStateCreateInfo-domainOrigin-parameter#
    -- @domainOrigin@ /must/ be a valid
    -- 'Vulkan.Core11.Enums.TessellationDomainOrigin.TessellationDomainOrigin'
    -- value
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

