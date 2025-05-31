{-# language CPP #-}
-- | = Name
--
-- VK_ARM_render_pass_striped - device extension
--
-- == VK_ARM_render_pass_striped
--
-- [__Name String__]
--     @VK_ARM_render_pass_striped@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     425
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_render_pass_striped] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_ARM_render_pass_striped extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_ARM_render_pass_striped.adoc VK_ARM_render_pass_striped>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-11-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Lisa Wu, Arm
--
--     -   Torbjorn Nilsson, Arm
--
--     -   Ying-Chieh Chen, Mediatek
--
--     -   Jim Chiu, Mediatek
--
-- == Description
--
-- This extension adds the ability to split a render pass instance into
-- stripes, and to get a notification when rendering has completed for each
-- stripe.
--
-- == New Structures
--
-- -   'RenderPassStripeInfoARM'
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.CommandBufferSubmitInfo':
--
--     -   'RenderPassStripeSubmitInfoARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRenderPassStripedFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceRenderPassStripedPropertiesARM'
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo',
--     'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo':
--
--     -   'RenderPassStripeBeginInfoARM'
--
-- == New Enum Constants
--
-- -   'ARM_RENDER_PASS_STRIPED_EXTENSION_NAME'
--
-- -   'ARM_RENDER_PASS_STRIPED_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_STRIPE_BEGIN_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_STRIPE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_STRIPE_SUBMIT_INFO_ARM'
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-11-21
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceRenderPassStripedFeaturesARM',
-- 'PhysicalDeviceRenderPassStripedPropertiesARM',
-- 'RenderPassStripeBeginInfoARM', 'RenderPassStripeInfoARM',
-- 'RenderPassStripeSubmitInfoARM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_ARM_render_pass_striped Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_render_pass_striped  ( PhysicalDeviceRenderPassStripedFeaturesARM(..)
                                                     , PhysicalDeviceRenderPassStripedPropertiesARM(..)
                                                     , RenderPassStripeInfoARM(..)
                                                     , RenderPassStripeBeginInfoARM(..)
                                                     , RenderPassStripeSubmitInfoARM(..)
                                                     , ARM_RENDER_PASS_STRIPED_SPEC_VERSION
                                                     , pattern ARM_RENDER_PASS_STRIPED_SPEC_VERSION
                                                     , ARM_RENDER_PASS_STRIPED_EXTENSION_NAME
                                                     , pattern ARM_RENDER_PASS_STRIPED_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (SemaphoreSubmitInfo)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_FEATURES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_PROPERTIES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_STRIPE_BEGIN_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_STRIPE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_STRIPE_SUBMIT_INFO_ARM))
-- | VkPhysicalDeviceRenderPassStripedFeaturesARM - Structure describing
-- whether striped rendering can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceRenderPassStripedFeaturesARM'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceRenderPassStripedFeaturesARM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceRenderPassStripedFeaturesARM' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_render_pass_striped VK_ARM_render_pass_striped>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRenderPassStripedFeaturesARM = PhysicalDeviceRenderPassStripedFeaturesARM
  { -- | #features-renderPassStriped# @renderPassStriped@ indicates that striped
    -- rendering is supported by the implementation.
    renderPassStriped :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRenderPassStripedFeaturesARM)
#endif
deriving instance Show PhysicalDeviceRenderPassStripedFeaturesARM

instance ToCStruct PhysicalDeviceRenderPassStripedFeaturesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRenderPassStripedFeaturesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (renderPassStriped))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRenderPassStripedFeaturesARM where
  peekCStruct p = do
    renderPassStriped <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceRenderPassStripedFeaturesARM
             (bool32ToBool renderPassStriped)

instance Storable PhysicalDeviceRenderPassStripedFeaturesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRenderPassStripedFeaturesARM where
  zero = PhysicalDeviceRenderPassStripedFeaturesARM
           zero


-- | VkPhysicalDeviceRenderPassStripedPropertiesARM - Structure describing
-- striped rendering limits of an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceRenderPassStripedPropertiesARM'
-- structure describe the following limits:
--
-- = Description
--
-- If the 'PhysicalDeviceRenderPassStripedPropertiesARM' structure is
-- included in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_render_pass_striped VK_ARM_render_pass_striped>,
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRenderPassStripedPropertiesARM = PhysicalDeviceRenderPassStripedPropertiesARM
  { -- | #limits-renderPassStripeGranularity# @renderPassStripeGranularity@
    -- indicates the minimum supported granularity of striped render pass
    -- regions.
    renderPassStripeGranularity :: Extent2D
  , -- | #limits-maxRenderPassStripes# @maxRenderPassStripes@ indicates the
    -- maximum number of stripes supported in striped rendering.
    maxRenderPassStripes :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRenderPassStripedPropertiesARM)
#endif
deriving instance Show PhysicalDeviceRenderPassStripedPropertiesARM

instance ToCStruct PhysicalDeviceRenderPassStripedPropertiesARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRenderPassStripedPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (renderPassStripeGranularity)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxRenderPassStripes)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceRenderPassStripedPropertiesARM where
  peekCStruct p = do
    renderPassStripeGranularity <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    maxRenderPassStripes <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ PhysicalDeviceRenderPassStripedPropertiesARM
             renderPassStripeGranularity maxRenderPassStripes

instance Storable PhysicalDeviceRenderPassStripedPropertiesARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRenderPassStripedPropertiesARM where
  zero = PhysicalDeviceRenderPassStripedPropertiesARM
           zero
           zero


-- | VkRenderPassStripeInfoARM - Structure specifying per rendering stripe
-- information
--
-- = Description
--
-- @stripeArea@ is the render area that is affected by this stripe of the
-- render pass instance. It /must/ be a subregion of the @renderArea@ of
-- the render pass instance.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_render_pass_striped VK_ARM_render_pass_striped>,
-- 'Vulkan.Core10.FundamentalTypes.Rect2D', 'RenderPassStripeBeginInfoARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderPassStripeInfoARM = RenderPassStripeInfoARM
  { -- | @stripeArea@ is the stripe area, and is described in more detail below.
    stripeArea :: Rect2D }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassStripeInfoARM)
#endif
deriving instance Show RenderPassStripeInfoARM

instance ToCStruct RenderPassStripeInfoARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassStripeInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_STRIPE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Rect2D)) (stripeArea)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_STRIPE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Rect2D)) (zero)
    f

instance FromCStruct RenderPassStripeInfoARM where
  peekCStruct p = do
    stripeArea <- peekCStruct @Rect2D ((p `plusPtr` 16 :: Ptr Rect2D))
    pure $ RenderPassStripeInfoARM
             stripeArea

instance Storable RenderPassStripeInfoARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderPassStripeInfoARM where
  zero = RenderPassStripeInfoARM
           zero


-- | VkRenderPassStripeBeginInfoARM - Structure specifying striped rendering
-- information
--
-- = Description
--
-- This structure can be included in the @pNext@ chain of
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo' or
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo' to
-- define how the render pass instance is split into stripes.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderPassStripeBeginInfoARM-stripeInfoCount-09450#
--     @stripeInfoCount@ /must/ be less than or equal to
--     'PhysicalDeviceRenderPassStripedPropertiesARM'::@maxRenderPassStripes@
--
-- -   #VUID-VkRenderPassStripeBeginInfoARM-stripeArea-09451# The
--     @stripeArea@ defined by each element of @pStripeInfos@ /must/ not
--     overlap the @stripeArea@ of any other element
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderPassStripeBeginInfoARM-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_STRIPE_BEGIN_INFO_ARM'
--
-- -   #VUID-VkRenderPassStripeBeginInfoARM-pStripeInfos-parameter#
--     @pStripeInfos@ /must/ be a valid pointer to an array of
--     @stripeInfoCount@ 'RenderPassStripeInfoARM' structures
--
-- -   #VUID-VkRenderPassStripeBeginInfoARM-stripeInfoCount-arraylength#
--     @stripeInfoCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_render_pass_striped VK_ARM_render_pass_striped>,
-- 'RenderPassStripeInfoARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderPassStripeBeginInfoARM = RenderPassStripeBeginInfoARM
  { -- | @stripeInfoCount@ is the number of stripes in this render pass instance
    stripeInfoCount :: Word32
  , -- | @pStripeInfos@ is a pointer to an array of @stripeInfoCount@
    -- 'RenderPassStripeInfoARM' structures describing the stripes used by the
    -- render pass instance.
    stripeInfos :: Ptr RenderPassStripeInfoARM
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassStripeBeginInfoARM)
#endif
deriving instance Show RenderPassStripeBeginInfoARM

instance ToCStruct RenderPassStripeBeginInfoARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassStripeBeginInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_STRIPE_BEGIN_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (stripeInfoCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr RenderPassStripeInfoARM))) (stripeInfos)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_STRIPE_BEGIN_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr RenderPassStripeInfoARM))) (zero)
    f

instance FromCStruct RenderPassStripeBeginInfoARM where
  peekCStruct p = do
    stripeInfoCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pStripeInfos <- peek @(Ptr RenderPassStripeInfoARM) ((p `plusPtr` 24 :: Ptr (Ptr RenderPassStripeInfoARM)))
    pure $ RenderPassStripeBeginInfoARM
             stripeInfoCount pStripeInfos

instance Storable RenderPassStripeBeginInfoARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderPassStripeBeginInfoARM where
  zero = RenderPassStripeBeginInfoARM
           zero
           zero


-- | VkRenderPassStripeSubmitInfoARM - Structure specifying striped rendering
-- submit information
--
-- = Description
--
-- This structure can be included in the @pNext@ chain of
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.CommandBufferSubmitInfo'
-- to provide a set of semaphores to be signaled for each striped render
-- pass instance.
--
-- The elements of @pStripeSemaphoreInfos@ are mapped to render pass
-- instances in
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.CommandBufferSubmitInfo'::@commandBuffer@
-- in submission order and in stripe order within each render pass
-- instance. Each semaphore in @pStripeSemaphoreInfos@ is signaled when the
-- implementation has completed execution of the associated stripe. In a
-- render pass instance that has multiview enabled, the stripe includes all
-- views in the view mask. In a render pass instance with @layerCount@
-- greater than 1, the stripe includes all layers.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderPassStripeSubmitInfoARM-semaphore-09447# The
--     @semaphore@ member of each element of @pStripeSemaphoreInfos@ /must/
--     have been created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderPassStripeSubmitInfoARM-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_STRIPE_SUBMIT_INFO_ARM'
--
-- -   #VUID-VkRenderPassStripeSubmitInfoARM-pStripeSemaphoreInfos-parameter#
--     @pStripeSemaphoreInfos@ /must/ be a valid pointer to an array of
--     @stripeSemaphoreInfoCount@ valid
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.SemaphoreSubmitInfo'
--     structures
--
-- -   #VUID-VkRenderPassStripeSubmitInfoARM-stripeSemaphoreInfoCount-arraylength#
--     @stripeSemaphoreInfoCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_render_pass_striped VK_ARM_render_pass_striped>,
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.SemaphoreSubmitInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderPassStripeSubmitInfoARM = RenderPassStripeSubmitInfoARM
  { -- | @pStripeSemaphoreInfos@ is a pointer to an array of
    -- @stripeSemaphoreInfoCount@
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.SemaphoreSubmitInfo'
    -- structures describing the semaphores used to signal stripe completion.
    stripeSemaphoreInfos :: Vector SemaphoreSubmitInfo }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassStripeSubmitInfoARM)
#endif
deriving instance Show RenderPassStripeSubmitInfoARM

instance ToCStruct RenderPassStripeSubmitInfoARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassStripeSubmitInfoARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_STRIPE_SUBMIT_INFO_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (stripeSemaphoreInfos)) :: Word32))
    pPStripeSemaphoreInfos' <- ContT $ allocaBytes @SemaphoreSubmitInfo ((Data.Vector.length (stripeSemaphoreInfos)) * 48)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPStripeSemaphoreInfos' `plusPtr` (48 * (i)) :: Ptr SemaphoreSubmitInfo) (e)) (stripeSemaphoreInfos)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr SemaphoreSubmitInfo))) (pPStripeSemaphoreInfos')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_STRIPE_SUBMIT_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct RenderPassStripeSubmitInfoARM where
  peekCStruct p = do
    stripeSemaphoreInfoCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pStripeSemaphoreInfos <- peek @(Ptr SemaphoreSubmitInfo) ((p `plusPtr` 24 :: Ptr (Ptr SemaphoreSubmitInfo)))
    pStripeSemaphoreInfos' <- generateM (fromIntegral stripeSemaphoreInfoCount) (\i -> peekCStruct @SemaphoreSubmitInfo ((pStripeSemaphoreInfos `advancePtrBytes` (48 * (i)) :: Ptr SemaphoreSubmitInfo)))
    pure $ RenderPassStripeSubmitInfoARM
             pStripeSemaphoreInfos'

instance Zero RenderPassStripeSubmitInfoARM where
  zero = RenderPassStripeSubmitInfoARM
           mempty


type ARM_RENDER_PASS_STRIPED_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_ARM_RENDER_PASS_STRIPED_SPEC_VERSION"
pattern ARM_RENDER_PASS_STRIPED_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_RENDER_PASS_STRIPED_SPEC_VERSION = 1


type ARM_RENDER_PASS_STRIPED_EXTENSION_NAME = "VK_ARM_render_pass_striped"

-- No documentation found for TopLevel "VK_ARM_RENDER_PASS_STRIPED_EXTENSION_NAME"
pattern ARM_RENDER_PASS_STRIPED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_RENDER_PASS_STRIPED_EXTENSION_NAME = "VK_ARM_render_pass_striped"

