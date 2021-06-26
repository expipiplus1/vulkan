{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_depth_stencil_resolve"
module Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve  ( PhysicalDeviceDepthStencilResolveProperties(..)
                                                                 , SubpassDescriptionDepthStencilResolve(..)
                                                                 , StructureType(..)
                                                                 , ResolveModeFlagBits(..)
                                                                 , ResolveModeFlags
                                                                 ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (AttachmentReference2)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE))
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits(..))
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceDepthStencilResolveProperties - Structure describing
-- depth\/stencil resolve properties that can be supported by an
-- implementation
--
-- = Description
--
-- If the 'PhysicalDeviceDepthStencilResolveProperties' structure is
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
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDepthStencilResolveProperties = PhysicalDeviceDepthStencilResolveProperties
  { -- | #extension-features-depthResolveModes# @supportedDepthResolveModes@ is a
    -- bitmask of 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits'
    -- indicating the set of supported depth resolve modes.
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_SAMPLE_ZERO_BIT'
    -- /must/ be included in the set but implementations /may/ support
    -- additional modes.
    supportedDepthResolveModes :: ResolveModeFlags
  , -- | #extension-features-stencilResolveModes# @supportedStencilResolveModes@
    -- is a bitmask of
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' indicating
    -- the set of supported stencil resolve modes.
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_SAMPLE_ZERO_BIT'
    -- /must/ be included in the set but implementations /may/ support
    -- additional modes.
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_AVERAGE_BIT'
    -- /must/ not be included in the set.
    supportedStencilResolveModes :: ResolveModeFlags
  , -- | #extension-features-independentResolveNone# @independentResolveNone@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports
    -- setting the depth and stencil resolve modes to different values when one
    -- of those modes is
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'. Otherwise
    -- the implementation only supports setting both modes to the same value.
    independentResolveNone :: Bool
  , -- | #extension-features-independentResolve# @independentResolve@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports all
    -- combinations of the supported depth and stencil resolve modes, including
    -- setting either depth or stencil resolve mode to
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'. An
    -- implementation that supports @independentResolve@ /must/ also support
    -- @independentResolveNone@.
    independentResolve :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDepthStencilResolveProperties)
#endif
deriving instance Show PhysicalDeviceDepthStencilResolveProperties

instance ToCStruct PhysicalDeviceDepthStencilResolveProperties where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDepthStencilResolveProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ResolveModeFlags)) (supportedDepthResolveModes)
    poke ((p `plusPtr` 20 :: Ptr ResolveModeFlags)) (supportedStencilResolveModes)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (independentResolveNone))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (independentResolve))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ResolveModeFlags)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ResolveModeFlags)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDepthStencilResolveProperties where
  peekCStruct p = do
    supportedDepthResolveModes <- peek @ResolveModeFlags ((p `plusPtr` 16 :: Ptr ResolveModeFlags))
    supportedStencilResolveModes <- peek @ResolveModeFlags ((p `plusPtr` 20 :: Ptr ResolveModeFlags))
    independentResolveNone <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    independentResolve <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pure $ PhysicalDeviceDepthStencilResolveProperties
             supportedDepthResolveModes supportedStencilResolveModes (bool32ToBool independentResolveNone) (bool32ToBool independentResolve)

instance Storable PhysicalDeviceDepthStencilResolveProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDepthStencilResolveProperties where
  zero = PhysicalDeviceDepthStencilResolveProperties
           zero
           zero
           zero
           zero


-- | VkSubpassDescriptionDepthStencilResolve - Structure specifying
-- depth\/stencil resolve operations for a subpass
--
-- == Valid Usage
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-pDepthStencilResolveAttachment-03177#
--     If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @pDepthStencilAttachment@ /must/ not have the value
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-pDepthStencilResolveAttachment-03178#
--     If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @depthResolveMode@ and @stencilResolveMode@ /must/ not both be
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-pDepthStencilResolveAttachment-03179#
--     If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @pDepthStencilAttachment@ /must/ not have a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-pDepthStencilResolveAttachment-03180#
--     If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @pDepthStencilResolveAttachment@ /must/ have a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-pDepthStencilResolveAttachment-02651#
--     If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' then it
--     /must/ have a format whose features contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-pDepthStencilResolveAttachment-03181#
--     If the 'Vulkan.Core10.Enums.Format.Format' of
--     @pDepthStencilResolveAttachment@ has a depth component, then the
--     'Vulkan.Core10.Enums.Format.Format' of @pDepthStencilAttachment@
--     /must/ have a depth component with the same number of bits and
--     numerical type
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-pDepthStencilResolveAttachment-03182#
--     If the 'Vulkan.Core10.Enums.Format.Format' of
--     @pDepthStencilResolveAttachment@ has a stencil component, then the
--     'Vulkan.Core10.Enums.Format.Format' of @pDepthStencilAttachment@
--     /must/ have a stencil component with the same number of bits and
--     numerical type
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-depthResolveMode-03183#
--     The value of @depthResolveMode@ /must/ be one of the bits set in
--     'PhysicalDeviceDepthStencilResolveProperties'::@supportedDepthResolveModes@
--     or 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-stencilResolveMode-03184#
--     The value of @stencilResolveMode@ /must/ be one of the bits set in
--     'PhysicalDeviceDepthStencilResolveProperties'::@supportedStencilResolveModes@
--     or 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-pDepthStencilResolveAttachment-03185#
--     If the 'Vulkan.Core10.Enums.Format.Format' of
--     @pDepthStencilResolveAttachment@ has both depth and stencil
--     components,
--     'PhysicalDeviceDepthStencilResolveProperties'::@independentResolve@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', and
--     'PhysicalDeviceDepthStencilResolveProperties'::@independentResolveNone@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', then the values of
--     @depthResolveMode@ and @stencilResolveMode@ /must/ be identical
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-pDepthStencilResolveAttachment-03186#
--     If the 'Vulkan.Core10.Enums.Format.Format' of
--     @pDepthStencilResolveAttachment@ has both depth and stencil
--     components,
--     'PhysicalDeviceDepthStencilResolveProperties'::@independentResolve@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE' and
--     'PhysicalDeviceDepthStencilResolveProperties'::@independentResolveNone@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', then the values of
--     @depthResolveMode@ and @stencilResolveMode@ /must/ be identical or
--     one of them /must/ be
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-pDepthStencilResolveAttachment-04588#
--     If the 'Vulkan.Core10.Enums.Format.Format' of
--     @pDepthStencilResolveAttachment@ has a depth component,
--     @depthResolveMode@ /must/ be a valid
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' value
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-pDepthStencilResolveAttachment-04589#
--     If the 'Vulkan.Core10.Enums.Format.Format' of
--     @pDepthStencilResolveAttachment@ has a stencil component,
--     @stencilResolveMode@ /must/ be a valid
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' value
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE'
--
-- -   #VUID-VkSubpassDescriptionDepthStencilResolve-pDepthStencilResolveAttachment-parameter#
--     If @pDepthStencilResolveAttachment@ is not @NULL@,
--     @pDepthStencilResolveAttachment@ /must/ be a valid pointer to a
--     valid
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentReference2'
--     structure
--
-- = See Also
--
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentReference2',
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SubpassDescriptionDepthStencilResolve = SubpassDescriptionDepthStencilResolve
  { -- | @depthResolveMode@ is a
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' value
    -- describing the depth resolve mode.
    depthResolveMode :: ResolveModeFlagBits
  , -- | @stencilResolveMode@ is a
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' value
    -- describing the stencil resolve mode.
    stencilResolveMode :: ResolveModeFlagBits
  , -- | @pDepthStencilResolveAttachment@ is @NULL@ or a pointer to a
    -- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentReference2'
    -- structure defining the depth\/stencil resolve attachment for this
    -- subpass and its layout.
    depthStencilResolveAttachment :: Maybe (SomeStruct AttachmentReference2)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassDescriptionDepthStencilResolve)
#endif
deriving instance Show SubpassDescriptionDepthStencilResolve

instance ToCStruct SubpassDescriptionDepthStencilResolve where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassDescriptionDepthStencilResolve{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr ResolveModeFlagBits)) (depthResolveMode)
    lift $ poke ((p `plusPtr` 20 :: Ptr ResolveModeFlagBits)) (stencilResolveMode)
    pDepthStencilResolveAttachment'' <- case (depthStencilResolveAttachment) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (AttachmentReference2 '[])) $ \cont -> withSomeCStruct @AttachmentReference2 (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (AttachmentReference2 _)))) pDepthStencilResolveAttachment''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ResolveModeFlagBits)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ResolveModeFlagBits)) (zero)
    f

instance FromCStruct SubpassDescriptionDepthStencilResolve where
  peekCStruct p = do
    depthResolveMode <- peek @ResolveModeFlagBits ((p `plusPtr` 16 :: Ptr ResolveModeFlagBits))
    stencilResolveMode <- peek @ResolveModeFlagBits ((p `plusPtr` 20 :: Ptr ResolveModeFlagBits))
    pDepthStencilResolveAttachment <- peek @(Ptr (AttachmentReference2 _)) ((p `plusPtr` 24 :: Ptr (Ptr (AttachmentReference2 _))))
    pDepthStencilResolveAttachment' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pDepthStencilResolveAttachment
    pure $ SubpassDescriptionDepthStencilResolve
             depthResolveMode stencilResolveMode pDepthStencilResolveAttachment'

instance Zero SubpassDescriptionDepthStencilResolve where
  zero = SubpassDescriptionDepthStencilResolve
           zero
           zero
           Nothing

