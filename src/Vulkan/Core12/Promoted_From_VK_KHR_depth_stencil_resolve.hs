{-# language CPP #-}
module Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve  ( PhysicalDeviceDepthStencilResolveProperties(..)
                                                                 , SubpassDescriptionDepthStencilResolve(..)
                                                                 , StructureType(..)
                                                                 , ResolveModeFlagBits(..)
                                                                 , ResolveModeFlags
                                                                 ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (AttachmentReference2)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE))
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits(..))
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceDepthStencilResolveProperties - Structure describing
-- depth\/stencil resolve properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceDepthStencilResolveProperties'
-- structure describe the following implementation-dependent limits:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDepthStencilResolveProperties = PhysicalDeviceDepthStencilResolveProperties
  { -- | @supportedDepthResolveModes@ is a bitmask of
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' indicating
    -- the set of supported depth resolve modes.
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_SAMPLE_ZERO_BIT'
    -- /must/ be included in the set but implementations /may/ support
    -- additional modes.
    supportedDepthResolveModes :: ResolveModeFlags
  , -- | @supportedStencilResolveModes@ is a bitmask of
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' indicating
    -- the set of supported stencil resolve modes.
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_SAMPLE_ZERO_BIT'
    -- /must/ be included in the set but implementations /may/ support
    -- additional modes.
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_AVERAGE_BIT'
    -- /must/ not be included in the set.
    supportedStencilResolveModes :: ResolveModeFlags
  , -- | @independentResolveNone@ is 'Vulkan.Core10.BaseType.TRUE' if the
    -- implementation supports setting the depth and stencil resolve modes to
    -- different values when one of those modes is
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'. Otherwise
    -- the implementation only supports setting both modes to the same value.
    independentResolveNone :: Bool
  , -- | @independentResolve@ is 'Vulkan.Core10.BaseType.TRUE' if the
    -- implementation supports all combinations of the supported depth and
    -- stencil resolve modes, including setting either depth or stencil resolve
    -- mode to 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'. An
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
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
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
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @pDepthStencilAttachment@ /must/ not have the value
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @depthResolveMode@ and @stencilResolveMode@ /must/ not both be
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @pDepthStencilAttachment@ /must/ not have a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED',
--     @pDepthStencilResolveAttachment@ /must/ have a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' then it
--     /must/ have a format whose features contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   If the 'Vulkan.Core10.Enums.Format.Format' of
--     @pDepthStencilResolveAttachment@ has a depth component, then the
--     'Vulkan.Core10.Enums.Format.Format' of @pDepthStencilAttachment@
--     /must/ have a depth component with the same number of bits and
--     numerical type
--
-- -   If the 'Vulkan.Core10.Enums.Format.Format' of
--     @pDepthStencilResolveAttachment@ has a stencil component, then the
--     'Vulkan.Core10.Enums.Format.Format' of @pDepthStencilAttachment@
--     /must/ have a stencil component with the same number of bits and
--     numerical type
--
-- -   The value of @depthResolveMode@ /must/ be one of the bits set in
--     'PhysicalDeviceDepthStencilResolveProperties'::@supportedDepthResolveModes@
--     or 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'
--
-- -   The value of @stencilResolveMode@ /must/ be one of the bits set in
--     'PhysicalDeviceDepthStencilResolveProperties'::@supportedStencilResolveModes@
--     or 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'
--
-- -   If the 'Vulkan.Core10.Enums.Format.Format' of
--     @pDepthStencilResolveAttachment@ has both depth and stencil
--     components,
--     'PhysicalDeviceDepthStencilResolveProperties'::@independentResolve@
--     is 'Vulkan.Core10.BaseType.FALSE', and
--     'PhysicalDeviceDepthStencilResolveProperties'::@independentResolveNone@
--     is 'Vulkan.Core10.BaseType.FALSE', then the values of
--     @depthResolveMode@ and @stencilResolveMode@ /must/ be identical
--
-- -   If the 'Vulkan.Core10.Enums.Format.Format' of
--     @pDepthStencilResolveAttachment@ has both depth and stencil
--     components,
--     'PhysicalDeviceDepthStencilResolveProperties'::@independentResolve@
--     is 'Vulkan.Core10.BaseType.FALSE' and
--     'PhysicalDeviceDepthStencilResolveProperties'::@independentResolveNone@
--     is 'Vulkan.Core10.BaseType.TRUE', then the values of
--     @depthResolveMode@ and @stencilResolveMode@ /must/ be identical or
--     one of them /must/ be
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE'
--
-- -   @depthResolveMode@ /must/ be a valid
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' value
--
-- -   @stencilResolveMode@ /must/ be a valid
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' value
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@,
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
  { -- | @depthResolveMode@ is a bitmask of
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' describing
    -- the depth resolve mode.
    depthResolveMode :: ResolveModeFlagBits
  , -- | @stencilResolveMode@ is a bitmask of
    -- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' describing
    -- the stencil resolve mode.
    stencilResolveMode :: ResolveModeFlagBits
  , -- | @pDepthStencilResolveAttachment@ is an optional
    -- 'Vulkan.Core10.Pass.AttachmentReference' structure defining the
    -- depth\/stencil resolve attachment for this subpass and its layout.
    depthStencilResolveAttachment :: Maybe (SomeStruct AttachmentReference2)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassDescriptionDepthStencilResolve)
#endif
deriving instance Show SubpassDescriptionDepthStencilResolve

instance ToCStruct SubpassDescriptionDepthStencilResolve where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
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
    pDepthStencilResolveAttachment <- peek @(Ptr (AttachmentReference2 _)) ((p `plusPtr` 24 :: Ptr (Ptr (AttachmentReference2 a))))
    pDepthStencilResolveAttachment' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pDepthStencilResolveAttachment
    pure $ SubpassDescriptionDepthStencilResolve
             depthResolveMode stencilResolveMode pDepthStencilResolveAttachment'

instance Zero SubpassDescriptionDepthStencilResolve where
  zero = SubpassDescriptionDepthStencilResolve
           zero
           zero
           Nothing

