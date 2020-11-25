{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_depth_stencil_resolve"
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (AttachmentReference2)
import Vulkan.Core10.FundamentalTypes (Bool32)
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

-- No documentation found for TopLevel "VkPhysicalDeviceDepthStencilResolveProperties"
data PhysicalDeviceDepthStencilResolveProperties = PhysicalDeviceDepthStencilResolveProperties
  { -- No documentation found for Nested "VkPhysicalDeviceDepthStencilResolveProperties" "supportedDepthResolveModes"
    supportedDepthResolveModes :: ResolveModeFlags
  , -- No documentation found for Nested "VkPhysicalDeviceDepthStencilResolveProperties" "supportedStencilResolveModes"
    supportedStencilResolveModes :: ResolveModeFlags
  , -- No documentation found for Nested "VkPhysicalDeviceDepthStencilResolveProperties" "independentResolveNone"
    independentResolveNone :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDepthStencilResolveProperties" "independentResolve"
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



-- No documentation found for TopLevel "VkSubpassDescriptionDepthStencilResolve"
data SubpassDescriptionDepthStencilResolve = SubpassDescriptionDepthStencilResolve
  { -- No documentation found for Nested "VkSubpassDescriptionDepthStencilResolve" "depthResolveMode"
    depthResolveMode :: ResolveModeFlagBits
  , -- No documentation found for Nested "VkSubpassDescriptionDepthStencilResolve" "stencilResolveMode"
    stencilResolveMode :: ResolveModeFlagBits
  , -- No documentation found for Nested "VkSubpassDescriptionDepthStencilResolve" "pDepthStencilResolveAttachment"
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

