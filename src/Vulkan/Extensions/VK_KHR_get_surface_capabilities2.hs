{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_get_surface_capabilities2"
module Vulkan.Extensions.VK_KHR_get_surface_capabilities2  ( getPhysicalDeviceSurfaceCapabilities2KHR
                                                           , getPhysicalDeviceSurfaceFormats2KHR
                                                           , PhysicalDeviceSurfaceInfo2KHR(..)
                                                           , SurfaceCapabilities2KHR(..)
                                                           , SurfaceFormat2KHR(..)
                                                           , KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION
                                                           , pattern KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION
                                                           , KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
                                                           , pattern KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
                                                           , SurfaceKHR(..)
                                                           , SurfaceCapabilitiesKHR(..)
                                                           , SurfaceFormatKHR(..)
                                                           , ColorSpaceKHR(..)
                                                           , CompositeAlphaFlagBitsKHR(..)
                                                           , CompositeAlphaFlagsKHR
                                                           , SurfaceTransformFlagBitsKHR(..)
                                                           , SurfaceTransformFlagsKHR
                                                           ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_display_native_hdr (DisplayNativeHdrSurfaceCapabilitiesAMD)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceCapabilities2KHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceFormats2KHR))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shared_presentable_image (SharedPresentSurfaceCapabilitiesKHR)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceCapabilitiesFullScreenExclusiveEXT)
import Vulkan.Extensions.VK_KHR_surface (SurfaceCapabilitiesKHR)
import Vulkan.Extensions.VK_KHR_surface (SurfaceFormatKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceFullScreenExclusiveInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceFullScreenExclusiveWin32InfoEXT)
import Vulkan.Extensions.Handles (SurfaceKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_surface_protected_capabilities (SurfaceProtectedCapabilitiesKHR)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_surface (ColorSpaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagsKHR)
import Vulkan.Extensions.VK_KHR_surface (SurfaceCapabilitiesKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceFormatKHR(..))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceCapabilities2KHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr (SomeStruct SurfaceCapabilities2KHR) -> IO Result) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr (SomeStruct SurfaceCapabilities2KHR) -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceCapabilities2KHR"
getPhysicalDeviceSurfaceCapabilities2KHR :: forall a b io
                                          . (Extendss PhysicalDeviceSurfaceInfo2KHR a, Extendss SurfaceCapabilities2KHR b, PokeChain a, PokeChain b, PeekChain b, MonadIO io)
                                         => -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceCapabilities2KHR" "physicalDevice"
                                            PhysicalDevice
                                         -> -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceCapabilities2KHR" "pSurfaceInfo"
                                            (PhysicalDeviceSurfaceInfo2KHR a)
                                         -> io (SurfaceCapabilities2KHR b)
getPhysicalDeviceSurfaceCapabilities2KHR physicalDevice surfaceInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceCapabilities2KHRPtr = pVkGetPhysicalDeviceSurfaceCapabilities2KHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfaceCapabilities2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfaceCapabilities2KHR is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfaceCapabilities2KHR' = mkVkGetPhysicalDeviceSurfaceCapabilities2KHR vkGetPhysicalDeviceSurfaceCapabilities2KHRPtr
  pSurfaceInfo <- ContT $ withCStruct (surfaceInfo)
  pPSurfaceCapabilities <- ContT (withZeroCStruct @(SurfaceCapabilities2KHR _))
  r <- lift $ vkGetPhysicalDeviceSurfaceCapabilities2KHR' (physicalDeviceHandle (physicalDevice)) (forgetExtensions pSurfaceInfo) (forgetExtensions (pPSurfaceCapabilities))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceCapabilities <- lift $ peekCStruct @(SurfaceCapabilities2KHR _) pPSurfaceCapabilities
  pure $ (pSurfaceCapabilities)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceFormats2KHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr Word32 -> Ptr SurfaceFormat2KHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr Word32 -> Ptr SurfaceFormat2KHR -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceFormats2KHR"
getPhysicalDeviceSurfaceFormats2KHR :: forall a io
                                     . (Extendss PhysicalDeviceSurfaceInfo2KHR a, PokeChain a, MonadIO io)
                                    => -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceFormats2KHR" "physicalDevice"
                                       PhysicalDevice
                                    -> -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceFormats2KHR" "pSurfaceInfo"
                                       (PhysicalDeviceSurfaceInfo2KHR a)
                                    -> io (Result, ("surfaceFormats" ::: Vector SurfaceFormat2KHR))
getPhysicalDeviceSurfaceFormats2KHR physicalDevice surfaceInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceFormats2KHRPtr = pVkGetPhysicalDeviceSurfaceFormats2KHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfaceFormats2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfaceFormats2KHR is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfaceFormats2KHR' = mkVkGetPhysicalDeviceSurfaceFormats2KHR vkGetPhysicalDeviceSurfaceFormats2KHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pSurfaceInfo <- ContT $ withCStruct (surfaceInfo)
  let x9 = forgetExtensions pSurfaceInfo
  pPSurfaceFormatCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceSurfaceFormats2KHR' physicalDevice' x9 (pPSurfaceFormatCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceFormatCount <- lift $ peek @Word32 pPSurfaceFormatCount
  pPSurfaceFormats <- ContT $ bracket (callocBytes @SurfaceFormat2KHR ((fromIntegral (pSurfaceFormatCount)) * 24)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPSurfaceFormats `advancePtrBytes` (i * 24) :: Ptr SurfaceFormat2KHR) . ($ ())) [0..(fromIntegral (pSurfaceFormatCount)) - 1]
  r' <- lift $ vkGetPhysicalDeviceSurfaceFormats2KHR' physicalDevice' x9 (pPSurfaceFormatCount) ((pPSurfaceFormats))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pSurfaceFormatCount' <- lift $ peek @Word32 pPSurfaceFormatCount
  pSurfaceFormats' <- lift $ generateM (fromIntegral (pSurfaceFormatCount')) (\i -> peekCStruct @SurfaceFormat2KHR (((pPSurfaceFormats) `advancePtrBytes` (24 * (i)) :: Ptr SurfaceFormat2KHR)))
  pure $ ((r'), pSurfaceFormats')



-- No documentation found for TopLevel "VkPhysicalDeviceSurfaceInfo2KHR"
data PhysicalDeviceSurfaceInfo2KHR (es :: [Type]) = PhysicalDeviceSurfaceInfo2KHR
  { -- No documentation found for Nested "VkPhysicalDeviceSurfaceInfo2KHR" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkPhysicalDeviceSurfaceInfo2KHR" "surface"
    surface :: SurfaceKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSurfaceInfo2KHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PhysicalDeviceSurfaceInfo2KHR es)

instance Extensible PhysicalDeviceSurfaceInfo2KHR where
  extensibleType = STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
  setNext x next = x{next = next}
  getNext PhysicalDeviceSurfaceInfo2KHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PhysicalDeviceSurfaceInfo2KHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SurfaceFullScreenExclusiveWin32InfoEXT = Just f
    | Just Refl <- eqT @e @SurfaceFullScreenExclusiveInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss PhysicalDeviceSurfaceInfo2KHR es, PokeChain es) => ToCStruct (PhysicalDeviceSurfaceInfo2KHR es) where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSurfaceInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SurfaceKHR)) (surface)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr SurfaceKHR)) (zero)
    lift $ f

instance (Extendss PhysicalDeviceSurfaceInfo2KHR es, PeekChain es) => FromCStruct (PhysicalDeviceSurfaceInfo2KHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    surface <- peek @SurfaceKHR ((p `plusPtr` 16 :: Ptr SurfaceKHR))
    pure $ PhysicalDeviceSurfaceInfo2KHR
             next surface

instance es ~ '[] => Zero (PhysicalDeviceSurfaceInfo2KHR es) where
  zero = PhysicalDeviceSurfaceInfo2KHR
           ()
           zero



-- No documentation found for TopLevel "VkSurfaceCapabilities2KHR"
data SurfaceCapabilities2KHR (es :: [Type]) = SurfaceCapabilities2KHR
  { -- No documentation found for Nested "VkSurfaceCapabilities2KHR" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkSurfaceCapabilities2KHR" "surfaceCapabilities"
    surfaceCapabilities :: SurfaceCapabilitiesKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceCapabilities2KHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SurfaceCapabilities2KHR es)

instance Extensible SurfaceCapabilities2KHR where
  extensibleType = STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
  setNext x next = x{next = next}
  getNext SurfaceCapabilities2KHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SurfaceCapabilities2KHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SurfaceCapabilitiesFullScreenExclusiveEXT = Just f
    | Just Refl <- eqT @e @SurfaceProtectedCapabilitiesKHR = Just f
    | Just Refl <- eqT @e @SharedPresentSurfaceCapabilitiesKHR = Just f
    | Just Refl <- eqT @e @DisplayNativeHdrSurfaceCapabilitiesAMD = Just f
    | otherwise = Nothing

instance (Extendss SurfaceCapabilities2KHR es, PokeChain es) => ToCStruct (SurfaceCapabilities2KHR es) where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilities2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SurfaceCapabilitiesKHR)) (surfaceCapabilities)
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr SurfaceCapabilitiesKHR)) (zero)
    lift $ f

instance (Extendss SurfaceCapabilities2KHR es, PeekChain es) => FromCStruct (SurfaceCapabilities2KHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    surfaceCapabilities <- peekCStruct @SurfaceCapabilitiesKHR ((p `plusPtr` 16 :: Ptr SurfaceCapabilitiesKHR))
    pure $ SurfaceCapabilities2KHR
             next surfaceCapabilities

instance es ~ '[] => Zero (SurfaceCapabilities2KHR es) where
  zero = SurfaceCapabilities2KHR
           ()
           zero



-- No documentation found for TopLevel "VkSurfaceFormat2KHR"
data SurfaceFormat2KHR = SurfaceFormat2KHR
  { -- No documentation found for Nested "VkSurfaceFormat2KHR" "surfaceFormat"
    surfaceFormat :: SurfaceFormatKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceFormat2KHR)
#endif
deriving instance Show SurfaceFormat2KHR

instance ToCStruct SurfaceFormat2KHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceFormat2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SurfaceFormatKHR)) (surfaceFormat)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SurfaceFormatKHR)) (zero)
    f

instance FromCStruct SurfaceFormat2KHR where
  peekCStruct p = do
    surfaceFormat <- peekCStruct @SurfaceFormatKHR ((p `plusPtr` 16 :: Ptr SurfaceFormatKHR))
    pure $ SurfaceFormat2KHR
             surfaceFormat


instance Storable SurfaceFormat2KHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceFormat2KHR where
  zero = SurfaceFormat2KHR
           zero


type KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION"
pattern KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1


type KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME = "VK_KHR_get_surface_capabilities2"

-- No documentation found for TopLevel "VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME"
pattern KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME = "VK_KHR_get_surface_capabilities2"

