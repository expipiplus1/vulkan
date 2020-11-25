{-# language CPP #-}
-- No documentation found for Chapter "VK_AMD_display_native_hdr"
module Vulkan.Extensions.VK_AMD_display_native_hdr  ( setLocalDimmingAMD
                                                    , DisplayNativeHdrSurfaceCapabilitiesAMD(..)
                                                    , SwapchainDisplayNativeHdrCreateInfoAMD(..)
                                                    , AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
                                                    , pattern AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
                                                    , AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
                                                    , pattern AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
                                                    , SwapchainKHR(..)
                                                    , ColorSpaceKHR(..)
                                                    ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkSetLocalDimmingAMD))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD))
import Vulkan.Extensions.VK_KHR_surface (ColorSpaceKHR(..))
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetLocalDimmingAMD
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Bool32 -> IO ()) -> Ptr Device_T -> SwapchainKHR -> Bool32 -> IO ()

-- No documentation found for TopLevel "vkSetLocalDimmingAMD"
setLocalDimmingAMD :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkSetLocalDimmingAMD" "device"
                      Device
                   -> -- No documentation found for Nested "vkSetLocalDimmingAMD" "swapChain"
                      SwapchainKHR
                   -> -- No documentation found for Nested "vkSetLocalDimmingAMD" "localDimmingEnable"
                      ("localDimmingEnable" ::: Bool)
                   -> io ()
setLocalDimmingAMD device swapChain localDimmingEnable = liftIO $ do
  let vkSetLocalDimmingAMDPtr = pVkSetLocalDimmingAMD (deviceCmds (device :: Device))
  unless (vkSetLocalDimmingAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetLocalDimmingAMD is null" Nothing Nothing
  let vkSetLocalDimmingAMD' = mkVkSetLocalDimmingAMD vkSetLocalDimmingAMDPtr
  vkSetLocalDimmingAMD' (deviceHandle (device)) (swapChain) (boolToBool32 (localDimmingEnable))
  pure $ ()



-- No documentation found for TopLevel "VkDisplayNativeHdrSurfaceCapabilitiesAMD"
data DisplayNativeHdrSurfaceCapabilitiesAMD = DisplayNativeHdrSurfaceCapabilitiesAMD
  { -- No documentation found for Nested "VkDisplayNativeHdrSurfaceCapabilitiesAMD" "localDimmingSupport"
    localDimmingSupport :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayNativeHdrSurfaceCapabilitiesAMD)
#endif
deriving instance Show DisplayNativeHdrSurfaceCapabilitiesAMD

instance ToCStruct DisplayNativeHdrSurfaceCapabilitiesAMD where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayNativeHdrSurfaceCapabilitiesAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (localDimmingSupport))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct DisplayNativeHdrSurfaceCapabilitiesAMD where
  peekCStruct p = do
    localDimmingSupport <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ DisplayNativeHdrSurfaceCapabilitiesAMD
             (bool32ToBool localDimmingSupport)


instance Storable DisplayNativeHdrSurfaceCapabilitiesAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayNativeHdrSurfaceCapabilitiesAMD where
  zero = DisplayNativeHdrSurfaceCapabilitiesAMD
           zero



-- No documentation found for TopLevel "VkSwapchainDisplayNativeHdrCreateInfoAMD"
data SwapchainDisplayNativeHdrCreateInfoAMD = SwapchainDisplayNativeHdrCreateInfoAMD
  { -- No documentation found for Nested "VkSwapchainDisplayNativeHdrCreateInfoAMD" "localDimmingEnable"
    localDimmingEnable :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainDisplayNativeHdrCreateInfoAMD)
#endif
deriving instance Show SwapchainDisplayNativeHdrCreateInfoAMD

instance ToCStruct SwapchainDisplayNativeHdrCreateInfoAMD where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainDisplayNativeHdrCreateInfoAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (localDimmingEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SwapchainDisplayNativeHdrCreateInfoAMD where
  peekCStruct p = do
    localDimmingEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ SwapchainDisplayNativeHdrCreateInfoAMD
             (bool32ToBool localDimmingEnable)


instance Storable SwapchainDisplayNativeHdrCreateInfoAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainDisplayNativeHdrCreateInfoAMD where
  zero = SwapchainDisplayNativeHdrCreateInfoAMD
           zero


type AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION"
pattern AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION = 1


type AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME = "VK_AMD_display_native_hdr"

-- No documentation found for TopLevel "VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME"
pattern AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME = "VK_AMD_display_native_hdr"

