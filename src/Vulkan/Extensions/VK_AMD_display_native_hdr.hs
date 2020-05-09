{-# language CPP #-}
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
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.BaseType (Bool32(..))
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

-- | vkSetLocalDimmingAMD - Set Local Dimming
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @swapChain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   Both of @device@, and @swapChain@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
--
-- == Valid Usage
--
-- -   It is only valid to call 'setLocalDimmingAMD' if
--     'DisplayNativeHdrSurfaceCapabilitiesAMD'::@localDimmingSupport@ is
--     supported
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32', 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
setLocalDimmingAMD :: forall io
                    . (MonadIO io)
                   => -- | @device@ is the device associated with @swapChain@.
                      Device
                   -> -- | @swapChain@ handle to enable local dimming.
                      SwapchainKHR
                   -> -- | @localDimmingEnable@ specifies whether local dimming is enabled for the
                      -- swapchain.
                      ("localDimmingEnable" ::: Bool)
                   -> io ()
setLocalDimmingAMD device swapChain localDimmingEnable = liftIO $ do
  let vkSetLocalDimmingAMDPtr = pVkSetLocalDimmingAMD (deviceCmds (device :: Device))
  unless (vkSetLocalDimmingAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetLocalDimmingAMD is null" Nothing Nothing
  let vkSetLocalDimmingAMD' = mkVkSetLocalDimmingAMD vkSetLocalDimmingAMDPtr
  vkSetLocalDimmingAMD' (deviceHandle (device)) (swapChain) (boolToBool32 (localDimmingEnable))
  pure $ ()


-- | VkDisplayNativeHdrSurfaceCapabilitiesAMD - Structure describing display
-- native HDR specific capabilities of a surface
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DisplayNativeHdrSurfaceCapabilitiesAMD = DisplayNativeHdrSurfaceCapabilitiesAMD
  { -- | @localDimmingSupport@ specifies whether the surface supports local
    -- dimming. If this is 'Vulkan.Core10.BaseType.TRUE',
    -- 'SwapchainDisplayNativeHdrCreateInfoAMD' /can/ be used to explicitly
    -- enable or disable local dimming for the surface. Local dimming may also
    -- be overriden by 'setLocalDimmingAMD' during the lifetime of the
    -- swapchain.
    localDimmingSupport :: Bool }
  deriving (Typeable, Eq)
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


-- | VkSwapchainDisplayNativeHdrCreateInfoAMD - Structure specifying display
-- native HDR parameters of a newly created swapchain object
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR' does not
-- include this structure, the default value for @localDimmingEnable@ is
-- 'Vulkan.Core10.BaseType.TRUE', meaning local dimming is initially
-- enabled for the swapchain.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD'
--
-- == Valid Usage
--
-- -   It is only valid to set @localDimmingEnable@ to
--     'Vulkan.Core10.BaseType.TRUE' if
--     'DisplayNativeHdrSurfaceCapabilitiesAMD'::@localDimmingSupport@ is
--     supported
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainDisplayNativeHdrCreateInfoAMD = SwapchainDisplayNativeHdrCreateInfoAMD
  { -- | @localDimmingEnable@ specifies whether local dimming is enabled for the
    -- swapchain.
    localDimmingEnable :: Bool }
  deriving (Typeable, Eq)
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

