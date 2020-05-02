{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_AMD_display_native_hdr  ( setLocalDimmingAMD
                                                             , DisplayNativeHdrSurfaceCapabilitiesAMD(..)
                                                             , SwapchainDisplayNativeHdrCreateInfoAMD(..)
                                                             , AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
                                                             , pattern AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
                                                             , AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
                                                             , pattern AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
                                                             , SwapchainKHR(..)
                                                             , ColorSpaceKHR(..)
                                                             ) where

import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.BaseType (Bool32(..))
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkSetLocalDimmingAMD))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.Extensions.Handles (SwapchainKHR)
import Graphics.Vulkan.Extensions.Handles (SwapchainKHR(..))
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD))
import Graphics.Vulkan.Extensions.VK_EXT_swapchain_colorspace (ColorSpaceKHR(..))
import Graphics.Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetLocalDimmingAMD
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Bool32 -> IO ()) -> Ptr Device_T -> SwapchainKHR -> Bool32 -> IO ()

-- | vkSetLocalDimmingAMD - Set Local Dimming
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapChain@.
--
-- -   @swapChain@ handle to enable local dimming.
--
-- -   @localDimmingEnable@ specifies whether local dimming is enabled for
--     the swapchain.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @swapChain@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   Both of @device@, and @swapChain@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Instance'
--
-- == Valid Usage
--
-- -   It is only valid to call 'setLocalDimmingAMD' if
--     'DisplayNativeHdrSurfaceCapabilitiesAMD'::@localDimmingSupport@ is
--     supported
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Extensions.Handles.SwapchainKHR'
setLocalDimmingAMD :: forall io . MonadIO io => Device -> SwapchainKHR -> ("localDimmingEnable" ::: Bool) -> io ()
setLocalDimmingAMD device swapChain localDimmingEnable = liftIO $ do
  let vkSetLocalDimmingAMD' = mkVkSetLocalDimmingAMD (pVkSetLocalDimmingAMD (deviceCmds (device :: Device)))
  vkSetLocalDimmingAMD' (deviceHandle (device)) (swapChain) (boolToBool32 (localDimmingEnable))
  pure $ ()


-- | VkDisplayNativeHdrSurfaceCapabilitiesAMD - Structure describing display
-- native HDR specific capabilities of a surface
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DisplayNativeHdrSurfaceCapabilitiesAMD = DisplayNativeHdrSurfaceCapabilitiesAMD
  { -- | @localDimmingSupport@ specifies whether the surface supports local
    -- dimming. If this is 'Graphics.Vulkan.Core10.BaseType.TRUE',
    -- 'SwapchainDisplayNativeHdrCreateInfoAMD' /can/ be used to explicitly
    -- enable or disable local dimming for the surface. Local dimming may also
    -- be overriden by 'setLocalDimmingAMD' during the lifetime of the
    -- swapchain.
    localDimmingSupport :: Bool }
  deriving (Typeable)
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
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'
-- does not include this structure, the default value for
-- @localDimmingEnable@ is 'Graphics.Vulkan.Core10.BaseType.TRUE', meaning
-- local dimming is initially enabled for the swapchain.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD'
--
-- == Valid Usage
--
-- -   It is only valid to set @localDimmingEnable@ to
--     'Graphics.Vulkan.Core10.BaseType.TRUE' if
--     'DisplayNativeHdrSurfaceCapabilitiesAMD'::@localDimmingSupport@ is
--     supported
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainDisplayNativeHdrCreateInfoAMD = SwapchainDisplayNativeHdrCreateInfoAMD
  { -- | @localDimmingEnable@ specifies whether local dimming is enabled for the
    -- swapchain.
    localDimmingEnable :: Bool }
  deriving (Typeable)
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

