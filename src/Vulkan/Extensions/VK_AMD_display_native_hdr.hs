{-# language CPP #-}
-- | = Name
--
-- VK_AMD_display_native_hdr - device extension
--
-- == VK_AMD_display_native_hdr
--
-- [__Name String__]
--     @VK_AMD_display_native_hdr@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     214
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
--     -   Requires @VK_KHR_get_surface_capabilities2@
--
--     -   Requires @VK_KHR_swapchain@
--
-- [__Contact__]
--
--     -   Matthaeus G. Chajdas
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_AMD_display_native_hdr:%20&body=@anteru%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-12-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Aaron Hagan, AMD
--
--     -   Aric Cyr, AMD
--
--     -   Timothy Lottes, AMD
--
--     -   Derrick Owens, AMD
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- This extension introduces the following display native HDR features to
-- Vulkan:
--
-- -   A new 'Vulkan.Extensions.VK_KHR_surface.ColorSpaceKHR' enum for
--     setting the native display colorspace. For example, this color space
--     would be set by the swapchain to use the native color space in
--     Freesync2 displays.
--
-- -   Local dimming control
--
-- == New Commands
--
-- -   'setLocalDimmingAMD'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'DisplayNativeHdrSurfaceCapabilitiesAMD'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR':
--
--     -   'SwapchainDisplayNativeHdrCreateInfoAMD'
--
-- == New Enum Constants
--
-- -   'AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME'
--
-- -   'AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_surface.ColorSpaceKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_DISPLAY_NATIVE_AMD'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD'
--
-- == Issues
--
-- None.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2018-12-18 (Daniel Rakos)
--
--     -   Initial revision
--
-- = See Also
--
-- 'DisplayNativeHdrSurfaceCapabilitiesAMD',
-- 'SwapchainDisplayNativeHdrCreateInfoAMD', 'setLocalDimmingAMD'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_display_native_hdr Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
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
-- -   #VUID-vkSetLocalDimmingAMD-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkSetLocalDimmingAMD-swapChain-parameter# @swapChain@ /must/
--     be a valid 'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-vkSetLocalDimmingAMD-commonparent# Both of @device@, and
--     @swapChain@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Instance'
--
-- == Valid Usage
--
-- -   #VUID-vkSetLocalDimmingAMD-localDimmingSupport-04618#
--     'DisplayNativeHdrSurfaceCapabilitiesAMD'::@localDimmingSupport@
--     /must/ be supported
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'Vulkan.Core10.Handles.Device',
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
  traceAroundEvent "vkSetLocalDimmingAMD" (vkSetLocalDimmingAMD' (deviceHandle (device)) (swapChain) (boolToBool32 (localDimmingEnable)))
  pure $ ()


-- | VkDisplayNativeHdrSurfaceCapabilitiesAMD - Structure describing display
-- native HDR specific capabilities of a surface
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DisplayNativeHdrSurfaceCapabilitiesAMD = DisplayNativeHdrSurfaceCapabilitiesAMD
  { -- | @localDimmingSupport@ specifies whether the surface supports local
    -- dimming. If this is 'Vulkan.Core10.FundamentalTypes.TRUE',
    -- 'SwapchainDisplayNativeHdrCreateInfoAMD' /can/ be used to explicitly
    -- enable or disable local dimming for the surface. Local dimming may also
    -- be overriden by 'setLocalDimmingAMD' during the lifetime of the
    -- swapchain.
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


-- | VkSwapchainDisplayNativeHdrCreateInfoAMD - Structure specifying display
-- native HDR parameters of a newly created swapchain object
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR' does not
-- include this structure, the default value for @localDimmingEnable@ is
-- 'Vulkan.Core10.FundamentalTypes.TRUE', meaning local dimming is
-- initially enabled for the swapchain.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSwapchainDisplayNativeHdrCreateInfoAMD-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD'
--
-- == Valid Usage
--
-- -   #VUID-VkSwapchainDisplayNativeHdrCreateInfoAMD-localDimmingEnable-04449#
--     It is only valid to set @localDimmingEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE' if
--     'DisplayNativeHdrSurfaceCapabilitiesAMD'::@localDimmingSupport@ is
--     supported
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainDisplayNativeHdrCreateInfoAMD = SwapchainDisplayNativeHdrCreateInfoAMD
  { -- | @localDimmingEnable@ specifies whether local dimming is enabled for the
    -- swapchain.
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

