{-# language CPP #-}
-- | = Name
--
-- VK_QNX_screen_surface - instance extension
--
-- == VK_QNX_screen_surface
--
-- [__Name String__]
--     @VK_QNX_screen_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     379
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@
--
-- [__Contact__]
--
--     -   Mike Gorchak
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QNX_screen_surface] @mgorchak-blackberry%0A<<Here describe the issue or question you have about the VK_QNX_screen_surface extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-01-11
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Gorchak, BlackBerry Limited
--
-- == Description
--
-- The @VK_QNX_screen_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) that refers to a QNX
-- Screen @window@, as well as a query to determine support for rendering
-- to a QNX Screen compositor.
--
-- == New Commands
--
-- -   'createScreenSurfaceQNX'
--
-- -   'getPhysicalDeviceScreenPresentationSupportQNX'
--
-- == New Structures
--
-- -   'ScreenSurfaceCreateInfoQNX'
--
-- == New Bitmasks
--
-- -   'ScreenSurfaceCreateFlagsQNX'
--
-- == New Enum Constants
--
-- -   'QNX_SCREEN_SURFACE_EXTENSION_NAME'
--
-- -   'QNX_SCREEN_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SCREEN_SURFACE_CREATE_INFO_QNX'
--
-- == Version History
--
-- -   Revision 1, 2021-01-11 (Mike Gorchak)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'ScreenSurfaceCreateFlagsQNX', 'ScreenSurfaceCreateInfoQNX',
-- 'createScreenSurfaceQNX',
-- 'getPhysicalDeviceScreenPresentationSupportQNX'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QNX_screen_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QNX_screen_surface  ( createScreenSurfaceQNX
                                                , getPhysicalDeviceScreenPresentationSupportQNX
                                                , ScreenSurfaceCreateInfoQNX(..)
                                                , ScreenSurfaceCreateFlagsQNX(..)
                                                , QNX_SCREEN_SURFACE_SPEC_VERSION
                                                , pattern QNX_SCREEN_SURFACE_SPEC_VERSION
                                                , QNX_SCREEN_SURFACE_EXTENSION_NAME
                                                , pattern QNX_SCREEN_SURFACE_EXTENSION_NAME
                                                , Screen_window
                                                , Screen_context
                                                , SurfaceKHR(..)
                                                ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateScreenSurfaceQNX))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceScreenPresentationSupportQNX))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SCREEN_SURFACE_CREATE_INFO_QNX))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateScreenSurfaceQNX
  :: FunPtr (Ptr Instance_T -> Ptr ScreenSurfaceCreateInfoQNX -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr ScreenSurfaceCreateInfoQNX -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateScreenSurfaceQNX - Create a
-- 'Vulkan.Extensions.Handles.SurfaceKHR' object for a QNX Screen window
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateScreenSurfaceQNX-instance-parameter# @instance@ /must/
--     be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateScreenSurfaceQNX-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'ScreenSurfaceCreateInfoQNX'
--     structure
--
-- -   #VUID-vkCreateScreenSurfaceQNX-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateScreenSurfaceQNX-pSurface-parameter# @pSurface@ /must/
--     be a valid pointer to a 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QNX_screen_surface VK_QNX_screen_surface>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Instance', 'ScreenSurfaceCreateInfoQNX',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
createScreenSurfaceQNX :: forall io
                        . (MonadIO io)
                       => -- | @instance@ is the instance to associate the surface with.
                          Instance
                       -> -- | @pCreateInfo@ is a pointer to a 'ScreenSurfaceCreateInfoQNX' structure
                          -- containing parameters affecting the creation of the surface object.
                          ScreenSurfaceCreateInfoQNX
                       -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                          -- surface object when there is no more specific allocator available (see
                          -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                          ("allocator" ::: Maybe AllocationCallbacks)
                       -> io (SurfaceKHR)
createScreenSurfaceQNX instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateScreenSurfaceQNXPtr = pVkCreateScreenSurfaceQNX (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateScreenSurfaceQNXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateScreenSurfaceQNX is null" Nothing Nothing
  let vkCreateScreenSurfaceQNX' = mkVkCreateScreenSurfaceQNX vkCreateScreenSurfaceQNXPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateScreenSurfaceQNX" (vkCreateScreenSurfaceQNX' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceScreenPresentationSupportQNX
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr Screen_window -> IO Bool32) -> Ptr PhysicalDevice_T -> Word32 -> Ptr Screen_window -> IO Bool32

-- | vkGetPhysicalDeviceScreenPresentationSupportQNX - Query physical device
-- for presentation to QNX Screen
--
-- = Description
--
-- This platform-specific function /can/ be called prior to creating a
-- surface.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QNX_screen_surface VK_QNX_screen_surface>,
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceScreenPresentationSupportQNX :: forall io
                                               . (MonadIO io)
                                              => -- | @physicalDevice@ is the physical device.
                                                 --
                                                 -- #VUID-vkGetPhysicalDeviceScreenPresentationSupportQNX-physicalDevice-parameter#
                                                 -- @physicalDevice@ /must/ be a valid
                                                 -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                                 PhysicalDevice
                                              -> -- | @queueFamilyIndex@ is the queue family index.
                                                 --
                                                 -- #VUID-vkGetPhysicalDeviceScreenPresentationSupportQNX-queueFamilyIndex-04743#
                                                 -- @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
                                                 -- returned by
                                                 -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
                                                 -- for the given @physicalDevice@
                                                 ("queueFamilyIndex" ::: Word32)
                                              -> -- | @window@ is the QNX Screen @window@ object.
                                                 --
                                                 -- #VUID-vkGetPhysicalDeviceScreenPresentationSupportQNX-window-parameter#
                                                 -- @window@ /must/ be a valid pointer to a 'Screen_window' value
                                                 (Ptr Screen_window)
                                              -> io (Bool)
getPhysicalDeviceScreenPresentationSupportQNX physicalDevice queueFamilyIndex window = liftIO $ do
  let vkGetPhysicalDeviceScreenPresentationSupportQNXPtr = pVkGetPhysicalDeviceScreenPresentationSupportQNX (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkGetPhysicalDeviceScreenPresentationSupportQNXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceScreenPresentationSupportQNX is null" Nothing Nothing
  let vkGetPhysicalDeviceScreenPresentationSupportQNX' = mkVkGetPhysicalDeviceScreenPresentationSupportQNX vkGetPhysicalDeviceScreenPresentationSupportQNXPtr
  r <- traceAroundEvent "vkGetPhysicalDeviceScreenPresentationSupportQNX" (vkGetPhysicalDeviceScreenPresentationSupportQNX' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (window))
  pure $ ((bool32ToBool r))


-- | VkScreenSurfaceCreateInfoQNX - Structure specifying parameters of a
-- newly created QNX Screen surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QNX_screen_surface VK_QNX_screen_surface>,
-- 'ScreenSurfaceCreateFlagsQNX',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createScreenSurfaceQNX'
data ScreenSurfaceCreateInfoQNX = ScreenSurfaceCreateInfoQNX
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkScreenSurfaceCreateInfoQNX-flags-zerobitmask# @flags@ /must/ be
    -- @0@
    flags :: ScreenSurfaceCreateFlagsQNX
  , -- | @context@ and @window@ are QNX Screen @context@ and @window@ to
    -- associate the surface with.
    --
    -- #VUID-VkScreenSurfaceCreateInfoQNX-context-04741# @context@ /must/ point
    -- to a valid QNX Screen @struct@ _screen_context
    context :: Ptr Screen_context
  , -- | #VUID-VkScreenSurfaceCreateInfoQNX-window-04742# @window@ /must/ point
    -- to a valid QNX Screen @struct@ _screen_window
    window :: Ptr Screen_window
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ScreenSurfaceCreateInfoQNX)
#endif
deriving instance Show ScreenSurfaceCreateInfoQNX

instance ToCStruct ScreenSurfaceCreateInfoQNX where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ScreenSurfaceCreateInfoQNX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SCREEN_SURFACE_CREATE_INFO_QNX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ScreenSurfaceCreateFlagsQNX)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Screen_context))) (context)
    poke ((p `plusPtr` 32 :: Ptr (Ptr Screen_window))) (window)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SCREEN_SURFACE_CREATE_INFO_QNX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Screen_context))) (zero)
    poke ((p `plusPtr` 32 :: Ptr (Ptr Screen_window))) (zero)
    f

instance FromCStruct ScreenSurfaceCreateInfoQNX where
  peekCStruct p = do
    flags <- peek @ScreenSurfaceCreateFlagsQNX ((p `plusPtr` 16 :: Ptr ScreenSurfaceCreateFlagsQNX))
    context <- peek @(Ptr Screen_context) ((p `plusPtr` 24 :: Ptr (Ptr Screen_context)))
    window <- peek @(Ptr Screen_window) ((p `plusPtr` 32 :: Ptr (Ptr Screen_window)))
    pure $ ScreenSurfaceCreateInfoQNX
             flags context window

instance Storable ScreenSurfaceCreateInfoQNX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ScreenSurfaceCreateInfoQNX where
  zero = ScreenSurfaceCreateInfoQNX
           zero
           zero
           zero


-- | VkScreenSurfaceCreateFlagsQNX - Reserved for future use
--
-- = Description
--
-- 'ScreenSurfaceCreateFlagsQNX' is a bitmask type for setting a mask, but
-- is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QNX_screen_surface VK_QNX_screen_surface>,
-- 'ScreenSurfaceCreateInfoQNX'
newtype ScreenSurfaceCreateFlagsQNX = ScreenSurfaceCreateFlagsQNX Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameScreenSurfaceCreateFlagsQNX :: String
conNameScreenSurfaceCreateFlagsQNX = "ScreenSurfaceCreateFlagsQNX"

enumPrefixScreenSurfaceCreateFlagsQNX :: String
enumPrefixScreenSurfaceCreateFlagsQNX = ""

showTableScreenSurfaceCreateFlagsQNX :: [(ScreenSurfaceCreateFlagsQNX, String)]
showTableScreenSurfaceCreateFlagsQNX = []

instance Show ScreenSurfaceCreateFlagsQNX where
  showsPrec = enumShowsPrec enumPrefixScreenSurfaceCreateFlagsQNX
                            showTableScreenSurfaceCreateFlagsQNX
                            conNameScreenSurfaceCreateFlagsQNX
                            (\(ScreenSurfaceCreateFlagsQNX x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ScreenSurfaceCreateFlagsQNX where
  readPrec = enumReadPrec enumPrefixScreenSurfaceCreateFlagsQNX
                          showTableScreenSurfaceCreateFlagsQNX
                          conNameScreenSurfaceCreateFlagsQNX
                          ScreenSurfaceCreateFlagsQNX


type QNX_SCREEN_SURFACE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QNX_SCREEN_SURFACE_SPEC_VERSION"
pattern QNX_SCREEN_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern QNX_SCREEN_SURFACE_SPEC_VERSION = 1


type QNX_SCREEN_SURFACE_EXTENSION_NAME = "VK_QNX_screen_surface"

-- No documentation found for TopLevel "VK_QNX_SCREEN_SURFACE_EXTENSION_NAME"
pattern QNX_SCREEN_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QNX_SCREEN_SURFACE_EXTENSION_NAME = "VK_QNX_screen_surface"


data Screen_window


data Screen_context

