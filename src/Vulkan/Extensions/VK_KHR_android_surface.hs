{-# language CPP #-}
-- | = Name
--
-- VK_KHR_android_surface - instance extension
--
-- == VK_KHR_android_surface
--
-- [__Name String__]
--     @VK_KHR_android_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     9
--
-- [__Revision__]
--     6
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_android_surface] @critsec%0A<<Here describe the issue or question you have about the VK_KHR_android_surface extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-01-14
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Patrick Doane, Blizzard
--
--     -   Jason Ekstrand, Intel
--
--     -   Ian Elliott, LunarG
--
--     -   Courtney Goeltzenleuchter, LunarG
--
--     -   Jesse Hall, Google
--
--     -   James Jones, NVIDIA
--
--     -   Antoine Labour, Google
--
--     -   Jon Leech, Khronos
--
--     -   David Mao, AMD
--
--     -   Norbert Nopper, Freescale
--
--     -   Alon Or-bach, Samsung
--
--     -   Daniel Rakos, AMD
--
--     -   Graham Sellers, AMD
--
--     -   Ray Smith, ARM
--
--     -   Jeff Vigil, Qualcomm
--
--     -   Chia-I Wu, LunarG
--
-- == Description
--
-- The @VK_KHR_android_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) that refers to an
-- 'ANativeWindow', Android’s native surface type. The 'ANativeWindow'
-- represents the producer endpoint of any buffer queue, regardless of
-- consumer endpoint. Common consumer endpoints for @ANativeWindows@ are
-- the system window compositor, video encoders, and application-specific
-- compositors importing the images through a @SurfaceTexture@.
--
-- == New Base Types
--
-- -   'ANativeWindow'
--
-- == New Commands
--
-- -   'createAndroidSurfaceKHR'
--
-- == New Structures
--
-- -   'AndroidSurfaceCreateInfoKHR'
--
-- == New Bitmasks
--
-- -   'AndroidSurfaceCreateFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_ANDROID_SURFACE_EXTENSION_NAME'
--
-- -   'KHR_ANDROID_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Does Android need a way to query for compatibility between a
-- particular physical device (and queue family?) and a specific Android
-- display?
--
-- __RESOLVED__: No. Currently on Android, any physical device is expected
-- to be able to present to the system compositor, and all queue families
-- must support the necessary image layout transitions and synchronization
-- operations.
--
-- == Version History
--
-- -   Revision 1, 2015-09-23 (Jesse Hall)
--
--     -   Initial draft.
--
-- -   Revision 2, 2015-10-26 (Ian Elliott)
--
--     -   Renamed from VK_EXT_KHR_android_surface to
--         VK_KHR_android_surface.
--
-- -   Revision 3, 2015-11-03 (Daniel Rakos)
--
--     -   Added allocation callbacks to surface creation function.
--
-- -   Revision 4, 2015-11-10 (Jesse Hall)
--
--     -   Removed VK_ERROR_INVALID_ANDROID_WINDOW_KHR.
--
-- -   Revision 5, 2015-11-28 (Daniel Rakos)
--
--     -   Updated the surface create function to take a pCreateInfo
--         structure.
--
-- -   Revision 6, 2016-01-14 (James Jones)
--
--     -   Moved VK_ERROR_NATIVE_WINDOW_IN_USE_KHR from the
--         VK_KHR_android_surface to the VK_KHR_surface extension.
--
-- == See Also
--
-- 'ANativeWindow', 'AndroidSurfaceCreateFlagsKHR',
-- 'AndroidSurfaceCreateInfoKHR', 'createAndroidSurfaceKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_android_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_android_surface  ( createAndroidSurfaceKHR
                                                 , AndroidSurfaceCreateInfoKHR(..)
                                                 , AndroidSurfaceCreateFlagsKHR(..)
                                                 , KHR_ANDROID_SURFACE_SPEC_VERSION
                                                 , pattern KHR_ANDROID_SURFACE_SPEC_VERSION
                                                 , KHR_ANDROID_SURFACE_EXTENSION_NAME
                                                 , pattern KHR_ANDROID_SURFACE_EXTENSION_NAME
                                                 , ANativeWindow
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Core10.Handles (Instance(Instance))
import Vulkan.Dynamic (InstanceCmds(pVkCreateAndroidSurfaceKHR))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateAndroidSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr AndroidSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr AndroidSurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateAndroidSurfaceKHR - Create a
-- 'Vulkan.Extensions.Handles.SurfaceKHR' object for an Android native
-- window
--
-- = Description
--
-- During the lifetime of a surface created using a particular
-- 'ANativeWindow' handle any attempts to create another surface for the
-- same 'ANativeWindow' and any attempts to connect to the same
-- 'ANativeWindow' through other platform mechanisms will fail.
--
-- Note
--
-- In particular, only one 'Vulkan.Extensions.Handles.SurfaceKHR' /can/
-- exist at a time for a given window. Similarly, a native window /cannot/
-- be used by both a 'Vulkan.Extensions.Handles.SurfaceKHR' and
-- @EGLSurface@ simultaneously.
--
-- If successful, 'createAndroidSurfaceKHR' increments the
-- 'ANativeWindow'’s reference count, and
-- 'Vulkan.Extensions.VK_KHR_surface.destroySurfaceKHR' will decrement it.
--
-- On Android, when a swapchain’s @imageExtent@ does not match the
-- surface’s @currentExtent@, the presentable images will be scaled to the
-- surface’s dimensions during presentation. @minImageExtent@ is (1,1), and
-- @maxImageExtent@ is the maximum image size supported by the consumer.
-- For the system compositor, @currentExtent@ is the window size (i.e. the
-- consumer’s preferred size).
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateAndroidSurfaceKHR-instance-parameter# @instance@
--     /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateAndroidSurfaceKHR-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'AndroidSurfaceCreateInfoKHR'
--     structure
--
-- -   #VUID-vkCreateAndroidSurfaceKHR-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateAndroidSurfaceKHR-pSurface-parameter# @pSurface@
--     /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.SurfaceKHR' handle
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_NATIVE_WINDOW_IN_USE_KHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_android_surface VK_KHR_android_surface>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'AndroidSurfaceCreateInfoKHR', 'Vulkan.Core10.Handles.Instance',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
createAndroidSurfaceKHR :: forall io
                         . (MonadIO io)
                        => -- | @instance@ is the instance to associate the surface with.
                           Instance
                        -> -- | @pCreateInfo@ is a pointer to a 'AndroidSurfaceCreateInfoKHR' structure
                           -- containing parameters affecting the creation of the surface object.
                           AndroidSurfaceCreateInfoKHR
                        -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                           -- surface object when there is no more specific allocator available (see
                           -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                           ("allocator" ::: Maybe AllocationCallbacks)
                        -> io (SurfaceKHR)
createAndroidSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateAndroidSurfaceKHRPtr = pVkCreateAndroidSurfaceKHR (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (vkCreateAndroidSurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateAndroidSurfaceKHR is null" Nothing Nothing
  let vkCreateAndroidSurfaceKHR' = mkVkCreateAndroidSurfaceKHR vkCreateAndroidSurfaceKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateAndroidSurfaceKHR" (vkCreateAndroidSurfaceKHR' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


-- | VkAndroidSurfaceCreateInfoKHR - Structure specifying parameters of a
-- newly created Android surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_android_surface VK_KHR_android_surface>,
-- 'AndroidSurfaceCreateFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createAndroidSurfaceKHR'
data AndroidSurfaceCreateInfoKHR = AndroidSurfaceCreateInfoKHR
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkAndroidSurfaceCreateInfoKHR-flags-zerobitmask# @flags@ /must/ be
    -- @0@
    flags :: AndroidSurfaceCreateFlagsKHR
  , -- | @window@ is a pointer to the 'ANativeWindow' to associate the surface
    -- with.
    --
    -- #VUID-VkAndroidSurfaceCreateInfoKHR-window-01248# @window@ /must/ point
    -- to a valid Android 'ANativeWindow'
    window :: Ptr ANativeWindow
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AndroidSurfaceCreateInfoKHR)
#endif
deriving instance Show AndroidSurfaceCreateInfoKHR

instance ToCStruct AndroidSurfaceCreateInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AndroidSurfaceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AndroidSurfaceCreateFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ANativeWindow))) (window)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ANativeWindow))) (zero)
    f

instance FromCStruct AndroidSurfaceCreateInfoKHR where
  peekCStruct p = do
    flags <- peek @AndroidSurfaceCreateFlagsKHR ((p `plusPtr` 16 :: Ptr AndroidSurfaceCreateFlagsKHR))
    window <- peek @(Ptr ANativeWindow) ((p `plusPtr` 24 :: Ptr (Ptr ANativeWindow)))
    pure $ AndroidSurfaceCreateInfoKHR
             flags window

instance Storable AndroidSurfaceCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AndroidSurfaceCreateInfoKHR where
  zero = AndroidSurfaceCreateInfoKHR
           zero
           zero


-- | VkAndroidSurfaceCreateFlagsKHR - Reserved for future use
--
-- = Description
--
-- 'AndroidSurfaceCreateFlagsKHR' is a bitmask type for setting a mask, but
-- is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_android_surface VK_KHR_android_surface>,
-- 'AndroidSurfaceCreateInfoKHR'
newtype AndroidSurfaceCreateFlagsKHR = AndroidSurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameAndroidSurfaceCreateFlagsKHR :: String
conNameAndroidSurfaceCreateFlagsKHR = "AndroidSurfaceCreateFlagsKHR"

enumPrefixAndroidSurfaceCreateFlagsKHR :: String
enumPrefixAndroidSurfaceCreateFlagsKHR = ""

showTableAndroidSurfaceCreateFlagsKHR :: [(AndroidSurfaceCreateFlagsKHR, String)]
showTableAndroidSurfaceCreateFlagsKHR = []

instance Show AndroidSurfaceCreateFlagsKHR where
  showsPrec = enumShowsPrec enumPrefixAndroidSurfaceCreateFlagsKHR
                            showTableAndroidSurfaceCreateFlagsKHR
                            conNameAndroidSurfaceCreateFlagsKHR
                            (\(AndroidSurfaceCreateFlagsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read AndroidSurfaceCreateFlagsKHR where
  readPrec = enumReadPrec enumPrefixAndroidSurfaceCreateFlagsKHR
                          showTableAndroidSurfaceCreateFlagsKHR
                          conNameAndroidSurfaceCreateFlagsKHR
                          AndroidSurfaceCreateFlagsKHR


type KHR_ANDROID_SURFACE_SPEC_VERSION = 6

-- No documentation found for TopLevel "VK_KHR_ANDROID_SURFACE_SPEC_VERSION"
pattern KHR_ANDROID_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_ANDROID_SURFACE_SPEC_VERSION = 6


type KHR_ANDROID_SURFACE_EXTENSION_NAME = "VK_KHR_android_surface"

-- No documentation found for TopLevel "VK_KHR_ANDROID_SURFACE_EXTENSION_NAME"
pattern KHR_ANDROID_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_ANDROID_SURFACE_EXTENSION_NAME = "VK_KHR_android_surface"


data ANativeWindow

