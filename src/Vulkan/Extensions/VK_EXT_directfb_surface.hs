{-# language CPP #-}
-- | = Name
--
-- VK_EXT_directfb_surface - instance extension
--
-- == VK_EXT_directfb_surface
--
-- [__Name String__]
--     @VK_EXT_directfb_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     347
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
--     -   Nicolas Caramelli
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_directfb_surface:%20&body=@caramelli%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-06-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Nicolas Caramelli
--
-- == Description
--
-- The @VK_EXT_directfb_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) that refers to a
-- DirectFB 'IDirectFBSurface', as well as a query to determine support for
-- rendering via DirectFB.
--
-- == New Commands
--
-- -   'createDirectFBSurfaceEXT'
--
-- -   'getPhysicalDeviceDirectFBPresentationSupportEXT'
--
-- == New Structures
--
-- -   'DirectFBSurfaceCreateInfoEXT'
--
-- == New Bitmasks
--
-- -   'DirectFBSurfaceCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DIRECTFB_SURFACE_EXTENSION_NAME'
--
-- -   'EXT_DIRECTFB_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2020-06-16 (Nicolas Caramelli)
--
--     -   Initial version
--
-- = See Also
--
-- 'DirectFBSurfaceCreateFlagsEXT', 'DirectFBSurfaceCreateInfoEXT',
-- 'createDirectFBSurfaceEXT',
-- 'getPhysicalDeviceDirectFBPresentationSupportEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_directfb_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_directfb_surface  ( createDirectFBSurfaceEXT
                                                  , getPhysicalDeviceDirectFBPresentationSupportEXT
                                                  , DirectFBSurfaceCreateInfoEXT(..)
                                                  , DirectFBSurfaceCreateFlagsEXT(..)
                                                  , EXT_DIRECTFB_SURFACE_SPEC_VERSION
                                                  , pattern EXT_DIRECTFB_SURFACE_SPEC_VERSION
                                                  , EXT_DIRECTFB_SURFACE_EXTENSION_NAME
                                                  , pattern EXT_DIRECTFB_SURFACE_EXTENSION_NAME
                                                  , IDirectFB
                                                  , IDirectFBSurface
                                                  , SurfaceKHR(..)
                                                  ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
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
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateDirectFBSurfaceEXT))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceDirectFBPresentationSupportEXT))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDirectFBSurfaceEXT
  :: FunPtr (Ptr Instance_T -> Ptr DirectFBSurfaceCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr DirectFBSurfaceCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateDirectFBSurfaceEXT - Create a
-- 'Vulkan.Extensions.Handles.SurfaceKHR' object for a DirectFB surface
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateDirectFBSurfaceEXT-instance-parameter# @instance@
--     /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateDirectFBSurfaceEXT-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'DirectFBSurfaceCreateInfoEXT' structure
--
-- -   #VUID-vkCreateDirectFBSurfaceEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateDirectFBSurfaceEXT-pSurface-parameter# @pSurface@
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
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'DirectFBSurfaceCreateInfoEXT', 'Vulkan.Core10.Handles.Instance',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
createDirectFBSurfaceEXT :: forall io
                          . (MonadIO io)
                         => -- | @instance@ is the instance to associate the surface with.
                            Instance
                         -> -- | @pCreateInfo@ is a pointer to a 'DirectFBSurfaceCreateInfoEXT' structure
                            -- containing parameters affecting the creation of the surface object.
                            DirectFBSurfaceCreateInfoEXT
                         -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                            -- surface object when there is no more specific allocator available (see
                            -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                            ("allocator" ::: Maybe AllocationCallbacks)
                         -> io (SurfaceKHR)
createDirectFBSurfaceEXT instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateDirectFBSurfaceEXTPtr = pVkCreateDirectFBSurfaceEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateDirectFBSurfaceEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDirectFBSurfaceEXT is null" Nothing Nothing
  let vkCreateDirectFBSurfaceEXT' = mkVkCreateDirectFBSurfaceEXT vkCreateDirectFBSurfaceEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateDirectFBSurfaceEXT' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDirectFBPresentationSupportEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr IDirectFB -> IO Bool32) -> Ptr PhysicalDevice_T -> Word32 -> Ptr IDirectFB -> IO Bool32

-- | vkGetPhysicalDeviceDirectFBPresentationSupportEXT - Query physical
-- device for presentation with DirectFB
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
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceDirectFBPresentationSupportEXT :: forall io
                                                 . (MonadIO io)
                                                => -- | @physicalDevice@ is the physical device.
                                                   --
                                                   -- #VUID-vkGetPhysicalDeviceDirectFBPresentationSupportEXT-physicalDevice-parameter#
                                                   -- @physicalDevice@ /must/ be a valid
                                                   -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                                   PhysicalDevice
                                                -> -- | @queueFamilyIndex@ is the queue family index.
                                                   --
                                                   -- #VUID-vkGetPhysicalDeviceDirectFBPresentationSupportEXT-queueFamilyIndex-04119#
                                                   -- @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
                                                   -- returned by
                                                   -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
                                                   -- for the given @physicalDevice@
                                                   ("queueFamilyIndex" ::: Word32)
                                                -> -- | @dfb@ is a pointer to the 'IDirectFB' main interface of DirectFB.
                                                   --
                                                   -- #VUID-vkGetPhysicalDeviceDirectFBPresentationSupportEXT-dfb-parameter#
                                                   -- @dfb@ /must/ be a valid pointer to an 'IDirectFB' value
                                                   ("dfb" ::: Ptr IDirectFB)
                                                -> io (Bool)
getPhysicalDeviceDirectFBPresentationSupportEXT physicalDevice queueFamilyIndex dfb = liftIO $ do
  let vkGetPhysicalDeviceDirectFBPresentationSupportEXTPtr = pVkGetPhysicalDeviceDirectFBPresentationSupportEXT (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkGetPhysicalDeviceDirectFBPresentationSupportEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceDirectFBPresentationSupportEXT is null" Nothing Nothing
  let vkGetPhysicalDeviceDirectFBPresentationSupportEXT' = mkVkGetPhysicalDeviceDirectFBPresentationSupportEXT vkGetPhysicalDeviceDirectFBPresentationSupportEXTPtr
  r <- vkGetPhysicalDeviceDirectFBPresentationSupportEXT' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (dfb)
  pure $ ((bool32ToBool r))


-- | VkDirectFBSurfaceCreateInfoEXT - Structure specifying parameters of a
-- newly created DirectFB surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DirectFBSurfaceCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createDirectFBSurfaceEXT'
data DirectFBSurfaceCreateInfoEXT = DirectFBSurfaceCreateInfoEXT
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkDirectFBSurfaceCreateInfoEXT-flags-zerobitmask# @flags@ /must/
    -- be @0@
    flags :: DirectFBSurfaceCreateFlagsEXT
  , -- | @dfb@ is a pointer to the 'IDirectFB' main interface of DirectFB.
    --
    -- #VUID-VkDirectFBSurfaceCreateInfoEXT-dfb-04117# @dfb@ /must/ point to a
    -- valid DirectFB 'IDirectFB'
    dfb :: Ptr IDirectFB
  , -- | @surface@ is a pointer to a 'IDirectFBSurface' surface interface.
    --
    -- #VUID-VkDirectFBSurfaceCreateInfoEXT-surface-04118# @surface@ /must/
    -- point to a valid DirectFB 'IDirectFBSurface'
    surface :: Ptr IDirectFBSurface
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DirectFBSurfaceCreateInfoEXT)
#endif
deriving instance Show DirectFBSurfaceCreateInfoEXT

instance ToCStruct DirectFBSurfaceCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DirectFBSurfaceCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DirectFBSurfaceCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr IDirectFB))) (dfb)
    poke ((p `plusPtr` 32 :: Ptr (Ptr IDirectFBSurface))) (surface)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr IDirectFB))) (zero)
    poke ((p `plusPtr` 32 :: Ptr (Ptr IDirectFBSurface))) (zero)
    f

instance FromCStruct DirectFBSurfaceCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @DirectFBSurfaceCreateFlagsEXT ((p `plusPtr` 16 :: Ptr DirectFBSurfaceCreateFlagsEXT))
    dfb <- peek @(Ptr IDirectFB) ((p `plusPtr` 24 :: Ptr (Ptr IDirectFB)))
    surface <- peek @(Ptr IDirectFBSurface) ((p `plusPtr` 32 :: Ptr (Ptr IDirectFBSurface)))
    pure $ DirectFBSurfaceCreateInfoEXT
             flags dfb surface

instance Storable DirectFBSurfaceCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DirectFBSurfaceCreateInfoEXT where
  zero = DirectFBSurfaceCreateInfoEXT
           zero
           zero
           zero


-- | VkDirectFBSurfaceCreateFlagsEXT - Reserved for future use
--
-- = Description
--
-- 'DirectFBSurfaceCreateFlagsEXT' is a bitmask type for setting a mask,
-- but is currently reserved for future use.
--
-- = See Also
--
-- 'DirectFBSurfaceCreateInfoEXT'
newtype DirectFBSurfaceCreateFlagsEXT = DirectFBSurfaceCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



instance Show DirectFBSurfaceCreateFlagsEXT where
  showsPrec p = \case
    DirectFBSurfaceCreateFlagsEXT x -> showParen (p >= 11) (showString "DirectFBSurfaceCreateFlagsEXT 0x" . showHex x)

instance Read DirectFBSurfaceCreateFlagsEXT where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "DirectFBSurfaceCreateFlagsEXT")
                       v <- step readPrec
                       pure (DirectFBSurfaceCreateFlagsEXT v)))


type EXT_DIRECTFB_SURFACE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DIRECTFB_SURFACE_SPEC_VERSION"
pattern EXT_DIRECTFB_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DIRECTFB_SURFACE_SPEC_VERSION = 1


type EXT_DIRECTFB_SURFACE_EXTENSION_NAME = "VK_EXT_directfb_surface"

-- No documentation found for TopLevel "VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME"
pattern EXT_DIRECTFB_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DIRECTFB_SURFACE_EXTENSION_NAME = "VK_EXT_directfb_surface"


data IDirectFB


data IDirectFBSurface

