{-# language CPP #-}
-- | = Name
--
-- VK_NV_external_memory_capabilities - instance extension
--
-- == VK_NV_external_memory_capabilities
--
-- [__Name String__]
--     @VK_NV_external_memory_capabilities@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     56
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Deprecation State__]
--
--     -   /Deprecated/ by
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_capabilities VK_KHR_external_memory_capabilities>
--         extension
--
--         -   Which in turn was /promoted/ to
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_external_memory_capabilities] @cubanismo%0A*Here describe the issue or question you have about the VK_NV_external_memory_capabilities extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-08-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with Vulkan 1.1.
--
--     -   Interacts with @VK_KHR_dedicated_allocation@.
--
--     -   Interacts with @VK_NV_dedicated_allocation@.
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
-- == Description
--
-- Applications may wish to import memory from the Direct 3D API, or export
-- memory to other Vulkan instances. This extension provides a set of
-- capability queries that allow applications determine what types of win32
-- memory handles an implementation supports for a given set of use cases.
--
-- == New Commands
--
-- -   'getPhysicalDeviceExternalImageFormatPropertiesNV'
--
-- == New Structures
--
-- -   'ExternalImageFormatPropertiesNV'
--
-- == New Enums
--
-- -   'ExternalMemoryFeatureFlagBitsNV'
--
-- -   'ExternalMemoryHandleTypeFlagBitsNV'
--
-- == New Bitmasks
--
-- -   'ExternalMemoryFeatureFlagsNV'
--
-- -   'ExternalMemoryHandleTypeFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME'
--
-- -   'NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION'
--
-- == Issues
--
-- 1) Why do so many external memory capabilities need to be queried on a
-- per-memory-handle-type basis?
--
-- __RESOLVED__: This is because some handle types are based on OS-native
-- objects that have far more limited capabilities than the very generic
-- Vulkan memory objects. Not all memory handle types can name memory
-- objects that support 3D images, for example. Some handle types cannot
-- even support the deferred image and memory binding behavior of Vulkan
-- and require specifying the image when allocating or importing the memory
-- object.
--
-- 2) Does the 'ExternalImageFormatPropertiesNV' struct need to include a
-- list of memory type bits that support the given handle type?
--
-- __RESOLVED__: No. The memory types that do not support the handle types
-- will simply be filtered out of the results returned by
-- 'Vulkan.Core10.MemoryManagement.getImageMemoryRequirements' when a set
-- of handle types was specified at image creation time.
--
-- 3) Should the non-opaque handle types be moved to their own extension?
--
-- __RESOLVED__: Perhaps. However, defining the handle type bits does very
-- little and does not require any platform-specific types on its own, and
-- it is easier to maintain the bitmask values in a single extension for
-- now. Presumably more handle types could be added by separate extensions
-- though, and it would be midly weird to have some platform-specific ones
-- defined in the core spec and some in extensions
--
-- == Version History
--
-- -   Revision 1, 2016-08-19 (James Jones)
--
--     -   Initial version
--
-- == See Also
--
-- 'ExternalImageFormatPropertiesNV', 'ExternalMemoryFeatureFlagBitsNV',
-- 'ExternalMemoryFeatureFlagsNV', 'ExternalMemoryHandleTypeFlagBitsNV',
-- 'ExternalMemoryHandleTypeFlagsNV',
-- 'getPhysicalDeviceExternalImageFormatPropertiesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_external_memory_capabilities Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_external_memory_capabilities  ( getPhysicalDeviceExternalImageFormatPropertiesNV
                                                             , ExternalImageFormatPropertiesNV(..)
                                                             , ExternalMemoryHandleTypeFlagsNV
                                                             , ExternalMemoryHandleTypeFlagBitsNV( EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV
                                                                                                 , EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV
                                                                                                 , EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV
                                                                                                 , EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV
                                                                                                 , ..
                                                                                                 )
                                                             , ExternalMemoryFeatureFlagsNV
                                                             , ExternalMemoryFeatureFlagBitsNV( EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV
                                                                                              , EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV
                                                                                              , EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV
                                                                                              , ..
                                                                                              )
                                                             , NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
                                                             , pattern NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
                                                             , NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
                                                             , pattern NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
                                                             ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
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
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.Format (Format(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.DeviceInitialization (ImageFormatProperties)
import Vulkan.Core10.Enums.ImageTiling (ImageTiling)
import Vulkan.Core10.Enums.ImageTiling (ImageTiling(..))
import Vulkan.Core10.Enums.ImageType (ImageType)
import Vulkan.Core10.Enums.ImageType (ImageType(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlagBits(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceExternalImageFormatPropertiesNV))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalImageFormatPropertiesNV
  :: FunPtr (Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> ExternalMemoryHandleTypeFlagsNV -> Ptr ExternalImageFormatPropertiesNV -> IO Result) -> Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> ExternalMemoryHandleTypeFlagsNV -> Ptr ExternalImageFormatPropertiesNV -> IO Result

-- | vkGetPhysicalDeviceExternalImageFormatPropertiesNV - Determine image
-- capabilities compatible with external memory handle types
--
-- = Description
--
-- If @externalHandleType@ is 0,
-- @pExternalImageFormatProperties->imageFormatProperties@ will return the
-- same values as a call to
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties',
-- and the other members of @pExternalImageFormatProperties@ will all be 0.
-- Otherwise, they are filled in as described for
-- 'ExternalImageFormatPropertiesNV'.
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_memory_capabilities VK_NV_external_memory_capabilities>,
-- 'ExternalImageFormatPropertiesNV', 'ExternalMemoryHandleTypeFlagsNV',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlags',
-- 'Vulkan.Core10.Enums.ImageTiling.ImageTiling',
-- 'Vulkan.Core10.Enums.ImageType.ImageType',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceExternalImageFormatPropertiesNV :: forall io
                                                  . (MonadIO io)
                                                 => -- | @physicalDevice@ is the physical device from which to query the image
                                                    -- capabilities
                                                    --
                                                    -- #VUID-vkGetPhysicalDeviceExternalImageFormatPropertiesNV-physicalDevice-parameter#
                                                    -- @physicalDevice@ /must/ be a valid
                                                    -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                                    PhysicalDevice
                                                 -> -- | @format@ is the image format, corresponding to
                                                    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@format@.
                                                    --
                                                    -- #VUID-vkGetPhysicalDeviceExternalImageFormatPropertiesNV-format-parameter#
                                                    -- @format@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
                                                    Format
                                                 -> -- | @type@ is the image type, corresponding to
                                                    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@imageType@.
                                                    --
                                                    -- #VUID-vkGetPhysicalDeviceExternalImageFormatPropertiesNV-type-parameter#
                                                    -- @type@ /must/ be a valid 'Vulkan.Core10.Enums.ImageType.ImageType' value
                                                    ImageType
                                                 -> -- | @tiling@ is the image tiling, corresponding to
                                                    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@tiling@.
                                                    --
                                                    -- #VUID-vkGetPhysicalDeviceExternalImageFormatPropertiesNV-tiling-parameter#
                                                    -- @tiling@ /must/ be a valid 'Vulkan.Core10.Enums.ImageTiling.ImageTiling'
                                                    -- value
                                                    ImageTiling
                                                 -> -- | @usage@ is the intended usage of the image, corresponding to
                                                    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@.
                                                    --
                                                    -- #VUID-vkGetPhysicalDeviceExternalImageFormatPropertiesNV-usage-parameter#
                                                    -- @usage@ /must/ be a valid combination of
                                                    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' values
                                                    --
                                                    -- #VUID-vkGetPhysicalDeviceExternalImageFormatPropertiesNV-usage-requiredbitmask#
                                                    -- @usage@ /must/ not be @0@
                                                    ImageUsageFlags
                                                 -> -- | @flags@ is a bitmask describing additional parameters of the image,
                                                    -- corresponding to 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@.
                                                    --
                                                    -- #VUID-vkGetPhysicalDeviceExternalImageFormatPropertiesNV-flags-parameter#
                                                    -- @flags@ /must/ be a valid combination of
                                                    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits' values
                                                    ImageCreateFlags
                                                 -> -- | @externalHandleType@ is either one of the bits from
                                                    -- 'ExternalMemoryHandleTypeFlagBitsNV', or 0.
                                                    --
                                                    -- #VUID-vkGetPhysicalDeviceExternalImageFormatPropertiesNV-externalHandleType-07721#
                                                    -- @externalHandleType@ /must/ not have more than one bit set
                                                    --
                                                    -- #VUID-vkGetPhysicalDeviceExternalImageFormatPropertiesNV-externalHandleType-parameter#
                                                    -- @externalHandleType@ /must/ be a valid combination of
                                                    -- 'ExternalMemoryHandleTypeFlagBitsNV' values
                                                    ("externalHandleType" ::: ExternalMemoryHandleTypeFlagsNV)
                                                 -> io (ExternalImageFormatPropertiesNV)
getPhysicalDeviceExternalImageFormatPropertiesNV physicalDevice
                                                   format
                                                   type'
                                                   tiling
                                                   usage
                                                   flags
                                                   externalHandleType = liftIO . evalContT $ do
  let vkGetPhysicalDeviceExternalImageFormatPropertiesNVPtr = pVkGetPhysicalDeviceExternalImageFormatPropertiesNV (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceExternalImageFormatPropertiesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceExternalImageFormatPropertiesNV is null" Nothing Nothing
  let vkGetPhysicalDeviceExternalImageFormatPropertiesNV' = mkVkGetPhysicalDeviceExternalImageFormatPropertiesNV vkGetPhysicalDeviceExternalImageFormatPropertiesNVPtr
  pPExternalImageFormatProperties <- ContT (withZeroCStruct @ExternalImageFormatPropertiesNV)
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceExternalImageFormatPropertiesNV" (vkGetPhysicalDeviceExternalImageFormatPropertiesNV'
                                                                                       (physicalDeviceHandle (physicalDevice))
                                                                                       (format)
                                                                                       (type')
                                                                                       (tiling)
                                                                                       (usage)
                                                                                       (flags)
                                                                                       (externalHandleType)
                                                                                       (pPExternalImageFormatProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pExternalImageFormatProperties <- lift $ peekCStruct @ExternalImageFormatPropertiesNV pPExternalImageFormatProperties
  pure $ (pExternalImageFormatProperties)


-- | VkExternalImageFormatPropertiesNV - Structure specifying external image
-- format properties
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_memory_capabilities VK_NV_external_memory_capabilities>,
-- 'ExternalMemoryFeatureFlagsNV', 'ExternalMemoryHandleTypeFlagsNV',
-- 'Vulkan.Core10.DeviceInitialization.ImageFormatProperties',
-- 'getPhysicalDeviceExternalImageFormatPropertiesNV'
data ExternalImageFormatPropertiesNV = ExternalImageFormatPropertiesNV
  { -- | @imageFormatProperties@ will be filled in as when calling
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties',
    -- but the values returned /may/ vary depending on the external handle type
    -- requested.
    imageFormatProperties :: ImageFormatProperties
  , -- | @externalMemoryFeatures@ is a bitmask of
    -- 'ExternalMemoryFeatureFlagBitsNV', indicating properties of the external
    -- memory handle type
    -- ('getPhysicalDeviceExternalImageFormatPropertiesNV'::@externalHandleType@)
    -- being queried, or 0 if the external memory handle type is 0.
    externalMemoryFeatures :: ExternalMemoryFeatureFlagsNV
  , -- | @exportFromImportedHandleTypes@ is a bitmask of
    -- 'ExternalMemoryHandleTypeFlagBitsNV' containing a bit set for every
    -- external handle type that /may/ be used to create memory from which the
    -- handles of the type specified in
    -- 'getPhysicalDeviceExternalImageFormatPropertiesNV'::@externalHandleType@
    -- /can/ be exported, or 0 if the external memory handle type is 0.
    exportFromImportedHandleTypes :: ExternalMemoryHandleTypeFlagsNV
  , -- | @compatibleHandleTypes@ is a bitmask of
    -- 'ExternalMemoryHandleTypeFlagBitsNV' containing a bit set for every
    -- external handle type that /may/ be specified simultaneously with the
    -- handle type specified by
    -- 'getPhysicalDeviceExternalImageFormatPropertiesNV'::@externalHandleType@
    -- when calling 'Vulkan.Core10.Memory.allocateMemory', or 0 if the external
    -- memory handle type is 0. @compatibleHandleTypes@ will always contain
    -- 'getPhysicalDeviceExternalImageFormatPropertiesNV'::@externalHandleType@
    compatibleHandleTypes :: ExternalMemoryHandleTypeFlagsNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalImageFormatPropertiesNV)
#endif
deriving instance Show ExternalImageFormatPropertiesNV

instance ToCStruct ExternalImageFormatPropertiesNV where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalImageFormatPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageFormatProperties)) (imageFormatProperties)
    poke ((p `plusPtr` 32 :: Ptr ExternalMemoryFeatureFlagsNV)) (externalMemoryFeatures)
    poke ((p `plusPtr` 36 :: Ptr ExternalMemoryHandleTypeFlagsNV)) (exportFromImportedHandleTypes)
    poke ((p `plusPtr` 40 :: Ptr ExternalMemoryHandleTypeFlagsNV)) (compatibleHandleTypes)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageFormatProperties)) (zero)
    f

instance FromCStruct ExternalImageFormatPropertiesNV where
  peekCStruct p = do
    imageFormatProperties <- peekCStruct @ImageFormatProperties ((p `plusPtr` 0 :: Ptr ImageFormatProperties))
    externalMemoryFeatures <- peek @ExternalMemoryFeatureFlagsNV ((p `plusPtr` 32 :: Ptr ExternalMemoryFeatureFlagsNV))
    exportFromImportedHandleTypes <- peek @ExternalMemoryHandleTypeFlagsNV ((p `plusPtr` 36 :: Ptr ExternalMemoryHandleTypeFlagsNV))
    compatibleHandleTypes <- peek @ExternalMemoryHandleTypeFlagsNV ((p `plusPtr` 40 :: Ptr ExternalMemoryHandleTypeFlagsNV))
    pure $ ExternalImageFormatPropertiesNV
             imageFormatProperties
             externalMemoryFeatures
             exportFromImportedHandleTypes
             compatibleHandleTypes

instance Storable ExternalImageFormatPropertiesNV where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalImageFormatPropertiesNV where
  zero = ExternalImageFormatPropertiesNV
           zero
           zero
           zero
           zero


type ExternalMemoryHandleTypeFlagsNV = ExternalMemoryHandleTypeFlagBitsNV

-- | VkExternalMemoryHandleTypeFlagBitsNV - Bitmask specifying external
-- memory handle types
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_memory_capabilities VK_NV_external_memory_capabilities>,
-- 'ExternalMemoryHandleTypeFlagsNV'
newtype ExternalMemoryHandleTypeFlagBitsNV = ExternalMemoryHandleTypeFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV' specifies a handle to
-- memory returned by
-- 'Vulkan.Extensions.VK_NV_external_memory_win32.getMemoryWin32HandleNV',
-- or one duplicated from such a handle using @DuplicateHandle()@.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV = ExternalMemoryHandleTypeFlagBitsNV 0x00000001

-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV' specifies a handle
-- to memory returned by
-- 'Vulkan.Extensions.VK_NV_external_memory_win32.getMemoryWin32HandleNV'.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV = ExternalMemoryHandleTypeFlagBitsNV 0x00000002

-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV' specifies a valid NT
-- handle to memory returned by @IDXGIResource1::CreateSharedHandle@, or a
-- handle duplicated from such a handle using @DuplicateHandle()@.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV = ExternalMemoryHandleTypeFlagBitsNV 0x00000004

-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV' specifies a handle
-- to memory returned by @IDXGIResource::GetSharedHandle()@.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV = ExternalMemoryHandleTypeFlagBitsNV 0x00000008

conNameExternalMemoryHandleTypeFlagBitsNV :: String
conNameExternalMemoryHandleTypeFlagBitsNV = "ExternalMemoryHandleTypeFlagBitsNV"

enumPrefixExternalMemoryHandleTypeFlagBitsNV :: String
enumPrefixExternalMemoryHandleTypeFlagBitsNV = "EXTERNAL_MEMORY_HANDLE_TYPE_"

showTableExternalMemoryHandleTypeFlagBitsNV :: [(ExternalMemoryHandleTypeFlagBitsNV, String)]
showTableExternalMemoryHandleTypeFlagBitsNV =
  [
    ( EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV
    , "OPAQUE_WIN32_BIT_NV"
    )
  ,
    ( EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV
    , "OPAQUE_WIN32_KMT_BIT_NV"
    )
  ,
    ( EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV
    , "D3D11_IMAGE_BIT_NV"
    )
  ,
    ( EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV
    , "D3D11_IMAGE_KMT_BIT_NV"
    )
  ]

instance Show ExternalMemoryHandleTypeFlagBitsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixExternalMemoryHandleTypeFlagBitsNV
      showTableExternalMemoryHandleTypeFlagBitsNV
      conNameExternalMemoryHandleTypeFlagBitsNV
      (\(ExternalMemoryHandleTypeFlagBitsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ExternalMemoryHandleTypeFlagBitsNV where
  readPrec =
    enumReadPrec
      enumPrefixExternalMemoryHandleTypeFlagBitsNV
      showTableExternalMemoryHandleTypeFlagBitsNV
      conNameExternalMemoryHandleTypeFlagBitsNV
      ExternalMemoryHandleTypeFlagBitsNV

type ExternalMemoryFeatureFlagsNV = ExternalMemoryFeatureFlagBitsNV

-- | VkExternalMemoryFeatureFlagBitsNV - Bitmask specifying external memory
-- features
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_memory_capabilities VK_NV_external_memory_capabilities>,
-- 'ExternalImageFormatPropertiesNV', 'ExternalMemoryFeatureFlagsNV',
-- 'getPhysicalDeviceExternalImageFormatPropertiesNV'
newtype ExternalMemoryFeatureFlagBitsNV = ExternalMemoryFeatureFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV' specifies that external
-- memory of the specified type /must/ be created as a dedicated allocation
-- when used in the manner specified.
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV = ExternalMemoryFeatureFlagBitsNV 0x00000001

-- | 'EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV' specifies that the
-- implementation supports exporting handles of the specified type.
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV = ExternalMemoryFeatureFlagBitsNV 0x00000002

-- | 'EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV' specifies that the
-- implementation supports importing handles of the specified type.
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV = ExternalMemoryFeatureFlagBitsNV 0x00000004

conNameExternalMemoryFeatureFlagBitsNV :: String
conNameExternalMemoryFeatureFlagBitsNV = "ExternalMemoryFeatureFlagBitsNV"

enumPrefixExternalMemoryFeatureFlagBitsNV :: String
enumPrefixExternalMemoryFeatureFlagBitsNV = "EXTERNAL_MEMORY_FEATURE_"

showTableExternalMemoryFeatureFlagBitsNV :: [(ExternalMemoryFeatureFlagBitsNV, String)]
showTableExternalMemoryFeatureFlagBitsNV =
  [
    ( EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV
    , "DEDICATED_ONLY_BIT_NV"
    )
  ,
    ( EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV
    , "EXPORTABLE_BIT_NV"
    )
  ,
    ( EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV
    , "IMPORTABLE_BIT_NV"
    )
  ]

instance Show ExternalMemoryFeatureFlagBitsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixExternalMemoryFeatureFlagBitsNV
      showTableExternalMemoryFeatureFlagBitsNV
      conNameExternalMemoryFeatureFlagBitsNV
      (\(ExternalMemoryFeatureFlagBitsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ExternalMemoryFeatureFlagBitsNV where
  readPrec =
    enumReadPrec
      enumPrefixExternalMemoryFeatureFlagBitsNV
      showTableExternalMemoryFeatureFlagBitsNV
      conNameExternalMemoryFeatureFlagBitsNV
      ExternalMemoryFeatureFlagBitsNV

type NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION :: forall a . Integral a => a
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1


type NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = "VK_NV_external_memory_capabilities"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = "VK_NV_external_memory_capabilities"

