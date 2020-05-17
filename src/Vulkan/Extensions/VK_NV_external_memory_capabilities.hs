{-# language CPP #-}
module Vulkan.Extensions.VK_NV_external_memory_capabilities  ( getPhysicalDeviceExternalImageFormatPropertiesNV
                                                             , ExternalImageFormatPropertiesNV(..)
                                                             , ExternalMemoryHandleTypeFlagBitsNV( EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV
                                                                                                 , EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV
                                                                                                 , EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV
                                                                                                 , EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV
                                                                                                 , ..
                                                                                                 )
                                                             , ExternalMemoryHandleTypeFlagsNV
                                                             , ExternalMemoryFeatureFlagBitsNV( EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV
                                                                                              , EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV
                                                                                              , EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV
                                                                                              , ..
                                                                                              )
                                                             , ExternalMemoryFeatureFlagsNV
                                                             , NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
                                                             , pattern NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
                                                             , NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
                                                             , pattern NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
                                                             ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
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
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.BaseType (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.Format (Format(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
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
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalImageFormatPropertiesNV
  :: FunPtr (Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> ExternalMemoryHandleTypeFlagsNV -> Ptr ExternalImageFormatPropertiesNV -> IO Result) -> Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> ExternalMemoryHandleTypeFlagsNV -> Ptr ExternalImageFormatPropertiesNV -> IO Result

-- | vkGetPhysicalDeviceExternalImageFormatPropertiesNV - determine image
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
                                                    -- @physicalDevice@ /must/ be a valid
                                                    -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                                    PhysicalDevice
                                                 -> -- | @format@ is the image format, corresponding to
                                                    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@format@.
                                                    --
                                                    -- @format@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
                                                    Format
                                                 -> -- | @type@ is the image type, corresponding to
                                                    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@imageType@.
                                                    --
                                                    -- @type@ /must/ be a valid 'Vulkan.Core10.Enums.ImageType.ImageType' value
                                                    ImageType
                                                 -> -- | @tiling@ is the image tiling, corresponding to
                                                    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@tiling@.
                                                    --
                                                    -- @tiling@ /must/ be a valid 'Vulkan.Core10.Enums.ImageTiling.ImageTiling'
                                                    -- value
                                                    ImageTiling
                                                 -> -- | @usage@ is the intended usage of the image, corresponding to
                                                    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@.
                                                    --
                                                    -- @usage@ /must/ be a valid combination of
                                                    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' values
                                                    --
                                                    -- @usage@ /must/ not be @0@
                                                    ImageUsageFlags
                                                 -> -- | @flags@ is a bitmask describing additional parameters of the image,
                                                    -- corresponding to 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@.
                                                    --
                                                    -- @flags@ /must/ be a valid combination of
                                                    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits' values
                                                    ImageCreateFlags
                                                 -> -- | @externalHandleType@ is either one of the bits from
                                                    -- 'ExternalMemoryHandleTypeFlagBitsNV', or 0.
                                                    --
                                                    -- @externalHandleType@ /must/ be a valid combination of
                                                    -- 'ExternalMemoryHandleTypeFlagBitsNV' values
                                                    ("externalHandleType" ::: ExternalMemoryHandleTypeFlagsNV)
                                                 -> io (ExternalImageFormatPropertiesNV)
getPhysicalDeviceExternalImageFormatPropertiesNV physicalDevice format type' tiling usage flags externalHandleType = liftIO . evalContT $ do
  let vkGetPhysicalDeviceExternalImageFormatPropertiesNVPtr = pVkGetPhysicalDeviceExternalImageFormatPropertiesNV (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceExternalImageFormatPropertiesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceExternalImageFormatPropertiesNV is null" Nothing Nothing
  let vkGetPhysicalDeviceExternalImageFormatPropertiesNV' = mkVkGetPhysicalDeviceExternalImageFormatPropertiesNV vkGetPhysicalDeviceExternalImageFormatPropertiesNVPtr
  pPExternalImageFormatProperties <- ContT (withZeroCStruct @ExternalImageFormatPropertiesNV)
  r <- lift $ vkGetPhysicalDeviceExternalImageFormatPropertiesNV' (physicalDeviceHandle (physicalDevice)) (format) (type') (tiling) (usage) (flags) (externalHandleType) (pPExternalImageFormatProperties)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pExternalImageFormatProperties <- lift $ peekCStruct @ExternalImageFormatPropertiesNV pPExternalImageFormatProperties
  pure $ (pExternalImageFormatProperties)


-- | VkExternalImageFormatPropertiesNV - Structure specifying external image
-- format properties
--
-- = See Also
--
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
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalImageFormatPropertiesNV{..} f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr ImageFormatProperties)) (imageFormatProperties) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr ExternalMemoryFeatureFlagsNV)) (externalMemoryFeatures)
    lift $ poke ((p `plusPtr` 36 :: Ptr ExternalMemoryHandleTypeFlagsNV)) (exportFromImportedHandleTypes)
    lift $ poke ((p `plusPtr` 40 :: Ptr ExternalMemoryHandleTypeFlagsNV)) (compatibleHandleTypes)
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr ImageFormatProperties)) (zero) . ($ ())
    lift $ f

instance FromCStruct ExternalImageFormatPropertiesNV where
  peekCStruct p = do
    imageFormatProperties <- peekCStruct @ImageFormatProperties ((p `plusPtr` 0 :: Ptr ImageFormatProperties))
    externalMemoryFeatures <- peek @ExternalMemoryFeatureFlagsNV ((p `plusPtr` 32 :: Ptr ExternalMemoryFeatureFlagsNV))
    exportFromImportedHandleTypes <- peek @ExternalMemoryHandleTypeFlagsNV ((p `plusPtr` 36 :: Ptr ExternalMemoryHandleTypeFlagsNV))
    compatibleHandleTypes <- peek @ExternalMemoryHandleTypeFlagsNV ((p `plusPtr` 40 :: Ptr ExternalMemoryHandleTypeFlagsNV))
    pure $ ExternalImageFormatPropertiesNV
             imageFormatProperties externalMemoryFeatures exportFromImportedHandleTypes compatibleHandleTypes

instance Zero ExternalImageFormatPropertiesNV where
  zero = ExternalImageFormatPropertiesNV
           zero
           zero
           zero
           zero


-- | VkExternalMemoryHandleTypeFlagBitsNV - Bitmask specifying external
-- memory handle types
--
-- = See Also
--
-- 'ExternalMemoryHandleTypeFlagsNV'
newtype ExternalMemoryHandleTypeFlagBitsNV = ExternalMemoryHandleTypeFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

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

type ExternalMemoryHandleTypeFlagsNV = ExternalMemoryHandleTypeFlagBitsNV

instance Show ExternalMemoryHandleTypeFlagBitsNV where
  showsPrec p = \case
    EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV"
    EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV"
    EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV"
    EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV -> showString "EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV"
    ExternalMemoryHandleTypeFlagBitsNV x -> showParen (p >= 11) (showString "ExternalMemoryHandleTypeFlagBitsNV 0x" . showHex x)

instance Read ExternalMemoryHandleTypeFlagBitsNV where
  readPrec = parens (choose [("EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV", pure EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV", pure EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV", pure EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV)
                            , ("EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV", pure EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "ExternalMemoryHandleTypeFlagBitsNV")
                       v <- step readPrec
                       pure (ExternalMemoryHandleTypeFlagBitsNV v)))


-- | VkExternalMemoryFeatureFlagBitsNV - Bitmask specifying external memory
-- features
--
-- = See Also
--
-- 'ExternalImageFormatPropertiesNV', 'ExternalMemoryFeatureFlagsNV',
-- 'getPhysicalDeviceExternalImageFormatPropertiesNV'
newtype ExternalMemoryFeatureFlagBitsNV = ExternalMemoryFeatureFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

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

type ExternalMemoryFeatureFlagsNV = ExternalMemoryFeatureFlagBitsNV

instance Show ExternalMemoryFeatureFlagBitsNV where
  showsPrec p = \case
    EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV -> showString "EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV"
    EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV -> showString "EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV"
    EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV -> showString "EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV"
    ExternalMemoryFeatureFlagBitsNV x -> showParen (p >= 11) (showString "ExternalMemoryFeatureFlagBitsNV 0x" . showHex x)

instance Read ExternalMemoryFeatureFlagBitsNV where
  readPrec = parens (choose [("EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV", pure EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV)
                            , ("EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV", pure EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV)
                            , ("EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV", pure EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "ExternalMemoryFeatureFlagBitsNV")
                       v <- step readPrec
                       pure (ExternalMemoryFeatureFlagBitsNV v)))


type NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION :: forall a . Integral a => a
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1


type NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = "VK_NV_external_memory_capabilities"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = "VK_NV_external_memory_capabilities"

