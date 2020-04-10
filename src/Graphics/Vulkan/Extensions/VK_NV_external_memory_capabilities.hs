{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities  ( getPhysicalDeviceExternalImageFormatPropertiesNV
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

import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
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
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Core10.Enums.Format (Format)
import Graphics.Vulkan.Core10.Enums.Format (Format(..))
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Graphics.Vulkan.Core10.DeviceInitialization (ImageFormatProperties)
import Graphics.Vulkan.Core10.Enums.ImageTiling (ImageTiling)
import Graphics.Vulkan.Core10.Enums.ImageTiling (ImageTiling(..))
import Graphics.Vulkan.Core10.Enums.ImageType (ImageType)
import Graphics.Vulkan.Core10.Enums.ImageType (ImageType(..))
import Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlagBits(..))
import Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceExternalImageFormatPropertiesNV))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice_T)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalImageFormatPropertiesNV
  :: FunPtr (Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> ExternalMemoryHandleTypeFlagsNV -> Ptr ExternalImageFormatPropertiesNV -> IO Result) -> Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> ExternalMemoryHandleTypeFlagsNV -> Ptr ExternalImageFormatPropertiesNV -> IO Result

-- | vkGetPhysicalDeviceExternalImageFormatPropertiesNV - determine image
-- capabilities compatible with external memory handle types
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     image capabilities
--
-- -   @format@ is the image format, corresponding to
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@format@.
--
-- -   @type@ is the image type, corresponding to
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@imageType@.
--
-- -   @tiling@ is the image tiling, corresponding to
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@tiling@.
--
-- -   @usage@ is the intended usage of the image, corresponding to
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@usage@.
--
-- -   @flags@ is a bitmask describing additional parameters of the image,
--     corresponding to
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@flags@.
--
-- -   @externalHandleType@ is either one of the bits from
--     'ExternalMemoryHandleTypeFlagBitsNV', or 0.
--
-- -   @pExternalImageFormatProperties@ is a pointer to a
--     'ExternalImageFormatPropertiesNV' structure in which capabilities
--     are returned.
--
-- = Description
--
-- If @externalHandleType@ is 0,
-- @pExternalImageFormatProperties->imageFormatProperties@ will return the
-- same values as a call to
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties',
-- and the other members of @pExternalImageFormatProperties@ will all be 0.
-- Otherwise, they are filled in as described for
-- 'ExternalImageFormatPropertiesNV'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'
--
-- = See Also
--
-- 'ExternalImageFormatPropertiesNV', 'ExternalMemoryHandleTypeFlagsNV',
-- 'Graphics.Vulkan.Core10.Enums.Format.Format',
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.ImageTiling',
-- 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType',
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceExternalImageFormatPropertiesNV :: PhysicalDevice -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> ("externalHandleType" ::: ExternalMemoryHandleTypeFlagsNV) -> IO (ExternalImageFormatPropertiesNV)
getPhysicalDeviceExternalImageFormatPropertiesNV physicalDevice format type' tiling usage flags externalHandleType = evalContT $ do
  let vkGetPhysicalDeviceExternalImageFormatPropertiesNV' = mkVkGetPhysicalDeviceExternalImageFormatPropertiesNV (pVkGetPhysicalDeviceExternalImageFormatPropertiesNV (instanceCmds (physicalDevice :: PhysicalDevice)))
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
-- 'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties',
-- 'getPhysicalDeviceExternalImageFormatPropertiesNV'
data ExternalImageFormatPropertiesNV = ExternalImageFormatPropertiesNV
  { -- | @imageFormatProperties@ will be filled in as when calling
    -- 'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties',
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
    -- when calling 'Graphics.Vulkan.Core10.Memory.allocateMemory', or 0 if the
    -- external memory handle type is 0. @compatibleHandleTypes@ will always
    -- contain
    -- 'getPhysicalDeviceExternalImageFormatPropertiesNV'::@externalHandleType@
    compatibleHandleTypes :: ExternalMemoryHandleTypeFlagsNV
  }
  deriving (Typeable)
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
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.getMemoryWin32HandleNV',
-- or one duplicated from such a handle using @DuplicateHandle()@.
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV = ExternalMemoryHandleTypeFlagBitsNV 0x00000001
-- | 'EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV' specifies a handle
-- to memory returned by
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.getMemoryWin32HandleNV'.
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

