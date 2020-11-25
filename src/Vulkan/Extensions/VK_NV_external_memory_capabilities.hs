{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_external_memory_capabilities"
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

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
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
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Flags)
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

-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalImageFormatPropertiesNV"
getPhysicalDeviceExternalImageFormatPropertiesNV :: forall io
                                                  . (MonadIO io)
                                                 => -- No documentation found for Nested "vkGetPhysicalDeviceExternalImageFormatPropertiesNV" "physicalDevice"
                                                    PhysicalDevice
                                                 -> -- No documentation found for Nested "vkGetPhysicalDeviceExternalImageFormatPropertiesNV" "format"
                                                    Format
                                                 -> -- No documentation found for Nested "vkGetPhysicalDeviceExternalImageFormatPropertiesNV" "type"
                                                    ImageType
                                                 -> -- No documentation found for Nested "vkGetPhysicalDeviceExternalImageFormatPropertiesNV" "tiling"
                                                    ImageTiling
                                                 -> -- No documentation found for Nested "vkGetPhysicalDeviceExternalImageFormatPropertiesNV" "usage"
                                                    ImageUsageFlags
                                                 -> -- No documentation found for Nested "vkGetPhysicalDeviceExternalImageFormatPropertiesNV" "flags"
                                                    ImageCreateFlags
                                                 -> -- No documentation found for Nested "vkGetPhysicalDeviceExternalImageFormatPropertiesNV" "externalHandleType"
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



-- No documentation found for TopLevel "VkExternalImageFormatPropertiesNV"
data ExternalImageFormatPropertiesNV = ExternalImageFormatPropertiesNV
  { -- No documentation found for Nested "VkExternalImageFormatPropertiesNV" "imageFormatProperties"
    imageFormatProperties :: ImageFormatProperties
  , -- No documentation found for Nested "VkExternalImageFormatPropertiesNV" "externalMemoryFeatures"
    externalMemoryFeatures :: ExternalMemoryFeatureFlagsNV
  , -- No documentation found for Nested "VkExternalImageFormatPropertiesNV" "exportFromImportedHandleTypes"
    exportFromImportedHandleTypes :: ExternalMemoryHandleTypeFlagsNV
  , -- No documentation found for Nested "VkExternalImageFormatPropertiesNV" "compatibleHandleTypes"
    compatibleHandleTypes :: ExternalMemoryHandleTypeFlagsNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalImageFormatPropertiesNV)
#endif
deriving instance Show ExternalImageFormatPropertiesNV

instance ToCStruct ExternalImageFormatPropertiesNV where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
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
             imageFormatProperties externalMemoryFeatures exportFromImportedHandleTypes compatibleHandleTypes


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

-- No documentation found for TopLevel "VkExternalMemoryHandleTypeFlagBitsNV"
newtype ExternalMemoryHandleTypeFlagBitsNV = ExternalMemoryHandleTypeFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBitsNV" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV     = ExternalMemoryHandleTypeFlagBitsNV 0x00000001
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBitsNV" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV = ExternalMemoryHandleTypeFlagBitsNV 0x00000002
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBitsNV" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV      = ExternalMemoryHandleTypeFlagBitsNV 0x00000004
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBitsNV" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV  = ExternalMemoryHandleTypeFlagBitsNV 0x00000008

conNameExternalMemoryHandleTypeFlagBitsNV :: String
conNameExternalMemoryHandleTypeFlagBitsNV = "ExternalMemoryHandleTypeFlagBitsNV"

enumPrefixExternalMemoryHandleTypeFlagBitsNV :: String
enumPrefixExternalMemoryHandleTypeFlagBitsNV = "EXTERNAL_MEMORY_HANDLE_TYPE_"

showTableExternalMemoryHandleTypeFlagBitsNV :: [(ExternalMemoryHandleTypeFlagBitsNV, String)]
showTableExternalMemoryHandleTypeFlagBitsNV =
  [ (EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV    , "OPAQUE_WIN32_BIT_NV")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV, "OPAQUE_WIN32_KMT_BIT_NV")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV     , "D3D11_IMAGE_BIT_NV")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV , "D3D11_IMAGE_KMT_BIT_NV")
  ]


instance Show ExternalMemoryHandleTypeFlagBitsNV where
showsPrec = enumShowsPrec enumPrefixExternalMemoryHandleTypeFlagBitsNV
                          showTableExternalMemoryHandleTypeFlagBitsNV
                          conNameExternalMemoryHandleTypeFlagBitsNV
                          (\(ExternalMemoryHandleTypeFlagBitsNV x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ExternalMemoryHandleTypeFlagBitsNV where
  readPrec = enumReadPrec enumPrefixExternalMemoryHandleTypeFlagBitsNV
                          showTableExternalMemoryHandleTypeFlagBitsNV
                          conNameExternalMemoryHandleTypeFlagBitsNV
                          ExternalMemoryHandleTypeFlagBitsNV


type ExternalMemoryFeatureFlagsNV = ExternalMemoryFeatureFlagBitsNV

-- No documentation found for TopLevel "VkExternalMemoryFeatureFlagBitsNV"
newtype ExternalMemoryFeatureFlagBitsNV = ExternalMemoryFeatureFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBitsNV" "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV"
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV = ExternalMemoryFeatureFlagBitsNV 0x00000001
-- No documentation found for Nested "VkExternalMemoryFeatureFlagBitsNV" "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV"
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV     = ExternalMemoryFeatureFlagBitsNV 0x00000002
-- No documentation found for Nested "VkExternalMemoryFeatureFlagBitsNV" "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV"
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV     = ExternalMemoryFeatureFlagBitsNV 0x00000004

conNameExternalMemoryFeatureFlagBitsNV :: String
conNameExternalMemoryFeatureFlagBitsNV = "ExternalMemoryFeatureFlagBitsNV"

enumPrefixExternalMemoryFeatureFlagBitsNV :: String
enumPrefixExternalMemoryFeatureFlagBitsNV = "EXTERNAL_MEMORY_FEATURE_"

showTableExternalMemoryFeatureFlagBitsNV :: [(ExternalMemoryFeatureFlagBitsNV, String)]
showTableExternalMemoryFeatureFlagBitsNV =
  [ (EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV, "DEDICATED_ONLY_BIT_NV")
  , (EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV    , "EXPORTABLE_BIT_NV")
  , (EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV    , "IMPORTABLE_BIT_NV")
  ]


instance Show ExternalMemoryFeatureFlagBitsNV where
showsPrec = enumShowsPrec enumPrefixExternalMemoryFeatureFlagBitsNV
                          showTableExternalMemoryFeatureFlagBitsNV
                          conNameExternalMemoryFeatureFlagBitsNV
                          (\(ExternalMemoryFeatureFlagBitsNV x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ExternalMemoryFeatureFlagBitsNV where
  readPrec = enumReadPrec enumPrefixExternalMemoryFeatureFlagBitsNV
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

