{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalImageFormatPropertiesNV(..)
  , VkExternalMemoryFeatureFlagBitsNV(..)
  , pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV
  , VkExternalMemoryFeatureFlagsNV
  , VkExternalMemoryHandleTypeFlagBitsNV(..)
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV
  , VkExternalMemoryHandleTypeFlagsNV
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDeviceExternalImageFormatPropertiesNV
#endif
  , FN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV
  , PFN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV
  , pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  , pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageFormatProperties(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkImageCreateFlags
  , VkImageUsageFlags
  , VkPhysicalDevice
  )

#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkImageUsageFlagBits(..)
  )
#endif
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkExternalImageFormatPropertiesNV"
data VkExternalImageFormatPropertiesNV = VkExternalImageFormatPropertiesNV
  { -- No documentation found for Nested "VkExternalImageFormatPropertiesNV" "imageFormatProperties"
  vkImageFormatProperties :: VkImageFormatProperties
  , -- No documentation found for Nested "VkExternalImageFormatPropertiesNV" "externalMemoryFeatures"
  vkExternalMemoryFeatures :: VkExternalMemoryFeatureFlagsNV
  , -- No documentation found for Nested "VkExternalImageFormatPropertiesNV" "exportFromImportedHandleTypes"
  vkExportFromImportedHandleTypes :: VkExternalMemoryHandleTypeFlagsNV
  , -- No documentation found for Nested "VkExternalImageFormatPropertiesNV" "compatibleHandleTypes"
  vkCompatibleHandleTypes :: VkExternalMemoryHandleTypeFlagsNV
  }
  deriving (Eq, Show)

instance Storable VkExternalImageFormatPropertiesNV where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkExternalImageFormatPropertiesNV <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 32)
                                               <*> peek (ptr `plusPtr` 36)
                                               <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkImageFormatProperties (poked :: VkExternalImageFormatPropertiesNV))
                *> poke (ptr `plusPtr` 32) (vkExternalMemoryFeatures (poked :: VkExternalImageFormatPropertiesNV))
                *> poke (ptr `plusPtr` 36) (vkExportFromImportedHandleTypes (poked :: VkExternalImageFormatPropertiesNV))
                *> poke (ptr `plusPtr` 40) (vkCompatibleHandleTypes (poked :: VkExternalImageFormatPropertiesNV))
-- ** VkExternalMemoryFeatureFlagBitsNV

-- No documentation found for TopLevel "VkExternalMemoryFeatureFlagBitsNV"
newtype VkExternalMemoryFeatureFlagBitsNV = VkExternalMemoryFeatureFlagBitsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalMemoryFeatureFlagBitsNV where
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV"
  showsPrec p (VkExternalMemoryFeatureFlagBitsNV x) = showParen (p >= 11) (showString "VkExternalMemoryFeatureFlagBitsNV " . showsPrec 11 x)

instance Read VkExternalMemoryFeatureFlagBitsNV where
  readPrec = parens ( choose [ ("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV", pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV",     pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV",     pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalMemoryFeatureFlagBitsNV")
                        v <- step readPrec
                        pure (VkExternalMemoryFeatureFlagBitsNV v)
                        )
                    )

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBitsNV" "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV"
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV :: VkExternalMemoryFeatureFlagBitsNV
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV = VkExternalMemoryFeatureFlagBitsNV 0x00000001

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBitsNV" "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV"
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV :: VkExternalMemoryFeatureFlagBitsNV
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV = VkExternalMemoryFeatureFlagBitsNV 0x00000002

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBitsNV" "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV"
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV :: VkExternalMemoryFeatureFlagBitsNV
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV = VkExternalMemoryFeatureFlagBitsNV 0x00000004
-- No documentation found for TopLevel "VkExternalMemoryFeatureFlagsNV"
type VkExternalMemoryFeatureFlagsNV = VkExternalMemoryFeatureFlagBitsNV
-- ** VkExternalMemoryHandleTypeFlagBitsNV

-- No documentation found for TopLevel "VkExternalMemoryHandleTypeFlagBitsNV"
newtype VkExternalMemoryHandleTypeFlagBitsNV = VkExternalMemoryHandleTypeFlagBitsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalMemoryHandleTypeFlagBitsNV where
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV"
  showsPrec p (VkExternalMemoryHandleTypeFlagBitsNV x) = showParen (p >= 11) (showString "VkExternalMemoryHandleTypeFlagBitsNV " . showsPrec 11 x)

instance Read VkExternalMemoryHandleTypeFlagBitsNV where
  readPrec = parens ( choose [ ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV",     pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV",      pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV",  pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalMemoryHandleTypeFlagBitsNV")
                        v <- step readPrec
                        pure (VkExternalMemoryHandleTypeFlagBitsNV v)
                        )
                    )

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBitsNV" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV :: VkExternalMemoryHandleTypeFlagBitsNV
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV = VkExternalMemoryHandleTypeFlagBitsNV 0x00000001

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBitsNV" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV :: VkExternalMemoryHandleTypeFlagBitsNV
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV = VkExternalMemoryHandleTypeFlagBitsNV 0x00000002

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBitsNV" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV :: VkExternalMemoryHandleTypeFlagBitsNV
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV = VkExternalMemoryHandleTypeFlagBitsNV 0x00000004

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBitsNV" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV :: VkExternalMemoryHandleTypeFlagBitsNV
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV = VkExternalMemoryHandleTypeFlagBitsNV 0x00000008
-- No documentation found for TopLevel "VkExternalMemoryHandleTypeFlagsNV"
type VkExternalMemoryHandleTypeFlagsNV = VkExternalMemoryHandleTypeFlagBitsNV
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalImageFormatPropertiesNV"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceExternalImageFormatPropertiesNV" vkGetPhysicalDeviceExternalImageFormatPropertiesNV :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("externalHandleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pExternalImageFormatProperties" ::: Ptr VkExternalImageFormatPropertiesNV) -> IO VkResult

#endif
type FN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("externalHandleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pExternalImageFormatProperties" ::: Ptr VkExternalImageFormatPropertiesNV) -> IO VkResult
type PFN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV = FunPtr FN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV
-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME"
pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = "VK_NV_external_memory_capabilities"
-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION"
pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1
