{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreFeatureFlagBits(..)
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
  , VkExternalSemaphoreFeatureFlags
  , VkExternalSemaphoreHandleTypeFlagBits(..)
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
  , VkExternalSemaphoreHandleTypeFlags
  , VkExternalSemaphoreProperties(..)
  , VkPhysicalDeviceExternalSemaphoreInfo(..)
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceExternalSemaphoreProperties
#endif
  , FN_vkGetPhysicalDeviceExternalSemaphoreProperties
  , PFN_vkGetPhysicalDeviceExternalSemaphoreProperties
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
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
  ( VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkExternalSemaphoreFeatureFlagBits

-- No documentation found for TopLevel "VkExternalSemaphoreFeatureFlagBits"
newtype VkExternalSemaphoreFeatureFlagBits = VkExternalSemaphoreFeatureFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalSemaphoreFeatureFlagBits where
  showsPrec _ VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT = showString "VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT"
  showsPrec _ VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT = showString "VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT"
  showsPrec p (VkExternalSemaphoreFeatureFlagBits x) = showParen (p >= 11) (showString "VkExternalSemaphoreFeatureFlagBits " . showsPrec 11 x)

instance Read VkExternalSemaphoreFeatureFlagBits where
  readPrec = parens ( choose [ ("VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT", pure VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT)
                             , ("VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT", pure VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalSemaphoreFeatureFlagBits")
                        v <- step readPrec
                        pure (VkExternalSemaphoreFeatureFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkExternalSemaphoreFeatureFlagBits" "VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT"
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT :: VkExternalSemaphoreFeatureFlagBits
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT = VkExternalSemaphoreFeatureFlagBits 0x00000001

-- No documentation found for Nested "VkExternalSemaphoreFeatureFlagBits" "VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT"
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT :: VkExternalSemaphoreFeatureFlagBits
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT = VkExternalSemaphoreFeatureFlagBits 0x00000002
-- No documentation found for TopLevel "VkExternalSemaphoreFeatureFlags"
type VkExternalSemaphoreFeatureFlags = VkExternalSemaphoreFeatureFlagBits
-- ** VkExternalSemaphoreHandleTypeFlagBits

-- No documentation found for TopLevel "VkExternalSemaphoreHandleTypeFlagBits"
newtype VkExternalSemaphoreHandleTypeFlagBits = VkExternalSemaphoreHandleTypeFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalSemaphoreHandleTypeFlagBits where
  showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT"
  showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT"
  showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
  showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT"
  showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT"
  showsPrec p (VkExternalSemaphoreHandleTypeFlagBits x) = showParen (p >= 11) (showString "VkExternalSemaphoreHandleTypeFlagBits " . showsPrec 11 x)

instance Read VkExternalSemaphoreHandleTypeFlagBits where
  readPrec = parens ( choose [ ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT",        pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT)
                             , ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT",     pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT)
                             , ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT", pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT)
                             , ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT",      pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT)
                             , ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT",          pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalSemaphoreHandleTypeFlagBits")
                        v <- step readPrec
                        pure (VkExternalSemaphoreHandleTypeFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkExternalSemaphoreHandleTypeFlagBits" "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT"
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT :: VkExternalSemaphoreHandleTypeFlagBits
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT = VkExternalSemaphoreHandleTypeFlagBits 0x00000001

-- No documentation found for Nested "VkExternalSemaphoreHandleTypeFlagBits" "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT"
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT :: VkExternalSemaphoreHandleTypeFlagBits
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT = VkExternalSemaphoreHandleTypeFlagBits 0x00000002

-- No documentation found for Nested "VkExternalSemaphoreHandleTypeFlagBits" "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT :: VkExternalSemaphoreHandleTypeFlagBits
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = VkExternalSemaphoreHandleTypeFlagBits 0x00000004

-- No documentation found for Nested "VkExternalSemaphoreHandleTypeFlagBits" "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT"
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT :: VkExternalSemaphoreHandleTypeFlagBits
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT = VkExternalSemaphoreHandleTypeFlagBits 0x00000008

-- No documentation found for Nested "VkExternalSemaphoreHandleTypeFlagBits" "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT"
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT :: VkExternalSemaphoreHandleTypeFlagBits
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT = VkExternalSemaphoreHandleTypeFlagBits 0x00000010
-- No documentation found for TopLevel "VkExternalSemaphoreHandleTypeFlags"
type VkExternalSemaphoreHandleTypeFlags = VkExternalSemaphoreHandleTypeFlagBits
-- No documentation found for TopLevel "VkExternalSemaphoreProperties"
data VkExternalSemaphoreProperties = VkExternalSemaphoreProperties
  { -- No documentation found for Nested "VkExternalSemaphoreProperties" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExternalSemaphoreProperties" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExternalSemaphoreProperties" "exportFromImportedHandleTypes"
  vkExportFromImportedHandleTypes :: VkExternalSemaphoreHandleTypeFlags
  , -- No documentation found for Nested "VkExternalSemaphoreProperties" "compatibleHandleTypes"
  vkCompatibleHandleTypes :: VkExternalSemaphoreHandleTypeFlags
  , -- No documentation found for Nested "VkExternalSemaphoreProperties" "externalSemaphoreFeatures"
  vkExternalSemaphoreFeatures :: VkExternalSemaphoreFeatureFlags
  }
  deriving (Eq, Show)

instance Storable VkExternalSemaphoreProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalSemaphoreProperties <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 20)
                                           <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalSemaphoreProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalSemaphoreProperties))
                *> poke (ptr `plusPtr` 16) (vkExportFromImportedHandleTypes (poked :: VkExternalSemaphoreProperties))
                *> poke (ptr `plusPtr` 20) (vkCompatibleHandleTypes (poked :: VkExternalSemaphoreProperties))
                *> poke (ptr `plusPtr` 24) (vkExternalSemaphoreFeatures (poked :: VkExternalSemaphoreProperties))
-- No documentation found for TopLevel "VkPhysicalDeviceExternalSemaphoreInfo"
data VkPhysicalDeviceExternalSemaphoreInfo = VkPhysicalDeviceExternalSemaphoreInfo
  { -- No documentation found for Nested "VkPhysicalDeviceExternalSemaphoreInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceExternalSemaphoreInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceExternalSemaphoreInfo" "handleType"
  vkHandleType :: VkExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExternalSemaphoreInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalSemaphoreInfo <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalSemaphoreInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalSemaphoreInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkPhysicalDeviceExternalSemaphoreInfo))
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalSemaphoreProperties"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceExternalSemaphoreProperties" vkGetPhysicalDeviceExternalSemaphoreProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalSemaphoreInfo" ::: Ptr VkPhysicalDeviceExternalSemaphoreInfo) -> ("pExternalSemaphoreProperties" ::: Ptr VkExternalSemaphoreProperties) -> IO ()

#endif
type FN_vkGetPhysicalDeviceExternalSemaphoreProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalSemaphoreInfo" ::: Ptr VkPhysicalDeviceExternalSemaphoreInfo) -> ("pExternalSemaphoreProperties" ::: Ptr VkExternalSemaphoreProperties) -> IO ()
type PFN_vkGetPhysicalDeviceExternalSemaphoreProperties = FunPtr FN_vkGetPhysicalDeviceExternalSemaphoreProperties
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES"
pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES = VkStructureType 1000076001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO = VkStructureType 1000076000
