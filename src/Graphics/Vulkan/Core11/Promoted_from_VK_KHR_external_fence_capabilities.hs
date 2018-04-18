{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlagBits(..)
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  , VkExternalFenceFeatureFlagBits(..)
  , pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , vkGetPhysicalDeviceExternalFenceProperties
  , VkPhysicalDeviceExternalFenceInfo(..)
  , VkExternalFenceProperties(..)
  , VkExternalFenceHandleTypeFlags
  , VkExternalFenceFeatureFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )


-- ** VkExternalFenceHandleTypeFlagBits

-- | 
newtype VkExternalFenceHandleTypeFlagBits = VkExternalFenceHandleTypeFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalFenceHandleTypeFlagBits where
  showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT"
  showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT"
  showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
  showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT"
  showsPrec p (VkExternalFenceHandleTypeFlagBits x) = showParen (p >= 11) (showString "VkExternalFenceHandleTypeFlagBits " . showsPrec 11 x)

instance Read VkExternalFenceHandleTypeFlagBits where
  readPrec = parens ( choose [ ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT",        pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT)
                             , ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT",     pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT)
                             , ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT", pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT)
                             , ("VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT",          pure VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalFenceHandleTypeFlagBits")
                        v <- step readPrec
                        pure (VkExternalFenceHandleTypeFlagBits v)
                        )
                    )

-- | 
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT :: VkExternalFenceHandleTypeFlagBits
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT = VkExternalFenceHandleTypeFlagBits 0x00000001

-- | 
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT :: VkExternalFenceHandleTypeFlagBits
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT = VkExternalFenceHandleTypeFlagBits 0x00000002

-- | 
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT :: VkExternalFenceHandleTypeFlagBits
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = VkExternalFenceHandleTypeFlagBits 0x00000004

-- | 
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT :: VkExternalFenceHandleTypeFlagBits
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT = VkExternalFenceHandleTypeFlagBits 0x00000008
-- ** VkExternalFenceFeatureFlagBits

-- | 
newtype VkExternalFenceFeatureFlagBits = VkExternalFenceFeatureFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalFenceFeatureFlagBits where
  showsPrec _ VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT = showString "VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT"
  showsPrec _ VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT = showString "VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT"
  showsPrec p (VkExternalFenceFeatureFlagBits x) = showParen (p >= 11) (showString "VkExternalFenceFeatureFlagBits " . showsPrec 11 x)

instance Read VkExternalFenceFeatureFlagBits where
  readPrec = parens ( choose [ ("VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT", pure VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT)
                             , ("VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT", pure VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalFenceFeatureFlagBits")
                        v <- step readPrec
                        pure (VkExternalFenceFeatureFlagBits v)
                        )
                    )

-- | 
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT :: VkExternalFenceFeatureFlagBits
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT = VkExternalFenceFeatureFlagBits 0x00000001

-- | 
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT :: VkExternalFenceFeatureFlagBits
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT = VkExternalFenceFeatureFlagBits 0x00000002
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO = VkStructureType 1000112000
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES = VkStructureType 1000112001
-- | 
foreign import ccall "vkGetPhysicalDeviceExternalFenceProperties" vkGetPhysicalDeviceExternalFenceProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalFenceInfo" ::: Ptr VkPhysicalDeviceExternalFenceInfo) -> ("pExternalFenceProperties" ::: Ptr VkExternalFenceProperties) -> IO ()
-- | TODO: Struct comments
data VkPhysicalDeviceExternalFenceInfo = VkPhysicalDeviceExternalFenceInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkHandleType :: VkExternalFenceHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExternalFenceInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalFenceInfo <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalFenceInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalFenceInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkPhysicalDeviceExternalFenceInfo))
-- | TODO: Struct comments
data VkExternalFenceProperties = VkExternalFenceProperties
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkExportFromImportedHandleTypes :: VkExternalFenceHandleTypeFlags
  , vkCompatibleHandleTypes :: VkExternalFenceHandleTypeFlags
  , vkExternalFenceFeatures :: VkExternalFenceFeatureFlags
  }
  deriving (Eq, Show)

instance Storable VkExternalFenceProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalFenceProperties <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 20)
                                       <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalFenceProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalFenceProperties))
                *> poke (ptr `plusPtr` 16) (vkExportFromImportedHandleTypes (poked :: VkExternalFenceProperties))
                *> poke (ptr `plusPtr` 20) (vkCompatibleHandleTypes (poked :: VkExternalFenceProperties))
                *> poke (ptr `plusPtr` 24) (vkExternalFenceFeatures (poked :: VkExternalFenceProperties))
type VkExternalFenceHandleTypeFlags = VkExternalFenceHandleTypeFlagBits
type VkExternalFenceFeatureFlags = VkExternalFenceFeatureFlagBits
