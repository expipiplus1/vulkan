{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateFlagBits(..)
  , pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT
  , pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
  , pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
  , VkBufferCreateFlags
  , VkBufferCreateInfo(..)
  , VkBufferUsageFlagBits(..)
  , pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT
  , pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT
  , pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT
  , VkBufferUsageFlags
  , VkSharingMode(..)
  , pattern VK_SHARING_MODE_EXCLUSIVE
  , pattern VK_SHARING_MODE_CONCURRENT
  , FN_vkCreateBuffer
  , PFN_vkCreateBuffer
  , vkCreateBuffer
  , FN_vkDestroyBuffer
  , PFN_vkDestroyBuffer
  , vkDestroyBuffer
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
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
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkBufferCreateFlagBits

-- No documentation found for TopLevel "VkBufferCreateFlagBits"
newtype VkBufferCreateFlagBits = VkBufferCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkBufferCreateFlagBits where
  showsPrec _ VK_BUFFER_CREATE_SPARSE_BINDING_BIT = showString "VK_BUFFER_CREATE_SPARSE_BINDING_BIT"
  showsPrec _ VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT = showString "VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
  showsPrec _ VK_BUFFER_CREATE_SPARSE_ALIASED_BIT = showString "VK_BUFFER_CREATE_SPARSE_ALIASED_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkBufferCreateFlagBits 0x00000008) = showString "VK_BUFFER_CREATE_PROTECTED_BIT"
  showsPrec _ (VkBufferCreateFlagBits 0x00000010) = showString "VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT"
  showsPrec p (VkBufferCreateFlagBits x) = showParen (p >= 11) (showString "VkBufferCreateFlagBits " . showsPrec 11 x)

instance Read VkBufferCreateFlagBits where
  readPrec = parens ( choose [ ("VK_BUFFER_CREATE_SPARSE_BINDING_BIT",   pure VK_BUFFER_CREATE_SPARSE_BINDING_BIT)
                             , ("VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT", pure VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT)
                             , ("VK_BUFFER_CREATE_SPARSE_ALIASED_BIT",   pure VK_BUFFER_CREATE_SPARSE_ALIASED_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_BUFFER_CREATE_PROTECTED_BIT",                         pure (VkBufferCreateFlagBits 0x00000008))
                             , ("VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT", pure (VkBufferCreateFlagBits 0x00000010))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBufferCreateFlagBits")
                        v <- step readPrec
                        pure (VkBufferCreateFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_SPARSE_BINDING_BIT"
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT = VkBufferCreateFlagBits 0x00000001

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT = VkBufferCreateFlagBits 0x00000002

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_SPARSE_ALIASED_BIT"
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT = VkBufferCreateFlagBits 0x00000004

-- No documentation found for TopLevel "VkBufferCreateFlags"
type VkBufferCreateFlags = VkBufferCreateFlagBits

-- No documentation found for TopLevel "VkBufferCreateInfo"
data VkBufferCreateInfo = VkBufferCreateInfo
  { -- No documentation found for Nested "VkBufferCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBufferCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBufferCreateInfo" "flags"
  vkFlags :: VkBufferCreateFlags
  , -- No documentation found for Nested "VkBufferCreateInfo" "size"
  vkSize :: VkDeviceSize
  , -- No documentation found for Nested "VkBufferCreateInfo" "usage"
  vkUsage :: VkBufferUsageFlags
  , -- No documentation found for Nested "VkBufferCreateInfo" "sharingMode"
  vkSharingMode :: VkSharingMode
  , -- No documentation found for Nested "VkBufferCreateInfo" "queueFamilyIndexCount"
  vkQueueFamilyIndexCount :: Word32
  , -- No documentation found for Nested "VkBufferCreateInfo" "pQueueFamilyIndices"
  vkPQueueFamilyIndices :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkBufferCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferCreateInfo <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 36)
                                <*> peek (ptr `plusPtr` 40)
                                <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkSize (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkUsage (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkSharingMode (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkQueueFamilyIndexCount (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkPQueueFamilyIndices (poked :: VkBufferCreateInfo))

instance Zero VkBufferCreateInfo where
  zero = VkBufferCreateInfo VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero

-- ** VkBufferUsageFlagBits

-- No documentation found for TopLevel "VkBufferUsageFlagBits"
newtype VkBufferUsageFlagBits = VkBufferUsageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkBufferUsageFlagBits where
  showsPrec _ VK_BUFFER_USAGE_TRANSFER_SRC_BIT = showString "VK_BUFFER_USAGE_TRANSFER_SRC_BIT"
  showsPrec _ VK_BUFFER_USAGE_TRANSFER_DST_BIT = showString "VK_BUFFER_USAGE_TRANSFER_DST_BIT"
  showsPrec _ VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = showString "VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = showString "VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT = showString "VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_STORAGE_BUFFER_BIT = showString "VK_BUFFER_USAGE_STORAGE_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_INDEX_BUFFER_BIT = showString "VK_BUFFER_USAGE_INDEX_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_VERTEX_BUFFER_BIT = showString "VK_BUFFER_USAGE_VERTEX_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT = showString "VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkBufferUsageFlagBits 0x00008000) = showString "VK_BUFFER_USAGE_RESERVED_15_BIT_KHR"
  showsPrec _ (VkBufferUsageFlagBits 0x00010000) = showString "VK_BUFFER_USAGE_RESERVED_16_BIT_KHR"
  showsPrec _ (VkBufferUsageFlagBits 0x00002000) = showString "VK_BUFFER_USAGE_RESERVED_13_BIT_KHR"
  showsPrec _ (VkBufferUsageFlagBits 0x00004000) = showString "VK_BUFFER_USAGE_RESERVED_14_BIT_KHR"
  showsPrec _ (VkBufferUsageFlagBits 0x00000800) = showString "VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT"
  showsPrec _ (VkBufferUsageFlagBits 0x00001000) = showString "VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT"
  showsPrec _ (VkBufferUsageFlagBits 0x00000200) = showString "VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT"
  showsPrec _ (VkBufferUsageFlagBits 0x00000400) = showString "VK_BUFFER_USAGE_RAY_TRACING_BIT_NV"
  showsPrec _ (VkBufferUsageFlagBits 0x00020000) = showString "VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT"
  showsPrec p (VkBufferUsageFlagBits x) = showParen (p >= 11) (showString "VkBufferUsageFlagBits " . showsPrec 11 x)

instance Read VkBufferUsageFlagBits where
  readPrec = parens ( choose [ ("VK_BUFFER_USAGE_TRANSFER_SRC_BIT",         pure VK_BUFFER_USAGE_TRANSFER_SRC_BIT)
                             , ("VK_BUFFER_USAGE_TRANSFER_DST_BIT",         pure VK_BUFFER_USAGE_TRANSFER_DST_BIT)
                             , ("VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT", pure VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT", pure VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT",       pure VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_STORAGE_BUFFER_BIT",       pure VK_BUFFER_USAGE_STORAGE_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_INDEX_BUFFER_BIT",         pure VK_BUFFER_USAGE_INDEX_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_VERTEX_BUFFER_BIT",        pure VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT",      pure VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_BUFFER_USAGE_RESERVED_15_BIT_KHR",                       pure (VkBufferUsageFlagBits 0x00008000))
                             , ("VK_BUFFER_USAGE_RESERVED_16_BIT_KHR",                       pure (VkBufferUsageFlagBits 0x00010000))
                             , ("VK_BUFFER_USAGE_RESERVED_13_BIT_KHR",                       pure (VkBufferUsageFlagBits 0x00002000))
                             , ("VK_BUFFER_USAGE_RESERVED_14_BIT_KHR",                       pure (VkBufferUsageFlagBits 0x00004000))
                             , ("VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT",         pure (VkBufferUsageFlagBits 0x00000800))
                             , ("VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT", pure (VkBufferUsageFlagBits 0x00001000))
                             , ("VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT",             pure (VkBufferUsageFlagBits 0x00000200))
                             , ("VK_BUFFER_USAGE_RAY_TRACING_BIT_NV",                        pure (VkBufferUsageFlagBits 0x00000400))
                             , ("VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT",             pure (VkBufferUsageFlagBits 0x00020000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBufferUsageFlagBits")
                        v <- step readPrec
                        pure (VkBufferUsageFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TRANSFER_SRC_BIT"
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT = VkBufferUsageFlagBits 0x00000001

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TRANSFER_DST_BIT"
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT = VkBufferUsageFlagBits 0x00000002

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT"
pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = VkBufferUsageFlagBits 0x00000004

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT"
pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = VkBufferUsageFlagBits 0x00000008

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT"
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT = VkBufferUsageFlagBits 0x00000010

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_STORAGE_BUFFER_BIT"
pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT = VkBufferUsageFlagBits 0x00000020

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_INDEX_BUFFER_BIT"
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT = VkBufferUsageFlagBits 0x00000040

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_VERTEX_BUFFER_BIT"
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT = VkBufferUsageFlagBits 0x00000080

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT"
pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT = VkBufferUsageFlagBits 0x00000100

-- No documentation found for TopLevel "VkBufferUsageFlags"
type VkBufferUsageFlags = VkBufferUsageFlagBits

-- ** VkSharingMode

-- No documentation found for TopLevel "VkSharingMode"
newtype VkSharingMode = VkSharingMode Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkSharingMode where
  showsPrec _ VK_SHARING_MODE_EXCLUSIVE = showString "VK_SHARING_MODE_EXCLUSIVE"
  showsPrec _ VK_SHARING_MODE_CONCURRENT = showString "VK_SHARING_MODE_CONCURRENT"
  showsPrec p (VkSharingMode x) = showParen (p >= 11) (showString "VkSharingMode " . showsPrec 11 x)

instance Read VkSharingMode where
  readPrec = parens ( choose [ ("VK_SHARING_MODE_EXCLUSIVE",  pure VK_SHARING_MODE_EXCLUSIVE)
                             , ("VK_SHARING_MODE_CONCURRENT", pure VK_SHARING_MODE_CONCURRENT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSharingMode")
                        v <- step readPrec
                        pure (VkSharingMode v)
                        )
                    )

-- No documentation found for Nested "VkSharingMode" "VK_SHARING_MODE_EXCLUSIVE"
pattern VK_SHARING_MODE_EXCLUSIVE :: VkSharingMode
pattern VK_SHARING_MODE_EXCLUSIVE = VkSharingMode 0

-- No documentation found for Nested "VkSharingMode" "VK_SHARING_MODE_CONCURRENT"
pattern VK_SHARING_MODE_CONCURRENT :: VkSharingMode
pattern VK_SHARING_MODE_CONCURRENT = VkSharingMode 1

-- No documentation found for TopLevel "vkCreateBuffer"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateBuffer" vkCreateBuffer :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult
#else
vkCreateBuffer :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult
vkCreateBuffer deviceCmds = mkVkCreateBuffer (pVkCreateBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateBuffer
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult)
#endif

type FN_vkCreateBuffer = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult
type PFN_vkCreateBuffer = FunPtr FN_vkCreateBuffer

-- No documentation found for TopLevel "vkDestroyBuffer"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyBuffer" vkDestroyBuffer :: ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyBuffer :: DeviceCmds -> ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyBuffer deviceCmds = mkVkDestroyBuffer (pVkDestroyBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyBuffer
  :: FunPtr (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyBuffer = ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyBuffer = FunPtr FN_vkDestroyBuffer
