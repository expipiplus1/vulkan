{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Image
  ( VkImageCreateInfo(..)
  , VkImageLayout(..)
  , pattern VK_IMAGE_LAYOUT_UNDEFINED
  , pattern VK_IMAGE_LAYOUT_GENERAL
  , pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_PREINITIALIZED
  , VkSubresourceLayout(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateImage
#endif
  , FN_vkCreateImage
  , PFN_vkCreateImage
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroyImage
#endif
  , FN_vkDestroyImage
  , PFN_vkDestroyImage
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkGetImageSubresourceLayout
#endif
  , FN_vkGetImageSubresourceLayout
  , PFN_vkGetImageSubresourceLayout
  ) where

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


import Graphics.Vulkan.C.Core10.Buffer
  ( VkSharingMode(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkExtent3D(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkSampleCountFlagBits(..)
  , VkDevice
  , VkDeviceSize
  , VkImageCreateFlags
  , VkImageUsageFlags
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkImage
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageSubresource(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkImageCreateInfo"
data VkImageCreateInfo = VkImageCreateInfo
  { -- No documentation found for Nested "VkImageCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageCreateInfo" "flags"
  vkFlags :: VkImageCreateFlags
  , -- No documentation found for Nested "VkImageCreateInfo" "imageType"
  vkImageType :: VkImageType
  , -- No documentation found for Nested "VkImageCreateInfo" "format"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkImageCreateInfo" "extent"
  vkExtent :: VkExtent3D
  , -- No documentation found for Nested "VkImageCreateInfo" "mipLevels"
  vkMipLevels :: Word32
  , -- No documentation found for Nested "VkImageCreateInfo" "arrayLayers"
  vkArrayLayers :: Word32
  , -- No documentation found for Nested "VkImageCreateInfo" "samples"
  vkSamples :: VkSampleCountFlagBits
  , -- No documentation found for Nested "VkImageCreateInfo" "tiling"
  vkTiling :: VkImageTiling
  , -- No documentation found for Nested "VkImageCreateInfo" "usage"
  vkUsage :: VkImageUsageFlags
  , -- No documentation found for Nested "VkImageCreateInfo" "sharingMode"
  vkSharingMode :: VkSharingMode
  , -- No documentation found for Nested "VkImageCreateInfo" "queueFamilyIndexCount"
  vkQueueFamilyIndexCount :: Word32
  , -- No documentation found for Nested "VkImageCreateInfo" "pQueueFamilyIndices"
  vkPQueueFamilyIndices :: Ptr Word32
  , -- No documentation found for Nested "VkImageCreateInfo" "initialLayout"
  vkInitialLayout :: VkImageLayout
  }
  deriving (Eq, Show)

instance Storable VkImageCreateInfo where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek ptr = VkImageCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 20)
                               <*> peek (ptr `plusPtr` 24)
                               <*> peek (ptr `plusPtr` 28)
                               <*> peek (ptr `plusPtr` 40)
                               <*> peek (ptr `plusPtr` 44)
                               <*> peek (ptr `plusPtr` 48)
                               <*> peek (ptr `plusPtr` 52)
                               <*> peek (ptr `plusPtr` 56)
                               <*> peek (ptr `plusPtr` 60)
                               <*> peek (ptr `plusPtr` 64)
                               <*> peek (ptr `plusPtr` 72)
                               <*> peek (ptr `plusPtr` 80)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkImageType (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkFormat (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkExtent (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkMipLevels (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkArrayLayers (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkSamples (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkTiling (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkUsage (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 60) (vkSharingMode (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkQueueFamilyIndexCount (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkPQueueFamilyIndices (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 80) (vkInitialLayout (poked :: VkImageCreateInfo))
-- ** VkImageLayout

-- No documentation found for TopLevel "VkImageLayout"
newtype VkImageLayout = VkImageLayout Int32
  deriving (Eq, Ord, Storable)

instance Show VkImageLayout where
  showsPrec _ VK_IMAGE_LAYOUT_UNDEFINED = showString "VK_IMAGE_LAYOUT_UNDEFINED"
  showsPrec _ VK_IMAGE_LAYOUT_GENERAL = showString "VK_IMAGE_LAYOUT_GENERAL"
  showsPrec _ VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = showString "VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = showString "VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = showString "VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = showString "VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = showString "VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = showString "VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_PREINITIALIZED = showString "VK_IMAGE_LAYOUT_PREINITIALIZED"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkImageLayout 1000117000) = showString "VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL"
  showsPrec _ (VkImageLayout 1000117001) = showString "VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL"
  showsPrec _ (VkImageLayout 1000001002) = showString "VK_IMAGE_LAYOUT_PRESENT_SRC_KHR"
  showsPrec _ (VkImageLayout 1000111000) = showString "VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR"
  showsPrec _ (VkImageLayout 1000164003) = showString "VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV"
  showsPrec _ (VkImageLayout 1000218000) = showString "VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT"
  showsPrec p (VkImageLayout x) = showParen (p >= 11) (showString "VkImageLayout " . showsPrec 11 x)

instance Read VkImageLayout where
  readPrec = parens ( choose [ ("VK_IMAGE_LAYOUT_UNDEFINED",                        pure VK_IMAGE_LAYOUT_UNDEFINED)
                             , ("VK_IMAGE_LAYOUT_GENERAL",                          pure VK_IMAGE_LAYOUT_GENERAL)
                             , ("VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL",         pure VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL", pure VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL",  pure VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL",         pure VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL",             pure VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL",             pure VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_PREINITIALIZED",                   pure VK_IMAGE_LAYOUT_PREINITIALIZED)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL", pure (VkImageLayout 1000117000))
                             , ("VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL", pure (VkImageLayout 1000117001))
                             , ("VK_IMAGE_LAYOUT_PRESENT_SRC_KHR",                            pure (VkImageLayout 1000001002))
                             , ("VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR",                         pure (VkImageLayout 1000111000))
                             , ("VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV",                    pure (VkImageLayout 1000164003))
                             , ("VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT",           pure (VkImageLayout 1000218000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageLayout")
                        v <- step readPrec
                        pure (VkImageLayout v)
                        )
                    )

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_UNDEFINED"
pattern VK_IMAGE_LAYOUT_UNDEFINED :: VkImageLayout
pattern VK_IMAGE_LAYOUT_UNDEFINED = VkImageLayout 0

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_GENERAL"
pattern VK_IMAGE_LAYOUT_GENERAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_GENERAL = VkImageLayout 1

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL"
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = VkImageLayout 2

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL"
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = VkImageLayout 3

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL"
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = VkImageLayout 4

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL"
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = VkImageLayout 5

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL"
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = VkImageLayout 6

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL"
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = VkImageLayout 7

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_PREINITIALIZED"
pattern VK_IMAGE_LAYOUT_PREINITIALIZED :: VkImageLayout
pattern VK_IMAGE_LAYOUT_PREINITIALIZED = VkImageLayout 8
-- No documentation found for TopLevel "VkSubresourceLayout"
data VkSubresourceLayout = VkSubresourceLayout
  { -- No documentation found for Nested "VkSubresourceLayout" "offset"
  vkOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkSubresourceLayout" "size"
  vkSize :: VkDeviceSize
  , -- No documentation found for Nested "VkSubresourceLayout" "rowPitch"
  vkRowPitch :: VkDeviceSize
  , -- No documentation found for Nested "VkSubresourceLayout" "arrayPitch"
  vkArrayPitch :: VkDeviceSize
  , -- No documentation found for Nested "VkSubresourceLayout" "depthPitch"
  vkDepthPitch :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkSubresourceLayout where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkSubresourceLayout <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkOffset (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 8) (vkSize (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 16) (vkRowPitch (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 24) (vkArrayPitch (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 32) (vkDepthPitch (poked :: VkSubresourceLayout))
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCreateImage"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateImage" vkCreateImage :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pImage" ::: Ptr VkImage) -> IO VkResult

#endif
type FN_vkCreateImage = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pImage" ::: Ptr VkImage) -> IO VkResult
type PFN_vkCreateImage = FunPtr FN_vkCreateImage
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkDestroyImage"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyImage" vkDestroyImage :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyImage = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyImage = FunPtr FN_vkDestroyImage
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkGetImageSubresourceLayout"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageSubresourceLayout" vkGetImageSubresourceLayout :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSubresource" ::: Ptr VkImageSubresource) -> ("pLayout" ::: Ptr VkSubresourceLayout) -> IO ()

#endif
type FN_vkGetImageSubresourceLayout = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSubresource" ::: Ptr VkImageSubresource) -> ("pLayout" ::: Ptr VkSubresourceLayout) -> IO ()
type PFN_vkGetImageSubresourceLayout = FunPtr FN_vkGetImageSubresourceLayout
