{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Image where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkInternalAllocationType(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkSystemAllocationScope(..)
                             , PFN_vkFreeFunction
                             , PFN_vkInternalFreeNotification
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Sampler( VkSampleCountFlagBits(..)
                              )
import Graphics.Vulkan.Core( VkExtent3D(..)
                           , VkResult(..)
                           , VkDeviceSize(..)
                           , VkFlags(..)
                           , VkFormat(..)
                           , VkStructureType(..)
                           , VkSharingMode(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

-- ** vkCreateImage
foreign import ccall "vkCreateImage" vkCreateImage ::
  VkDevice ->
  Ptr VkImageCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkImage -> IO VkResult

-- ** VkImageCreateFlags

newtype VkImageCreateFlagBits = VkImageCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkImageCreateFlagBits
type VkImageCreateFlags = VkImageCreateFlagBits

instance Show VkImageCreateFlagBits where
  showsPrec _ VK_IMAGE_CREATE_SPARSE_BINDING_BIT = showString "VK_IMAGE_CREATE_SPARSE_BINDING_BIT"
  showsPrec _ VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = showString "VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT"
  showsPrec _ VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = showString "VK_IMAGE_CREATE_SPARSE_ALIASED_BIT"
  showsPrec _ VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = showString "VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT"
  showsPrec _ VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = showString "VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT"
  
  showsPrec p (VkImageCreateFlagBits x) = showParen (p >= 11) (showString "VkImageCreateFlagBits " . showsPrec 11 x)

instance Read VkImageCreateFlagBits where
  readPrec = parens ( choose [ ("VK_IMAGE_CREATE_SPARSE_BINDING_BIT", pure VK_IMAGE_CREATE_SPARSE_BINDING_BIT)
                             , ("VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT", pure VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT)
                             , ("VK_IMAGE_CREATE_SPARSE_ALIASED_BIT", pure VK_IMAGE_CREATE_SPARSE_ALIASED_BIT)
                             , ("VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT", pure VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT)
                             , ("VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT", pure VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageCreateFlagBits")
                        v <- step readPrec
                        pure (VkImageCreateFlagBits v)
                        )
                    )

-- | Image should support sparse backing
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT = VkImageCreateFlagBits 0x1
-- | Image should support sparse backing with partial residency
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = VkImageCreateFlagBits 0x2
-- | Image should support constent data access to physical memory ranges mapped into multiple locations of sparse images
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = VkImageCreateFlagBits 0x4
-- | Allows image views to have different format than the base image
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = VkImageCreateFlagBits 0x8
-- | Allows creating image views with cube type from the created image
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = VkImageCreateFlagBits 0x10


-- ** VkImageUsageFlags

newtype VkImageUsageFlagBits = VkImageUsageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkImageUsageFlagBits
type VkImageUsageFlags = VkImageUsageFlagBits

instance Show VkImageUsageFlagBits where
  showsPrec _ VK_IMAGE_USAGE_TRANSFER_SRC_BIT = showString "VK_IMAGE_USAGE_TRANSFER_SRC_BIT"
  showsPrec _ VK_IMAGE_USAGE_TRANSFER_DST_BIT = showString "VK_IMAGE_USAGE_TRANSFER_DST_BIT"
  showsPrec _ VK_IMAGE_USAGE_SAMPLED_BIT = showString "VK_IMAGE_USAGE_SAMPLED_BIT"
  showsPrec _ VK_IMAGE_USAGE_STORAGE_BIT = showString "VK_IMAGE_USAGE_STORAGE_BIT"
  showsPrec _ VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
  
  showsPrec p (VkImageUsageFlagBits x) = showParen (p >= 11) (showString "VkImageUsageFlagBits " . showsPrec 11 x)

instance Read VkImageUsageFlagBits where
  readPrec = parens ( choose [ ("VK_IMAGE_USAGE_TRANSFER_SRC_BIT", pure VK_IMAGE_USAGE_TRANSFER_SRC_BIT)
                             , ("VK_IMAGE_USAGE_TRANSFER_DST_BIT", pure VK_IMAGE_USAGE_TRANSFER_DST_BIT)
                             , ("VK_IMAGE_USAGE_SAMPLED_BIT", pure VK_IMAGE_USAGE_SAMPLED_BIT)
                             , ("VK_IMAGE_USAGE_STORAGE_BIT", pure VK_IMAGE_USAGE_STORAGE_BIT)
                             , ("VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT", pure VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
                             , ("VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT", pure VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT)
                             , ("VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT", pure VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT)
                             , ("VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT", pure VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageUsageFlagBits")
                        v <- step readPrec
                        pure (VkImageUsageFlagBits v)
                        )
                    )

-- | Can be used as a source of transfer operations
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = VkImageUsageFlagBits 0x1
-- | Can be used as a destination of transfer operations
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = VkImageUsageFlagBits 0x2
-- | Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
pattern VK_IMAGE_USAGE_SAMPLED_BIT = VkImageUsageFlagBits 0x4
-- | Can be used as storage image (STORAGE_IMAGE descriptor type)
pattern VK_IMAGE_USAGE_STORAGE_BIT = VkImageUsageFlagBits 0x8
-- | Can be used as framebuffer color attachment
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = VkImageUsageFlagBits 0x10
-- | Can be used as framebuffer depth/stencil attachment
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = VkImageUsageFlagBits 0x20
-- | Image data not needed outside of rendering
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = VkImageUsageFlagBits 0x40
-- | Can be used as framebuffer input attachment
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = VkImageUsageFlagBits 0x80


newtype VkImage = VkImage Word64
  deriving (Eq, Ord, Storable, Show)

-- ** VkImageAspectFlags

newtype VkImageAspectFlagBits = VkImageAspectFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkImageAspectFlagBits
type VkImageAspectFlags = VkImageAspectFlagBits

instance Show VkImageAspectFlagBits where
  showsPrec _ VK_IMAGE_ASPECT_COLOR_BIT = showString "VK_IMAGE_ASPECT_COLOR_BIT"
  showsPrec _ VK_IMAGE_ASPECT_DEPTH_BIT = showString "VK_IMAGE_ASPECT_DEPTH_BIT"
  showsPrec _ VK_IMAGE_ASPECT_STENCIL_BIT = showString "VK_IMAGE_ASPECT_STENCIL_BIT"
  showsPrec _ VK_IMAGE_ASPECT_METADATA_BIT = showString "VK_IMAGE_ASPECT_METADATA_BIT"
  
  showsPrec p (VkImageAspectFlagBits x) = showParen (p >= 11) (showString "VkImageAspectFlagBits " . showsPrec 11 x)

instance Read VkImageAspectFlagBits where
  readPrec = parens ( choose [ ("VK_IMAGE_ASPECT_COLOR_BIT", pure VK_IMAGE_ASPECT_COLOR_BIT)
                             , ("VK_IMAGE_ASPECT_DEPTH_BIT", pure VK_IMAGE_ASPECT_DEPTH_BIT)
                             , ("VK_IMAGE_ASPECT_STENCIL_BIT", pure VK_IMAGE_ASPECT_STENCIL_BIT)
                             , ("VK_IMAGE_ASPECT_METADATA_BIT", pure VK_IMAGE_ASPECT_METADATA_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageAspectFlagBits")
                        v <- step readPrec
                        pure (VkImageAspectFlagBits v)
                        )
                    )


pattern VK_IMAGE_ASPECT_COLOR_BIT = VkImageAspectFlagBits 0x1

pattern VK_IMAGE_ASPECT_DEPTH_BIT = VkImageAspectFlagBits 0x2

pattern VK_IMAGE_ASPECT_STENCIL_BIT = VkImageAspectFlagBits 0x4

pattern VK_IMAGE_ASPECT_METADATA_BIT = VkImageAspectFlagBits 0x8



data VkSubresourceLayout =
  VkSubresourceLayout{ vkOffset :: VkDeviceSize 
                     , vkSize :: VkDeviceSize 
                     , vkRowPitch :: VkDeviceSize 
                     , vkArrayPitch :: VkDeviceSize 
                     , vkDepthPitch :: VkDeviceSize 
                     }
  deriving (Eq, Ord, Show)

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


-- ** VkImageTiling

newtype VkImageTiling = VkImageTiling Int32
  deriving (Eq, Ord, Storable)

instance Show VkImageTiling where
  showsPrec _ VK_IMAGE_TILING_OPTIMAL = showString "VK_IMAGE_TILING_OPTIMAL"
  showsPrec _ VK_IMAGE_TILING_LINEAR = showString "VK_IMAGE_TILING_LINEAR"
  showsPrec p (VkImageTiling x) = showParen (p >= 11) (showString "VkImageTiling " . showsPrec 11 x)

instance Read VkImageTiling where
  readPrec = parens ( choose [ ("VK_IMAGE_TILING_OPTIMAL", pure VK_IMAGE_TILING_OPTIMAL)
                             , ("VK_IMAGE_TILING_LINEAR", pure VK_IMAGE_TILING_LINEAR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageTiling")
                        v <- step readPrec
                        pure (VkImageTiling v)
                        )
                    )


pattern VK_IMAGE_TILING_OPTIMAL = VkImageTiling 0

pattern VK_IMAGE_TILING_LINEAR = VkImageTiling 1

-- ** VkImageLayout

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
  showsPrec p (VkImageLayout x) = showParen (p >= 11) (showString "VkImageLayout " . showsPrec 11 x)

instance Read VkImageLayout where
  readPrec = parens ( choose [ ("VK_IMAGE_LAYOUT_UNDEFINED", pure VK_IMAGE_LAYOUT_UNDEFINED)
                             , ("VK_IMAGE_LAYOUT_GENERAL", pure VK_IMAGE_LAYOUT_GENERAL)
                             , ("VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL", pure VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL", pure VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL", pure VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL", pure VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL", pure VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL", pure VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL)
                             , ("VK_IMAGE_LAYOUT_PREINITIALIZED", pure VK_IMAGE_LAYOUT_PREINITIALIZED)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageLayout")
                        v <- step readPrec
                        pure (VkImageLayout v)
                        )
                    )

-- | Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
pattern VK_IMAGE_LAYOUT_UNDEFINED = VkImageLayout 0
-- | General layout when image can be used for any kind of access
pattern VK_IMAGE_LAYOUT_GENERAL = VkImageLayout 1
-- | Optimal layout when image is only used for color attachment read/write
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = VkImageLayout 2
-- | Optimal layout when image is only used for depth/stencil attachment read/write
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = VkImageLayout 3
-- | Optimal layout when image is used for read only depth/stencil attachment and shader access
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = VkImageLayout 4
-- | Optimal layout when image is used for read only shader access
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = VkImageLayout 5
-- | Optimal layout when image is used only as source of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = VkImageLayout 6
-- | Optimal layout when image is used only as destination of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = VkImageLayout 7
-- | Initial layout used when the data is populated by the CPU
pattern VK_IMAGE_LAYOUT_PREINITIALIZED = VkImageLayout 8

-- ** VkImageType

newtype VkImageType = VkImageType Int32
  deriving (Eq, Ord, Storable)

instance Show VkImageType where
  showsPrec _ VK_IMAGE_TYPE_1D = showString "VK_IMAGE_TYPE_1D"
  showsPrec _ VK_IMAGE_TYPE_2D = showString "VK_IMAGE_TYPE_2D"
  showsPrec _ VK_IMAGE_TYPE_3D = showString "VK_IMAGE_TYPE_3D"
  showsPrec p (VkImageType x) = showParen (p >= 11) (showString "VkImageType " . showsPrec 11 x)

instance Read VkImageType where
  readPrec = parens ( choose [ ("VK_IMAGE_TYPE_1D", pure VK_IMAGE_TYPE_1D)
                             , ("VK_IMAGE_TYPE_2D", pure VK_IMAGE_TYPE_2D)
                             , ("VK_IMAGE_TYPE_3D", pure VK_IMAGE_TYPE_3D)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageType")
                        v <- step readPrec
                        pure (VkImageType v)
                        )
                    )


pattern VK_IMAGE_TYPE_1D = VkImageType 0

pattern VK_IMAGE_TYPE_2D = VkImageType 1

pattern VK_IMAGE_TYPE_3D = VkImageType 2

-- ** vkDestroyImage
foreign import ccall "vkDestroyImage" vkDestroyImage ::
  VkDevice -> VkImage -> Ptr VkAllocationCallbacks -> IO ()


data VkImageSubresource =
  VkImageSubresource{ vkAspectMask :: VkImageAspectFlags 
                    , vkMipLevel :: Word32 
                    , vkArrayLayer :: Word32 
                    }
  deriving (Eq, Ord, Show)

instance Storable VkImageSubresource where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkImageSubresource <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkImageSubresource))
                *> poke (ptr `plusPtr` 4) (vkMipLevel (poked :: VkImageSubresource))
                *> poke (ptr `plusPtr` 8) (vkArrayLayer (poked :: VkImageSubresource))



data VkImageSubresourceRange =
  VkImageSubresourceRange{ vkAspectMask :: VkImageAspectFlags 
                         , vkBaseMipLevel :: Word32 
                         , vkLevelCount :: Word32 
                         , vkBaseArrayLayer :: Word32 
                         , vkLayerCount :: Word32 
                         }
  deriving (Eq, Ord, Show)

instance Storable VkImageSubresourceRange where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkImageSubresourceRange <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 4) (vkBaseMipLevel (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 8) (vkLevelCount (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 12) (vkBaseArrayLayer (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 16) (vkLayerCount (poked :: VkImageSubresourceRange))


-- ** vkGetImageSubresourceLayout
foreign import ccall "vkGetImageSubresourceLayout" vkGetImageSubresourceLayout ::
  VkDevice ->
  VkImage ->
    Ptr VkImageSubresource -> Ptr VkSubresourceLayout -> IO ()


data VkImageCreateInfo =
  VkImageCreateInfo{ vkSType :: VkStructureType 
                   , vkPNext :: Ptr Void 
                   , vkFlags :: VkImageCreateFlags 
                   , vkImageType :: VkImageType 
                   , vkFormat :: VkFormat 
                   , vkExtent :: VkExtent3D 
                   , vkMipLevels :: Word32 
                   , vkArrayLayers :: Word32 
                   , vkSamples :: VkSampleCountFlagBits 
                   , vkTiling :: VkImageTiling 
                   , vkUsage :: VkImageUsageFlags 
                   , vkSharingMode :: VkSharingMode 
                   , vkQueueFamilyIndexCount :: Word32 
                   , vkPQueueFamilyIndices :: Ptr Word32 
                   , vkInitialLayout :: VkImageLayout 
                   }
  deriving (Eq, Ord, Show)

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


