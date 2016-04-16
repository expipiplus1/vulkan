{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Image where

import Graphics.Vulkan.Device( Device(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( VkAllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Sampler( VkSampleCountFlags(..)
                              )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkFormat(..)
                           , VkFlags(..)
                           , VkResult(..)
                           , VkDeviceSize(..)
                           , VkExtent3D(..)
                           , VkSharingMode(..)
                           )

-- ** vkCreateImage
foreign import ccall "vkCreateImage" vkCreateImage ::
  Device ->
  Ptr VkImageCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr Image -> IO VkResult

-- ** VkImageCreateFlags

newtype VkImageCreateFlags = VkImageCreateFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkImageCreateFlags where
  showsPrec _ VK_IMAGE_CREATE_SPARSE_BINDING_BIT = showString "VK_IMAGE_CREATE_SPARSE_BINDING_BIT"
  showsPrec _ VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = showString "VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT"
  showsPrec _ VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = showString "VK_IMAGE_CREATE_SPARSE_ALIASED_BIT"
  showsPrec _ VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = showString "VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT"
  showsPrec _ VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = showString "VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT"
  
  showsPrec p (VkImageCreateFlags x) = showParen (p >= 11) (showString "VkImageCreateFlags " . showsPrec 11 x)

instance Read VkImageCreateFlags where
  readPrec = parens ( choose [ ("VK_IMAGE_CREATE_SPARSE_BINDING_BIT", pure VK_IMAGE_CREATE_SPARSE_BINDING_BIT)
                             , ("VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT", pure VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT)
                             , ("VK_IMAGE_CREATE_SPARSE_ALIASED_BIT", pure VK_IMAGE_CREATE_SPARSE_ALIASED_BIT)
                             , ("VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT", pure VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT)
                             , ("VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT", pure VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageCreateFlags")
                        v <- step readPrec
                        pure (VkImageCreateFlags v)
                        )
                    )

-- | Image should support sparse backing
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT = VkImageCreateFlags 0x1
-- | Image should support sparse backing with partial residency
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = VkImageCreateFlags 0x2
-- | Image should support constent data access to physical memory blocks mapped into multiple locations of sparse images
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = VkImageCreateFlags 0x4
-- | Allows image views to have different format than the base image
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = VkImageCreateFlags 0x8
-- | Allows creating image views with cube type from the created image
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = VkImageCreateFlags 0x10


-- ** VkImageUsageFlags

newtype VkImageUsageFlags = VkImageUsageFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkImageUsageFlags where
  showsPrec _ VK_IMAGE_USAGE_TRANSFER_SRC_BIT = showString "VK_IMAGE_USAGE_TRANSFER_SRC_BIT"
  showsPrec _ VK_IMAGE_USAGE_TRANSFER_DST_BIT = showString "VK_IMAGE_USAGE_TRANSFER_DST_BIT"
  showsPrec _ VK_IMAGE_USAGE_SAMPLED_BIT = showString "VK_IMAGE_USAGE_SAMPLED_BIT"
  showsPrec _ VK_IMAGE_USAGE_STORAGE_BIT = showString "VK_IMAGE_USAGE_STORAGE_BIT"
  showsPrec _ VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
  
  showsPrec p (VkImageUsageFlags x) = showParen (p >= 11) (showString "VkImageUsageFlags " . showsPrec 11 x)

instance Read VkImageUsageFlags where
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
                        expectP (Ident "VkImageUsageFlags")
                        v <- step readPrec
                        pure (VkImageUsageFlags v)
                        )
                    )

-- | Can be used as a source of transfer operations
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = VkImageUsageFlags 0x1
-- | Can be used as a destination of transfer operations
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = VkImageUsageFlags 0x2
-- | Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
pattern VK_IMAGE_USAGE_SAMPLED_BIT = VkImageUsageFlags 0x4
-- | Can be used as storage image (STORAGE_IMAGE descriptor type)
pattern VK_IMAGE_USAGE_STORAGE_BIT = VkImageUsageFlags 0x8
-- | Can be used as framebuffer color attachment
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = VkImageUsageFlags 0x10
-- | Can be used as framebuffer depth/stencil attachment
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = VkImageUsageFlags 0x20
-- | Image data not needed outside of rendering
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = VkImageUsageFlags 0x40
-- | Can be used as framebuffer input attachment
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = VkImageUsageFlags 0x80


newtype Image = Image Word64
  deriving (Eq, Storable)

-- ** VkImageAspectFlags

newtype VkImageAspectFlags = VkImageAspectFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkImageAspectFlags where
  showsPrec _ VK_IMAGE_ASPECT_COLOR_BIT = showString "VK_IMAGE_ASPECT_COLOR_BIT"
  showsPrec _ VK_IMAGE_ASPECT_DEPTH_BIT = showString "VK_IMAGE_ASPECT_DEPTH_BIT"
  showsPrec _ VK_IMAGE_ASPECT_STENCIL_BIT = showString "VK_IMAGE_ASPECT_STENCIL_BIT"
  showsPrec _ VK_IMAGE_ASPECT_METADATA_BIT = showString "VK_IMAGE_ASPECT_METADATA_BIT"
  
  showsPrec p (VkImageAspectFlags x) = showParen (p >= 11) (showString "VkImageAspectFlags " . showsPrec 11 x)

instance Read VkImageAspectFlags where
  readPrec = parens ( choose [ ("VK_IMAGE_ASPECT_COLOR_BIT", pure VK_IMAGE_ASPECT_COLOR_BIT)
                             , ("VK_IMAGE_ASPECT_DEPTH_BIT", pure VK_IMAGE_ASPECT_DEPTH_BIT)
                             , ("VK_IMAGE_ASPECT_STENCIL_BIT", pure VK_IMAGE_ASPECT_STENCIL_BIT)
                             , ("VK_IMAGE_ASPECT_METADATA_BIT", pure VK_IMAGE_ASPECT_METADATA_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageAspectFlags")
                        v <- step readPrec
                        pure (VkImageAspectFlags v)
                        )
                    )


pattern VK_IMAGE_ASPECT_COLOR_BIT = VkImageAspectFlags 0x1

pattern VK_IMAGE_ASPECT_DEPTH_BIT = VkImageAspectFlags 0x2

pattern VK_IMAGE_ASPECT_STENCIL_BIT = VkImageAspectFlags 0x4

pattern VK_IMAGE_ASPECT_METADATA_BIT = VkImageAspectFlags 0x8



data VkSubresourceLayout =
  VkSubresourceLayout{ offset :: VkDeviceSize 
                     , size :: VkDeviceSize 
                     , rowPitch :: VkDeviceSize 
                     , arrayPitch :: VkDeviceSize 
                     , depthPitch :: VkDeviceSize 
                     }
  deriving (Eq)

instance Storable VkSubresourceLayout where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkSubresourceLayout <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (offset (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 8) (size (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 16) (rowPitch (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 24) (arrayPitch (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 32) (depthPitch (poked :: VkSubresourceLayout))


-- ** VkImageTiling

newtype VkImageTiling = VkImageTiling Int32
  deriving (Eq, Storable)

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
  deriving (Eq, Storable)

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
  deriving (Eq, Storable)

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
  Device -> Image -> Ptr VkAllocationCallbacks -> IO ()


data VkImageSubresource =
  VkImageSubresource{ aspectMask :: VkImageAspectFlags 
                    , mipLevel :: Word32 
                    , arrayLayer :: Word32 
                    }
  deriving (Eq)

instance Storable VkImageSubresource where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkImageSubresource <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (aspectMask (poked :: VkImageSubresource))
                *> poke (ptr `plusPtr` 4) (mipLevel (poked :: VkImageSubresource))
                *> poke (ptr `plusPtr` 8) (arrayLayer (poked :: VkImageSubresource))



data VkImageSubresourceRange =
  VkImageSubresourceRange{ aspectMask :: VkImageAspectFlags 
                         , baseMipLevel :: Word32 
                         , levelCount :: Word32 
                         , baseArrayLayer :: Word32 
                         , layerCount :: Word32 
                         }
  deriving (Eq)

instance Storable VkImageSubresourceRange where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkImageSubresourceRange <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (aspectMask (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 4) (baseMipLevel (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 8) (levelCount (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 12) (baseArrayLayer (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 16) (layerCount (poked :: VkImageSubresourceRange))


-- ** vkGetImageSubresourceLayout
foreign import ccall "vkGetImageSubresourceLayout" vkGetImageSubresourceLayout ::
  Device ->
  Image -> Ptr VkImageSubresource -> Ptr VkSubresourceLayout -> IO ()


data VkImageCreateInfo =
  VkImageCreateInfo{ sType :: VkStructureType 
                   , pNext :: Ptr Void 
                   , flags :: VkImageCreateFlags 
                   , imageType :: VkImageType 
                   , format :: VkFormat 
                   , extent :: VkExtent3D 
                   , mipLevels :: Word32 
                   , arrayLayers :: Word32 
                   , samples :: VkSampleCountFlags 
                   , tiling :: VkImageTiling 
                   , usage :: VkImageUsageFlags 
                   , sharingMode :: VkSharingMode 
                   , queueFamilyIndexCount :: Word32 
                   , pQueueFamilyIndices :: Ptr Word32 
                   , initialLayout :: VkImageLayout 
                   }
  deriving (Eq)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 20) (imageType (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 24) (format (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 28) (extent (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 40) (mipLevels (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 44) (arrayLayers (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 48) (samples (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 52) (tiling (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 56) (usage (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 60) (sharingMode (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 64) (queueFamilyIndexCount (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 72) (pQueueFamilyIndices (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 80) (initialLayout (poked :: VkImageCreateInfo))


