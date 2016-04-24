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
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Sampler( SampleCountFlags(..)
                              )
import Graphics.Vulkan.Core( SharingMode(..)
                           , StructureType(..)
                           , Format(..)
                           , Extent3D(..)
                           , Result(..)
                           , DeviceSize(..)
                           , Flags(..)
                           )

-- ** createImage
foreign import ccall "vkCreateImage" createImage ::
  Device ->
  Ptr ImageCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Image -> IO Result

-- ** ImageCreateFlags

newtype ImageCreateFlags = ImageCreateFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show ImageCreateFlags where
  showsPrec _ VK_IMAGE_CREATE_SPARSE_BINDING_BIT = showString "VK_IMAGE_CREATE_SPARSE_BINDING_BIT"
  showsPrec _ VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = showString "VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT"
  showsPrec _ VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = showString "VK_IMAGE_CREATE_SPARSE_ALIASED_BIT"
  showsPrec _ VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = showString "VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT"
  showsPrec _ VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = showString "VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT"
  
  showsPrec p (ImageCreateFlags x) = showParen (p >= 11) (showString "ImageCreateFlags " . showsPrec 11 x)

instance Read ImageCreateFlags where
  readPrec = parens ( choose [ ("VK_IMAGE_CREATE_SPARSE_BINDING_BIT", pure VK_IMAGE_CREATE_SPARSE_BINDING_BIT)
                             , ("VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT", pure VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT)
                             , ("VK_IMAGE_CREATE_SPARSE_ALIASED_BIT", pure VK_IMAGE_CREATE_SPARSE_ALIASED_BIT)
                             , ("VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT", pure VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT)
                             , ("VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT", pure VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ImageCreateFlags")
                        v <- step readPrec
                        pure (ImageCreateFlags v)
                        )
                    )

-- | Image should support sparse backing
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT = ImageCreateFlags 0x1
-- | Image should support sparse backing with partial residency
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = ImageCreateFlags 0x2
-- | Image should support constent data access to physical memory blocks mapped into multiple locations of sparse images
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = ImageCreateFlags 0x4
-- | Allows image views to have different format than the base image
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = ImageCreateFlags 0x8
-- | Allows creating image views with cube type from the created image
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = ImageCreateFlags 0x10


-- ** ImageUsageFlags

newtype ImageUsageFlags = ImageUsageFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show ImageUsageFlags where
  showsPrec _ VK_IMAGE_USAGE_TRANSFER_SRC_BIT = showString "VK_IMAGE_USAGE_TRANSFER_SRC_BIT"
  showsPrec _ VK_IMAGE_USAGE_TRANSFER_DST_BIT = showString "VK_IMAGE_USAGE_TRANSFER_DST_BIT"
  showsPrec _ VK_IMAGE_USAGE_SAMPLED_BIT = showString "VK_IMAGE_USAGE_SAMPLED_BIT"
  showsPrec _ VK_IMAGE_USAGE_STORAGE_BIT = showString "VK_IMAGE_USAGE_STORAGE_BIT"
  showsPrec _ VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
  
  showsPrec p (ImageUsageFlags x) = showParen (p >= 11) (showString "ImageUsageFlags " . showsPrec 11 x)

instance Read ImageUsageFlags where
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
                        expectP (Ident "ImageUsageFlags")
                        v <- step readPrec
                        pure (ImageUsageFlags v)
                        )
                    )

-- | Can be used as a source of transfer operations
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = ImageUsageFlags 0x1
-- | Can be used as a destination of transfer operations
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = ImageUsageFlags 0x2
-- | Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
pattern VK_IMAGE_USAGE_SAMPLED_BIT = ImageUsageFlags 0x4
-- | Can be used as storage image (STORAGE_IMAGE descriptor type)
pattern VK_IMAGE_USAGE_STORAGE_BIT = ImageUsageFlags 0x8
-- | Can be used as framebuffer color attachment
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = ImageUsageFlags 0x10
-- | Can be used as framebuffer depth/stencil attachment
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = ImageUsageFlags 0x20
-- | Image data not needed outside of rendering
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = ImageUsageFlags 0x40
-- | Can be used as framebuffer input attachment
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = ImageUsageFlags 0x80


newtype Image = Image Word64
  deriving (Eq, Storable)

-- ** ImageAspectFlags

newtype ImageAspectFlags = ImageAspectFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show ImageAspectFlags where
  showsPrec _ VK_IMAGE_ASPECT_COLOR_BIT = showString "VK_IMAGE_ASPECT_COLOR_BIT"
  showsPrec _ VK_IMAGE_ASPECT_DEPTH_BIT = showString "VK_IMAGE_ASPECT_DEPTH_BIT"
  showsPrec _ VK_IMAGE_ASPECT_STENCIL_BIT = showString "VK_IMAGE_ASPECT_STENCIL_BIT"
  showsPrec _ VK_IMAGE_ASPECT_METADATA_BIT = showString "VK_IMAGE_ASPECT_METADATA_BIT"
  
  showsPrec p (ImageAspectFlags x) = showParen (p >= 11) (showString "ImageAspectFlags " . showsPrec 11 x)

instance Read ImageAspectFlags where
  readPrec = parens ( choose [ ("VK_IMAGE_ASPECT_COLOR_BIT", pure VK_IMAGE_ASPECT_COLOR_BIT)
                             , ("VK_IMAGE_ASPECT_DEPTH_BIT", pure VK_IMAGE_ASPECT_DEPTH_BIT)
                             , ("VK_IMAGE_ASPECT_STENCIL_BIT", pure VK_IMAGE_ASPECT_STENCIL_BIT)
                             , ("VK_IMAGE_ASPECT_METADATA_BIT", pure VK_IMAGE_ASPECT_METADATA_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ImageAspectFlags")
                        v <- step readPrec
                        pure (ImageAspectFlags v)
                        )
                    )


pattern VK_IMAGE_ASPECT_COLOR_BIT = ImageAspectFlags 0x1

pattern VK_IMAGE_ASPECT_DEPTH_BIT = ImageAspectFlags 0x2

pattern VK_IMAGE_ASPECT_STENCIL_BIT = ImageAspectFlags 0x4

pattern VK_IMAGE_ASPECT_METADATA_BIT = ImageAspectFlags 0x8



data SubresourceLayout =
  SubresourceLayout{ offset :: DeviceSize 
                   , size :: DeviceSize 
                   , rowPitch :: DeviceSize 
                   , arrayPitch :: DeviceSize 
                   , depthPitch :: DeviceSize 
                   }
  deriving (Eq)

instance Storable SubresourceLayout where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = SubresourceLayout <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 24)
                               <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (offset (poked :: SubresourceLayout))
                *> poke (ptr `plusPtr` 8) (size (poked :: SubresourceLayout))
                *> poke (ptr `plusPtr` 16) (rowPitch (poked :: SubresourceLayout))
                *> poke (ptr `plusPtr` 24) (arrayPitch (poked :: SubresourceLayout))
                *> poke (ptr `plusPtr` 32) (depthPitch (poked :: SubresourceLayout))


-- ** ImageTiling

newtype ImageTiling = ImageTiling Int32
  deriving (Eq, Storable)

instance Show ImageTiling where
  showsPrec _ VK_IMAGE_TILING_OPTIMAL = showString "VK_IMAGE_TILING_OPTIMAL"
  showsPrec _ VK_IMAGE_TILING_LINEAR = showString "VK_IMAGE_TILING_LINEAR"
  showsPrec p (ImageTiling x) = showParen (p >= 11) (showString "ImageTiling " . showsPrec 11 x)

instance Read ImageTiling where
  readPrec = parens ( choose [ ("VK_IMAGE_TILING_OPTIMAL", pure VK_IMAGE_TILING_OPTIMAL)
                             , ("VK_IMAGE_TILING_LINEAR", pure VK_IMAGE_TILING_LINEAR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ImageTiling")
                        v <- step readPrec
                        pure (ImageTiling v)
                        )
                    )


pattern VK_IMAGE_TILING_OPTIMAL = ImageTiling 0

pattern VK_IMAGE_TILING_LINEAR = ImageTiling 1

-- ** ImageLayout

newtype ImageLayout = ImageLayout Int32
  deriving (Eq, Storable)

instance Show ImageLayout where
  showsPrec _ VK_IMAGE_LAYOUT_UNDEFINED = showString "VK_IMAGE_LAYOUT_UNDEFINED"
  showsPrec _ VK_IMAGE_LAYOUT_GENERAL = showString "VK_IMAGE_LAYOUT_GENERAL"
  showsPrec _ VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = showString "VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = showString "VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = showString "VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = showString "VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = showString "VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = showString "VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL"
  showsPrec _ VK_IMAGE_LAYOUT_PREINITIALIZED = showString "VK_IMAGE_LAYOUT_PREINITIALIZED"
  showsPrec p (ImageLayout x) = showParen (p >= 11) (showString "ImageLayout " . showsPrec 11 x)

instance Read ImageLayout where
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
                        expectP (Ident "ImageLayout")
                        v <- step readPrec
                        pure (ImageLayout v)
                        )
                    )

-- | Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
pattern VK_IMAGE_LAYOUT_UNDEFINED = ImageLayout 0
-- | General layout when image can be used for any kind of access
pattern VK_IMAGE_LAYOUT_GENERAL = ImageLayout 1
-- | Optimal layout when image is only used for color attachment read/write
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = ImageLayout 2
-- | Optimal layout when image is only used for depth/stencil attachment read/write
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = ImageLayout 3
-- | Optimal layout when image is used for read only depth/stencil attachment and shader access
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = ImageLayout 4
-- | Optimal layout when image is used for read only shader access
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = ImageLayout 5
-- | Optimal layout when image is used only as source of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = ImageLayout 6
-- | Optimal layout when image is used only as destination of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = ImageLayout 7
-- | Initial layout used when the data is populated by the CPU
pattern VK_IMAGE_LAYOUT_PREINITIALIZED = ImageLayout 8

-- ** ImageType

newtype ImageType = ImageType Int32
  deriving (Eq, Storable)

instance Show ImageType where
  showsPrec _ VK_IMAGE_TYPE_1D = showString "VK_IMAGE_TYPE_1D"
  showsPrec _ VK_IMAGE_TYPE_2D = showString "VK_IMAGE_TYPE_2D"
  showsPrec _ VK_IMAGE_TYPE_3D = showString "VK_IMAGE_TYPE_3D"
  showsPrec p (ImageType x) = showParen (p >= 11) (showString "ImageType " . showsPrec 11 x)

instance Read ImageType where
  readPrec = parens ( choose [ ("VK_IMAGE_TYPE_1D", pure VK_IMAGE_TYPE_1D)
                             , ("VK_IMAGE_TYPE_2D", pure VK_IMAGE_TYPE_2D)
                             , ("VK_IMAGE_TYPE_3D", pure VK_IMAGE_TYPE_3D)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ImageType")
                        v <- step readPrec
                        pure (ImageType v)
                        )
                    )


pattern VK_IMAGE_TYPE_1D = ImageType 0

pattern VK_IMAGE_TYPE_2D = ImageType 1

pattern VK_IMAGE_TYPE_3D = ImageType 2

-- ** destroyImage
foreign import ccall "vkDestroyImage" destroyImage ::
  Device -> Image -> Ptr AllocationCallbacks -> IO ()


data ImageSubresource =
  ImageSubresource{ aspectMask :: ImageAspectFlags 
                  , mipLevel :: Word32 
                  , arrayLayer :: Word32 
                  }
  deriving (Eq)

instance Storable ImageSubresource where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = ImageSubresource <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 4)
                              <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (aspectMask (poked :: ImageSubresource))
                *> poke (ptr `plusPtr` 4) (mipLevel (poked :: ImageSubresource))
                *> poke (ptr `plusPtr` 8) (arrayLayer (poked :: ImageSubresource))



data ImageSubresourceRange =
  ImageSubresourceRange{ aspectMask :: ImageAspectFlags 
                       , baseMipLevel :: Word32 
                       , levelCount :: Word32 
                       , baseArrayLayer :: Word32 
                       , layerCount :: Word32 
                       }
  deriving (Eq)

instance Storable ImageSubresourceRange where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = ImageSubresourceRange <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 12)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (aspectMask (poked :: ImageSubresourceRange))
                *> poke (ptr `plusPtr` 4) (baseMipLevel (poked :: ImageSubresourceRange))
                *> poke (ptr `plusPtr` 8) (levelCount (poked :: ImageSubresourceRange))
                *> poke (ptr `plusPtr` 12) (baseArrayLayer (poked :: ImageSubresourceRange))
                *> poke (ptr `plusPtr` 16) (layerCount (poked :: ImageSubresourceRange))


-- ** getImageSubresourceLayout
foreign import ccall "vkGetImageSubresourceLayout" getImageSubresourceLayout ::
  Device ->
  Image -> Ptr ImageSubresource -> Ptr SubresourceLayout -> IO ()


data ImageCreateInfo =
  ImageCreateInfo{ sType :: StructureType 
                 , pNext :: Ptr Void 
                 , flags :: ImageCreateFlags 
                 , imageType :: ImageType 
                 , format :: Format 
                 , extent :: Extent3D 
                 , mipLevels :: Word32 
                 , arrayLayers :: Word32 
                 , samples :: SampleCountFlags 
                 , tiling :: ImageTiling 
                 , usage :: ImageUsageFlags 
                 , sharingMode :: SharingMode 
                 , queueFamilyIndexCount :: Word32 
                 , pQueueFamilyIndices :: Ptr Word32 
                 , initialLayout :: ImageLayout 
                 }
  deriving (Eq)

instance Storable ImageCreateInfo where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek ptr = ImageCreateInfo <$> peek (ptr `plusPtr` 0)
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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 20) (imageType (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 24) (format (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 28) (extent (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 40) (mipLevels (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 44) (arrayLayers (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 48) (samples (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 52) (tiling (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 56) (usage (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 60) (sharingMode (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 64) (queueFamilyIndexCount (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 72) (pQueueFamilyIndices (poked :: ImageCreateInfo))
                *> poke (ptr `plusPtr` 80) (initialLayout (poked :: ImageCreateInfo))


