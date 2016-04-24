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
  showsPrec _ ImageCreateSparseBindingBit = showString "ImageCreateSparseBindingBit"
  showsPrec _ ImageCreateSparseResidencyBit = showString "ImageCreateSparseResidencyBit"
  showsPrec _ ImageCreateSparseAliasedBit = showString "ImageCreateSparseAliasedBit"
  showsPrec _ ImageCreateMutableFormatBit = showString "ImageCreateMutableFormatBit"
  showsPrec _ ImageCreateCubeCompatibleBit = showString "ImageCreateCubeCompatibleBit"
  
  showsPrec p (ImageCreateFlags x) = showParen (p >= 11) (showString "ImageCreateFlags " . showsPrec 11 x)

instance Read ImageCreateFlags where
  readPrec = parens ( choose [ ("ImageCreateSparseBindingBit", pure ImageCreateSparseBindingBit)
                             , ("ImageCreateSparseResidencyBit", pure ImageCreateSparseResidencyBit)
                             , ("ImageCreateSparseAliasedBit", pure ImageCreateSparseAliasedBit)
                             , ("ImageCreateMutableFormatBit", pure ImageCreateMutableFormatBit)
                             , ("ImageCreateCubeCompatibleBit", pure ImageCreateCubeCompatibleBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ImageCreateFlags")
                        v <- step readPrec
                        pure (ImageCreateFlags v)
                        )
                    )

-- | Image should support sparse backing
pattern ImageCreateSparseBindingBit = ImageCreateFlags 0x1
-- | Image should support sparse backing with partial residency
pattern ImageCreateSparseResidencyBit = ImageCreateFlags 0x2
-- | Image should support constent data access to physical memory blocks mapped into multiple locations of sparse images
pattern ImageCreateSparseAliasedBit = ImageCreateFlags 0x4
-- | Allows image views to have different format than the base image
pattern ImageCreateMutableFormatBit = ImageCreateFlags 0x8
-- | Allows creating image views with cube type from the created image
pattern ImageCreateCubeCompatibleBit = ImageCreateFlags 0x10


-- ** ImageUsageFlags

newtype ImageUsageFlags = ImageUsageFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show ImageUsageFlags where
  showsPrec _ ImageUsageTransferSrcBit = showString "ImageUsageTransferSrcBit"
  showsPrec _ ImageUsageTransferDstBit = showString "ImageUsageTransferDstBit"
  showsPrec _ ImageUsageSampledBit = showString "ImageUsageSampledBit"
  showsPrec _ ImageUsageStorageBit = showString "ImageUsageStorageBit"
  showsPrec _ ImageUsageColorAttachmentBit = showString "ImageUsageColorAttachmentBit"
  showsPrec _ ImageUsageDepthStencilAttachmentBit = showString "ImageUsageDepthStencilAttachmentBit"
  showsPrec _ ImageUsageTransientAttachmentBit = showString "ImageUsageTransientAttachmentBit"
  showsPrec _ ImageUsageInputAttachmentBit = showString "ImageUsageInputAttachmentBit"
  
  showsPrec p (ImageUsageFlags x) = showParen (p >= 11) (showString "ImageUsageFlags " . showsPrec 11 x)

instance Read ImageUsageFlags where
  readPrec = parens ( choose [ ("ImageUsageTransferSrcBit", pure ImageUsageTransferSrcBit)
                             , ("ImageUsageTransferDstBit", pure ImageUsageTransferDstBit)
                             , ("ImageUsageSampledBit", pure ImageUsageSampledBit)
                             , ("ImageUsageStorageBit", pure ImageUsageStorageBit)
                             , ("ImageUsageColorAttachmentBit", pure ImageUsageColorAttachmentBit)
                             , ("ImageUsageDepthStencilAttachmentBit", pure ImageUsageDepthStencilAttachmentBit)
                             , ("ImageUsageTransientAttachmentBit", pure ImageUsageTransientAttachmentBit)
                             , ("ImageUsageInputAttachmentBit", pure ImageUsageInputAttachmentBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ImageUsageFlags")
                        v <- step readPrec
                        pure (ImageUsageFlags v)
                        )
                    )

-- | Can be used as a source of transfer operations
pattern ImageUsageTransferSrcBit = ImageUsageFlags 0x1
-- | Can be used as a destination of transfer operations
pattern ImageUsageTransferDstBit = ImageUsageFlags 0x2
-- | Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
pattern ImageUsageSampledBit = ImageUsageFlags 0x4
-- | Can be used as storage image (STORAGE_IMAGE descriptor type)
pattern ImageUsageStorageBit = ImageUsageFlags 0x8
-- | Can be used as framebuffer color attachment
pattern ImageUsageColorAttachmentBit = ImageUsageFlags 0x10
-- | Can be used as framebuffer depth/stencil attachment
pattern ImageUsageDepthStencilAttachmentBit = ImageUsageFlags 0x20
-- | Image data not needed outside of rendering
pattern ImageUsageTransientAttachmentBit = ImageUsageFlags 0x40
-- | Can be used as framebuffer input attachment
pattern ImageUsageInputAttachmentBit = ImageUsageFlags 0x80


newtype Image = Image Word64
  deriving (Eq, Storable)

-- ** ImageAspectFlags

newtype ImageAspectFlags = ImageAspectFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show ImageAspectFlags where
  showsPrec _ ImageAspectColorBit = showString "ImageAspectColorBit"
  showsPrec _ ImageAspectDepthBit = showString "ImageAspectDepthBit"
  showsPrec _ ImageAspectStencilBit = showString "ImageAspectStencilBit"
  showsPrec _ ImageAspectMetadataBit = showString "ImageAspectMetadataBit"
  
  showsPrec p (ImageAspectFlags x) = showParen (p >= 11) (showString "ImageAspectFlags " . showsPrec 11 x)

instance Read ImageAspectFlags where
  readPrec = parens ( choose [ ("ImageAspectColorBit", pure ImageAspectColorBit)
                             , ("ImageAspectDepthBit", pure ImageAspectDepthBit)
                             , ("ImageAspectStencilBit", pure ImageAspectStencilBit)
                             , ("ImageAspectMetadataBit", pure ImageAspectMetadataBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ImageAspectFlags")
                        v <- step readPrec
                        pure (ImageAspectFlags v)
                        )
                    )


pattern ImageAspectColorBit = ImageAspectFlags 0x1

pattern ImageAspectDepthBit = ImageAspectFlags 0x2

pattern ImageAspectStencilBit = ImageAspectFlags 0x4

pattern ImageAspectMetadataBit = ImageAspectFlags 0x8



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
  showsPrec _ ImageTilingOptimal = showString "ImageTilingOptimal"
  showsPrec _ ImageTilingLinear = showString "ImageTilingLinear"
  showsPrec p (ImageTiling x) = showParen (p >= 11) (showString "ImageTiling " . showsPrec 11 x)

instance Read ImageTiling where
  readPrec = parens ( choose [ ("ImageTilingOptimal", pure ImageTilingOptimal)
                             , ("ImageTilingLinear", pure ImageTilingLinear)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ImageTiling")
                        v <- step readPrec
                        pure (ImageTiling v)
                        )
                    )


pattern ImageTilingOptimal = ImageTiling 0

pattern ImageTilingLinear = ImageTiling 1

-- ** ImageLayout

newtype ImageLayout = ImageLayout Int32
  deriving (Eq, Storable)

instance Show ImageLayout where
  showsPrec _ ImageLayoutUndefined = showString "ImageLayoutUndefined"
  showsPrec _ ImageLayoutGeneral = showString "ImageLayoutGeneral"
  showsPrec _ ImageLayoutColorAttachmentOptimal = showString "ImageLayoutColorAttachmentOptimal"
  showsPrec _ ImageLayoutDepthStencilAttachmentOptimal = showString "ImageLayoutDepthStencilAttachmentOptimal"
  showsPrec _ ImageLayoutDepthStencilReadOnlyOptimal = showString "ImageLayoutDepthStencilReadOnlyOptimal"
  showsPrec _ ImageLayoutShaderReadOnlyOptimal = showString "ImageLayoutShaderReadOnlyOptimal"
  showsPrec _ ImageLayoutTransferSrcOptimal = showString "ImageLayoutTransferSrcOptimal"
  showsPrec _ ImageLayoutTransferDstOptimal = showString "ImageLayoutTransferDstOptimal"
  showsPrec _ ImageLayoutPreinitialized = showString "ImageLayoutPreinitialized"
  showsPrec p (ImageLayout x) = showParen (p >= 11) (showString "ImageLayout " . showsPrec 11 x)

instance Read ImageLayout where
  readPrec = parens ( choose [ ("ImageLayoutUndefined", pure ImageLayoutUndefined)
                             , ("ImageLayoutGeneral", pure ImageLayoutGeneral)
                             , ("ImageLayoutColorAttachmentOptimal", pure ImageLayoutColorAttachmentOptimal)
                             , ("ImageLayoutDepthStencilAttachmentOptimal", pure ImageLayoutDepthStencilAttachmentOptimal)
                             , ("ImageLayoutDepthStencilReadOnlyOptimal", pure ImageLayoutDepthStencilReadOnlyOptimal)
                             , ("ImageLayoutShaderReadOnlyOptimal", pure ImageLayoutShaderReadOnlyOptimal)
                             , ("ImageLayoutTransferSrcOptimal", pure ImageLayoutTransferSrcOptimal)
                             , ("ImageLayoutTransferDstOptimal", pure ImageLayoutTransferDstOptimal)
                             , ("ImageLayoutPreinitialized", pure ImageLayoutPreinitialized)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ImageLayout")
                        v <- step readPrec
                        pure (ImageLayout v)
                        )
                    )

-- | Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
pattern ImageLayoutUndefined = ImageLayout 0
-- | General layout when image can be used for any kind of access
pattern ImageLayoutGeneral = ImageLayout 1
-- | Optimal layout when image is only used for color attachment read/write
pattern ImageLayoutColorAttachmentOptimal = ImageLayout 2
-- | Optimal layout when image is only used for depth/stencil attachment read/write
pattern ImageLayoutDepthStencilAttachmentOptimal = ImageLayout 3
-- | Optimal layout when image is used for read only depth/stencil attachment and shader access
pattern ImageLayoutDepthStencilReadOnlyOptimal = ImageLayout 4
-- | Optimal layout when image is used for read only shader access
pattern ImageLayoutShaderReadOnlyOptimal = ImageLayout 5
-- | Optimal layout when image is used only as source of transfer operations
pattern ImageLayoutTransferSrcOptimal = ImageLayout 6
-- | Optimal layout when image is used only as destination of transfer operations
pattern ImageLayoutTransferDstOptimal = ImageLayout 7
-- | Initial layout used when the data is populated by the CPU
pattern ImageLayoutPreinitialized = ImageLayout 8

-- ** ImageType

newtype ImageType = ImageType Int32
  deriving (Eq, Storable)

instance Show ImageType where
  showsPrec _ ImageType1d = showString "ImageType1d"
  showsPrec _ ImageType2d = showString "ImageType2d"
  showsPrec _ ImageType3d = showString "ImageType3d"
  showsPrec p (ImageType x) = showParen (p >= 11) (showString "ImageType " . showsPrec 11 x)

instance Read ImageType where
  readPrec = parens ( choose [ ("ImageType1d", pure ImageType1d)
                             , ("ImageType2d", pure ImageType2d)
                             , ("ImageType3d", pure ImageType3d)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ImageType")
                        v <- step readPrec
                        pure (ImageType v)
                        )
                    )


pattern ImageType1d = ImageType 0

pattern ImageType2d = ImageType 1

pattern ImageType3d = ImageType 2

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


