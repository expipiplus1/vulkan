{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.ImageView where

import Graphics.Vulkan.Device( Device(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Data.Int( Int32
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
import Graphics.Vulkan.Image( Image(..)
                            , ImageSubresourceRange(..)
                            )
import Graphics.Vulkan.Core( VkFlags(..)
                           , StructureType(..)
                           , Format(..)
                           , Result(..)
                           )


data ImageViewCreateInfo =
  ImageViewCreateInfo{ sType :: StructureType 
                     , pNext :: Ptr Void 
                     , flags :: ImageViewCreateFlags 
                     , image :: Image 
                     , viewType :: ImageViewType 
                     , format :: Format 
                     , components :: ComponentMapping 
                     , subresourceRange :: ImageSubresourceRange 
                     }
  deriving (Eq)

instance Storable ImageViewCreateInfo where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek ptr = ImageViewCreateInfo <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
                                 <*> peek (ptr `plusPtr` 36)
                                 <*> peek (ptr `plusPtr` 40)
                                 <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: ImageViewCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: ImageViewCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: ImageViewCreateInfo))
                *> poke (ptr `plusPtr` 24) (image (poked :: ImageViewCreateInfo))
                *> poke (ptr `plusPtr` 32) (viewType (poked :: ImageViewCreateInfo))
                *> poke (ptr `plusPtr` 36) (format (poked :: ImageViewCreateInfo))
                *> poke (ptr `plusPtr` 40) (components (poked :: ImageViewCreateInfo))
                *> poke (ptr `plusPtr` 56) (subresourceRange (poked :: ImageViewCreateInfo))


-- ** vkCreateImageView
foreign import ccall "vkCreateImageView" vkCreateImageView ::
  Device ->
  Ptr ImageViewCreateInfo ->
    Ptr AllocationCallbacks -> Ptr ImageView -> IO Result

newtype ImageView = ImageView Word64
  deriving (Eq, Storable)

-- ** ImageViewType

newtype ImageViewType = ImageViewType Int32
  deriving (Eq, Storable)

instance Show ImageViewType where
  showsPrec _ VK_IMAGE_VIEW_TYPE_1D = showString "VK_IMAGE_VIEW_TYPE_1D"
  showsPrec _ VK_IMAGE_VIEW_TYPE_2D = showString "VK_IMAGE_VIEW_TYPE_2D"
  showsPrec _ VK_IMAGE_VIEW_TYPE_3D = showString "VK_IMAGE_VIEW_TYPE_3D"
  showsPrec _ VK_IMAGE_VIEW_TYPE_CUBE = showString "VK_IMAGE_VIEW_TYPE_CUBE"
  showsPrec _ VK_IMAGE_VIEW_TYPE_1D_ARRAY = showString "VK_IMAGE_VIEW_TYPE_1D_ARRAY"
  showsPrec _ VK_IMAGE_VIEW_TYPE_2D_ARRAY = showString "VK_IMAGE_VIEW_TYPE_2D_ARRAY"
  showsPrec _ VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = showString "VK_IMAGE_VIEW_TYPE_CUBE_ARRAY"
  showsPrec p (ImageViewType x) = showParen (p >= 11) (showString "ImageViewType " . showsPrec 11 x)

instance Read ImageViewType where
  readPrec = parens ( choose [ ("VK_IMAGE_VIEW_TYPE_1D", pure VK_IMAGE_VIEW_TYPE_1D)
                             , ("VK_IMAGE_VIEW_TYPE_2D", pure VK_IMAGE_VIEW_TYPE_2D)
                             , ("VK_IMAGE_VIEW_TYPE_3D", pure VK_IMAGE_VIEW_TYPE_3D)
                             , ("VK_IMAGE_VIEW_TYPE_CUBE", pure VK_IMAGE_VIEW_TYPE_CUBE)
                             , ("VK_IMAGE_VIEW_TYPE_1D_ARRAY", pure VK_IMAGE_VIEW_TYPE_1D_ARRAY)
                             , ("VK_IMAGE_VIEW_TYPE_2D_ARRAY", pure VK_IMAGE_VIEW_TYPE_2D_ARRAY)
                             , ("VK_IMAGE_VIEW_TYPE_CUBE_ARRAY", pure VK_IMAGE_VIEW_TYPE_CUBE_ARRAY)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ImageViewType")
                        v <- step readPrec
                        pure (ImageViewType v)
                        )
                    )


pattern VK_IMAGE_VIEW_TYPE_1D = ImageViewType 0

pattern VK_IMAGE_VIEW_TYPE_2D = ImageViewType 1

pattern VK_IMAGE_VIEW_TYPE_3D = ImageViewType 2

pattern VK_IMAGE_VIEW_TYPE_CUBE = ImageViewType 3

pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY = ImageViewType 4

pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY = ImageViewType 5

pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = ImageViewType 6

-- ** ImageViewCreateFlags
-- | Opaque flag
newtype ImageViewCreateFlags = ImageViewCreateFlags VkFlags
  deriving (Eq, Storable)


data ComponentMapping =
  ComponentMapping{ r :: ComponentSwizzle 
                  , g :: ComponentSwizzle 
                  , b :: ComponentSwizzle 
                  , a :: ComponentSwizzle 
                  }
  deriving (Eq)

instance Storable ComponentMapping where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = ComponentMapping <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 4)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (r (poked :: ComponentMapping))
                *> poke (ptr `plusPtr` 4) (g (poked :: ComponentMapping))
                *> poke (ptr `plusPtr` 8) (b (poked :: ComponentMapping))
                *> poke (ptr `plusPtr` 12) (a (poked :: ComponentMapping))


-- ** ComponentSwizzle

newtype ComponentSwizzle = ComponentSwizzle Int32
  deriving (Eq, Storable)

instance Show ComponentSwizzle where
  showsPrec _ VK_COMPONENT_SWIZZLE_IDENTITY = showString "VK_COMPONENT_SWIZZLE_IDENTITY"
  showsPrec _ VK_COMPONENT_SWIZZLE_ZERO = showString "VK_COMPONENT_SWIZZLE_ZERO"
  showsPrec _ VK_COMPONENT_SWIZZLE_ONE = showString "VK_COMPONENT_SWIZZLE_ONE"
  showsPrec _ VK_COMPONENT_SWIZZLE_R = showString "VK_COMPONENT_SWIZZLE_R"
  showsPrec _ VK_COMPONENT_SWIZZLE_G = showString "VK_COMPONENT_SWIZZLE_G"
  showsPrec _ VK_COMPONENT_SWIZZLE_B = showString "VK_COMPONENT_SWIZZLE_B"
  showsPrec _ VK_COMPONENT_SWIZZLE_A = showString "VK_COMPONENT_SWIZZLE_A"
  showsPrec p (ComponentSwizzle x) = showParen (p >= 11) (showString "ComponentSwizzle " . showsPrec 11 x)

instance Read ComponentSwizzle where
  readPrec = parens ( choose [ ("VK_COMPONENT_SWIZZLE_IDENTITY", pure VK_COMPONENT_SWIZZLE_IDENTITY)
                             , ("VK_COMPONENT_SWIZZLE_ZERO", pure VK_COMPONENT_SWIZZLE_ZERO)
                             , ("VK_COMPONENT_SWIZZLE_ONE", pure VK_COMPONENT_SWIZZLE_ONE)
                             , ("VK_COMPONENT_SWIZZLE_R", pure VK_COMPONENT_SWIZZLE_R)
                             , ("VK_COMPONENT_SWIZZLE_G", pure VK_COMPONENT_SWIZZLE_G)
                             , ("VK_COMPONENT_SWIZZLE_B", pure VK_COMPONENT_SWIZZLE_B)
                             , ("VK_COMPONENT_SWIZZLE_A", pure VK_COMPONENT_SWIZZLE_A)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ComponentSwizzle")
                        v <- step readPrec
                        pure (ComponentSwizzle v)
                        )
                    )


pattern VK_COMPONENT_SWIZZLE_IDENTITY = ComponentSwizzle 0

pattern VK_COMPONENT_SWIZZLE_ZERO = ComponentSwizzle 1

pattern VK_COMPONENT_SWIZZLE_ONE = ComponentSwizzle 2

pattern VK_COMPONENT_SWIZZLE_R = ComponentSwizzle 3

pattern VK_COMPONENT_SWIZZLE_G = ComponentSwizzle 4

pattern VK_COMPONENT_SWIZZLE_B = ComponentSwizzle 5

pattern VK_COMPONENT_SWIZZLE_A = ComponentSwizzle 6

-- ** vkDestroyImageView
foreign import ccall "vkDestroyImageView" vkDestroyImageView ::
  Device -> ImageView -> Ptr AllocationCallbacks -> IO ()

