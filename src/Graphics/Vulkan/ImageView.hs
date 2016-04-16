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
import Graphics.Vulkan.Memory( VkAllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Image( Image(..)
                            , VkImageSubresourceRange(..)
                            )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkFormat(..)
                           , VkFlags(..)
                           , VkResult(..)
                           )


data VkImageViewCreateInfo =
  VkImageViewCreateInfo{ sType :: VkStructureType 
                       , pNext :: Ptr Void 
                       , flags :: VkImageViewCreateFlags 
                       , image :: Image 
                       , viewType :: VkImageViewType 
                       , format :: VkFormat 
                       , components :: VkComponentMapping 
                       , subresourceRange :: VkImageSubresourceRange 
                       }
  deriving (Eq)

instance Storable VkImageViewCreateInfo where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek ptr = VkImageViewCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 36)
                                   <*> peek (ptr `plusPtr` 40)
                                   <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 24) (image (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 32) (viewType (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 36) (format (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 40) (components (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 56) (subresourceRange (poked :: VkImageViewCreateInfo))


-- ** vkCreateImageView
foreign import ccall "vkCreateImageView" vkCreateImageView ::
  Device ->
  Ptr VkImageViewCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr ImageView -> IO VkResult

newtype ImageView = ImageView Word64
  deriving (Eq, Storable)

-- ** VkImageViewType

newtype VkImageViewType = VkImageViewType Int32
  deriving (Eq, Storable)

instance Show VkImageViewType where
  showsPrec _ VK_IMAGE_VIEW_TYPE_1D = showString "VK_IMAGE_VIEW_TYPE_1D"
  showsPrec _ VK_IMAGE_VIEW_TYPE_2D = showString "VK_IMAGE_VIEW_TYPE_2D"
  showsPrec _ VK_IMAGE_VIEW_TYPE_3D = showString "VK_IMAGE_VIEW_TYPE_3D"
  showsPrec _ VK_IMAGE_VIEW_TYPE_CUBE = showString "VK_IMAGE_VIEW_TYPE_CUBE"
  showsPrec _ VK_IMAGE_VIEW_TYPE_1D_ARRAY = showString "VK_IMAGE_VIEW_TYPE_1D_ARRAY"
  showsPrec _ VK_IMAGE_VIEW_TYPE_2D_ARRAY = showString "VK_IMAGE_VIEW_TYPE_2D_ARRAY"
  showsPrec _ VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = showString "VK_IMAGE_VIEW_TYPE_CUBE_ARRAY"
  showsPrec p (VkImageViewType x) = showParen (p >= 11) (showString "VkImageViewType " . showsPrec 11 x)

instance Read VkImageViewType where
  readPrec = parens ( choose [ ("VK_IMAGE_VIEW_TYPE_1D", pure VK_IMAGE_VIEW_TYPE_1D)
                             , ("VK_IMAGE_VIEW_TYPE_2D", pure VK_IMAGE_VIEW_TYPE_2D)
                             , ("VK_IMAGE_VIEW_TYPE_3D", pure VK_IMAGE_VIEW_TYPE_3D)
                             , ("VK_IMAGE_VIEW_TYPE_CUBE", pure VK_IMAGE_VIEW_TYPE_CUBE)
                             , ("VK_IMAGE_VIEW_TYPE_1D_ARRAY", pure VK_IMAGE_VIEW_TYPE_1D_ARRAY)
                             , ("VK_IMAGE_VIEW_TYPE_2D_ARRAY", pure VK_IMAGE_VIEW_TYPE_2D_ARRAY)
                             , ("VK_IMAGE_VIEW_TYPE_CUBE_ARRAY", pure VK_IMAGE_VIEW_TYPE_CUBE_ARRAY)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageViewType")
                        v <- step readPrec
                        pure (VkImageViewType v)
                        )
                    )


pattern VK_IMAGE_VIEW_TYPE_1D = VkImageViewType 0

pattern VK_IMAGE_VIEW_TYPE_2D = VkImageViewType 1

pattern VK_IMAGE_VIEW_TYPE_3D = VkImageViewType 2

pattern VK_IMAGE_VIEW_TYPE_CUBE = VkImageViewType 3

pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY = VkImageViewType 4

pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY = VkImageViewType 5

pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = VkImageViewType 6

-- ** VkImageViewCreateFlags
-- | Opaque flag
newtype VkImageViewCreateFlags = VkImageViewCreateFlags VkFlags
  deriving (Eq, Storable)


data VkComponentMapping =
  VkComponentMapping{ r :: VkComponentSwizzle 
                    , g :: VkComponentSwizzle 
                    , b :: VkComponentSwizzle 
                    , a :: VkComponentSwizzle 
                    }
  deriving (Eq)

instance Storable VkComponentMapping where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkComponentMapping <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (r (poked :: VkComponentMapping))
                *> poke (ptr `plusPtr` 4) (g (poked :: VkComponentMapping))
                *> poke (ptr `plusPtr` 8) (b (poked :: VkComponentMapping))
                *> poke (ptr `plusPtr` 12) (a (poked :: VkComponentMapping))


-- ** VkComponentSwizzle

newtype VkComponentSwizzle = VkComponentSwizzle Int32
  deriving (Eq, Storable)

instance Show VkComponentSwizzle where
  showsPrec _ VK_COMPONENT_SWIZZLE_IDENTITY = showString "VK_COMPONENT_SWIZZLE_IDENTITY"
  showsPrec _ VK_COMPONENT_SWIZZLE_ZERO = showString "VK_COMPONENT_SWIZZLE_ZERO"
  showsPrec _ VK_COMPONENT_SWIZZLE_ONE = showString "VK_COMPONENT_SWIZZLE_ONE"
  showsPrec _ VK_COMPONENT_SWIZZLE_R = showString "VK_COMPONENT_SWIZZLE_R"
  showsPrec _ VK_COMPONENT_SWIZZLE_G = showString "VK_COMPONENT_SWIZZLE_G"
  showsPrec _ VK_COMPONENT_SWIZZLE_B = showString "VK_COMPONENT_SWIZZLE_B"
  showsPrec _ VK_COMPONENT_SWIZZLE_A = showString "VK_COMPONENT_SWIZZLE_A"
  showsPrec p (VkComponentSwizzle x) = showParen (p >= 11) (showString "VkComponentSwizzle " . showsPrec 11 x)

instance Read VkComponentSwizzle where
  readPrec = parens ( choose [ ("VK_COMPONENT_SWIZZLE_IDENTITY", pure VK_COMPONENT_SWIZZLE_IDENTITY)
                             , ("VK_COMPONENT_SWIZZLE_ZERO", pure VK_COMPONENT_SWIZZLE_ZERO)
                             , ("VK_COMPONENT_SWIZZLE_ONE", pure VK_COMPONENT_SWIZZLE_ONE)
                             , ("VK_COMPONENT_SWIZZLE_R", pure VK_COMPONENT_SWIZZLE_R)
                             , ("VK_COMPONENT_SWIZZLE_G", pure VK_COMPONENT_SWIZZLE_G)
                             , ("VK_COMPONENT_SWIZZLE_B", pure VK_COMPONENT_SWIZZLE_B)
                             , ("VK_COMPONENT_SWIZZLE_A", pure VK_COMPONENT_SWIZZLE_A)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkComponentSwizzle")
                        v <- step readPrec
                        pure (VkComponentSwizzle v)
                        )
                    )


pattern VK_COMPONENT_SWIZZLE_IDENTITY = VkComponentSwizzle 0

pattern VK_COMPONENT_SWIZZLE_ZERO = VkComponentSwizzle 1

pattern VK_COMPONENT_SWIZZLE_ONE = VkComponentSwizzle 2

pattern VK_COMPONENT_SWIZZLE_R = VkComponentSwizzle 3

pattern VK_COMPONENT_SWIZZLE_G = VkComponentSwizzle 4

pattern VK_COMPONENT_SWIZZLE_B = VkComponentSwizzle 5

pattern VK_COMPONENT_SWIZZLE_A = VkComponentSwizzle 6

-- ** vkDestroyImageView
foreign import ccall "vkDestroyImageView" vkDestroyImageView ::
  Device -> ImageView -> Ptr VkAllocationCallbacks -> IO ()

