{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.ImageView where

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
import Graphics.Vulkan.Image( VkImage(..)
                            , VkImageAspectFlagBits(..)
                            , VkImageSubresourceRange(..)
                            , VkImageAspectFlags(..)
                            )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkFlags(..)
                           , VkFormat(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize(..)
                      )


data VkImageViewCreateInfo =
  VkImageViewCreateInfo{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkFlags :: VkImageViewCreateFlags 
                       , vkImage :: VkImage 
                       , vkViewType :: VkImageViewType 
                       , vkFormat :: VkFormat 
                       , vkComponents :: VkComponentMapping 
                       , vkSubresourceRange :: VkImageSubresourceRange 
                       }
  deriving (Eq, Ord)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkImage (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkViewType (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkFormat (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkComponents (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkSubresourceRange (poked :: VkImageViewCreateInfo))


-- ** vkCreateImageView
foreign import ccall "vkCreateImageView" vkCreateImageView ::
  VkDevice ->
  Ptr VkImageViewCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkImageView -> IO VkResult

newtype VkImageView = VkImageView Word64
  deriving (Eq, Ord, Storable)

-- ** VkImageViewType

newtype VkImageViewType = VkImageViewType Int32
  deriving (Eq, Ord, Storable)

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
  deriving (Eq, Ord, Storable, Bits, FiniteBits)


data VkComponentMapping =
  VkComponentMapping{ vkR :: VkComponentSwizzle 
                    , vkG :: VkComponentSwizzle 
                    , vkB :: VkComponentSwizzle 
                    , vkA :: VkComponentSwizzle 
                    }
  deriving (Eq, Ord)

instance Storable VkComponentMapping where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkComponentMapping <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkR (poked :: VkComponentMapping))
                *> poke (ptr `plusPtr` 4) (vkG (poked :: VkComponentMapping))
                *> poke (ptr `plusPtr` 8) (vkB (poked :: VkComponentMapping))
                *> poke (ptr `plusPtr` 12) (vkA (poked :: VkComponentMapping))


-- ** VkComponentSwizzle

newtype VkComponentSwizzle = VkComponentSwizzle Int32
  deriving (Eq, Ord, Storable)

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
  VkDevice -> VkImageView -> Ptr VkAllocationCallbacks -> IO ()

