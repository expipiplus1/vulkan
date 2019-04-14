{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.ImageView
  ( VkComponentMapping(..)
  , VkComponentSwizzle(..)
  , pattern VK_COMPONENT_SWIZZLE_IDENTITY
  , pattern VK_COMPONENT_SWIZZLE_ZERO
  , pattern VK_COMPONENT_SWIZZLE_ONE
  , pattern VK_COMPONENT_SWIZZLE_R
  , pattern VK_COMPONENT_SWIZZLE_G
  , pattern VK_COMPONENT_SWIZZLE_B
  , pattern VK_COMPONENT_SWIZZLE_A
  , VkImageSubresourceRange(..)
  , VkImageView
  , VkImageViewCreateFlags(..)
  , VkImageViewCreateInfo(..)
  , VkImageViewType(..)
  , pattern VK_IMAGE_VIEW_TYPE_1D
  , pattern VK_IMAGE_VIEW_TYPE_2D
  , pattern VK_IMAGE_VIEW_TYPE_3D
  , pattern VK_IMAGE_VIEW_TYPE_CUBE
  , pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY
  , pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY
  , pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateImageView
#endif
  , FN_vkCreateImageView
  , PFN_vkCreateImageView
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroyImageView
#endif
  , FN_vkDestroyImageView
  , PFN_vkDestroyImageView
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
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkImage
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlags
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkComponentMapping"
data VkComponentMapping = VkComponentMapping
  { -- No documentation found for Nested "VkComponentMapping" "r"
  vkR :: VkComponentSwizzle
  , -- No documentation found for Nested "VkComponentMapping" "g"
  vkG :: VkComponentSwizzle
  , -- No documentation found for Nested "VkComponentMapping" "b"
  vkB :: VkComponentSwizzle
  , -- No documentation found for Nested "VkComponentMapping" "a"
  vkA :: VkComponentSwizzle
  }
  deriving (Eq, Show)

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

-- No documentation found for TopLevel "VkComponentSwizzle"
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
                             , ("VK_COMPONENT_SWIZZLE_ZERO",     pure VK_COMPONENT_SWIZZLE_ZERO)
                             , ("VK_COMPONENT_SWIZZLE_ONE",      pure VK_COMPONENT_SWIZZLE_ONE)
                             , ("VK_COMPONENT_SWIZZLE_R",        pure VK_COMPONENT_SWIZZLE_R)
                             , ("VK_COMPONENT_SWIZZLE_G",        pure VK_COMPONENT_SWIZZLE_G)
                             , ("VK_COMPONENT_SWIZZLE_B",        pure VK_COMPONENT_SWIZZLE_B)
                             , ("VK_COMPONENT_SWIZZLE_A",        pure VK_COMPONENT_SWIZZLE_A)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkComponentSwizzle")
                        v <- step readPrec
                        pure (VkComponentSwizzle v)
                        )
                    )

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_IDENTITY"
pattern VK_COMPONENT_SWIZZLE_IDENTITY :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_IDENTITY = VkComponentSwizzle 0

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_ZERO"
pattern VK_COMPONENT_SWIZZLE_ZERO :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_ZERO = VkComponentSwizzle 1

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_ONE"
pattern VK_COMPONENT_SWIZZLE_ONE :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_ONE = VkComponentSwizzle 2

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_R"
pattern VK_COMPONENT_SWIZZLE_R :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_R = VkComponentSwizzle 3

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_G"
pattern VK_COMPONENT_SWIZZLE_G :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_G = VkComponentSwizzle 4

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_B"
pattern VK_COMPONENT_SWIZZLE_B :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_B = VkComponentSwizzle 5

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_A"
pattern VK_COMPONENT_SWIZZLE_A :: VkComponentSwizzle
pattern VK_COMPONENT_SWIZZLE_A = VkComponentSwizzle 6
-- No documentation found for TopLevel "VkImageSubresourceRange"
data VkImageSubresourceRange = VkImageSubresourceRange
  { -- No documentation found for Nested "VkImageSubresourceRange" "aspectMask"
  vkAspectMask :: VkImageAspectFlags
  , -- No documentation found for Nested "VkImageSubresourceRange" "baseMipLevel"
  vkBaseMipLevel :: Word32
  , -- No documentation found for Nested "VkImageSubresourceRange" "levelCount"
  vkLevelCount :: Word32
  , -- No documentation found for Nested "VkImageSubresourceRange" "baseArrayLayer"
  vkBaseArrayLayer :: Word32
  , -- No documentation found for Nested "VkImageSubresourceRange" "layerCount"
  vkLayerCount :: Word32
  }
  deriving (Eq, Show)

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
-- | Dummy data to tag the 'Ptr' with
data VkImageView_T
-- No documentation found for TopLevel "VkImageView"
type VkImageView = Ptr VkImageView_T
-- ** VkImageViewCreateFlags

-- No documentation found for TopLevel "VkImageViewCreateFlags"
newtype VkImageViewCreateFlags = VkImageViewCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkImageViewCreateFlags where
  
  showsPrec p (VkImageViewCreateFlags x) = showParen (p >= 11) (showString "VkImageViewCreateFlags " . showsPrec 11 x)

instance Read VkImageViewCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageViewCreateFlags")
                        v <- step readPrec
                        pure (VkImageViewCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkImageViewCreateInfo"
data VkImageViewCreateInfo = VkImageViewCreateInfo
  { -- No documentation found for Nested "VkImageViewCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageViewCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageViewCreateInfo" "flags"
  vkFlags :: VkImageViewCreateFlags
  , -- No documentation found for Nested "VkImageViewCreateInfo" "image"
  vkImage :: VkImage
  , -- No documentation found for Nested "VkImageViewCreateInfo" "viewType"
  vkViewType :: VkImageViewType
  , -- No documentation found for Nested "VkImageViewCreateInfo" "format"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkImageViewCreateInfo" "components"
  vkComponents :: VkComponentMapping
  , -- No documentation found for Nested "VkImageViewCreateInfo" "subresourceRange"
  vkSubresourceRange :: VkImageSubresourceRange
  }
  deriving (Eq, Show)

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
-- ** VkImageViewType

-- No documentation found for TopLevel "VkImageViewType"
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
  readPrec = parens ( choose [ ("VK_IMAGE_VIEW_TYPE_1D",         pure VK_IMAGE_VIEW_TYPE_1D)
                             , ("VK_IMAGE_VIEW_TYPE_2D",         pure VK_IMAGE_VIEW_TYPE_2D)
                             , ("VK_IMAGE_VIEW_TYPE_3D",         pure VK_IMAGE_VIEW_TYPE_3D)
                             , ("VK_IMAGE_VIEW_TYPE_CUBE",       pure VK_IMAGE_VIEW_TYPE_CUBE)
                             , ("VK_IMAGE_VIEW_TYPE_1D_ARRAY",   pure VK_IMAGE_VIEW_TYPE_1D_ARRAY)
                             , ("VK_IMAGE_VIEW_TYPE_2D_ARRAY",   pure VK_IMAGE_VIEW_TYPE_2D_ARRAY)
                             , ("VK_IMAGE_VIEW_TYPE_CUBE_ARRAY", pure VK_IMAGE_VIEW_TYPE_CUBE_ARRAY)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageViewType")
                        v <- step readPrec
                        pure (VkImageViewType v)
                        )
                    )

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_1D"
pattern VK_IMAGE_VIEW_TYPE_1D :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_1D = VkImageViewType 0

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_2D"
pattern VK_IMAGE_VIEW_TYPE_2D :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_2D = VkImageViewType 1

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_3D"
pattern VK_IMAGE_VIEW_TYPE_3D :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_3D = VkImageViewType 2

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_CUBE"
pattern VK_IMAGE_VIEW_TYPE_CUBE :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_CUBE = VkImageViewType 3

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_1D_ARRAY"
pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY = VkImageViewType 4

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_2D_ARRAY"
pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY = VkImageViewType 5

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_CUBE_ARRAY"
pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY :: VkImageViewType
pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = VkImageViewType 6
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCreateImageView"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateImageView" vkCreateImageView :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult

#endif
type FN_vkCreateImageView = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult
type PFN_vkCreateImageView = FunPtr FN_vkCreateImageView
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkDestroyImageView"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyImageView" vkDestroyImageView :: ("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyImageView = ("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyImageView = FunPtr FN_vkDestroyImageView
