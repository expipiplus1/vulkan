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
import Graphics.Vulkan.Core( StructureType(..)
                           , Format(..)
                           , Result(..)
                           , Flags(..)
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
  deriving (Eq, Ord)

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


-- ** createImageView
foreign import ccall "vkCreateImageView" createImageView ::
  Device ->
  Ptr ImageViewCreateInfo ->
    Ptr AllocationCallbacks -> Ptr ImageView -> IO Result

newtype ImageView = ImageView Word64
  deriving (Eq, Ord, Storable)

-- ** ImageViewType

newtype ImageViewType = ImageViewType Int32
  deriving (Eq, Ord, Storable)

instance Show ImageViewType where
  showsPrec _ ImageViewType1d = showString "ImageViewType1d"
  showsPrec _ ImageViewType2d = showString "ImageViewType2d"
  showsPrec _ ImageViewType3d = showString "ImageViewType3d"
  showsPrec _ ImageViewTypeCube = showString "ImageViewTypeCube"
  showsPrec _ ImageViewType1dArray = showString "ImageViewType1dArray"
  showsPrec _ ImageViewType2dArray = showString "ImageViewType2dArray"
  showsPrec _ ImageViewTypeCubeArray = showString "ImageViewTypeCubeArray"
  showsPrec p (ImageViewType x) = showParen (p >= 11) (showString "ImageViewType " . showsPrec 11 x)

instance Read ImageViewType where
  readPrec = parens ( choose [ ("ImageViewType1d", pure ImageViewType1d)
                             , ("ImageViewType2d", pure ImageViewType2d)
                             , ("ImageViewType3d", pure ImageViewType3d)
                             , ("ImageViewTypeCube", pure ImageViewTypeCube)
                             , ("ImageViewType1dArray", pure ImageViewType1dArray)
                             , ("ImageViewType2dArray", pure ImageViewType2dArray)
                             , ("ImageViewTypeCubeArray", pure ImageViewTypeCubeArray)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ImageViewType")
                        v <- step readPrec
                        pure (ImageViewType v)
                        )
                    )


pattern ImageViewType1d = ImageViewType 0

pattern ImageViewType2d = ImageViewType 1

pattern ImageViewType3d = ImageViewType 2

pattern ImageViewTypeCube = ImageViewType 3

pattern ImageViewType1dArray = ImageViewType 4

pattern ImageViewType2dArray = ImageViewType 5

pattern ImageViewTypeCubeArray = ImageViewType 6

-- ** ImageViewCreateFlags
-- | Opaque flag
newtype ImageViewCreateFlags = ImageViewCreateFlags Flags
  deriving (Eq, Ord, Storable)


data ComponentMapping =
  ComponentMapping{ red :: ComponentSwizzle 
                  , green :: ComponentSwizzle 
                  , blue :: ComponentSwizzle 
                  , alpha :: ComponentSwizzle 
                  }
  deriving (Eq, Ord)

instance Storable ComponentMapping where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = ComponentMapping <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 4)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (red (poked :: ComponentMapping))
                *> poke (ptr `plusPtr` 4) (green (poked :: ComponentMapping))
                *> poke (ptr `plusPtr` 8) (blue (poked :: ComponentMapping))
                *> poke (ptr `plusPtr` 12) (alpha (poked :: ComponentMapping))


-- ** ComponentSwizzle

newtype ComponentSwizzle = ComponentSwizzle Int32
  deriving (Eq, Ord, Storable)

instance Show ComponentSwizzle where
  showsPrec _ ComponentSwizzleIdentity = showString "ComponentSwizzleIdentity"
  showsPrec _ ComponentSwizzleZero = showString "ComponentSwizzleZero"
  showsPrec _ ComponentSwizzleOne = showString "ComponentSwizzleOne"
  showsPrec _ ComponentSwizzleR = showString "ComponentSwizzleR"
  showsPrec _ ComponentSwizzleG = showString "ComponentSwizzleG"
  showsPrec _ ComponentSwizzleB = showString "ComponentSwizzleB"
  showsPrec _ ComponentSwizzleA = showString "ComponentSwizzleA"
  showsPrec p (ComponentSwizzle x) = showParen (p >= 11) (showString "ComponentSwizzle " . showsPrec 11 x)

instance Read ComponentSwizzle where
  readPrec = parens ( choose [ ("ComponentSwizzleIdentity", pure ComponentSwizzleIdentity)
                             , ("ComponentSwizzleZero", pure ComponentSwizzleZero)
                             , ("ComponentSwizzleOne", pure ComponentSwizzleOne)
                             , ("ComponentSwizzleR", pure ComponentSwizzleR)
                             , ("ComponentSwizzleG", pure ComponentSwizzleG)
                             , ("ComponentSwizzleB", pure ComponentSwizzleB)
                             , ("ComponentSwizzleA", pure ComponentSwizzleA)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ComponentSwizzle")
                        v <- step readPrec
                        pure (ComponentSwizzle v)
                        )
                    )


pattern ComponentSwizzleIdentity = ComponentSwizzle 0

pattern ComponentSwizzleZero = ComponentSwizzle 1

pattern ComponentSwizzleOne = ComponentSwizzle 2

pattern ComponentSwizzleR = ComponentSwizzle 3

pattern ComponentSwizzleG = ComponentSwizzle 4

pattern ComponentSwizzleB = ComponentSwizzle 5

pattern ComponentSwizzleA = ComponentSwizzle 6

-- ** destroyImageView
foreign import ccall "vkDestroyImageView" destroyImageView ::
  Device -> ImageView -> Ptr AllocationCallbacks -> IO ()

