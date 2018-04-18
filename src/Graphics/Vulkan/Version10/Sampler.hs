{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Version10.Sampler
  ( VkBorderColor(..)
  , pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
  , pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK
  , pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK
  , pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK
  , pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE
  , pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE
  , VkSamplerAddressMode(..)
  , pattern VK_SAMPLER_ADDRESS_MODE_REPEAT
  , pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
  , pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  , pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
  , VkFilter(..)
  , pattern VK_FILTER_NEAREST
  , pattern VK_FILTER_LINEAR
  , VkSamplerMipmapMode(..)
  , pattern VK_SAMPLER_MIPMAP_MODE_NEAREST
  , pattern VK_SAMPLER_MIPMAP_MODE_LINEAR
  , VkSamplerCreateFlags(..)
  , VkSampler
  , vkCreateSampler
  , vkDestroySampler
  , VkSamplerCreateInfo(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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


import Graphics.Vulkan.Version10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Version10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Version10.Pipeline
  ( VkCompareOp(..)
  )


-- ** VkBorderColor

-- | 
newtype VkBorderColor = VkBorderColor Int32
  deriving (Eq, Ord, Storable)

instance Show VkBorderColor where
  showsPrec _ VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = showString "VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK"
  showsPrec _ VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = showString "VK_BORDER_COLOR_INT_TRANSPARENT_BLACK"
  showsPrec _ VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = showString "VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK"
  showsPrec _ VK_BORDER_COLOR_INT_OPAQUE_BLACK = showString "VK_BORDER_COLOR_INT_OPAQUE_BLACK"
  showsPrec _ VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = showString "VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE"
  showsPrec _ VK_BORDER_COLOR_INT_OPAQUE_WHITE = showString "VK_BORDER_COLOR_INT_OPAQUE_WHITE"
  showsPrec p (VkBorderColor x) = showParen (p >= 11) (showString "VkBorderColor " . showsPrec 11 x)

instance Read VkBorderColor where
  readPrec = parens ( choose [ ("VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK", pure VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK)
                             , ("VK_BORDER_COLOR_INT_TRANSPARENT_BLACK",   pure VK_BORDER_COLOR_INT_TRANSPARENT_BLACK)
                             , ("VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK",      pure VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK)
                             , ("VK_BORDER_COLOR_INT_OPAQUE_BLACK",        pure VK_BORDER_COLOR_INT_OPAQUE_BLACK)
                             , ("VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE",      pure VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE)
                             , ("VK_BORDER_COLOR_INT_OPAQUE_WHITE",        pure VK_BORDER_COLOR_INT_OPAQUE_WHITE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBorderColor")
                        v <- step readPrec
                        pure (VkBorderColor v)
                        )
                    )

-- | 
pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK :: VkBorderColor
pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = VkBorderColor 0

-- | 
pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK :: VkBorderColor
pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = VkBorderColor 1

-- | 
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK :: VkBorderColor
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = VkBorderColor 2

-- | 
pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK :: VkBorderColor
pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK = VkBorderColor 3

-- | 
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE :: VkBorderColor
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = VkBorderColor 4

-- | 
pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE :: VkBorderColor
pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE = VkBorderColor 5
-- ** VkSamplerAddressMode

-- | 
newtype VkSamplerAddressMode = VkSamplerAddressMode Int32
  deriving (Eq, Ord, Storable)

instance Show VkSamplerAddressMode where
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_REPEAT = showString "VK_SAMPLER_ADDRESS_MODE_REPEAT"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = showString "VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkSamplerAddressMode 4) = showString "VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE"
  showsPrec p (VkSamplerAddressMode x) = showParen (p >= 11) (showString "VkSamplerAddressMode " . showsPrec 11 x)

instance Read VkSamplerAddressMode where
  readPrec = parens ( choose [ ("VK_SAMPLER_ADDRESS_MODE_REPEAT",          pure VK_SAMPLER_ADDRESS_MODE_REPEAT)
                             , ("VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT", pure VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT)
                             , ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE",   pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)
                             , ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER", pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE", pure (VkSamplerAddressMode 4))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerAddressMode")
                        v <- step readPrec
                        pure (VkSamplerAddressMode v)
                        )
                    )

-- | 
pattern VK_SAMPLER_ADDRESS_MODE_REPEAT :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_REPEAT = VkSamplerAddressMode 0

-- | 
pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = VkSamplerAddressMode 1

-- | 
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = VkSamplerAddressMode 2

-- | 
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = VkSamplerAddressMode 3
-- ** VkFilter

-- | 
newtype VkFilter = VkFilter Int32
  deriving (Eq, Ord, Storable)

instance Show VkFilter where
  showsPrec _ VK_FILTER_NEAREST = showString "VK_FILTER_NEAREST"
  showsPrec _ VK_FILTER_LINEAR = showString "VK_FILTER_LINEAR"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkFilter 1000015000) = showString "VK_FILTER_CUBIC_IMG"
  showsPrec p (VkFilter x) = showParen (p >= 11) (showString "VkFilter " . showsPrec 11 x)

instance Read VkFilter where
  readPrec = parens ( choose [ ("VK_FILTER_NEAREST", pure VK_FILTER_NEAREST)
                             , ("VK_FILTER_LINEAR",  pure VK_FILTER_LINEAR)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_FILTER_CUBIC_IMG", pure (VkFilter 1000015000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFilter")
                        v <- step readPrec
                        pure (VkFilter v)
                        )
                    )

-- | 
pattern VK_FILTER_NEAREST :: VkFilter
pattern VK_FILTER_NEAREST = VkFilter 0

-- | 
pattern VK_FILTER_LINEAR :: VkFilter
pattern VK_FILTER_LINEAR = VkFilter 1
-- ** VkSamplerMipmapMode

-- | 
newtype VkSamplerMipmapMode = VkSamplerMipmapMode Int32
  deriving (Eq, Ord, Storable)

instance Show VkSamplerMipmapMode where
  showsPrec _ VK_SAMPLER_MIPMAP_MODE_NEAREST = showString "VK_SAMPLER_MIPMAP_MODE_NEAREST"
  showsPrec _ VK_SAMPLER_MIPMAP_MODE_LINEAR = showString "VK_SAMPLER_MIPMAP_MODE_LINEAR"
  showsPrec p (VkSamplerMipmapMode x) = showParen (p >= 11) (showString "VkSamplerMipmapMode " . showsPrec 11 x)

instance Read VkSamplerMipmapMode where
  readPrec = parens ( choose [ ("VK_SAMPLER_MIPMAP_MODE_NEAREST", pure VK_SAMPLER_MIPMAP_MODE_NEAREST)
                             , ("VK_SAMPLER_MIPMAP_MODE_LINEAR",  pure VK_SAMPLER_MIPMAP_MODE_LINEAR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerMipmapMode")
                        v <- step readPrec
                        pure (VkSamplerMipmapMode v)
                        )
                    )

-- | Choose nearest mip level
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST :: VkSamplerMipmapMode
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = VkSamplerMipmapMode 0

-- | Linear filter between mip levels
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR :: VkSamplerMipmapMode
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR = VkSamplerMipmapMode 1
-- ** VkSamplerCreateFlags

-- | 
newtype VkSamplerCreateFlags = VkSamplerCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkSamplerCreateFlags where
  
  showsPrec p (VkSamplerCreateFlags x) = showParen (p >= 11) (showString "VkSamplerCreateFlags " . showsPrec 11 x)

instance Read VkSamplerCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerCreateFlags")
                        v <- step readPrec
                        pure (VkSamplerCreateFlags v)
                        )
                    )


-- |
data VkSampler_T
type VkSampler = Ptr VkSampler_T
-- | 
foreign import ccall "vkCreateSampler" vkCreateSampler :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSampler" ::: Ptr VkSampler) -> IO VkResult
-- | 
foreign import ccall "vkDestroySampler" vkDestroySampler :: ("device" ::: VkDevice) -> ("sampler" ::: VkSampler) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | TODO: Struct comments
data VkSamplerCreateInfo = VkSamplerCreateInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkSamplerCreateFlags
  , vkMagFilter :: VkFilter
  , vkMinFilter :: VkFilter
  , vkMipmapMode :: VkSamplerMipmapMode
  , vkAddressModeU :: VkSamplerAddressMode
  , vkAddressModeV :: VkSamplerAddressMode
  , vkAddressModeW :: VkSamplerAddressMode
  , vkMipLodBias :: CFloat
  , vkAnisotropyEnable :: VkBool32
  , vkMaxAnisotropy :: CFloat
  , vkCompareEnable :: VkBool32
  , vkCompareOp :: VkCompareOp
  , vkMinLod :: CFloat
  , vkMaxLod :: CFloat
  , vkBorderColor :: VkBorderColor
  , vkUnnormalizedCoordinates :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkSamplerCreateInfo where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek ptr = VkSamplerCreateInfo <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 20)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 28)
                                 <*> peek (ptr `plusPtr` 32)
                                 <*> peek (ptr `plusPtr` 36)
                                 <*> peek (ptr `plusPtr` 40)
                                 <*> peek (ptr `plusPtr` 44)
                                 <*> peek (ptr `plusPtr` 48)
                                 <*> peek (ptr `plusPtr` 52)
                                 <*> peek (ptr `plusPtr` 56)
                                 <*> peek (ptr `plusPtr` 60)
                                 <*> peek (ptr `plusPtr` 64)
                                 <*> peek (ptr `plusPtr` 68)
                                 <*> peek (ptr `plusPtr` 72)
                                 <*> peek (ptr `plusPtr` 76)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkMagFilter (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkMinFilter (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkMipmapMode (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkAddressModeU (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkAddressModeV (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkAddressModeW (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkMipLodBias (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkAnisotropyEnable (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkMaxAnisotropy (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkCompareEnable (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 60) (vkCompareOp (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkMinLod (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 68) (vkMaxLod (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkBorderColor (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 76) (vkUnnormalizedCoordinates (poked :: VkSamplerCreateInfo))
