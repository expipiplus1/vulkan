{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Sampler where

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
import Graphics.Vulkan.Core( VkResult(..)
                           , VkBool32(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CFloat
                      , CFloat(..)
                      , CSize(..)
                      )

-- ** VkSamplerAddressMode

newtype VkSamplerAddressMode = VkSamplerAddressMode Int32
  deriving (Eq, Ord, Storable)

instance Show VkSamplerAddressMode where
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_REPEAT = showString "VK_SAMPLER_ADDRESS_MODE_REPEAT"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = showString "VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER"
  showsPrec p (VkSamplerAddressMode x) = showParen (p >= 11) (showString "VkSamplerAddressMode " . showsPrec 11 x)

instance Read VkSamplerAddressMode where
  readPrec = parens ( choose [ ("VK_SAMPLER_ADDRESS_MODE_REPEAT", pure VK_SAMPLER_ADDRESS_MODE_REPEAT)
                             , ("VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT", pure VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT)
                             , ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE", pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)
                             , ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER", pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerAddressMode")
                        v <- step readPrec
                        pure (VkSamplerAddressMode v)
                        )
                    )


pattern VK_SAMPLER_ADDRESS_MODE_REPEAT = VkSamplerAddressMode 0

pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = VkSamplerAddressMode 1

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = VkSamplerAddressMode 2

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = VkSamplerAddressMode 3

-- ** VkFilter

newtype VkFilter = VkFilter Int32
  deriving (Eq, Ord, Storable)

instance Show VkFilter where
  showsPrec _ VK_FILTER_NEAREST = showString "VK_FILTER_NEAREST"
  showsPrec _ VK_FILTER_LINEAR = showString "VK_FILTER_LINEAR"
  showsPrec p (VkFilter x) = showParen (p >= 11) (showString "VkFilter " . showsPrec 11 x)

instance Read VkFilter where
  readPrec = parens ( choose [ ("VK_FILTER_NEAREST", pure VK_FILTER_NEAREST)
                             , ("VK_FILTER_LINEAR", pure VK_FILTER_LINEAR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFilter")
                        v <- step readPrec
                        pure (VkFilter v)
                        )
                    )


pattern VK_FILTER_NEAREST = VkFilter 0

pattern VK_FILTER_LINEAR = VkFilter 1

-- ** VkBorderColor

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
                             , ("VK_BORDER_COLOR_INT_TRANSPARENT_BLACK", pure VK_BORDER_COLOR_INT_TRANSPARENT_BLACK)
                             , ("VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK", pure VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK)
                             , ("VK_BORDER_COLOR_INT_OPAQUE_BLACK", pure VK_BORDER_COLOR_INT_OPAQUE_BLACK)
                             , ("VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE", pure VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE)
                             , ("VK_BORDER_COLOR_INT_OPAQUE_WHITE", pure VK_BORDER_COLOR_INT_OPAQUE_WHITE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBorderColor")
                        v <- step readPrec
                        pure (VkBorderColor v)
                        )
                    )


pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = VkBorderColor 0

pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = VkBorderColor 1

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = VkBorderColor 2

pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK = VkBorderColor 3

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = VkBorderColor 4

pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE = VkBorderColor 5

-- ** VkCompareOp

newtype VkCompareOp = VkCompareOp Int32
  deriving (Eq, Ord, Storable)

instance Show VkCompareOp where
  showsPrec _ VK_COMPARE_OP_NEVER = showString "VK_COMPARE_OP_NEVER"
  showsPrec _ VK_COMPARE_OP_LESS = showString "VK_COMPARE_OP_LESS"
  showsPrec _ VK_COMPARE_OP_EQUAL = showString "VK_COMPARE_OP_EQUAL"
  showsPrec _ VK_COMPARE_OP_LESS_OR_EQUAL = showString "VK_COMPARE_OP_LESS_OR_EQUAL"
  showsPrec _ VK_COMPARE_OP_GREATER = showString "VK_COMPARE_OP_GREATER"
  showsPrec _ VK_COMPARE_OP_NOT_EQUAL = showString "VK_COMPARE_OP_NOT_EQUAL"
  showsPrec _ VK_COMPARE_OP_GREATER_OR_EQUAL = showString "VK_COMPARE_OP_GREATER_OR_EQUAL"
  showsPrec _ VK_COMPARE_OP_ALWAYS = showString "VK_COMPARE_OP_ALWAYS"
  showsPrec p (VkCompareOp x) = showParen (p >= 11) (showString "VkCompareOp " . showsPrec 11 x)

instance Read VkCompareOp where
  readPrec = parens ( choose [ ("VK_COMPARE_OP_NEVER", pure VK_COMPARE_OP_NEVER)
                             , ("VK_COMPARE_OP_LESS", pure VK_COMPARE_OP_LESS)
                             , ("VK_COMPARE_OP_EQUAL", pure VK_COMPARE_OP_EQUAL)
                             , ("VK_COMPARE_OP_LESS_OR_EQUAL", pure VK_COMPARE_OP_LESS_OR_EQUAL)
                             , ("VK_COMPARE_OP_GREATER", pure VK_COMPARE_OP_GREATER)
                             , ("VK_COMPARE_OP_NOT_EQUAL", pure VK_COMPARE_OP_NOT_EQUAL)
                             , ("VK_COMPARE_OP_GREATER_OR_EQUAL", pure VK_COMPARE_OP_GREATER_OR_EQUAL)
                             , ("VK_COMPARE_OP_ALWAYS", pure VK_COMPARE_OP_ALWAYS)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCompareOp")
                        v <- step readPrec
                        pure (VkCompareOp v)
                        )
                    )


pattern VK_COMPARE_OP_NEVER = VkCompareOp 0

pattern VK_COMPARE_OP_LESS = VkCompareOp 1

pattern VK_COMPARE_OP_EQUAL = VkCompareOp 2

pattern VK_COMPARE_OP_LESS_OR_EQUAL = VkCompareOp 3

pattern VK_COMPARE_OP_GREATER = VkCompareOp 4

pattern VK_COMPARE_OP_NOT_EQUAL = VkCompareOp 5

pattern VK_COMPARE_OP_GREATER_OR_EQUAL = VkCompareOp 6

pattern VK_COMPARE_OP_ALWAYS = VkCompareOp 7

newtype VkSampler = VkSampler Word64
  deriving (Eq, Ord, Storable, Show)


data VkSamplerCreateInfo =
  VkSamplerCreateInfo{ vkSType :: VkStructureType 
                     , vkPNext :: Ptr Void 
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
  deriving (Eq, Ord, Show)

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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerCreateInfo))
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


-- ** VkSamplerCreateFlags
-- | Opaque flag
newtype VkSamplerCreateFlags = VkSamplerCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)

-- ** VkSamplerMipmapMode

newtype VkSamplerMipmapMode = VkSamplerMipmapMode Int32
  deriving (Eq, Ord, Storable)

instance Show VkSamplerMipmapMode where
  showsPrec _ VK_SAMPLER_MIPMAP_MODE_NEAREST = showString "VK_SAMPLER_MIPMAP_MODE_NEAREST"
  showsPrec _ VK_SAMPLER_MIPMAP_MODE_LINEAR = showString "VK_SAMPLER_MIPMAP_MODE_LINEAR"
  showsPrec p (VkSamplerMipmapMode x) = showParen (p >= 11) (showString "VkSamplerMipmapMode " . showsPrec 11 x)

instance Read VkSamplerMipmapMode where
  readPrec = parens ( choose [ ("VK_SAMPLER_MIPMAP_MODE_NEAREST", pure VK_SAMPLER_MIPMAP_MODE_NEAREST)
                             , ("VK_SAMPLER_MIPMAP_MODE_LINEAR", pure VK_SAMPLER_MIPMAP_MODE_LINEAR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerMipmapMode")
                        v <- step readPrec
                        pure (VkSamplerMipmapMode v)
                        )
                    )

-- | Choose nearest mip level
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = VkSamplerMipmapMode 0
-- | Linear filter between mip levels
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR = VkSamplerMipmapMode 1

-- ** vkCreateSampler
foreign import ccall "vkCreateSampler" vkCreateSampler ::
  VkDevice ->
  Ptr VkSamplerCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkSampler -> IO VkResult

-- ** VkSampleCountFlags

newtype VkSampleCountFlagBits = VkSampleCountFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkSampleCountFlagBits
type VkSampleCountFlags = VkSampleCountFlagBits

instance Show VkSampleCountFlagBits where
  showsPrec _ VK_SAMPLE_COUNT_1_BIT = showString "VK_SAMPLE_COUNT_1_BIT"
  showsPrec _ VK_SAMPLE_COUNT_2_BIT = showString "VK_SAMPLE_COUNT_2_BIT"
  showsPrec _ VK_SAMPLE_COUNT_4_BIT = showString "VK_SAMPLE_COUNT_4_BIT"
  showsPrec _ VK_SAMPLE_COUNT_8_BIT = showString "VK_SAMPLE_COUNT_8_BIT"
  showsPrec _ VK_SAMPLE_COUNT_16_BIT = showString "VK_SAMPLE_COUNT_16_BIT"
  showsPrec _ VK_SAMPLE_COUNT_32_BIT = showString "VK_SAMPLE_COUNT_32_BIT"
  showsPrec _ VK_SAMPLE_COUNT_64_BIT = showString "VK_SAMPLE_COUNT_64_BIT"
  
  showsPrec p (VkSampleCountFlagBits x) = showParen (p >= 11) (showString "VkSampleCountFlagBits " . showsPrec 11 x)

instance Read VkSampleCountFlagBits where
  readPrec = parens ( choose [ ("VK_SAMPLE_COUNT_1_BIT", pure VK_SAMPLE_COUNT_1_BIT)
                             , ("VK_SAMPLE_COUNT_2_BIT", pure VK_SAMPLE_COUNT_2_BIT)
                             , ("VK_SAMPLE_COUNT_4_BIT", pure VK_SAMPLE_COUNT_4_BIT)
                             , ("VK_SAMPLE_COUNT_8_BIT", pure VK_SAMPLE_COUNT_8_BIT)
                             , ("VK_SAMPLE_COUNT_16_BIT", pure VK_SAMPLE_COUNT_16_BIT)
                             , ("VK_SAMPLE_COUNT_32_BIT", pure VK_SAMPLE_COUNT_32_BIT)
                             , ("VK_SAMPLE_COUNT_64_BIT", pure VK_SAMPLE_COUNT_64_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSampleCountFlagBits")
                        v <- step readPrec
                        pure (VkSampleCountFlagBits v)
                        )
                    )

-- | Sample count 1 supported
pattern VK_SAMPLE_COUNT_1_BIT = VkSampleCountFlagBits 0x1
-- | Sample count 2 supported
pattern VK_SAMPLE_COUNT_2_BIT = VkSampleCountFlagBits 0x2
-- | Sample count 4 supported
pattern VK_SAMPLE_COUNT_4_BIT = VkSampleCountFlagBits 0x4
-- | Sample count 8 supported
pattern VK_SAMPLE_COUNT_8_BIT = VkSampleCountFlagBits 0x8
-- | Sample count 16 supported
pattern VK_SAMPLE_COUNT_16_BIT = VkSampleCountFlagBits 0x10
-- | Sample count 32 supported
pattern VK_SAMPLE_COUNT_32_BIT = VkSampleCountFlagBits 0x20
-- | Sample count 64 supported
pattern VK_SAMPLE_COUNT_64_BIT = VkSampleCountFlagBits 0x40


-- ** vkDestroySampler
foreign import ccall "vkDestroySampler" vkDestroySampler ::
  VkDevice -> VkSampler -> Ptr VkAllocationCallbacks -> IO ()

