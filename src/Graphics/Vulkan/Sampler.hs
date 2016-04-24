{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Sampler where

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
import Graphics.Vulkan.Core( Bool32(..)
                           , StructureType(..)
                           , Result(..)
                           , Flags(..)
                           )
import Foreign.C.Types( CFloat(..)
                      )

-- ** SamplerAddressMode

newtype SamplerAddressMode = SamplerAddressMode Int32
  deriving (Eq, Storable)

instance Show SamplerAddressMode where
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_REPEAT = showString "VK_SAMPLER_ADDRESS_MODE_REPEAT"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = showString "VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = showString "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER"
  showsPrec _ VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE = showString "VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE"
  showsPrec p (SamplerAddressMode x) = showParen (p >= 11) (showString "SamplerAddressMode " . showsPrec 11 x)

instance Read SamplerAddressMode where
  readPrec = parens ( choose [ ("VK_SAMPLER_ADDRESS_MODE_REPEAT", pure VK_SAMPLER_ADDRESS_MODE_REPEAT)
                             , ("VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT", pure VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT)
                             , ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE", pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)
                             , ("VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER", pure VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER)
                             , ("VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE", pure VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SamplerAddressMode")
                        v <- step readPrec
                        pure (SamplerAddressMode v)
                        )
                    )


pattern VK_SAMPLER_ADDRESS_MODE_REPEAT = SamplerAddressMode 0

pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = SamplerAddressMode 1

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = SamplerAddressMode 2

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = SamplerAddressMode 3

pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE = SamplerAddressMode 4

-- ** Filter

newtype Filter = Filter Int32
  deriving (Eq, Storable)

instance Show Filter where
  showsPrec _ VK_FILTER_NEAREST = showString "VK_FILTER_NEAREST"
  showsPrec _ VK_FILTER_LINEAR = showString "VK_FILTER_LINEAR"
  showsPrec p (Filter x) = showParen (p >= 11) (showString "Filter " . showsPrec 11 x)

instance Read Filter where
  readPrec = parens ( choose [ ("VK_FILTER_NEAREST", pure VK_FILTER_NEAREST)
                             , ("VK_FILTER_LINEAR", pure VK_FILTER_LINEAR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "Filter")
                        v <- step readPrec
                        pure (Filter v)
                        )
                    )


pattern VK_FILTER_NEAREST = Filter 0

pattern VK_FILTER_LINEAR = Filter 1

-- ** BorderColor

newtype BorderColor = BorderColor Int32
  deriving (Eq, Storable)

instance Show BorderColor where
  showsPrec _ VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = showString "VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK"
  showsPrec _ VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = showString "VK_BORDER_COLOR_INT_TRANSPARENT_BLACK"
  showsPrec _ VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = showString "VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK"
  showsPrec _ VK_BORDER_COLOR_INT_OPAQUE_BLACK = showString "VK_BORDER_COLOR_INT_OPAQUE_BLACK"
  showsPrec _ VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = showString "VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE"
  showsPrec _ VK_BORDER_COLOR_INT_OPAQUE_WHITE = showString "VK_BORDER_COLOR_INT_OPAQUE_WHITE"
  showsPrec p (BorderColor x) = showParen (p >= 11) (showString "BorderColor " . showsPrec 11 x)

instance Read BorderColor where
  readPrec = parens ( choose [ ("VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK", pure VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK)
                             , ("VK_BORDER_COLOR_INT_TRANSPARENT_BLACK", pure VK_BORDER_COLOR_INT_TRANSPARENT_BLACK)
                             , ("VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK", pure VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK)
                             , ("VK_BORDER_COLOR_INT_OPAQUE_BLACK", pure VK_BORDER_COLOR_INT_OPAQUE_BLACK)
                             , ("VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE", pure VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE)
                             , ("VK_BORDER_COLOR_INT_OPAQUE_WHITE", pure VK_BORDER_COLOR_INT_OPAQUE_WHITE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "BorderColor")
                        v <- step readPrec
                        pure (BorderColor v)
                        )
                    )


pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = BorderColor 0

pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = BorderColor 1

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = BorderColor 2

pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK = BorderColor 3

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = BorderColor 4

pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE = BorderColor 5

-- ** CompareOp

newtype CompareOp = CompareOp Int32
  deriving (Eq, Storable)

instance Show CompareOp where
  showsPrec _ VK_COMPARE_OP_NEVER = showString "VK_COMPARE_OP_NEVER"
  showsPrec _ VK_COMPARE_OP_LESS = showString "VK_COMPARE_OP_LESS"
  showsPrec _ VK_COMPARE_OP_EQUAL = showString "VK_COMPARE_OP_EQUAL"
  showsPrec _ VK_COMPARE_OP_LESS_OR_EQUAL = showString "VK_COMPARE_OP_LESS_OR_EQUAL"
  showsPrec _ VK_COMPARE_OP_GREATER = showString "VK_COMPARE_OP_GREATER"
  showsPrec _ VK_COMPARE_OP_NOT_EQUAL = showString "VK_COMPARE_OP_NOT_EQUAL"
  showsPrec _ VK_COMPARE_OP_GREATER_OR_EQUAL = showString "VK_COMPARE_OP_GREATER_OR_EQUAL"
  showsPrec _ VK_COMPARE_OP_ALWAYS = showString "VK_COMPARE_OP_ALWAYS"
  showsPrec p (CompareOp x) = showParen (p >= 11) (showString "CompareOp " . showsPrec 11 x)

instance Read CompareOp where
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
                        expectP (Ident "CompareOp")
                        v <- step readPrec
                        pure (CompareOp v)
                        )
                    )


pattern VK_COMPARE_OP_NEVER = CompareOp 0

pattern VK_COMPARE_OP_LESS = CompareOp 1

pattern VK_COMPARE_OP_EQUAL = CompareOp 2

pattern VK_COMPARE_OP_LESS_OR_EQUAL = CompareOp 3

pattern VK_COMPARE_OP_GREATER = CompareOp 4

pattern VK_COMPARE_OP_NOT_EQUAL = CompareOp 5

pattern VK_COMPARE_OP_GREATER_OR_EQUAL = CompareOp 6

pattern VK_COMPARE_OP_ALWAYS = CompareOp 7

newtype Sampler = Sampler Word64
  deriving (Eq, Storable)


data SamplerCreateInfo =
  SamplerCreateInfo{ sType :: StructureType 
                   , pNext :: Ptr Void 
                   , flags :: SamplerCreateFlags 
                   , magFilter :: Filter 
                   , minFilter :: Filter 
                   , mipmapMode :: SamplerMipmapMode 
                   , addressModeU :: SamplerAddressMode 
                   , addressModeV :: SamplerAddressMode 
                   , addressModeW :: SamplerAddressMode 
                   , mipLodBias :: CFloat 
                   , anisotropyEnable :: Bool32 
                   , maxAnisotropy :: CFloat 
                   , compareEnable :: Bool32 
                   , compareOp :: CompareOp 
                   , minLod :: CFloat 
                   , maxLod :: CFloat 
                   , borderColor :: BorderColor 
                   , unnormalizedCoordinates :: Bool32 
                   }
  deriving (Eq)

instance Storable SamplerCreateInfo where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek ptr = SamplerCreateInfo <$> peek (ptr `plusPtr` 0)
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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 20) (magFilter (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 24) (minFilter (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 28) (mipmapMode (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 32) (addressModeU (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 36) (addressModeV (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 40) (addressModeW (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 44) (mipLodBias (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 48) (anisotropyEnable (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 52) (maxAnisotropy (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 56) (compareEnable (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 60) (compareOp (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 64) (minLod (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 68) (maxLod (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 72) (borderColor (poked :: SamplerCreateInfo))
                *> poke (ptr `plusPtr` 76) (unnormalizedCoordinates (poked :: SamplerCreateInfo))


-- ** SamplerCreateFlags
-- | Opaque flag
newtype SamplerCreateFlags = SamplerCreateFlags Flags
  deriving (Eq, Storable)

-- ** SamplerMipmapMode

newtype SamplerMipmapMode = SamplerMipmapMode Int32
  deriving (Eq, Storable)

instance Show SamplerMipmapMode where
  showsPrec _ VK_SAMPLER_MIPMAP_MODE_NEAREST = showString "VK_SAMPLER_MIPMAP_MODE_NEAREST"
  showsPrec _ VK_SAMPLER_MIPMAP_MODE_LINEAR = showString "VK_SAMPLER_MIPMAP_MODE_LINEAR"
  showsPrec p (SamplerMipmapMode x) = showParen (p >= 11) (showString "SamplerMipmapMode " . showsPrec 11 x)

instance Read SamplerMipmapMode where
  readPrec = parens ( choose [ ("VK_SAMPLER_MIPMAP_MODE_NEAREST", pure VK_SAMPLER_MIPMAP_MODE_NEAREST)
                             , ("VK_SAMPLER_MIPMAP_MODE_LINEAR", pure VK_SAMPLER_MIPMAP_MODE_LINEAR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SamplerMipmapMode")
                        v <- step readPrec
                        pure (SamplerMipmapMode v)
                        )
                    )

-- | Choose nearest mip level
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = SamplerMipmapMode 0
-- | Linear filter between mip levels
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR = SamplerMipmapMode 1

-- ** createSampler
foreign import ccall "vkCreateSampler" createSampler ::
  Device ->
  Ptr SamplerCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Sampler -> IO Result

-- ** SampleCountFlags

newtype SampleCountFlags = SampleCountFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show SampleCountFlags where
  showsPrec _ VK_SAMPLE_COUNT_1_BIT = showString "VK_SAMPLE_COUNT_1_BIT"
  showsPrec _ VK_SAMPLE_COUNT_2_BIT = showString "VK_SAMPLE_COUNT_2_BIT"
  showsPrec _ VK_SAMPLE_COUNT_4_BIT = showString "VK_SAMPLE_COUNT_4_BIT"
  showsPrec _ VK_SAMPLE_COUNT_8_BIT = showString "VK_SAMPLE_COUNT_8_BIT"
  showsPrec _ VK_SAMPLE_COUNT_16_BIT = showString "VK_SAMPLE_COUNT_16_BIT"
  showsPrec _ VK_SAMPLE_COUNT_32_BIT = showString "VK_SAMPLE_COUNT_32_BIT"
  showsPrec _ VK_SAMPLE_COUNT_64_BIT = showString "VK_SAMPLE_COUNT_64_BIT"
  
  showsPrec p (SampleCountFlags x) = showParen (p >= 11) (showString "SampleCountFlags " . showsPrec 11 x)

instance Read SampleCountFlags where
  readPrec = parens ( choose [ ("VK_SAMPLE_COUNT_1_BIT", pure VK_SAMPLE_COUNT_1_BIT)
                             , ("VK_SAMPLE_COUNT_2_BIT", pure VK_SAMPLE_COUNT_2_BIT)
                             , ("VK_SAMPLE_COUNT_4_BIT", pure VK_SAMPLE_COUNT_4_BIT)
                             , ("VK_SAMPLE_COUNT_8_BIT", pure VK_SAMPLE_COUNT_8_BIT)
                             , ("VK_SAMPLE_COUNT_16_BIT", pure VK_SAMPLE_COUNT_16_BIT)
                             , ("VK_SAMPLE_COUNT_32_BIT", pure VK_SAMPLE_COUNT_32_BIT)
                             , ("VK_SAMPLE_COUNT_64_BIT", pure VK_SAMPLE_COUNT_64_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SampleCountFlags")
                        v <- step readPrec
                        pure (SampleCountFlags v)
                        )
                    )

-- | Sample count 1 supported
pattern VK_SAMPLE_COUNT_1_BIT = SampleCountFlags 0x1
-- | Sample count 2 supported
pattern VK_SAMPLE_COUNT_2_BIT = SampleCountFlags 0x2
-- | Sample count 4 supported
pattern VK_SAMPLE_COUNT_4_BIT = SampleCountFlags 0x4
-- | Sample count 8 supported
pattern VK_SAMPLE_COUNT_8_BIT = SampleCountFlags 0x8
-- | Sample count 16 supported
pattern VK_SAMPLE_COUNT_16_BIT = SampleCountFlags 0x10
-- | Sample count 32 supported
pattern VK_SAMPLE_COUNT_32_BIT = SampleCountFlags 0x20
-- | Sample count 64 supported
pattern VK_SAMPLE_COUNT_64_BIT = SampleCountFlags 0x40


-- ** destroySampler
foreign import ccall "vkDestroySampler" destroySampler ::
  Device -> Sampler -> Ptr AllocationCallbacks -> IO ()

