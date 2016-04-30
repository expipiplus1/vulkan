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
  deriving (Eq, Ord, Storable)

instance Show SamplerAddressMode where
  showsPrec _ SamplerAddressModeRepeat = showString "SamplerAddressModeRepeat"
  showsPrec _ SamplerAddressModeMirroredRepeat = showString "SamplerAddressModeMirroredRepeat"
  showsPrec _ SamplerAddressModeClampToEdge = showString "SamplerAddressModeClampToEdge"
  showsPrec _ SamplerAddressModeClampToBorder = showString "SamplerAddressModeClampToBorder"
  showsPrec _ SamplerAddressModeMirrorClampToEdge = showString "SamplerAddressModeMirrorClampToEdge"
  showsPrec p (SamplerAddressMode x) = showParen (p >= 11) (showString "SamplerAddressMode " . showsPrec 11 x)

instance Read SamplerAddressMode where
  readPrec = parens ( choose [ ("SamplerAddressModeRepeat", pure SamplerAddressModeRepeat)
                             , ("SamplerAddressModeMirroredRepeat", pure SamplerAddressModeMirroredRepeat)
                             , ("SamplerAddressModeClampToEdge", pure SamplerAddressModeClampToEdge)
                             , ("SamplerAddressModeClampToBorder", pure SamplerAddressModeClampToBorder)
                             , ("SamplerAddressModeMirrorClampToEdge", pure SamplerAddressModeMirrorClampToEdge)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SamplerAddressMode")
                        v <- step readPrec
                        pure (SamplerAddressMode v)
                        )
                    )


pattern SamplerAddressModeRepeat = SamplerAddressMode 0

pattern SamplerAddressModeMirroredRepeat = SamplerAddressMode 1

pattern SamplerAddressModeClampToEdge = SamplerAddressMode 2

pattern SamplerAddressModeClampToBorder = SamplerAddressMode 3

pattern SamplerAddressModeMirrorClampToEdge = SamplerAddressMode 4

-- ** Filter

newtype Filter = Filter Int32
  deriving (Eq, Ord, Storable)

instance Show Filter where
  showsPrec _ FilterNearest = showString "FilterNearest"
  showsPrec _ FilterLinear = showString "FilterLinear"
  showsPrec p (Filter x) = showParen (p >= 11) (showString "Filter " . showsPrec 11 x)

instance Read Filter where
  readPrec = parens ( choose [ ("FilterNearest", pure FilterNearest)
                             , ("FilterLinear", pure FilterLinear)
                             ] +++
                      prec 10 (do
                        expectP (Ident "Filter")
                        v <- step readPrec
                        pure (Filter v)
                        )
                    )


pattern FilterNearest = Filter 0

pattern FilterLinear = Filter 1

-- ** BorderColor

newtype BorderColor = BorderColor Int32
  deriving (Eq, Ord, Storable)

instance Show BorderColor where
  showsPrec _ BorderColorFloatTransparentBlack = showString "BorderColorFloatTransparentBlack"
  showsPrec _ BorderColorIntTransparentBlack = showString "BorderColorIntTransparentBlack"
  showsPrec _ BorderColorFloatOpaqueBlack = showString "BorderColorFloatOpaqueBlack"
  showsPrec _ BorderColorIntOpaqueBlack = showString "BorderColorIntOpaqueBlack"
  showsPrec _ BorderColorFloatOpaqueWhite = showString "BorderColorFloatOpaqueWhite"
  showsPrec _ BorderColorIntOpaqueWhite = showString "BorderColorIntOpaqueWhite"
  showsPrec p (BorderColor x) = showParen (p >= 11) (showString "BorderColor " . showsPrec 11 x)

instance Read BorderColor where
  readPrec = parens ( choose [ ("BorderColorFloatTransparentBlack", pure BorderColorFloatTransparentBlack)
                             , ("BorderColorIntTransparentBlack", pure BorderColorIntTransparentBlack)
                             , ("BorderColorFloatOpaqueBlack", pure BorderColorFloatOpaqueBlack)
                             , ("BorderColorIntOpaqueBlack", pure BorderColorIntOpaqueBlack)
                             , ("BorderColorFloatOpaqueWhite", pure BorderColorFloatOpaqueWhite)
                             , ("BorderColorIntOpaqueWhite", pure BorderColorIntOpaqueWhite)
                             ] +++
                      prec 10 (do
                        expectP (Ident "BorderColor")
                        v <- step readPrec
                        pure (BorderColor v)
                        )
                    )


pattern BorderColorFloatTransparentBlack = BorderColor 0

pattern BorderColorIntTransparentBlack = BorderColor 1

pattern BorderColorFloatOpaqueBlack = BorderColor 2

pattern BorderColorIntOpaqueBlack = BorderColor 3

pattern BorderColorFloatOpaqueWhite = BorderColor 4

pattern BorderColorIntOpaqueWhite = BorderColor 5

-- ** CompareOp

newtype CompareOp = CompareOp Int32
  deriving (Eq, Ord, Storable)

instance Show CompareOp where
  showsPrec _ CompareOpNever = showString "CompareOpNever"
  showsPrec _ CompareOpLess = showString "CompareOpLess"
  showsPrec _ CompareOpEqual = showString "CompareOpEqual"
  showsPrec _ CompareOpLessOrEqual = showString "CompareOpLessOrEqual"
  showsPrec _ CompareOpGreater = showString "CompareOpGreater"
  showsPrec _ CompareOpNotEqual = showString "CompareOpNotEqual"
  showsPrec _ CompareOpGreaterOrEqual = showString "CompareOpGreaterOrEqual"
  showsPrec _ CompareOpAlways = showString "CompareOpAlways"
  showsPrec p (CompareOp x) = showParen (p >= 11) (showString "CompareOp " . showsPrec 11 x)

instance Read CompareOp where
  readPrec = parens ( choose [ ("CompareOpNever", pure CompareOpNever)
                             , ("CompareOpLess", pure CompareOpLess)
                             , ("CompareOpEqual", pure CompareOpEqual)
                             , ("CompareOpLessOrEqual", pure CompareOpLessOrEqual)
                             , ("CompareOpGreater", pure CompareOpGreater)
                             , ("CompareOpNotEqual", pure CompareOpNotEqual)
                             , ("CompareOpGreaterOrEqual", pure CompareOpGreaterOrEqual)
                             , ("CompareOpAlways", pure CompareOpAlways)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CompareOp")
                        v <- step readPrec
                        pure (CompareOp v)
                        )
                    )


pattern CompareOpNever = CompareOp 0

pattern CompareOpLess = CompareOp 1

pattern CompareOpEqual = CompareOp 2

pattern CompareOpLessOrEqual = CompareOp 3

pattern CompareOpGreater = CompareOp 4

pattern CompareOpNotEqual = CompareOp 5

pattern CompareOpGreaterOrEqual = CompareOp 6

pattern CompareOpAlways = CompareOp 7

newtype Sampler = Sampler Word64
  deriving (Eq, Ord, Storable)


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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord, Storable)

-- ** SamplerMipmapMode

newtype SamplerMipmapMode = SamplerMipmapMode Int32
  deriving (Eq, Ord, Storable)

instance Show SamplerMipmapMode where
  showsPrec _ SamplerMipmapModeNearest = showString "SamplerMipmapModeNearest"
  showsPrec _ SamplerMipmapModeLinear = showString "SamplerMipmapModeLinear"
  showsPrec p (SamplerMipmapMode x) = showParen (p >= 11) (showString "SamplerMipmapMode " . showsPrec 11 x)

instance Read SamplerMipmapMode where
  readPrec = parens ( choose [ ("SamplerMipmapModeNearest", pure SamplerMipmapModeNearest)
                             , ("SamplerMipmapModeLinear", pure SamplerMipmapModeLinear)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SamplerMipmapMode")
                        v <- step readPrec
                        pure (SamplerMipmapMode v)
                        )
                    )

-- | Choose nearest mip level
pattern SamplerMipmapModeNearest = SamplerMipmapMode 0
-- | Linear filter between mip levels
pattern SamplerMipmapModeLinear = SamplerMipmapMode 1

-- ** createSampler
foreign import ccall "vkCreateSampler" createSampler ::
  Device ->
  Ptr SamplerCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Sampler -> IO Result

-- ** SampleCountFlags

newtype SampleCountFlags = SampleCountFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show SampleCountFlags where
  showsPrec _ SampleCount1Bit = showString "SampleCount1Bit"
  showsPrec _ SampleCount2Bit = showString "SampleCount2Bit"
  showsPrec _ SampleCount4Bit = showString "SampleCount4Bit"
  showsPrec _ SampleCount8Bit = showString "SampleCount8Bit"
  showsPrec _ SampleCount16Bit = showString "SampleCount16Bit"
  showsPrec _ SampleCount32Bit = showString "SampleCount32Bit"
  showsPrec _ SampleCount64Bit = showString "SampleCount64Bit"
  
  showsPrec p (SampleCountFlags x) = showParen (p >= 11) (showString "SampleCountFlags " . showsPrec 11 x)

instance Read SampleCountFlags where
  readPrec = parens ( choose [ ("SampleCount1Bit", pure SampleCount1Bit)
                             , ("SampleCount2Bit", pure SampleCount2Bit)
                             , ("SampleCount4Bit", pure SampleCount4Bit)
                             , ("SampleCount8Bit", pure SampleCount8Bit)
                             , ("SampleCount16Bit", pure SampleCount16Bit)
                             , ("SampleCount32Bit", pure SampleCount32Bit)
                             , ("SampleCount64Bit", pure SampleCount64Bit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SampleCountFlags")
                        v <- step readPrec
                        pure (SampleCountFlags v)
                        )
                    )

-- | Sample count 1 supported
pattern SampleCount1Bit = SampleCountFlags 0x1
-- | Sample count 2 supported
pattern SampleCount2Bit = SampleCountFlags 0x2
-- | Sample count 4 supported
pattern SampleCount4Bit = SampleCountFlags 0x4
-- | Sample count 8 supported
pattern SampleCount8Bit = SampleCountFlags 0x8
-- | Sample count 16 supported
pattern SampleCount16Bit = SampleCountFlags 0x10
-- | Sample count 32 supported
pattern SampleCount32Bit = SampleCountFlags 0x20
-- | Sample count 64 supported
pattern SampleCount64Bit = SampleCountFlags 0x40


-- ** destroySampler
foreign import ccall "vkDestroySampler" destroySampler ::
  Device -> Sampler -> Ptr AllocationCallbacks -> IO ()

