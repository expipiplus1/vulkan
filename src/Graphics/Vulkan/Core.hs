{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Core where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( plusPtr
                  )
import Data.Int( Int32(..)
               , Int32
               )
import Foreign.Storable( Storable(..)
                       )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Foreign.C.Types( CFloat(..)
                      )

newtype DeviceSize = DeviceSize Word64
  deriving (Eq, Ord, Storable)

-- ** Format
-- | Vulkan format definitions
newtype Format = Format Int32
  deriving (Eq, Ord, Storable)

instance Show Format where
  showsPrec _ FormatUndefined = showString "FormatUndefined"
  showsPrec _ FormatR4g4UnormPack8 = showString "FormatR4g4UnormPack8"
  showsPrec _ FormatR4g4b4a4UnormPack16 = showString "FormatR4g4b4a4UnormPack16"
  showsPrec _ FormatB4g4r4a4UnormPack16 = showString "FormatB4g4r4a4UnormPack16"
  showsPrec _ FormatR5g6b5UnormPack16 = showString "FormatR5g6b5UnormPack16"
  showsPrec _ FormatB5g6r5UnormPack16 = showString "FormatB5g6r5UnormPack16"
  showsPrec _ FormatR5g5b5a1UnormPack16 = showString "FormatR5g5b5a1UnormPack16"
  showsPrec _ FormatB5g5r5a1UnormPack16 = showString "FormatB5g5r5a1UnormPack16"
  showsPrec _ FormatA1r5g5b5UnormPack16 = showString "FormatA1r5g5b5UnormPack16"
  showsPrec _ FormatR8Unorm = showString "FormatR8Unorm"
  showsPrec _ FormatR8Snorm = showString "FormatR8Snorm"
  showsPrec _ FormatR8Uscaled = showString "FormatR8Uscaled"
  showsPrec _ FormatR8Sscaled = showString "FormatR8Sscaled"
  showsPrec _ FormatR8Uint = showString "FormatR8Uint"
  showsPrec _ FormatR8Sint = showString "FormatR8Sint"
  showsPrec _ FormatR8Srgb = showString "FormatR8Srgb"
  showsPrec _ FormatR8g8Unorm = showString "FormatR8g8Unorm"
  showsPrec _ FormatR8g8Snorm = showString "FormatR8g8Snorm"
  showsPrec _ FormatR8g8Uscaled = showString "FormatR8g8Uscaled"
  showsPrec _ FormatR8g8Sscaled = showString "FormatR8g8Sscaled"
  showsPrec _ FormatR8g8Uint = showString "FormatR8g8Uint"
  showsPrec _ FormatR8g8Sint = showString "FormatR8g8Sint"
  showsPrec _ FormatR8g8Srgb = showString "FormatR8g8Srgb"
  showsPrec _ FormatR8g8b8Unorm = showString "FormatR8g8b8Unorm"
  showsPrec _ FormatR8g8b8Snorm = showString "FormatR8g8b8Snorm"
  showsPrec _ FormatR8g8b8Uscaled = showString "FormatR8g8b8Uscaled"
  showsPrec _ FormatR8g8b8Sscaled = showString "FormatR8g8b8Sscaled"
  showsPrec _ FormatR8g8b8Uint = showString "FormatR8g8b8Uint"
  showsPrec _ FormatR8g8b8Sint = showString "FormatR8g8b8Sint"
  showsPrec _ FormatR8g8b8Srgb = showString "FormatR8g8b8Srgb"
  showsPrec _ FormatB8g8r8Unorm = showString "FormatB8g8r8Unorm"
  showsPrec _ FormatB8g8r8Snorm = showString "FormatB8g8r8Snorm"
  showsPrec _ FormatB8g8r8Uscaled = showString "FormatB8g8r8Uscaled"
  showsPrec _ FormatB8g8r8Sscaled = showString "FormatB8g8r8Sscaled"
  showsPrec _ FormatB8g8r8Uint = showString "FormatB8g8r8Uint"
  showsPrec _ FormatB8g8r8Sint = showString "FormatB8g8r8Sint"
  showsPrec _ FormatB8g8r8Srgb = showString "FormatB8g8r8Srgb"
  showsPrec _ FormatR8g8b8a8Unorm = showString "FormatR8g8b8a8Unorm"
  showsPrec _ FormatR8g8b8a8Snorm = showString "FormatR8g8b8a8Snorm"
  showsPrec _ FormatR8g8b8a8Uscaled = showString "FormatR8g8b8a8Uscaled"
  showsPrec _ FormatR8g8b8a8Sscaled = showString "FormatR8g8b8a8Sscaled"
  showsPrec _ FormatR8g8b8a8Uint = showString "FormatR8g8b8a8Uint"
  showsPrec _ FormatR8g8b8a8Sint = showString "FormatR8g8b8a8Sint"
  showsPrec _ FormatR8g8b8a8Srgb = showString "FormatR8g8b8a8Srgb"
  showsPrec _ FormatB8g8r8a8Unorm = showString "FormatB8g8r8a8Unorm"
  showsPrec _ FormatB8g8r8a8Snorm = showString "FormatB8g8r8a8Snorm"
  showsPrec _ FormatB8g8r8a8Uscaled = showString "FormatB8g8r8a8Uscaled"
  showsPrec _ FormatB8g8r8a8Sscaled = showString "FormatB8g8r8a8Sscaled"
  showsPrec _ FormatB8g8r8a8Uint = showString "FormatB8g8r8a8Uint"
  showsPrec _ FormatB8g8r8a8Sint = showString "FormatB8g8r8a8Sint"
  showsPrec _ FormatB8g8r8a8Srgb = showString "FormatB8g8r8a8Srgb"
  showsPrec _ FormatA8b8g8r8UnormPack32 = showString "FormatA8b8g8r8UnormPack32"
  showsPrec _ FormatA8b8g8r8SnormPack32 = showString "FormatA8b8g8r8SnormPack32"
  showsPrec _ FormatA8b8g8r8UscaledPack32 = showString "FormatA8b8g8r8UscaledPack32"
  showsPrec _ FormatA8b8g8r8SscaledPack32 = showString "FormatA8b8g8r8SscaledPack32"
  showsPrec _ FormatA8b8g8r8UintPack32 = showString "FormatA8b8g8r8UintPack32"
  showsPrec _ FormatA8b8g8r8SintPack32 = showString "FormatA8b8g8r8SintPack32"
  showsPrec _ FormatA8b8g8r8SrgbPack32 = showString "FormatA8b8g8r8SrgbPack32"
  showsPrec _ FormatA2r10g10b10UnormPack32 = showString "FormatA2r10g10b10UnormPack32"
  showsPrec _ FormatA2r10g10b10SnormPack32 = showString "FormatA2r10g10b10SnormPack32"
  showsPrec _ FormatA2r10g10b10UscaledPack32 = showString "FormatA2r10g10b10UscaledPack32"
  showsPrec _ FormatA2r10g10b10SscaledPack32 = showString "FormatA2r10g10b10SscaledPack32"
  showsPrec _ FormatA2r10g10b10UintPack32 = showString "FormatA2r10g10b10UintPack32"
  showsPrec _ FormatA2r10g10b10SintPack32 = showString "FormatA2r10g10b10SintPack32"
  showsPrec _ FormatA2b10g10r10UnormPack32 = showString "FormatA2b10g10r10UnormPack32"
  showsPrec _ FormatA2b10g10r10SnormPack32 = showString "FormatA2b10g10r10SnormPack32"
  showsPrec _ FormatA2b10g10r10UscaledPack32 = showString "FormatA2b10g10r10UscaledPack32"
  showsPrec _ FormatA2b10g10r10SscaledPack32 = showString "FormatA2b10g10r10SscaledPack32"
  showsPrec _ FormatA2b10g10r10UintPack32 = showString "FormatA2b10g10r10UintPack32"
  showsPrec _ FormatA2b10g10r10SintPack32 = showString "FormatA2b10g10r10SintPack32"
  showsPrec _ FormatR16Unorm = showString "FormatR16Unorm"
  showsPrec _ FormatR16Snorm = showString "FormatR16Snorm"
  showsPrec _ FormatR16Uscaled = showString "FormatR16Uscaled"
  showsPrec _ FormatR16Sscaled = showString "FormatR16Sscaled"
  showsPrec _ FormatR16Uint = showString "FormatR16Uint"
  showsPrec _ FormatR16Sint = showString "FormatR16Sint"
  showsPrec _ FormatR16Sfloat = showString "FormatR16Sfloat"
  showsPrec _ FormatR16g16Unorm = showString "FormatR16g16Unorm"
  showsPrec _ FormatR16g16Snorm = showString "FormatR16g16Snorm"
  showsPrec _ FormatR16g16Uscaled = showString "FormatR16g16Uscaled"
  showsPrec _ FormatR16g16Sscaled = showString "FormatR16g16Sscaled"
  showsPrec _ FormatR16g16Uint = showString "FormatR16g16Uint"
  showsPrec _ FormatR16g16Sint = showString "FormatR16g16Sint"
  showsPrec _ FormatR16g16Sfloat = showString "FormatR16g16Sfloat"
  showsPrec _ FormatR16g16b16Unorm = showString "FormatR16g16b16Unorm"
  showsPrec _ FormatR16g16b16Snorm = showString "FormatR16g16b16Snorm"
  showsPrec _ FormatR16g16b16Uscaled = showString "FormatR16g16b16Uscaled"
  showsPrec _ FormatR16g16b16Sscaled = showString "FormatR16g16b16Sscaled"
  showsPrec _ FormatR16g16b16Uint = showString "FormatR16g16b16Uint"
  showsPrec _ FormatR16g16b16Sint = showString "FormatR16g16b16Sint"
  showsPrec _ FormatR16g16b16Sfloat = showString "FormatR16g16b16Sfloat"
  showsPrec _ FormatR16g16b16a16Unorm = showString "FormatR16g16b16a16Unorm"
  showsPrec _ FormatR16g16b16a16Snorm = showString "FormatR16g16b16a16Snorm"
  showsPrec _ FormatR16g16b16a16Uscaled = showString "FormatR16g16b16a16Uscaled"
  showsPrec _ FormatR16g16b16a16Sscaled = showString "FormatR16g16b16a16Sscaled"
  showsPrec _ FormatR16g16b16a16Uint = showString "FormatR16g16b16a16Uint"
  showsPrec _ FormatR16g16b16a16Sint = showString "FormatR16g16b16a16Sint"
  showsPrec _ FormatR16g16b16a16Sfloat = showString "FormatR16g16b16a16Sfloat"
  showsPrec _ FormatR32Uint = showString "FormatR32Uint"
  showsPrec _ FormatR32Sint = showString "FormatR32Sint"
  showsPrec _ FormatR32Sfloat = showString "FormatR32Sfloat"
  showsPrec _ FormatR32g32Uint = showString "FormatR32g32Uint"
  showsPrec _ FormatR32g32Sint = showString "FormatR32g32Sint"
  showsPrec _ FormatR32g32Sfloat = showString "FormatR32g32Sfloat"
  showsPrec _ FormatR32g32b32Uint = showString "FormatR32g32b32Uint"
  showsPrec _ FormatR32g32b32Sint = showString "FormatR32g32b32Sint"
  showsPrec _ FormatR32g32b32Sfloat = showString "FormatR32g32b32Sfloat"
  showsPrec _ FormatR32g32b32a32Uint = showString "FormatR32g32b32a32Uint"
  showsPrec _ FormatR32g32b32a32Sint = showString "FormatR32g32b32a32Sint"
  showsPrec _ FormatR32g32b32a32Sfloat = showString "FormatR32g32b32a32Sfloat"
  showsPrec _ FormatR64Uint = showString "FormatR64Uint"
  showsPrec _ FormatR64Sint = showString "FormatR64Sint"
  showsPrec _ FormatR64Sfloat = showString "FormatR64Sfloat"
  showsPrec _ FormatR64g64Uint = showString "FormatR64g64Uint"
  showsPrec _ FormatR64g64Sint = showString "FormatR64g64Sint"
  showsPrec _ FormatR64g64Sfloat = showString "FormatR64g64Sfloat"
  showsPrec _ FormatR64g64b64Uint = showString "FormatR64g64b64Uint"
  showsPrec _ FormatR64g64b64Sint = showString "FormatR64g64b64Sint"
  showsPrec _ FormatR64g64b64Sfloat = showString "FormatR64g64b64Sfloat"
  showsPrec _ FormatR64g64b64a64Uint = showString "FormatR64g64b64a64Uint"
  showsPrec _ FormatR64g64b64a64Sint = showString "FormatR64g64b64a64Sint"
  showsPrec _ FormatR64g64b64a64Sfloat = showString "FormatR64g64b64a64Sfloat"
  showsPrec _ FormatB10g11r11UfloatPack32 = showString "FormatB10g11r11UfloatPack32"
  showsPrec _ FormatE5b9g9r9UfloatPack32 = showString "FormatE5b9g9r9UfloatPack32"
  showsPrec _ FormatD16Unorm = showString "FormatD16Unorm"
  showsPrec _ FormatX8D24UnormPack32 = showString "FormatX8D24UnormPack32"
  showsPrec _ FormatD32Sfloat = showString "FormatD32Sfloat"
  showsPrec _ FormatS8Uint = showString "FormatS8Uint"
  showsPrec _ FormatD16UnormS8Uint = showString "FormatD16UnormS8Uint"
  showsPrec _ FormatD24UnormS8Uint = showString "FormatD24UnormS8Uint"
  showsPrec _ FormatD32SfloatS8Uint = showString "FormatD32SfloatS8Uint"
  showsPrec _ FormatBc1RgbUnormBlock = showString "FormatBc1RgbUnormBlock"
  showsPrec _ FormatBc1RgbSrgbBlock = showString "FormatBc1RgbSrgbBlock"
  showsPrec _ FormatBc1RgbaUnormBlock = showString "FormatBc1RgbaUnormBlock"
  showsPrec _ FormatBc1RgbaSrgbBlock = showString "FormatBc1RgbaSrgbBlock"
  showsPrec _ FormatBc2UnormBlock = showString "FormatBc2UnormBlock"
  showsPrec _ FormatBc2SrgbBlock = showString "FormatBc2SrgbBlock"
  showsPrec _ FormatBc3UnormBlock = showString "FormatBc3UnormBlock"
  showsPrec _ FormatBc3SrgbBlock = showString "FormatBc3SrgbBlock"
  showsPrec _ FormatBc4UnormBlock = showString "FormatBc4UnormBlock"
  showsPrec _ FormatBc4SnormBlock = showString "FormatBc4SnormBlock"
  showsPrec _ FormatBc5UnormBlock = showString "FormatBc5UnormBlock"
  showsPrec _ FormatBc5SnormBlock = showString "FormatBc5SnormBlock"
  showsPrec _ FormatBc6hUfloatBlock = showString "FormatBc6hUfloatBlock"
  showsPrec _ FormatBc6hSfloatBlock = showString "FormatBc6hSfloatBlock"
  showsPrec _ FormatBc7UnormBlock = showString "FormatBc7UnormBlock"
  showsPrec _ FormatBc7SrgbBlock = showString "FormatBc7SrgbBlock"
  showsPrec _ FormatEtc2R8g8b8UnormBlock = showString "FormatEtc2R8g8b8UnormBlock"
  showsPrec _ FormatEtc2R8g8b8SrgbBlock = showString "FormatEtc2R8g8b8SrgbBlock"
  showsPrec _ FormatEtc2R8g8b8a1UnormBlock = showString "FormatEtc2R8g8b8a1UnormBlock"
  showsPrec _ FormatEtc2R8g8b8a1SrgbBlock = showString "FormatEtc2R8g8b8a1SrgbBlock"
  showsPrec _ FormatEtc2R8g8b8a8UnormBlock = showString "FormatEtc2R8g8b8a8UnormBlock"
  showsPrec _ FormatEtc2R8g8b8a8SrgbBlock = showString "FormatEtc2R8g8b8a8SrgbBlock"
  showsPrec _ FormatEacR11UnormBlock = showString "FormatEacR11UnormBlock"
  showsPrec _ FormatEacR11SnormBlock = showString "FormatEacR11SnormBlock"
  showsPrec _ FormatEacR11g11UnormBlock = showString "FormatEacR11g11UnormBlock"
  showsPrec _ FormatEacR11g11SnormBlock = showString "FormatEacR11g11SnormBlock"
  showsPrec _ FormatAstc4x4UnormBlock = showString "FormatAstc4x4UnormBlock"
  showsPrec _ FormatAstc4x4SrgbBlock = showString "FormatAstc4x4SrgbBlock"
  showsPrec _ FormatAstc5x4UnormBlock = showString "FormatAstc5x4UnormBlock"
  showsPrec _ FormatAstc5x4SrgbBlock = showString "FormatAstc5x4SrgbBlock"
  showsPrec _ FormatAstc5x5UnormBlock = showString "FormatAstc5x5UnormBlock"
  showsPrec _ FormatAstc5x5SrgbBlock = showString "FormatAstc5x5SrgbBlock"
  showsPrec _ FormatAstc6x5UnormBlock = showString "FormatAstc6x5UnormBlock"
  showsPrec _ FormatAstc6x5SrgbBlock = showString "FormatAstc6x5SrgbBlock"
  showsPrec _ FormatAstc6x6UnormBlock = showString "FormatAstc6x6UnormBlock"
  showsPrec _ FormatAstc6x6SrgbBlock = showString "FormatAstc6x6SrgbBlock"
  showsPrec _ FormatAstc8x5UnormBlock = showString "FormatAstc8x5UnormBlock"
  showsPrec _ FormatAstc8x5SrgbBlock = showString "FormatAstc8x5SrgbBlock"
  showsPrec _ FormatAstc8x6UnormBlock = showString "FormatAstc8x6UnormBlock"
  showsPrec _ FormatAstc8x6SrgbBlock = showString "FormatAstc8x6SrgbBlock"
  showsPrec _ FormatAstc8x8UnormBlock = showString "FormatAstc8x8UnormBlock"
  showsPrec _ FormatAstc8x8SrgbBlock = showString "FormatAstc8x8SrgbBlock"
  showsPrec _ FormatAstc10x5UnormBlock = showString "FormatAstc10x5UnormBlock"
  showsPrec _ FormatAstc10x5SrgbBlock = showString "FormatAstc10x5SrgbBlock"
  showsPrec _ FormatAstc10x6UnormBlock = showString "FormatAstc10x6UnormBlock"
  showsPrec _ FormatAstc10x6SrgbBlock = showString "FormatAstc10x6SrgbBlock"
  showsPrec _ FormatAstc10x8UnormBlock = showString "FormatAstc10x8UnormBlock"
  showsPrec _ FormatAstc10x8SrgbBlock = showString "FormatAstc10x8SrgbBlock"
  showsPrec _ FormatAstc10x10UnormBlock = showString "FormatAstc10x10UnormBlock"
  showsPrec _ FormatAstc10x10SrgbBlock = showString "FormatAstc10x10SrgbBlock"
  showsPrec _ FormatAstc12x10UnormBlock = showString "FormatAstc12x10UnormBlock"
  showsPrec _ FormatAstc12x10SrgbBlock = showString "FormatAstc12x10SrgbBlock"
  showsPrec _ FormatAstc12x12UnormBlock = showString "FormatAstc12x12UnormBlock"
  showsPrec _ FormatAstc12x12SrgbBlock = showString "FormatAstc12x12SrgbBlock"
  showsPrec p (Format x) = showParen (p >= 11) (showString "Format " . showsPrec 11 x)

instance Read Format where
  readPrec = parens ( choose [ ("FormatUndefined", pure FormatUndefined)
                             , ("FormatR4g4UnormPack8", pure FormatR4g4UnormPack8)
                             , ("FormatR4g4b4a4UnormPack16", pure FormatR4g4b4a4UnormPack16)
                             , ("FormatB4g4r4a4UnormPack16", pure FormatB4g4r4a4UnormPack16)
                             , ("FormatR5g6b5UnormPack16", pure FormatR5g6b5UnormPack16)
                             , ("FormatB5g6r5UnormPack16", pure FormatB5g6r5UnormPack16)
                             , ("FormatR5g5b5a1UnormPack16", pure FormatR5g5b5a1UnormPack16)
                             , ("FormatB5g5r5a1UnormPack16", pure FormatB5g5r5a1UnormPack16)
                             , ("FormatA1r5g5b5UnormPack16", pure FormatA1r5g5b5UnormPack16)
                             , ("FormatR8Unorm", pure FormatR8Unorm)
                             , ("FormatR8Snorm", pure FormatR8Snorm)
                             , ("FormatR8Uscaled", pure FormatR8Uscaled)
                             , ("FormatR8Sscaled", pure FormatR8Sscaled)
                             , ("FormatR8Uint", pure FormatR8Uint)
                             , ("FormatR8Sint", pure FormatR8Sint)
                             , ("FormatR8Srgb", pure FormatR8Srgb)
                             , ("FormatR8g8Unorm", pure FormatR8g8Unorm)
                             , ("FormatR8g8Snorm", pure FormatR8g8Snorm)
                             , ("FormatR8g8Uscaled", pure FormatR8g8Uscaled)
                             , ("FormatR8g8Sscaled", pure FormatR8g8Sscaled)
                             , ("FormatR8g8Uint", pure FormatR8g8Uint)
                             , ("FormatR8g8Sint", pure FormatR8g8Sint)
                             , ("FormatR8g8Srgb", pure FormatR8g8Srgb)
                             , ("FormatR8g8b8Unorm", pure FormatR8g8b8Unorm)
                             , ("FormatR8g8b8Snorm", pure FormatR8g8b8Snorm)
                             , ("FormatR8g8b8Uscaled", pure FormatR8g8b8Uscaled)
                             , ("FormatR8g8b8Sscaled", pure FormatR8g8b8Sscaled)
                             , ("FormatR8g8b8Uint", pure FormatR8g8b8Uint)
                             , ("FormatR8g8b8Sint", pure FormatR8g8b8Sint)
                             , ("FormatR8g8b8Srgb", pure FormatR8g8b8Srgb)
                             , ("FormatB8g8r8Unorm", pure FormatB8g8r8Unorm)
                             , ("FormatB8g8r8Snorm", pure FormatB8g8r8Snorm)
                             , ("FormatB8g8r8Uscaled", pure FormatB8g8r8Uscaled)
                             , ("FormatB8g8r8Sscaled", pure FormatB8g8r8Sscaled)
                             , ("FormatB8g8r8Uint", pure FormatB8g8r8Uint)
                             , ("FormatB8g8r8Sint", pure FormatB8g8r8Sint)
                             , ("FormatB8g8r8Srgb", pure FormatB8g8r8Srgb)
                             , ("FormatR8g8b8a8Unorm", pure FormatR8g8b8a8Unorm)
                             , ("FormatR8g8b8a8Snorm", pure FormatR8g8b8a8Snorm)
                             , ("FormatR8g8b8a8Uscaled", pure FormatR8g8b8a8Uscaled)
                             , ("FormatR8g8b8a8Sscaled", pure FormatR8g8b8a8Sscaled)
                             , ("FormatR8g8b8a8Uint", pure FormatR8g8b8a8Uint)
                             , ("FormatR8g8b8a8Sint", pure FormatR8g8b8a8Sint)
                             , ("FormatR8g8b8a8Srgb", pure FormatR8g8b8a8Srgb)
                             , ("FormatB8g8r8a8Unorm", pure FormatB8g8r8a8Unorm)
                             , ("FormatB8g8r8a8Snorm", pure FormatB8g8r8a8Snorm)
                             , ("FormatB8g8r8a8Uscaled", pure FormatB8g8r8a8Uscaled)
                             , ("FormatB8g8r8a8Sscaled", pure FormatB8g8r8a8Sscaled)
                             , ("FormatB8g8r8a8Uint", pure FormatB8g8r8a8Uint)
                             , ("FormatB8g8r8a8Sint", pure FormatB8g8r8a8Sint)
                             , ("FormatB8g8r8a8Srgb", pure FormatB8g8r8a8Srgb)
                             , ("FormatA8b8g8r8UnormPack32", pure FormatA8b8g8r8UnormPack32)
                             , ("FormatA8b8g8r8SnormPack32", pure FormatA8b8g8r8SnormPack32)
                             , ("FormatA8b8g8r8UscaledPack32", pure FormatA8b8g8r8UscaledPack32)
                             , ("FormatA8b8g8r8SscaledPack32", pure FormatA8b8g8r8SscaledPack32)
                             , ("FormatA8b8g8r8UintPack32", pure FormatA8b8g8r8UintPack32)
                             , ("FormatA8b8g8r8SintPack32", pure FormatA8b8g8r8SintPack32)
                             , ("FormatA8b8g8r8SrgbPack32", pure FormatA8b8g8r8SrgbPack32)
                             , ("FormatA2r10g10b10UnormPack32", pure FormatA2r10g10b10UnormPack32)
                             , ("FormatA2r10g10b10SnormPack32", pure FormatA2r10g10b10SnormPack32)
                             , ("FormatA2r10g10b10UscaledPack32", pure FormatA2r10g10b10UscaledPack32)
                             , ("FormatA2r10g10b10SscaledPack32", pure FormatA2r10g10b10SscaledPack32)
                             , ("FormatA2r10g10b10UintPack32", pure FormatA2r10g10b10UintPack32)
                             , ("FormatA2r10g10b10SintPack32", pure FormatA2r10g10b10SintPack32)
                             , ("FormatA2b10g10r10UnormPack32", pure FormatA2b10g10r10UnormPack32)
                             , ("FormatA2b10g10r10SnormPack32", pure FormatA2b10g10r10SnormPack32)
                             , ("FormatA2b10g10r10UscaledPack32", pure FormatA2b10g10r10UscaledPack32)
                             , ("FormatA2b10g10r10SscaledPack32", pure FormatA2b10g10r10SscaledPack32)
                             , ("FormatA2b10g10r10UintPack32", pure FormatA2b10g10r10UintPack32)
                             , ("FormatA2b10g10r10SintPack32", pure FormatA2b10g10r10SintPack32)
                             , ("FormatR16Unorm", pure FormatR16Unorm)
                             , ("FormatR16Snorm", pure FormatR16Snorm)
                             , ("FormatR16Uscaled", pure FormatR16Uscaled)
                             , ("FormatR16Sscaled", pure FormatR16Sscaled)
                             , ("FormatR16Uint", pure FormatR16Uint)
                             , ("FormatR16Sint", pure FormatR16Sint)
                             , ("FormatR16Sfloat", pure FormatR16Sfloat)
                             , ("FormatR16g16Unorm", pure FormatR16g16Unorm)
                             , ("FormatR16g16Snorm", pure FormatR16g16Snorm)
                             , ("FormatR16g16Uscaled", pure FormatR16g16Uscaled)
                             , ("FormatR16g16Sscaled", pure FormatR16g16Sscaled)
                             , ("FormatR16g16Uint", pure FormatR16g16Uint)
                             , ("FormatR16g16Sint", pure FormatR16g16Sint)
                             , ("FormatR16g16Sfloat", pure FormatR16g16Sfloat)
                             , ("FormatR16g16b16Unorm", pure FormatR16g16b16Unorm)
                             , ("FormatR16g16b16Snorm", pure FormatR16g16b16Snorm)
                             , ("FormatR16g16b16Uscaled", pure FormatR16g16b16Uscaled)
                             , ("FormatR16g16b16Sscaled", pure FormatR16g16b16Sscaled)
                             , ("FormatR16g16b16Uint", pure FormatR16g16b16Uint)
                             , ("FormatR16g16b16Sint", pure FormatR16g16b16Sint)
                             , ("FormatR16g16b16Sfloat", pure FormatR16g16b16Sfloat)
                             , ("FormatR16g16b16a16Unorm", pure FormatR16g16b16a16Unorm)
                             , ("FormatR16g16b16a16Snorm", pure FormatR16g16b16a16Snorm)
                             , ("FormatR16g16b16a16Uscaled", pure FormatR16g16b16a16Uscaled)
                             , ("FormatR16g16b16a16Sscaled", pure FormatR16g16b16a16Sscaled)
                             , ("FormatR16g16b16a16Uint", pure FormatR16g16b16a16Uint)
                             , ("FormatR16g16b16a16Sint", pure FormatR16g16b16a16Sint)
                             , ("FormatR16g16b16a16Sfloat", pure FormatR16g16b16a16Sfloat)
                             , ("FormatR32Uint", pure FormatR32Uint)
                             , ("FormatR32Sint", pure FormatR32Sint)
                             , ("FormatR32Sfloat", pure FormatR32Sfloat)
                             , ("FormatR32g32Uint", pure FormatR32g32Uint)
                             , ("FormatR32g32Sint", pure FormatR32g32Sint)
                             , ("FormatR32g32Sfloat", pure FormatR32g32Sfloat)
                             , ("FormatR32g32b32Uint", pure FormatR32g32b32Uint)
                             , ("FormatR32g32b32Sint", pure FormatR32g32b32Sint)
                             , ("FormatR32g32b32Sfloat", pure FormatR32g32b32Sfloat)
                             , ("FormatR32g32b32a32Uint", pure FormatR32g32b32a32Uint)
                             , ("FormatR32g32b32a32Sint", pure FormatR32g32b32a32Sint)
                             , ("FormatR32g32b32a32Sfloat", pure FormatR32g32b32a32Sfloat)
                             , ("FormatR64Uint", pure FormatR64Uint)
                             , ("FormatR64Sint", pure FormatR64Sint)
                             , ("FormatR64Sfloat", pure FormatR64Sfloat)
                             , ("FormatR64g64Uint", pure FormatR64g64Uint)
                             , ("FormatR64g64Sint", pure FormatR64g64Sint)
                             , ("FormatR64g64Sfloat", pure FormatR64g64Sfloat)
                             , ("FormatR64g64b64Uint", pure FormatR64g64b64Uint)
                             , ("FormatR64g64b64Sint", pure FormatR64g64b64Sint)
                             , ("FormatR64g64b64Sfloat", pure FormatR64g64b64Sfloat)
                             , ("FormatR64g64b64a64Uint", pure FormatR64g64b64a64Uint)
                             , ("FormatR64g64b64a64Sint", pure FormatR64g64b64a64Sint)
                             , ("FormatR64g64b64a64Sfloat", pure FormatR64g64b64a64Sfloat)
                             , ("FormatB10g11r11UfloatPack32", pure FormatB10g11r11UfloatPack32)
                             , ("FormatE5b9g9r9UfloatPack32", pure FormatE5b9g9r9UfloatPack32)
                             , ("FormatD16Unorm", pure FormatD16Unorm)
                             , ("FormatX8D24UnormPack32", pure FormatX8D24UnormPack32)
                             , ("FormatD32Sfloat", pure FormatD32Sfloat)
                             , ("FormatS8Uint", pure FormatS8Uint)
                             , ("FormatD16UnormS8Uint", pure FormatD16UnormS8Uint)
                             , ("FormatD24UnormS8Uint", pure FormatD24UnormS8Uint)
                             , ("FormatD32SfloatS8Uint", pure FormatD32SfloatS8Uint)
                             , ("FormatBc1RgbUnormBlock", pure FormatBc1RgbUnormBlock)
                             , ("FormatBc1RgbSrgbBlock", pure FormatBc1RgbSrgbBlock)
                             , ("FormatBc1RgbaUnormBlock", pure FormatBc1RgbaUnormBlock)
                             , ("FormatBc1RgbaSrgbBlock", pure FormatBc1RgbaSrgbBlock)
                             , ("FormatBc2UnormBlock", pure FormatBc2UnormBlock)
                             , ("FormatBc2SrgbBlock", pure FormatBc2SrgbBlock)
                             , ("FormatBc3UnormBlock", pure FormatBc3UnormBlock)
                             , ("FormatBc3SrgbBlock", pure FormatBc3SrgbBlock)
                             , ("FormatBc4UnormBlock", pure FormatBc4UnormBlock)
                             , ("FormatBc4SnormBlock", pure FormatBc4SnormBlock)
                             , ("FormatBc5UnormBlock", pure FormatBc5UnormBlock)
                             , ("FormatBc5SnormBlock", pure FormatBc5SnormBlock)
                             , ("FormatBc6hUfloatBlock", pure FormatBc6hUfloatBlock)
                             , ("FormatBc6hSfloatBlock", pure FormatBc6hSfloatBlock)
                             , ("FormatBc7UnormBlock", pure FormatBc7UnormBlock)
                             , ("FormatBc7SrgbBlock", pure FormatBc7SrgbBlock)
                             , ("FormatEtc2R8g8b8UnormBlock", pure FormatEtc2R8g8b8UnormBlock)
                             , ("FormatEtc2R8g8b8SrgbBlock", pure FormatEtc2R8g8b8SrgbBlock)
                             , ("FormatEtc2R8g8b8a1UnormBlock", pure FormatEtc2R8g8b8a1UnormBlock)
                             , ("FormatEtc2R8g8b8a1SrgbBlock", pure FormatEtc2R8g8b8a1SrgbBlock)
                             , ("FormatEtc2R8g8b8a8UnormBlock", pure FormatEtc2R8g8b8a8UnormBlock)
                             , ("FormatEtc2R8g8b8a8SrgbBlock", pure FormatEtc2R8g8b8a8SrgbBlock)
                             , ("FormatEacR11UnormBlock", pure FormatEacR11UnormBlock)
                             , ("FormatEacR11SnormBlock", pure FormatEacR11SnormBlock)
                             , ("FormatEacR11g11UnormBlock", pure FormatEacR11g11UnormBlock)
                             , ("FormatEacR11g11SnormBlock", pure FormatEacR11g11SnormBlock)
                             , ("FormatAstc4x4UnormBlock", pure FormatAstc4x4UnormBlock)
                             , ("FormatAstc4x4SrgbBlock", pure FormatAstc4x4SrgbBlock)
                             , ("FormatAstc5x4UnormBlock", pure FormatAstc5x4UnormBlock)
                             , ("FormatAstc5x4SrgbBlock", pure FormatAstc5x4SrgbBlock)
                             , ("FormatAstc5x5UnormBlock", pure FormatAstc5x5UnormBlock)
                             , ("FormatAstc5x5SrgbBlock", pure FormatAstc5x5SrgbBlock)
                             , ("FormatAstc6x5UnormBlock", pure FormatAstc6x5UnormBlock)
                             , ("FormatAstc6x5SrgbBlock", pure FormatAstc6x5SrgbBlock)
                             , ("FormatAstc6x6UnormBlock", pure FormatAstc6x6UnormBlock)
                             , ("FormatAstc6x6SrgbBlock", pure FormatAstc6x6SrgbBlock)
                             , ("FormatAstc8x5UnormBlock", pure FormatAstc8x5UnormBlock)
                             , ("FormatAstc8x5SrgbBlock", pure FormatAstc8x5SrgbBlock)
                             , ("FormatAstc8x6UnormBlock", pure FormatAstc8x6UnormBlock)
                             , ("FormatAstc8x6SrgbBlock", pure FormatAstc8x6SrgbBlock)
                             , ("FormatAstc8x8UnormBlock", pure FormatAstc8x8UnormBlock)
                             , ("FormatAstc8x8SrgbBlock", pure FormatAstc8x8SrgbBlock)
                             , ("FormatAstc10x5UnormBlock", pure FormatAstc10x5UnormBlock)
                             , ("FormatAstc10x5SrgbBlock", pure FormatAstc10x5SrgbBlock)
                             , ("FormatAstc10x6UnormBlock", pure FormatAstc10x6UnormBlock)
                             , ("FormatAstc10x6SrgbBlock", pure FormatAstc10x6SrgbBlock)
                             , ("FormatAstc10x8UnormBlock", pure FormatAstc10x8UnormBlock)
                             , ("FormatAstc10x8SrgbBlock", pure FormatAstc10x8SrgbBlock)
                             , ("FormatAstc10x10UnormBlock", pure FormatAstc10x10UnormBlock)
                             , ("FormatAstc10x10SrgbBlock", pure FormatAstc10x10SrgbBlock)
                             , ("FormatAstc12x10UnormBlock", pure FormatAstc12x10UnormBlock)
                             , ("FormatAstc12x10SrgbBlock", pure FormatAstc12x10SrgbBlock)
                             , ("FormatAstc12x12UnormBlock", pure FormatAstc12x12UnormBlock)
                             , ("FormatAstc12x12SrgbBlock", pure FormatAstc12x12SrgbBlock)
                             ] +++
                      prec 10 (do
                        expectP (Ident "Format")
                        v <- step readPrec
                        pure (Format v)
                        )
                    )


pattern FormatUndefined = Format 0

pattern FormatR4g4UnormPack8 = Format 1

pattern FormatR4g4b4a4UnormPack16 = Format 2

pattern FormatB4g4r4a4UnormPack16 = Format 3

pattern FormatR5g6b5UnormPack16 = Format 4

pattern FormatB5g6r5UnormPack16 = Format 5

pattern FormatR5g5b5a1UnormPack16 = Format 6

pattern FormatB5g5r5a1UnormPack16 = Format 7

pattern FormatA1r5g5b5UnormPack16 = Format 8

pattern FormatR8Unorm = Format 9

pattern FormatR8Snorm = Format 10

pattern FormatR8Uscaled = Format 11

pattern FormatR8Sscaled = Format 12

pattern FormatR8Uint = Format 13

pattern FormatR8Sint = Format 14

pattern FormatR8Srgb = Format 15

pattern FormatR8g8Unorm = Format 16

pattern FormatR8g8Snorm = Format 17

pattern FormatR8g8Uscaled = Format 18

pattern FormatR8g8Sscaled = Format 19

pattern FormatR8g8Uint = Format 20

pattern FormatR8g8Sint = Format 21

pattern FormatR8g8Srgb = Format 22

pattern FormatR8g8b8Unorm = Format 23

pattern FormatR8g8b8Snorm = Format 24

pattern FormatR8g8b8Uscaled = Format 25

pattern FormatR8g8b8Sscaled = Format 26

pattern FormatR8g8b8Uint = Format 27

pattern FormatR8g8b8Sint = Format 28

pattern FormatR8g8b8Srgb = Format 29

pattern FormatB8g8r8Unorm = Format 30

pattern FormatB8g8r8Snorm = Format 31

pattern FormatB8g8r8Uscaled = Format 32

pattern FormatB8g8r8Sscaled = Format 33

pattern FormatB8g8r8Uint = Format 34

pattern FormatB8g8r8Sint = Format 35

pattern FormatB8g8r8Srgb = Format 36

pattern FormatR8g8b8a8Unorm = Format 37

pattern FormatR8g8b8a8Snorm = Format 38

pattern FormatR8g8b8a8Uscaled = Format 39

pattern FormatR8g8b8a8Sscaled = Format 40

pattern FormatR8g8b8a8Uint = Format 41

pattern FormatR8g8b8a8Sint = Format 42

pattern FormatR8g8b8a8Srgb = Format 43

pattern FormatB8g8r8a8Unorm = Format 44

pattern FormatB8g8r8a8Snorm = Format 45

pattern FormatB8g8r8a8Uscaled = Format 46

pattern FormatB8g8r8a8Sscaled = Format 47

pattern FormatB8g8r8a8Uint = Format 48

pattern FormatB8g8r8a8Sint = Format 49

pattern FormatB8g8r8a8Srgb = Format 50

pattern FormatA8b8g8r8UnormPack32 = Format 51

pattern FormatA8b8g8r8SnormPack32 = Format 52

pattern FormatA8b8g8r8UscaledPack32 = Format 53

pattern FormatA8b8g8r8SscaledPack32 = Format 54

pattern FormatA8b8g8r8UintPack32 = Format 55

pattern FormatA8b8g8r8SintPack32 = Format 56

pattern FormatA8b8g8r8SrgbPack32 = Format 57

pattern FormatA2r10g10b10UnormPack32 = Format 58

pattern FormatA2r10g10b10SnormPack32 = Format 59

pattern FormatA2r10g10b10UscaledPack32 = Format 60

pattern FormatA2r10g10b10SscaledPack32 = Format 61

pattern FormatA2r10g10b10UintPack32 = Format 62

pattern FormatA2r10g10b10SintPack32 = Format 63

pattern FormatA2b10g10r10UnormPack32 = Format 64

pattern FormatA2b10g10r10SnormPack32 = Format 65

pattern FormatA2b10g10r10UscaledPack32 = Format 66

pattern FormatA2b10g10r10SscaledPack32 = Format 67

pattern FormatA2b10g10r10UintPack32 = Format 68

pattern FormatA2b10g10r10SintPack32 = Format 69

pattern FormatR16Unorm = Format 70

pattern FormatR16Snorm = Format 71

pattern FormatR16Uscaled = Format 72

pattern FormatR16Sscaled = Format 73

pattern FormatR16Uint = Format 74

pattern FormatR16Sint = Format 75

pattern FormatR16Sfloat = Format 76

pattern FormatR16g16Unorm = Format 77

pattern FormatR16g16Snorm = Format 78

pattern FormatR16g16Uscaled = Format 79

pattern FormatR16g16Sscaled = Format 80

pattern FormatR16g16Uint = Format 81

pattern FormatR16g16Sint = Format 82

pattern FormatR16g16Sfloat = Format 83

pattern FormatR16g16b16Unorm = Format 84

pattern FormatR16g16b16Snorm = Format 85

pattern FormatR16g16b16Uscaled = Format 86

pattern FormatR16g16b16Sscaled = Format 87

pattern FormatR16g16b16Uint = Format 88

pattern FormatR16g16b16Sint = Format 89

pattern FormatR16g16b16Sfloat = Format 90

pattern FormatR16g16b16a16Unorm = Format 91

pattern FormatR16g16b16a16Snorm = Format 92

pattern FormatR16g16b16a16Uscaled = Format 93

pattern FormatR16g16b16a16Sscaled = Format 94

pattern FormatR16g16b16a16Uint = Format 95

pattern FormatR16g16b16a16Sint = Format 96

pattern FormatR16g16b16a16Sfloat = Format 97

pattern FormatR32Uint = Format 98

pattern FormatR32Sint = Format 99

pattern FormatR32Sfloat = Format 100

pattern FormatR32g32Uint = Format 101

pattern FormatR32g32Sint = Format 102

pattern FormatR32g32Sfloat = Format 103

pattern FormatR32g32b32Uint = Format 104

pattern FormatR32g32b32Sint = Format 105

pattern FormatR32g32b32Sfloat = Format 106

pattern FormatR32g32b32a32Uint = Format 107

pattern FormatR32g32b32a32Sint = Format 108

pattern FormatR32g32b32a32Sfloat = Format 109

pattern FormatR64Uint = Format 110

pattern FormatR64Sint = Format 111

pattern FormatR64Sfloat = Format 112

pattern FormatR64g64Uint = Format 113

pattern FormatR64g64Sint = Format 114

pattern FormatR64g64Sfloat = Format 115

pattern FormatR64g64b64Uint = Format 116

pattern FormatR64g64b64Sint = Format 117

pattern FormatR64g64b64Sfloat = Format 118

pattern FormatR64g64b64a64Uint = Format 119

pattern FormatR64g64b64a64Sint = Format 120

pattern FormatR64g64b64a64Sfloat = Format 121

pattern FormatB10g11r11UfloatPack32 = Format 122

pattern FormatE5b9g9r9UfloatPack32 = Format 123

pattern FormatD16Unorm = Format 124

pattern FormatX8D24UnormPack32 = Format 125

pattern FormatD32Sfloat = Format 126

pattern FormatS8Uint = Format 127

pattern FormatD16UnormS8Uint = Format 128

pattern FormatD24UnormS8Uint = Format 129

pattern FormatD32SfloatS8Uint = Format 130

pattern FormatBc1RgbUnormBlock = Format 131

pattern FormatBc1RgbSrgbBlock = Format 132

pattern FormatBc1RgbaUnormBlock = Format 133

pattern FormatBc1RgbaSrgbBlock = Format 134

pattern FormatBc2UnormBlock = Format 135

pattern FormatBc2SrgbBlock = Format 136

pattern FormatBc3UnormBlock = Format 137

pattern FormatBc3SrgbBlock = Format 138

pattern FormatBc4UnormBlock = Format 139

pattern FormatBc4SnormBlock = Format 140

pattern FormatBc5UnormBlock = Format 141

pattern FormatBc5SnormBlock = Format 142

pattern FormatBc6hUfloatBlock = Format 143

pattern FormatBc6hSfloatBlock = Format 144

pattern FormatBc7UnormBlock = Format 145

pattern FormatBc7SrgbBlock = Format 146

pattern FormatEtc2R8g8b8UnormBlock = Format 147

pattern FormatEtc2R8g8b8SrgbBlock = Format 148

pattern FormatEtc2R8g8b8a1UnormBlock = Format 149

pattern FormatEtc2R8g8b8a1SrgbBlock = Format 150

pattern FormatEtc2R8g8b8a8UnormBlock = Format 151

pattern FormatEtc2R8g8b8a8SrgbBlock = Format 152

pattern FormatEacR11UnormBlock = Format 153

pattern FormatEacR11SnormBlock = Format 154

pattern FormatEacR11g11UnormBlock = Format 155

pattern FormatEacR11g11SnormBlock = Format 156

pattern FormatAstc4x4UnormBlock = Format 157

pattern FormatAstc4x4SrgbBlock = Format 158

pattern FormatAstc5x4UnormBlock = Format 159

pattern FormatAstc5x4SrgbBlock = Format 160

pattern FormatAstc5x5UnormBlock = Format 161

pattern FormatAstc5x5SrgbBlock = Format 162

pattern FormatAstc6x5UnormBlock = Format 163

pattern FormatAstc6x5SrgbBlock = Format 164

pattern FormatAstc6x6UnormBlock = Format 165

pattern FormatAstc6x6SrgbBlock = Format 166

pattern FormatAstc8x5UnormBlock = Format 167

pattern FormatAstc8x5SrgbBlock = Format 168

pattern FormatAstc8x6UnormBlock = Format 169

pattern FormatAstc8x6SrgbBlock = Format 170

pattern FormatAstc8x8UnormBlock = Format 171

pattern FormatAstc8x8SrgbBlock = Format 172

pattern FormatAstc10x5UnormBlock = Format 173

pattern FormatAstc10x5SrgbBlock = Format 174

pattern FormatAstc10x6UnormBlock = Format 175

pattern FormatAstc10x6SrgbBlock = Format 176

pattern FormatAstc10x8UnormBlock = Format 177

pattern FormatAstc10x8SrgbBlock = Format 178

pattern FormatAstc10x10UnormBlock = Format 179

pattern FormatAstc10x10SrgbBlock = Format 180

pattern FormatAstc12x10UnormBlock = Format 181

pattern FormatAstc12x10SrgbBlock = Format 182

pattern FormatAstc12x12UnormBlock = Format 183

pattern FormatAstc12x12SrgbBlock = Format 184

type Flags = Word32


data Extent2D =
  Extent2D{ width :: Word32 
          , height :: Word32 
          }
  deriving (Eq, Ord)

instance Storable Extent2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = Extent2D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (width (poked :: Extent2D))
                *> poke (ptr `plusPtr` 4) (height (poked :: Extent2D))


-- ** SharingMode

newtype SharingMode = SharingMode Int32
  deriving (Eq, Ord, Storable)

instance Show SharingMode where
  showsPrec _ SharingModeExclusive = showString "SharingModeExclusive"
  showsPrec _ SharingModeConcurrent = showString "SharingModeConcurrent"
  showsPrec p (SharingMode x) = showParen (p >= 11) (showString "SharingMode " . showsPrec 11 x)

instance Read SharingMode where
  readPrec = parens ( choose [ ("SharingModeExclusive", pure SharingModeExclusive)
                             , ("SharingModeConcurrent", pure SharingModeConcurrent)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SharingMode")
                        v <- step readPrec
                        pure (SharingMode v)
                        )
                    )


pattern SharingModeExclusive = SharingMode 0

pattern SharingModeConcurrent = SharingMode 1

-- ** StructureType
-- | Structure type enumerant
newtype StructureType = StructureType Int32
  deriving (Eq, Ord, Storable)

instance Show StructureType where
  showsPrec _ StructureTypeApplicationInfo = showString "StructureTypeApplicationInfo"
  showsPrec _ StructureTypeInstanceCreateInfo = showString "StructureTypeInstanceCreateInfo"
  showsPrec _ StructureTypeDeviceQueueCreateInfo = showString "StructureTypeDeviceQueueCreateInfo"
  showsPrec _ StructureTypeDeviceCreateInfo = showString "StructureTypeDeviceCreateInfo"
  showsPrec _ StructureTypeSubmitInfo = showString "StructureTypeSubmitInfo"
  showsPrec _ StructureTypeMemoryAllocateInfo = showString "StructureTypeMemoryAllocateInfo"
  showsPrec _ StructureTypeMappedMemoryRange = showString "StructureTypeMappedMemoryRange"
  showsPrec _ StructureTypeBindSparseInfo = showString "StructureTypeBindSparseInfo"
  showsPrec _ StructureTypeFenceCreateInfo = showString "StructureTypeFenceCreateInfo"
  showsPrec _ StructureTypeSemaphoreCreateInfo = showString "StructureTypeSemaphoreCreateInfo"
  showsPrec _ StructureTypeEventCreateInfo = showString "StructureTypeEventCreateInfo"
  showsPrec _ StructureTypeQueryPoolCreateInfo = showString "StructureTypeQueryPoolCreateInfo"
  showsPrec _ StructureTypeBufferCreateInfo = showString "StructureTypeBufferCreateInfo"
  showsPrec _ StructureTypeBufferViewCreateInfo = showString "StructureTypeBufferViewCreateInfo"
  showsPrec _ StructureTypeImageCreateInfo = showString "StructureTypeImageCreateInfo"
  showsPrec _ StructureTypeImageViewCreateInfo = showString "StructureTypeImageViewCreateInfo"
  showsPrec _ StructureTypeShaderModuleCreateInfo = showString "StructureTypeShaderModuleCreateInfo"
  showsPrec _ StructureTypePipelineCacheCreateInfo = showString "StructureTypePipelineCacheCreateInfo"
  showsPrec _ StructureTypePipelineShaderStageCreateInfo = showString "StructureTypePipelineShaderStageCreateInfo"
  showsPrec _ StructureTypePipelineVertexInputStateCreateInfo = showString "StructureTypePipelineVertexInputStateCreateInfo"
  showsPrec _ StructureTypePipelineInputAssemblyStateCreateInfo = showString "StructureTypePipelineInputAssemblyStateCreateInfo"
  showsPrec _ StructureTypePipelineTessellationStateCreateInfo = showString "StructureTypePipelineTessellationStateCreateInfo"
  showsPrec _ StructureTypePipelineViewportStateCreateInfo = showString "StructureTypePipelineViewportStateCreateInfo"
  showsPrec _ StructureTypePipelineRasterizationStateCreateInfo = showString "StructureTypePipelineRasterizationStateCreateInfo"
  showsPrec _ StructureTypePipelineMultisampleStateCreateInfo = showString "StructureTypePipelineMultisampleStateCreateInfo"
  showsPrec _ StructureTypePipelineDepthStencilStateCreateInfo = showString "StructureTypePipelineDepthStencilStateCreateInfo"
  showsPrec _ StructureTypePipelineColorBlendStateCreateInfo = showString "StructureTypePipelineColorBlendStateCreateInfo"
  showsPrec _ StructureTypePipelineDynamicStateCreateInfo = showString "StructureTypePipelineDynamicStateCreateInfo"
  showsPrec _ StructureTypeGraphicsPipelineCreateInfo = showString "StructureTypeGraphicsPipelineCreateInfo"
  showsPrec _ StructureTypeComputePipelineCreateInfo = showString "StructureTypeComputePipelineCreateInfo"
  showsPrec _ StructureTypePipelineLayoutCreateInfo = showString "StructureTypePipelineLayoutCreateInfo"
  showsPrec _ StructureTypeSamplerCreateInfo = showString "StructureTypeSamplerCreateInfo"
  showsPrec _ StructureTypeDescriptorSetLayoutCreateInfo = showString "StructureTypeDescriptorSetLayoutCreateInfo"
  showsPrec _ StructureTypeDescriptorPoolCreateInfo = showString "StructureTypeDescriptorPoolCreateInfo"
  showsPrec _ StructureTypeDescriptorSetAllocateInfo = showString "StructureTypeDescriptorSetAllocateInfo"
  showsPrec _ StructureTypeWriteDescriptorSet = showString "StructureTypeWriteDescriptorSet"
  showsPrec _ StructureTypeCopyDescriptorSet = showString "StructureTypeCopyDescriptorSet"
  showsPrec _ StructureTypeFramebufferCreateInfo = showString "StructureTypeFramebufferCreateInfo"
  showsPrec _ StructureTypeRenderPassCreateInfo = showString "StructureTypeRenderPassCreateInfo"
  showsPrec _ StructureTypeCommandPoolCreateInfo = showString "StructureTypeCommandPoolCreateInfo"
  showsPrec _ StructureTypeCommandBufferAllocateInfo = showString "StructureTypeCommandBufferAllocateInfo"
  showsPrec _ StructureTypeCommandBufferInheritanceInfo = showString "StructureTypeCommandBufferInheritanceInfo"
  showsPrec _ StructureTypeCommandBufferBeginInfo = showString "StructureTypeCommandBufferBeginInfo"
  showsPrec _ StructureTypeRenderPassBeginInfo = showString "StructureTypeRenderPassBeginInfo"
  showsPrec _ StructureTypeBufferMemoryBarrier = showString "StructureTypeBufferMemoryBarrier"
  showsPrec _ StructureTypeImageMemoryBarrier = showString "StructureTypeImageMemoryBarrier"
  showsPrec _ StructureTypeMemoryBarrier = showString "StructureTypeMemoryBarrier"
  showsPrec _ StructureTypeLoaderInstanceCreateInfo = showString "StructureTypeLoaderInstanceCreateInfo"
  showsPrec _ StructureTypeLoaderDeviceCreateInfo = showString "StructureTypeLoaderDeviceCreateInfo"
  showsPrec p (StructureType x) = showParen (p >= 11) (showString "StructureType " . showsPrec 11 x)

instance Read StructureType where
  readPrec = parens ( choose [ ("StructureTypeApplicationInfo", pure StructureTypeApplicationInfo)
                             , ("StructureTypeInstanceCreateInfo", pure StructureTypeInstanceCreateInfo)
                             , ("StructureTypeDeviceQueueCreateInfo", pure StructureTypeDeviceQueueCreateInfo)
                             , ("StructureTypeDeviceCreateInfo", pure StructureTypeDeviceCreateInfo)
                             , ("StructureTypeSubmitInfo", pure StructureTypeSubmitInfo)
                             , ("StructureTypeMemoryAllocateInfo", pure StructureTypeMemoryAllocateInfo)
                             , ("StructureTypeMappedMemoryRange", pure StructureTypeMappedMemoryRange)
                             , ("StructureTypeBindSparseInfo", pure StructureTypeBindSparseInfo)
                             , ("StructureTypeFenceCreateInfo", pure StructureTypeFenceCreateInfo)
                             , ("StructureTypeSemaphoreCreateInfo", pure StructureTypeSemaphoreCreateInfo)
                             , ("StructureTypeEventCreateInfo", pure StructureTypeEventCreateInfo)
                             , ("StructureTypeQueryPoolCreateInfo", pure StructureTypeQueryPoolCreateInfo)
                             , ("StructureTypeBufferCreateInfo", pure StructureTypeBufferCreateInfo)
                             , ("StructureTypeBufferViewCreateInfo", pure StructureTypeBufferViewCreateInfo)
                             , ("StructureTypeImageCreateInfo", pure StructureTypeImageCreateInfo)
                             , ("StructureTypeImageViewCreateInfo", pure StructureTypeImageViewCreateInfo)
                             , ("StructureTypeShaderModuleCreateInfo", pure StructureTypeShaderModuleCreateInfo)
                             , ("StructureTypePipelineCacheCreateInfo", pure StructureTypePipelineCacheCreateInfo)
                             , ("StructureTypePipelineShaderStageCreateInfo", pure StructureTypePipelineShaderStageCreateInfo)
                             , ("StructureTypePipelineVertexInputStateCreateInfo", pure StructureTypePipelineVertexInputStateCreateInfo)
                             , ("StructureTypePipelineInputAssemblyStateCreateInfo", pure StructureTypePipelineInputAssemblyStateCreateInfo)
                             , ("StructureTypePipelineTessellationStateCreateInfo", pure StructureTypePipelineTessellationStateCreateInfo)
                             , ("StructureTypePipelineViewportStateCreateInfo", pure StructureTypePipelineViewportStateCreateInfo)
                             , ("StructureTypePipelineRasterizationStateCreateInfo", pure StructureTypePipelineRasterizationStateCreateInfo)
                             , ("StructureTypePipelineMultisampleStateCreateInfo", pure StructureTypePipelineMultisampleStateCreateInfo)
                             , ("StructureTypePipelineDepthStencilStateCreateInfo", pure StructureTypePipelineDepthStencilStateCreateInfo)
                             , ("StructureTypePipelineColorBlendStateCreateInfo", pure StructureTypePipelineColorBlendStateCreateInfo)
                             , ("StructureTypePipelineDynamicStateCreateInfo", pure StructureTypePipelineDynamicStateCreateInfo)
                             , ("StructureTypeGraphicsPipelineCreateInfo", pure StructureTypeGraphicsPipelineCreateInfo)
                             , ("StructureTypeComputePipelineCreateInfo", pure StructureTypeComputePipelineCreateInfo)
                             , ("StructureTypePipelineLayoutCreateInfo", pure StructureTypePipelineLayoutCreateInfo)
                             , ("StructureTypeSamplerCreateInfo", pure StructureTypeSamplerCreateInfo)
                             , ("StructureTypeDescriptorSetLayoutCreateInfo", pure StructureTypeDescriptorSetLayoutCreateInfo)
                             , ("StructureTypeDescriptorPoolCreateInfo", pure StructureTypeDescriptorPoolCreateInfo)
                             , ("StructureTypeDescriptorSetAllocateInfo", pure StructureTypeDescriptorSetAllocateInfo)
                             , ("StructureTypeWriteDescriptorSet", pure StructureTypeWriteDescriptorSet)
                             , ("StructureTypeCopyDescriptorSet", pure StructureTypeCopyDescriptorSet)
                             , ("StructureTypeFramebufferCreateInfo", pure StructureTypeFramebufferCreateInfo)
                             , ("StructureTypeRenderPassCreateInfo", pure StructureTypeRenderPassCreateInfo)
                             , ("StructureTypeCommandPoolCreateInfo", pure StructureTypeCommandPoolCreateInfo)
                             , ("StructureTypeCommandBufferAllocateInfo", pure StructureTypeCommandBufferAllocateInfo)
                             , ("StructureTypeCommandBufferInheritanceInfo", pure StructureTypeCommandBufferInheritanceInfo)
                             , ("StructureTypeCommandBufferBeginInfo", pure StructureTypeCommandBufferBeginInfo)
                             , ("StructureTypeRenderPassBeginInfo", pure StructureTypeRenderPassBeginInfo)
                             , ("StructureTypeBufferMemoryBarrier", pure StructureTypeBufferMemoryBarrier)
                             , ("StructureTypeImageMemoryBarrier", pure StructureTypeImageMemoryBarrier)
                             , ("StructureTypeMemoryBarrier", pure StructureTypeMemoryBarrier)
                             , ("StructureTypeLoaderInstanceCreateInfo", pure StructureTypeLoaderInstanceCreateInfo)
                             , ("StructureTypeLoaderDeviceCreateInfo", pure StructureTypeLoaderDeviceCreateInfo)
                             ] +++
                      prec 10 (do
                        expectP (Ident "StructureType")
                        v <- step readPrec
                        pure (StructureType v)
                        )
                    )


pattern StructureTypeApplicationInfo = StructureType 0

pattern StructureTypeInstanceCreateInfo = StructureType 1

pattern StructureTypeDeviceQueueCreateInfo = StructureType 2

pattern StructureTypeDeviceCreateInfo = StructureType 3

pattern StructureTypeSubmitInfo = StructureType 4

pattern StructureTypeMemoryAllocateInfo = StructureType 5

pattern StructureTypeMappedMemoryRange = StructureType 6

pattern StructureTypeBindSparseInfo = StructureType 7

pattern StructureTypeFenceCreateInfo = StructureType 8

pattern StructureTypeSemaphoreCreateInfo = StructureType 9

pattern StructureTypeEventCreateInfo = StructureType 10

pattern StructureTypeQueryPoolCreateInfo = StructureType 11

pattern StructureTypeBufferCreateInfo = StructureType 12

pattern StructureTypeBufferViewCreateInfo = StructureType 13

pattern StructureTypeImageCreateInfo = StructureType 14

pattern StructureTypeImageViewCreateInfo = StructureType 15

pattern StructureTypeShaderModuleCreateInfo = StructureType 16

pattern StructureTypePipelineCacheCreateInfo = StructureType 17

pattern StructureTypePipelineShaderStageCreateInfo = StructureType 18

pattern StructureTypePipelineVertexInputStateCreateInfo = StructureType 19

pattern StructureTypePipelineInputAssemblyStateCreateInfo = StructureType 20

pattern StructureTypePipelineTessellationStateCreateInfo = StructureType 21

pattern StructureTypePipelineViewportStateCreateInfo = StructureType 22

pattern StructureTypePipelineRasterizationStateCreateInfo = StructureType 23

pattern StructureTypePipelineMultisampleStateCreateInfo = StructureType 24

pattern StructureTypePipelineDepthStencilStateCreateInfo = StructureType 25

pattern StructureTypePipelineColorBlendStateCreateInfo = StructureType 26

pattern StructureTypePipelineDynamicStateCreateInfo = StructureType 27

pattern StructureTypeGraphicsPipelineCreateInfo = StructureType 28

pattern StructureTypeComputePipelineCreateInfo = StructureType 29

pattern StructureTypePipelineLayoutCreateInfo = StructureType 30

pattern StructureTypeSamplerCreateInfo = StructureType 31

pattern StructureTypeDescriptorSetLayoutCreateInfo = StructureType 32

pattern StructureTypeDescriptorPoolCreateInfo = StructureType 33

pattern StructureTypeDescriptorSetAllocateInfo = StructureType 34

pattern StructureTypeWriteDescriptorSet = StructureType 35

pattern StructureTypeCopyDescriptorSet = StructureType 36

pattern StructureTypeFramebufferCreateInfo = StructureType 37

pattern StructureTypeRenderPassCreateInfo = StructureType 38

pattern StructureTypeCommandPoolCreateInfo = StructureType 39

pattern StructureTypeCommandBufferAllocateInfo = StructureType 40

pattern StructureTypeCommandBufferInheritanceInfo = StructureType 41

pattern StructureTypeCommandBufferBeginInfo = StructureType 42

pattern StructureTypeRenderPassBeginInfo = StructureType 43

pattern StructureTypeBufferMemoryBarrier = StructureType 44

pattern StructureTypeImageMemoryBarrier = StructureType 45

pattern StructureTypeMemoryBarrier = StructureType 46

pattern StructureTypeLoaderInstanceCreateInfo = StructureType 47

pattern StructureTypeLoaderDeviceCreateInfo = StructureType 48

newtype Bool32 = Bool32 Word32
  deriving (Eq, Ord, Storable)


data Offset2D =
  Offset2D{ x :: Int32 
          , y :: Int32 
          }
  deriving (Eq, Ord)

instance Storable Offset2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = Offset2D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (x (poked :: Offset2D))
                *> poke (ptr `plusPtr` 4) (y (poked :: Offset2D))



data Offset3D =
  Offset3D{ x :: Int32 
          , y :: Int32 
          , z :: Int32 
          }
  deriving (Eq, Ord)

instance Storable Offset3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = Offset3D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 4)
                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (x (poked :: Offset3D))
                *> poke (ptr `plusPtr` 4) (y (poked :: Offset3D))
                *> poke (ptr `plusPtr` 8) (z (poked :: Offset3D))



data Extent3D =
  Extent3D{ width :: Word32 
          , height :: Word32 
          , depth :: Word32 
          }
  deriving (Eq, Ord)

instance Storable Extent3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = Extent3D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 4)
                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (width (poked :: Extent3D))
                *> poke (ptr `plusPtr` 4) (height (poked :: Extent3D))
                *> poke (ptr `plusPtr` 8) (depth (poked :: Extent3D))



data Rect3D =
  Rect3D{ offset :: Offset3D 
        , extent :: Extent3D 
        }
  deriving (Eq, Ord)

instance Storable Rect3D where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = Rect3D <$> peek (ptr `plusPtr` 0)
                    <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (offset (poked :: Rect3D))
                *> poke (ptr `plusPtr` 12) (extent (poked :: Rect3D))


-- ** Result
-- | Error and return codes
newtype Result = Result Int32
  deriving (Eq, Ord, Storable)

instance Show Result where
  showsPrec _ Success = showString "Success"
  showsPrec _ NotReady = showString "NotReady"
  showsPrec _ Timeout = showString "Timeout"
  showsPrec _ EventSet = showString "EventSet"
  showsPrec _ EventReset = showString "EventReset"
  showsPrec _ Incomplete = showString "Incomplete"
  showsPrec _ ErrorOutOfHostMemory = showString "ErrorOutOfHostMemory"
  showsPrec _ ErrorOutOfDeviceMemory = showString "ErrorOutOfDeviceMemory"
  showsPrec _ ErrorInitializationFailed = showString "ErrorInitializationFailed"
  showsPrec _ ErrorDeviceLost = showString "ErrorDeviceLost"
  showsPrec _ ErrorMemoryMapFailed = showString "ErrorMemoryMapFailed"
  showsPrec _ ErrorLayerNotPresent = showString "ErrorLayerNotPresent"
  showsPrec _ ErrorExtensionNotPresent = showString "ErrorExtensionNotPresent"
  showsPrec _ ErrorFeatureNotPresent = showString "ErrorFeatureNotPresent"
  showsPrec _ ErrorIncompatibleDriver = showString "ErrorIncompatibleDriver"
  showsPrec _ ErrorTooManyObjects = showString "ErrorTooManyObjects"
  showsPrec _ ErrorFormatNotSupported = showString "ErrorFormatNotSupported"
  showsPrec p (Result x) = showParen (p >= 11) (showString "Result " . showsPrec 11 x)

instance Read Result where
  readPrec = parens ( choose [ ("Success", pure Success)
                             , ("NotReady", pure NotReady)
                             , ("Timeout", pure Timeout)
                             , ("EventSet", pure EventSet)
                             , ("EventReset", pure EventReset)
                             , ("Incomplete", pure Incomplete)
                             , ("ErrorOutOfHostMemory", pure ErrorOutOfHostMemory)
                             , ("ErrorOutOfDeviceMemory", pure ErrorOutOfDeviceMemory)
                             , ("ErrorInitializationFailed", pure ErrorInitializationFailed)
                             , ("ErrorDeviceLost", pure ErrorDeviceLost)
                             , ("ErrorMemoryMapFailed", pure ErrorMemoryMapFailed)
                             , ("ErrorLayerNotPresent", pure ErrorLayerNotPresent)
                             , ("ErrorExtensionNotPresent", pure ErrorExtensionNotPresent)
                             , ("ErrorFeatureNotPresent", pure ErrorFeatureNotPresent)
                             , ("ErrorIncompatibleDriver", pure ErrorIncompatibleDriver)
                             , ("ErrorTooManyObjects", pure ErrorTooManyObjects)
                             , ("ErrorFormatNotSupported", pure ErrorFormatNotSupported)
                             ] +++
                      prec 10 (do
                        expectP (Ident "Result")
                        v <- step readPrec
                        pure (Result v)
                        )
                    )

-- | Command completed successfully
pattern Success = Result 0
-- | A fence or query has not yet completed
pattern NotReady = Result 1
-- | A wait operation has not completed in the specified time
pattern Timeout = Result 2
-- | An event is signaled
pattern EventSet = Result 3
-- | An event is unsignalled
pattern EventReset = Result 4
-- | A return array was too small for the resul
pattern Incomplete = Result 5
-- | A host memory allocation has failed
pattern ErrorOutOfHostMemory = Result (-1)
-- | A device memory allocation has failed
pattern ErrorOutOfDeviceMemory = Result (-2)
-- | The logical device has been lost. See <<devsandqueues-lost-device>>
pattern ErrorInitializationFailed = Result (-3)
-- | Initialization of a object has failed
pattern ErrorDeviceLost = Result (-4)
-- | Mapping of a memory object has failed
pattern ErrorMemoryMapFailed = Result (-5)
-- | Layer specified does not exist
pattern ErrorLayerNotPresent = Result (-6)
-- | Extension specified does not exist
pattern ErrorExtensionNotPresent = Result (-7)
-- | Requested feature is not available on this device
pattern ErrorFeatureNotPresent = Result (-8)
-- | Unable to find a Vulkan driver
pattern ErrorIncompatibleDriver = Result (-9)
-- | Too many objects of the type have already been created
pattern ErrorTooManyObjects = Result (-10)
-- | Requested format is not supported on this device
pattern ErrorFormatNotSupported = Result (-11)


data Viewport =
  Viewport{ x :: CFloat 
          , y :: CFloat 
          , width :: CFloat 
          , height :: CFloat 
          , minDepth :: CFloat 
          , maxDepth :: CFloat 
          }
  deriving (Eq, Ord)

instance Storable Viewport where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = Viewport <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 4)
                      <*> peek (ptr `plusPtr` 8)
                      <*> peek (ptr `plusPtr` 12)
                      <*> peek (ptr `plusPtr` 16)
                      <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (x (poked :: Viewport))
                *> poke (ptr `plusPtr` 4) (y (poked :: Viewport))
                *> poke (ptr `plusPtr` 8) (width (poked :: Viewport))
                *> poke (ptr `plusPtr` 12) (height (poked :: Viewport))
                *> poke (ptr `plusPtr` 16) (minDepth (poked :: Viewport))
                *> poke (ptr `plusPtr` 20) (maxDepth (poked :: Viewport))



data Rect2D =
  Rect2D{ offset :: Offset2D 
        , extent :: Extent2D 
        }
  deriving (Eq, Ord)

instance Storable Rect2D where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = Rect2D <$> peek (ptr `plusPtr` 0)
                    <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (offset (poked :: Rect2D))
                *> poke (ptr `plusPtr` 8) (extent (poked :: Rect2D))


