{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle
  ( VkViewportCoordinateSwizzleNV(..)
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV
  , pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV
  , VkPipelineViewportSwizzleStateCreateFlagsNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
  , pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION
  , pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
  , VkViewportSwizzleNV(..)
  , VkPipelineViewportSwizzleStateCreateInfoNV(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
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


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkFlags
  )


-- ** VkViewportCoordinateSwizzleNV

-- | 
newtype VkViewportCoordinateSwizzleNV = VkViewportCoordinateSwizzleNV Int32
  deriving (Eq, Ord, Storable)

instance Show VkViewportCoordinateSwizzleNV where
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV"
  showsPrec p (VkViewportCoordinateSwizzleNV x) = showParen (p >= 11) (showString "VkViewportCoordinateSwizzleNV " . showsPrec 11 x)

instance Read VkViewportCoordinateSwizzleNV where
  readPrec = parens ( choose [ ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkViewportCoordinateSwizzleNV")
                        v <- step readPrec
                        pure (VkViewportCoordinateSwizzleNV v)
                        )
                    )

-- | 
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV = VkViewportCoordinateSwizzleNV 0

-- | 
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV = VkViewportCoordinateSwizzleNV 1

-- | 
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV = VkViewportCoordinateSwizzleNV 2

-- | 
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV = VkViewportCoordinateSwizzleNV 3

-- | 
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV = VkViewportCoordinateSwizzleNV 4

-- | 
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV = VkViewportCoordinateSwizzleNV 5

-- | 
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV = VkViewportCoordinateSwizzleNV 6

-- | 
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV = VkViewportCoordinateSwizzleNV 7
-- ** VkPipelineViewportSwizzleStateCreateFlagsNV

-- | 
newtype VkPipelineViewportSwizzleStateCreateFlagsNV = VkPipelineViewportSwizzleStateCreateFlagsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineViewportSwizzleStateCreateFlagsNV where
  
  showsPrec p (VkPipelineViewportSwizzleStateCreateFlagsNV x) = showParen (p >= 11) (showString "VkPipelineViewportSwizzleStateCreateFlagsNV " . showsPrec 11 x)

instance Read VkPipelineViewportSwizzleStateCreateFlagsNV where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineViewportSwizzleStateCreateFlagsNV")
                        v <- step readPrec
                        pure (VkPipelineViewportSwizzleStateCreateFlagsNV v)
                        )
                    )


-- | Nothing
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV = VkStructureType 1000098000
pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION :: Integral a => a
pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1
pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME = "VK_NV_viewport_swizzle"
-- | TODO: Struct comments
data VkViewportSwizzleNV = VkViewportSwizzleNV
  { vkX :: VkViewportCoordinateSwizzleNV
  , vkY :: VkViewportCoordinateSwizzleNV
  , vkZ :: VkViewportCoordinateSwizzleNV
  , vkW :: VkViewportCoordinateSwizzleNV
  }
  deriving (Eq, Show)

instance Storable VkViewportSwizzleNV where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkViewportSwizzleNV <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkViewportSwizzleNV))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkViewportSwizzleNV))
                *> poke (ptr `plusPtr` 8) (vkZ (poked :: VkViewportSwizzleNV))
                *> poke (ptr `plusPtr` 12) (vkW (poked :: VkViewportSwizzleNV))
-- | TODO: Struct comments
data VkPipelineViewportSwizzleStateCreateInfoNV = VkPipelineViewportSwizzleStateCreateInfoNV
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkPipelineViewportSwizzleStateCreateFlagsNV
  , vkViewportCount :: Word32
  , vkViewportSwizzles :: Ptr VkViewportSwizzleNV
  }
  deriving (Eq, Show)

instance Storable VkPipelineViewportSwizzleStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineViewportSwizzleStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
                                                        <*> peek (ptr `plusPtr` 20)
                                                        <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkViewportCount (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkViewportSwizzles (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
