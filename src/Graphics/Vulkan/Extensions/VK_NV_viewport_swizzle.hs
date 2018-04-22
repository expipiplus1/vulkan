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
  ( Ptr
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


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkFlags
  )


-- ** VkViewportCoordinateSwizzleNV

-- | VkViewportCoordinateSwizzleNV - Specify how a viewport coordinate is
-- swizzled
--
-- = Description
--
-- These values are described in detail in [Viewport
-- Swizzle](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vertexpostproc-viewport-swizzle).
--
-- = See Also
--
-- 'VkViewportSwizzleNV'
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

-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV"
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV = VkViewportCoordinateSwizzleNV 0

-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV"
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV = VkViewportCoordinateSwizzleNV 1

-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV"
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV = VkViewportCoordinateSwizzleNV 2

-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV"
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV = VkViewportCoordinateSwizzleNV 3

-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV"
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV = VkViewportCoordinateSwizzleNV 4

-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV"
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV = VkViewportCoordinateSwizzleNV 5

-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV"
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV = VkViewportCoordinateSwizzleNV 6

-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV"
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV :: VkViewportCoordinateSwizzleNV
pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV = VkViewportCoordinateSwizzleNV 7
-- ** VkPipelineViewportSwizzleStateCreateFlagsNV

-- | VkPipelineViewportSwizzleStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- @VkPipelineViewportSwizzleStateCreateFlagsNV@ is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'VkPipelineViewportSwizzleStateCreateInfoNV'
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


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV = VkStructureType 1000098000
-- No documentation found for TopLevel "VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION"
pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION :: Integral a => a
pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME"
pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME = "VK_NV_viewport_swizzle"
-- | VkViewportSwizzleNV - Structure specifying a viewport swizzle
--
-- == Valid Usage (Implicit)
--
-- -   @x@ /must/ be a valid 'VkViewportCoordinateSwizzleNV' value
--
-- -   @y@ /must/ be a valid 'VkViewportCoordinateSwizzleNV' value
--
-- -   @z@ /must/ be a valid 'VkViewportCoordinateSwizzleNV' value
--
-- -   @w@ /must/ be a valid 'VkViewportCoordinateSwizzleNV' value
--
-- = See Also
--
-- 'VkPipelineViewportSwizzleStateCreateInfoNV',
-- 'VkViewportCoordinateSwizzleNV'
data VkViewportSwizzleNV = VkViewportSwizzleNV
  { -- | @x@ is a 'VkViewportCoordinateSwizzleNV' value specifying the swizzle
  -- operation to apply to the x component of the primitive
  vkX :: VkViewportCoordinateSwizzleNV
  , -- | @y@ is a 'VkViewportCoordinateSwizzleNV' value specifying the swizzle
  -- operation to apply to the y component of the primitive
  vkY :: VkViewportCoordinateSwizzleNV
  , -- | @z@ is a 'VkViewportCoordinateSwizzleNV' value specifying the swizzle
  -- operation to apply to the z component of the primitive
  vkZ :: VkViewportCoordinateSwizzleNV
  , -- | @w@ is a 'VkViewportCoordinateSwizzleNV' value specifying the swizzle
  -- operation to apply to the w component of the primitive
  vkW :: VkViewportCoordinateSwizzleNV
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
-- | VkPipelineViewportSwizzleStateCreateInfoNV - Structure specifying
-- swizzle applied to primitive clip coordinates
--
-- == Valid Usage
--
-- -   @viewportCount@ /must/ match the @viewportCount@ set in
--     @VkPipelineViewportStateCreateInfo@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV@
--
-- -   @flags@ /must/ be @0@
--
-- -   @viewportCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'VkPipelineViewportSwizzleStateCreateFlagsNV',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'VkViewportSwizzleNV'
data VkPipelineViewportSwizzleStateCreateInfoNV = VkPipelineViewportSwizzleStateCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineViewportSwizzleStateCreateFlagsNV
  , -- | @viewportCount@ is the number of viewport swizzles used by the pipeline.
  vkViewportCount :: Word32
  , -- | @pViewportSwizzles@ is a pointer to an array of 'VkViewportSwizzleNV'
  -- structures, defining the viewport swizzles.
  vkPViewportSwizzles :: Ptr VkViewportSwizzleNV
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkViewportCount (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPViewportSwizzles (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
