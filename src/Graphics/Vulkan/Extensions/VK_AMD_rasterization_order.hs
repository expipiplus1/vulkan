{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_AMD_rasterization_order
  ( VkRasterizationOrderAMD(..)
  , pattern VK_RASTERIZATION_ORDER_STRICT_AMD
  , pattern VK_RASTERIZATION_ORDER_RELAXED_AMD
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
  , pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION
  , pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
  , VkPipelineRasterizationStateRasterizationOrderAMD(..)
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
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
  )


-- ** VkRasterizationOrderAMD

-- | VkRasterizationOrderAMD - Specify rasterization order for a graphics
-- pipeline
--
-- = See Also
--
-- 'VkPipelineRasterizationStateRasterizationOrderAMD'
newtype VkRasterizationOrderAMD = VkRasterizationOrderAMD Int32
  deriving (Eq, Ord, Storable)

instance Show VkRasterizationOrderAMD where
  showsPrec _ VK_RASTERIZATION_ORDER_STRICT_AMD = showString "VK_RASTERIZATION_ORDER_STRICT_AMD"
  showsPrec _ VK_RASTERIZATION_ORDER_RELAXED_AMD = showString "VK_RASTERIZATION_ORDER_RELAXED_AMD"
  showsPrec p (VkRasterizationOrderAMD x) = showParen (p >= 11) (showString "VkRasterizationOrderAMD " . showsPrec 11 x)

instance Read VkRasterizationOrderAMD where
  readPrec = parens ( choose [ ("VK_RASTERIZATION_ORDER_STRICT_AMD",  pure VK_RASTERIZATION_ORDER_STRICT_AMD)
                             , ("VK_RASTERIZATION_ORDER_RELAXED_AMD", pure VK_RASTERIZATION_ORDER_RELAXED_AMD)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkRasterizationOrderAMD")
                        v <- step readPrec
                        pure (VkRasterizationOrderAMD v)
                        )
                    )

-- | @VK_RASTERIZATION_ORDER_STRICT_AMD@ specifies that operations for each
-- primitive in a subpass /must/ occur in [primitive
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#drawing-primitive-order).
pattern VK_RASTERIZATION_ORDER_STRICT_AMD :: VkRasterizationOrderAMD
pattern VK_RASTERIZATION_ORDER_STRICT_AMD = VkRasterizationOrderAMD 0

-- | @VK_RASTERIZATION_ORDER_RELAXED_AMD@ specifies that operations for each
-- primitive in a subpass /may/ not occur in [primitive
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#drawing-primitive-order).
pattern VK_RASTERIZATION_ORDER_RELAXED_AMD :: VkRasterizationOrderAMD
pattern VK_RASTERIZATION_ORDER_RELAXED_AMD = VkRasterizationOrderAMD 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD"
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD = VkStructureType 1000018000
-- No documentation found for TopLevel "VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION"
pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION :: Integral a => a
pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME"
pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME = "VK_AMD_rasterization_order"
-- | VkPipelineRasterizationStateRasterizationOrderAMD - Structure defining
-- rasterization order for a graphics pipeline
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD@
--
-- -   @rasterizationOrder@ /must/ be a valid 'VkRasterizationOrderAMD'
--     value
--
-- If the @{html_spec_relative}#VK_AMD_rasterization_order@ device
-- extension is not enabled or the application does not request a
-- particular rasterization order through specifying a
-- @VkPipelineRasterizationStateRasterizationOrderAMD@ structure then the
-- rasterization order used by the graphics pipeline defaults to
-- @VK_RASTERIZATION_ORDER_STRICT_AMD@.
--
-- = See Also
--
-- 'VkRasterizationOrderAMD', 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineRasterizationStateRasterizationOrderAMD = VkPipelineRasterizationStateRasterizationOrderAMD
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @rasterizationOrder@ is a 'VkRasterizationOrderAMD' value specifying the
  -- primitive rasterization order to use.
  vkRasterizationOrder :: VkRasterizationOrderAMD
  }
  deriving (Eq, Show)

instance Storable VkPipelineRasterizationStateRasterizationOrderAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPipelineRasterizationStateRasterizationOrderAMD <$> peek (ptr `plusPtr` 0)
                                                               <*> peek (ptr `plusPtr` 8)
                                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineRasterizationStateRasterizationOrderAMD))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineRasterizationStateRasterizationOrderAMD))
                *> poke (ptr `plusPtr` 16) (vkRasterizationOrder (poked :: VkPipelineRasterizationStateRasterizationOrderAMD))
