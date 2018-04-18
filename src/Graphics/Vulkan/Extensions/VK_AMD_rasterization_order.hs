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
  )


-- ** VkRasterizationOrderAMD

-- | 
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

-- | 
pattern VK_RASTERIZATION_ORDER_STRICT_AMD :: VkRasterizationOrderAMD
pattern VK_RASTERIZATION_ORDER_STRICT_AMD = VkRasterizationOrderAMD 0

-- | 
pattern VK_RASTERIZATION_ORDER_RELAXED_AMD :: VkRasterizationOrderAMD
pattern VK_RASTERIZATION_ORDER_RELAXED_AMD = VkRasterizationOrderAMD 1
-- | Nothing
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD = VkStructureType 1000018000
pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION :: Integral a => a
pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1
pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME = "VK_AMD_rasterization_order"
-- | TODO: Struct comments
data VkPipelineRasterizationStateRasterizationOrderAMD = VkPipelineRasterizationStateRasterizationOrderAMD
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkRasterizationOrder :: VkRasterizationOrderAMD
  }
  deriving (Eq, Show)

instance Storable VkPipelineRasterizationStateRasterizationOrderAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPipelineRasterizationStateRasterizationOrderAMD <$> peek (ptr `plusPtr` 0)
                                                               <*> peek (ptr `plusPtr` 8)
                                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineRasterizationStateRasterizationOrderAMD))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPipelineRasterizationStateRasterizationOrderAMD))
                *> poke (ptr `plusPtr` 16) (vkRasterizationOrder (poked :: VkPipelineRasterizationStateRasterizationOrderAMD))
