{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.AMD.RasterizationOrder where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Core( VkStructureType(..)
                           )

-- ** VkRasterizationOrderAMD

newtype VkRasterizationOrderAMD = VkRasterizationOrderAMD Int32
  deriving (Eq, Ord, Storable)

instance Show VkRasterizationOrderAMD where
  showsPrec _ VK_RASTERIZATION_ORDER_STRICT_AMD = showString "VK_RASTERIZATION_ORDER_STRICT_AMD"
  showsPrec _ VK_RASTERIZATION_ORDER_RELAXED_AMD = showString "VK_RASTERIZATION_ORDER_RELAXED_AMD"
  showsPrec p (VkRasterizationOrderAMD x) = showParen (p >= 11) (showString "VkRasterizationOrderAMD " . showsPrec 11 x)

instance Read VkRasterizationOrderAMD where
  readPrec = parens ( choose [ ("VK_RASTERIZATION_ORDER_STRICT_AMD", pure VK_RASTERIZATION_ORDER_STRICT_AMD)
                             , ("VK_RASTERIZATION_ORDER_RELAXED_AMD", pure VK_RASTERIZATION_ORDER_RELAXED_AMD)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkRasterizationOrderAMD")
                        v <- step readPrec
                        pure (VkRasterizationOrderAMD v)
                        )
                    )


pattern VK_RASTERIZATION_ORDER_STRICT_AMD = VkRasterizationOrderAMD 0

pattern VK_RASTERIZATION_ORDER_RELAXED_AMD = VkRasterizationOrderAMD 1


data VkPipelineRasterizationStateRasterizationOrderAMD =
  VkPipelineRasterizationStateRasterizationOrderAMD{ vkSType :: VkStructureType 
                                                   , vkPNext :: Ptr Void 
                                                   , vkRasterizationOrder :: VkRasterizationOrderAMD 
                                                   }
  deriving (Eq, Ord, Show)

instance Storable VkPipelineRasterizationStateRasterizationOrderAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPipelineRasterizationStateRasterizationOrderAMD <$> peek (ptr `plusPtr` 0)
                                                               <*> peek (ptr `plusPtr` 8)
                                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineRasterizationStateRasterizationOrderAMD))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineRasterizationStateRasterizationOrderAMD))
                *> poke (ptr `plusPtr` 16) (vkRasterizationOrder (poked :: VkPipelineRasterizationStateRasterizationOrderAMD))


