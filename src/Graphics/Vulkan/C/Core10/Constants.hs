{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}

module Graphics.Vulkan.C.Core10.Constants
  ( VkPipelineCacheHeaderVersion(..)
  , pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE
  , pattern VK_ATTACHMENT_UNUSED
  , pattern VK_LOD_CLAMP_NONE
  , pattern VK_NULL_HANDLE
  , pattern VK_QUEUE_FAMILY_IGNORED
  , pattern VK_REMAINING_ARRAY_LAYERS
  , pattern VK_REMAINING_MIP_LEVELS
  , pattern VK_SUBPASS_EXTERNAL
  , pattern VK_WHOLE_SIZE
  , pattern VK_TRUE
  , pattern VK_FALSE
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.C.Types
  ( CFloat
  )
import Foreign.Ptr
  ( Ptr
  , nullPtr
  )
import Foreign.Storable
  ( Storable(..)
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


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_FALSE
  , pattern VK_TRUE
  )


-- ** VkPipelineCacheHeaderVersion

-- No documentation found for TopLevel "VkPipelineCacheHeaderVersion"
newtype VkPipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion Int32
  deriving (Eq, Ord, Storable)

instance Show VkPipelineCacheHeaderVersion where
  showsPrec _ VK_PIPELINE_CACHE_HEADER_VERSION_ONE = showString "VK_PIPELINE_CACHE_HEADER_VERSION_ONE"
  showsPrec p (VkPipelineCacheHeaderVersion x) = showParen (p >= 11) (showString "VkPipelineCacheHeaderVersion " . showsPrec 11 x)

instance Read VkPipelineCacheHeaderVersion where
  readPrec = parens ( choose [ ("VK_PIPELINE_CACHE_HEADER_VERSION_ONE", pure VK_PIPELINE_CACHE_HEADER_VERSION_ONE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineCacheHeaderVersion")
                        v <- step readPrec
                        pure (VkPipelineCacheHeaderVersion v)
                        )
                    )

-- No documentation found for Nested "VkPipelineCacheHeaderVersion" "VK_PIPELINE_CACHE_HEADER_VERSION_ONE"
pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE :: VkPipelineCacheHeaderVersion
pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE = VkPipelineCacheHeaderVersion 1
-- No documentation found for Nested "Word32" "VK_ATTACHMENT_UNUSED"
pattern VK_ATTACHMENT_UNUSED :: Word32
pattern VK_ATTACHMENT_UNUSED = 0xffffffff
-- No documentation found for Nested "CFloat" "VK_LOD_CLAMP_NONE"
pattern VK_LOD_CLAMP_NONE :: CFloat
pattern VK_LOD_CLAMP_NONE = 1000.0
-- No documentation found for TopLevel "VK_NULL_HANDLE"
pattern VK_NULL_HANDLE :: Ptr a
pattern VK_NULL_HANDLE <- ((== nullPtr) -> True)
  where VK_NULL_HANDLE = nullPtr
-- No documentation found for Nested "Word32" "VK_QUEUE_FAMILY_IGNORED"
pattern VK_QUEUE_FAMILY_IGNORED :: Word32
pattern VK_QUEUE_FAMILY_IGNORED = 0xffffffff
-- No documentation found for Nested "Word32" "VK_REMAINING_ARRAY_LAYERS"
pattern VK_REMAINING_ARRAY_LAYERS :: Word32
pattern VK_REMAINING_ARRAY_LAYERS = 0xffffffff
-- No documentation found for Nested "Word32" "VK_REMAINING_MIP_LEVELS"
pattern VK_REMAINING_MIP_LEVELS :: Word32
pattern VK_REMAINING_MIP_LEVELS = 0xffffffff
-- No documentation found for Nested "Word32" "VK_SUBPASS_EXTERNAL"
pattern VK_SUBPASS_EXTERNAL :: Word32
pattern VK_SUBPASS_EXTERNAL = 0xffffffff
-- No documentation found for Nested "Word64" "VK_WHOLE_SIZE"
pattern VK_WHOLE_SIZE :: Word64
pattern VK_WHOLE_SIZE = 0xffffffffffffffff
