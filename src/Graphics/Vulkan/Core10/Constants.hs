{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}

module Graphics.Vulkan.Core10.Constants
  ( pattern VK_NULL_HANDLE
  , VkPipelineCacheHeaderVersion(..)
  , pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE
  , pattern VK_LOD_CLAMP_NONE
  , pattern VK_REMAINING_MIP_LEVELS
  , pattern VK_REMAINING_ARRAY_LAYERS
  , pattern VK_WHOLE_SIZE
  , pattern VK_ATTACHMENT_UNUSED
  , pattern VK_QUEUE_FAMILY_IGNORED
  , pattern VK_SUBPASS_EXTERNAL
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





-- | VK_NULL_HANDLE - Reserved non-valid object handle
--
-- = See Also
--
-- No cross-references are available
pattern VK_NULL_HANDLE :: Ptr a
pattern VK_NULL_HANDLE <- ((== nullPtr) -> True)
  where VK_NULL_HANDLE = nullPtr
-- ** VkPipelineCacheHeaderVersion

-- | VkPipelineCacheHeaderVersion - Encode pipeline cache version
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.PipelineCache.vkCreatePipelineCache',
-- 'Graphics.Vulkan.Core10.PipelineCache.vkGetPipelineCacheData'
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

-- | @VK_PIPELINE_CACHE_HEADER_VERSION_ONE@ specifies version one of the
-- pipeline cache.
pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE :: VkPipelineCacheHeaderVersion
pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE = VkPipelineCacheHeaderVersion 1
-- No documentation found for Nested "CFloat" "VK_LOD_CLAMP_NONE"
pattern VK_LOD_CLAMP_NONE :: CFloat
pattern VK_LOD_CLAMP_NONE = 1000.0
-- No documentation found for Nested "Word32" "VK_REMAINING_MIP_LEVELS"
pattern VK_REMAINING_MIP_LEVELS :: Word32
pattern VK_REMAINING_MIP_LEVELS = 0xffffffff
-- No documentation found for Nested "Word32" "VK_REMAINING_ARRAY_LAYERS"
pattern VK_REMAINING_ARRAY_LAYERS :: Word32
pattern VK_REMAINING_ARRAY_LAYERS = 0xffffffff
-- No documentation found for Nested "Word64" "VK_WHOLE_SIZE"
pattern VK_WHOLE_SIZE :: Word64
pattern VK_WHOLE_SIZE = 0xffffffffffffffff
-- No documentation found for Nested "Word32" "VK_ATTACHMENT_UNUSED"
pattern VK_ATTACHMENT_UNUSED :: Word32
pattern VK_ATTACHMENT_UNUSED = 0xffffffff
-- No documentation found for Nested "Word32" "VK_QUEUE_FAMILY_IGNORED"
pattern VK_QUEUE_FAMILY_IGNORED :: Word32
pattern VK_QUEUE_FAMILY_IGNORED = 0xffffffff
-- No documentation found for Nested "Word32" "VK_SUBPASS_EXTERNAL"
pattern VK_SUBPASS_EXTERNAL :: Word32
pattern VK_SUBPASS_EXTERNAL = 0xffffffff
