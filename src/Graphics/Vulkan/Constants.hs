{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Constants where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64
                , Word32
                )
import Data.Int( Int32
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


pattern VK_SUBPASS_EXTERNAL = 0xffffffff :: Word32

pattern VK_UUID_SIZE = 16
type VK_UUID_SIZE = 16

pattern VK_REMAINING_MIP_LEVELS = 0xffffffff :: Word32

pattern VK_LOD_CLAMP_NONE = 1000.0

pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256
type VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256

pattern VK_ATTACHMENT_UNUSED = 0xffffffff :: Word32

pattern VK_MAX_MEMORY_TYPES = 32
type VK_MAX_MEMORY_TYPES = 32

pattern VK_WHOLE_SIZE = 0xffffffffffffffff :: Word64

pattern VK_REMAINING_ARRAY_LAYERS = 0xffffffff :: Word32

pattern VK_QUEUE_FAMILY_IGNORED = 0xffffffff :: Word32

pattern VK_MAX_MEMORY_HEAPS = 16
type VK_MAX_MEMORY_HEAPS = 16

pattern VK_FALSE = 0
type VK_FALSE = 0
-- ** VkPipelineCacheHeaderVersion

newtype VkPipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion Int32
  deriving (Eq, Storable)

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


pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE = VkPipelineCacheHeaderVersion 1


pattern VK_MAX_EXTENSION_NAME_SIZE = 256
type VK_MAX_EXTENSION_NAME_SIZE = 256

pattern VK_MAX_DESCRIPTION_SIZE = 256
type VK_MAX_DESCRIPTION_SIZE = 256

pattern VK_TRUE = 1
type VK_TRUE = 1
