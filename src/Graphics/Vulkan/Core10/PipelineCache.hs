{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.PipelineCache
  ( VkPipelineCacheCreateFlags(..)
  , VkPipelineCache
  , vkCreatePipelineCache
  , vkDestroyPipelineCache
  , vkGetPipelineCacheData
  , vkMergePipelineCaches
  , VkPipelineCacheCreateInfo(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
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
import Graphics.Vulkan.NamedType
  ( (:::)
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
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )


-- ** VkPipelineCacheCreateFlags

-- | 
newtype VkPipelineCacheCreateFlags = VkPipelineCacheCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineCacheCreateFlags where
  
  showsPrec p (VkPipelineCacheCreateFlags x) = showParen (p >= 11) (showString "VkPipelineCacheCreateFlags " . showsPrec 11 x)

instance Read VkPipelineCacheCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineCacheCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineCacheCreateFlags v)
                        )
                    )


-- |
data VkPipelineCache_T
type VkPipelineCache = Ptr VkPipelineCache_T
-- | 
foreign import ccall "vkCreatePipelineCache" vkCreatePipelineCache :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult
-- | 
foreign import ccall "vkDestroyPipelineCache" vkDestroyPipelineCache :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | 
foreign import ccall "vkGetPipelineCacheData" vkGetPipelineCacheData :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
-- | 
foreign import ccall "vkMergePipelineCaches" vkMergePipelineCaches :: ("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult
-- | TODO: Struct comments
data VkPipelineCacheCreateInfo = VkPipelineCacheCreateInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkPipelineCacheCreateFlags
  , vkInitialDataSize :: CSize
  , vkInitialData :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkPipelineCacheCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPipelineCacheCreateInfo <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkInitialDataSize (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkInitialData (poked :: VkPipelineCacheCreateInfo))
