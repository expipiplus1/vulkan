{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCache
  , VkPipelineCacheCreateFlags(..)
  , VkPipelineCacheCreateInfo(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreatePipelineCache
#endif
  , FN_vkCreatePipelineCache
  , PFN_vkCreatePipelineCache
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroyPipelineCache
#endif
  , FN_vkDestroyPipelineCache
  , PFN_vkDestroyPipelineCache
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkGetPipelineCacheData
#endif
  , FN_vkGetPipelineCacheData
  , PFN_vkGetPipelineCacheData
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkMergePipelineCaches
#endif
  , FN_vkMergePipelineCaches
  , PFN_vkMergePipelineCaches
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
  ( FunPtr
  , Ptr
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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkPipelineCache_T
-- No documentation found for TopLevel "VkPipelineCache"
type VkPipelineCache = Ptr VkPipelineCache_T
-- ** VkPipelineCacheCreateFlags

-- No documentation found for TopLevel "VkPipelineCacheCreateFlags"
newtype VkPipelineCacheCreateFlags = VkPipelineCacheCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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


-- No documentation found for TopLevel "VkPipelineCacheCreateInfo"
data VkPipelineCacheCreateInfo = VkPipelineCacheCreateInfo
  { -- No documentation found for Nested "VkPipelineCacheCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineCacheCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineCacheCreateInfo" "flags"
  vkFlags :: VkPipelineCacheCreateFlags
  , -- No documentation found for Nested "VkPipelineCacheCreateInfo" "initialDataSize"
  vkInitialDataSize :: CSize
  , -- No documentation found for Nested "VkPipelineCacheCreateInfo" "pInitialData"
  vkPInitialData :: Ptr ()
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkInitialDataSize (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPInitialData (poked :: VkPipelineCacheCreateInfo))

instance Zero VkPipelineCacheCreateInfo where
  zero = VkPipelineCacheCreateInfo zero
                                   zero
                                   zero
                                   zero
                                   zero
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCreatePipelineCache"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreatePipelineCache" vkCreatePipelineCache :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult

#endif
type FN_vkCreatePipelineCache = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult
type PFN_vkCreatePipelineCache = FunPtr FN_vkCreatePipelineCache
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkDestroyPipelineCache"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyPipelineCache" vkDestroyPipelineCache :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyPipelineCache = ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyPipelineCache = FunPtr FN_vkDestroyPipelineCache
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkGetPipelineCacheData"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPipelineCacheData" vkGetPipelineCacheData :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult

#endif
type FN_vkGetPipelineCacheData = ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
type PFN_vkGetPipelineCacheData = FunPtr FN_vkGetPipelineCacheData
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkMergePipelineCaches"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkMergePipelineCaches" vkMergePipelineCaches :: ("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult

#endif
type FN_vkMergePipelineCaches = ("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult
type PFN_vkMergePipelineCaches = FunPtr FN_vkMergePipelineCaches
