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
  , FN_vkCreatePipelineCache
  , PFN_vkCreatePipelineCache
  , vkCreatePipelineCache
  , FN_vkDestroyPipelineCache
  , PFN_vkDestroyPipelineCache
  , vkDestroyPipelineCache
  , FN_vkGetPipelineCacheData
  , PFN_vkGetPipelineCacheData
  , vkGetPipelineCacheData
  , FN_vkMergePipelineCaches
  , PFN_vkMergePipelineCaches
  , vkMergePipelineCaches
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
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
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
  zero = VkPipelineCacheCreateInfo VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
                                   zero
                                   zero
                                   zero
                                   zero

-- No documentation found for TopLevel "vkCreatePipelineCache"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreatePipelineCache" vkCreatePipelineCache :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult
#else
vkCreatePipelineCache :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult
vkCreatePipelineCache deviceCmds = mkVkCreatePipelineCache (pVkCreatePipelineCache deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePipelineCache
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult)
#endif

type FN_vkCreatePipelineCache = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult
type PFN_vkCreatePipelineCache = FunPtr FN_vkCreatePipelineCache

-- No documentation found for TopLevel "vkDestroyPipelineCache"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyPipelineCache" vkDestroyPipelineCache :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyPipelineCache :: DeviceCmds -> ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyPipelineCache deviceCmds = mkVkDestroyPipelineCache (pVkDestroyPipelineCache deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipelineCache
  :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyPipelineCache = ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyPipelineCache = FunPtr FN_vkDestroyPipelineCache

-- No documentation found for TopLevel "vkGetPipelineCacheData"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPipelineCacheData" vkGetPipelineCacheData :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
#else
vkGetPipelineCacheData :: DeviceCmds -> ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
vkGetPipelineCacheData deviceCmds = mkVkGetPipelineCacheData (pVkGetPipelineCacheData deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineCacheData
  :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
#endif

type FN_vkGetPipelineCacheData = ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
type PFN_vkGetPipelineCacheData = FunPtr FN_vkGetPipelineCacheData

-- No documentation found for TopLevel "vkMergePipelineCaches"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkMergePipelineCaches" vkMergePipelineCaches :: ("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult
#else
vkMergePipelineCaches :: DeviceCmds -> ("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult
vkMergePipelineCaches deviceCmds = mkVkMergePipelineCaches (pVkMergePipelineCaches deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMergePipelineCaches
  :: FunPtr (("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult) -> (("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult)
#endif

type FN_vkMergePipelineCaches = ("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult
type PFN_vkMergePipelineCaches = FunPtr FN_vkMergePipelineCaches
