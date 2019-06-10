{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkShaderModuleValidationCacheCreateInfoEXT(..)
  , VkValidationCacheCreateFlagsEXT(..)
  , VkValidationCacheCreateInfoEXT(..)
  , VkValidationCacheEXT
  , VkValidationCacheHeaderVersionEXT(..)
  , pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT
  , FN_vkCreateValidationCacheEXT
  , PFN_vkCreateValidationCacheEXT
  , vkCreateValidationCacheEXT
  , FN_vkDestroyValidationCacheEXT
  , PFN_vkDestroyValidationCacheEXT
  , vkDestroyValidationCacheEXT
  , FN_vkGetValidationCacheDataEXT
  , PFN_vkGetValidationCacheDataEXT
  , vkGetValidationCacheDataEXT
  , FN_vkMergeValidationCachesEXT
  , PFN_vkMergeValidationCachesEXT
  , vkMergeValidationCachesEXT
  , pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME
  , pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
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
  ( VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
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


-- No documentation found for TopLevel "VkShaderModuleValidationCacheCreateInfoEXT"
data VkShaderModuleValidationCacheCreateInfoEXT = VkShaderModuleValidationCacheCreateInfoEXT
  { -- No documentation found for Nested "VkShaderModuleValidationCacheCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkShaderModuleValidationCacheCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkShaderModuleValidationCacheCreateInfoEXT" "validationCache"
  vkValidationCache :: VkValidationCacheEXT
  }
  deriving (Eq, Show)

instance Storable VkShaderModuleValidationCacheCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkShaderModuleValidationCacheCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkShaderModuleValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkShaderModuleValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkValidationCache (poked :: VkShaderModuleValidationCacheCreateInfoEXT))

instance Zero VkShaderModuleValidationCacheCreateInfoEXT where
  zero = VkShaderModuleValidationCacheCreateInfoEXT VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
                                                    zero
                                                    zero

-- ** VkValidationCacheCreateFlagsEXT

-- No documentation found for TopLevel "VkValidationCacheCreateFlagsEXT"
newtype VkValidationCacheCreateFlagsEXT = VkValidationCacheCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkValidationCacheCreateFlagsEXT where
  
  showsPrec p (VkValidationCacheCreateFlagsEXT x) = showParen (p >= 11) (showString "VkValidationCacheCreateFlagsEXT " . showsPrec 11 x)

instance Read VkValidationCacheCreateFlagsEXT where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkValidationCacheCreateFlagsEXT")
                        v <- step readPrec
                        pure (VkValidationCacheCreateFlagsEXT v)
                        )
                    )



-- No documentation found for TopLevel "VkValidationCacheCreateInfoEXT"
data VkValidationCacheCreateInfoEXT = VkValidationCacheCreateInfoEXT
  { -- No documentation found for Nested "VkValidationCacheCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkValidationCacheCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkValidationCacheCreateInfoEXT" "flags"
  vkFlags :: VkValidationCacheCreateFlagsEXT
  , -- No documentation found for Nested "VkValidationCacheCreateInfoEXT" "initialDataSize"
  vkInitialDataSize :: CSize
  , -- No documentation found for Nested "VkValidationCacheCreateInfoEXT" "pInitialData"
  vkPInitialData :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkValidationCacheCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkValidationCacheCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 24)
                                            <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkInitialDataSize (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPInitialData (poked :: VkValidationCacheCreateInfoEXT))

instance Zero VkValidationCacheCreateInfoEXT where
  zero = VkValidationCacheCreateInfoEXT VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
                                        zero
                                        zero
                                        zero
                                        zero

-- | Dummy data to tag the 'Ptr' with
data VkValidationCacheEXT_T
-- No documentation found for TopLevel "VkValidationCacheEXT"
type VkValidationCacheEXT = Ptr VkValidationCacheEXT_T

-- ** VkValidationCacheHeaderVersionEXT

-- No documentation found for TopLevel "VkValidationCacheHeaderVersionEXT"
newtype VkValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkValidationCacheHeaderVersionEXT where
  showsPrec _ VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT = showString "VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT"
  showsPrec p (VkValidationCacheHeaderVersionEXT x) = showParen (p >= 11) (showString "VkValidationCacheHeaderVersionEXT " . showsPrec 11 x)

instance Read VkValidationCacheHeaderVersionEXT where
  readPrec = parens ( choose [ ("VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT", pure VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkValidationCacheHeaderVersionEXT")
                        v <- step readPrec
                        pure (VkValidationCacheHeaderVersionEXT v)
                        )
                    )

-- No documentation found for Nested "VkValidationCacheHeaderVersionEXT" "VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT"
pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT :: VkValidationCacheHeaderVersionEXT
pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT = VkValidationCacheHeaderVersionEXT 1

-- No documentation found for TopLevel "vkCreateValidationCacheEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateValidationCacheEXT" vkCreateValidationCacheEXT :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pValidationCache" ::: Ptr VkValidationCacheEXT) -> IO VkResult
#else
vkCreateValidationCacheEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pValidationCache" ::: Ptr VkValidationCacheEXT) -> IO VkResult
vkCreateValidationCacheEXT deviceCmds = mkVkCreateValidationCacheEXT (pVkCreateValidationCacheEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateValidationCacheEXT
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pValidationCache" ::: Ptr VkValidationCacheEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pValidationCache" ::: Ptr VkValidationCacheEXT) -> IO VkResult)
#endif

type FN_vkCreateValidationCacheEXT = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pValidationCache" ::: Ptr VkValidationCacheEXT) -> IO VkResult
type PFN_vkCreateValidationCacheEXT = FunPtr FN_vkCreateValidationCacheEXT

-- No documentation found for TopLevel "vkDestroyValidationCacheEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyValidationCacheEXT" vkDestroyValidationCacheEXT :: ("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyValidationCacheEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyValidationCacheEXT deviceCmds = mkVkDestroyValidationCacheEXT (pVkDestroyValidationCacheEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyValidationCacheEXT
  :: FunPtr (("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyValidationCacheEXT = ("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyValidationCacheEXT = FunPtr FN_vkDestroyValidationCacheEXT

-- No documentation found for TopLevel "vkGetValidationCacheDataEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetValidationCacheDataEXT" vkGetValidationCacheDataEXT :: ("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
#else
vkGetValidationCacheDataEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
vkGetValidationCacheDataEXT deviceCmds = mkVkGetValidationCacheDataEXT (pVkGetValidationCacheDataEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetValidationCacheDataEXT
  :: FunPtr (("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult) -> (("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
#endif

type FN_vkGetValidationCacheDataEXT = ("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
type PFN_vkGetValidationCacheDataEXT = FunPtr FN_vkGetValidationCacheDataEXT

-- No documentation found for TopLevel "vkMergeValidationCachesEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkMergeValidationCachesEXT" vkMergeValidationCachesEXT :: ("device" ::: VkDevice) -> ("dstCache" ::: VkValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkValidationCacheEXT) -> IO VkResult
#else
vkMergeValidationCachesEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("dstCache" ::: VkValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkValidationCacheEXT) -> IO VkResult
vkMergeValidationCachesEXT deviceCmds = mkVkMergeValidationCachesEXT (pVkMergeValidationCachesEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMergeValidationCachesEXT
  :: FunPtr (("device" ::: VkDevice) -> ("dstCache" ::: VkValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkValidationCacheEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("dstCache" ::: VkValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkValidationCacheEXT) -> IO VkResult)
#endif

type FN_vkMergeValidationCachesEXT = ("device" ::: VkDevice) -> ("dstCache" ::: VkValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkValidationCacheEXT) -> IO VkResult
type PFN_vkMergeValidationCachesEXT = FunPtr FN_vkMergeValidationCachesEXT

-- No documentation found for TopLevel "VK_EXT_VALIDATION_CACHE_EXTENSION_NAME"
pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME = "VK_EXT_validation_cache"

-- No documentation found for TopLevel "VK_EXT_VALIDATION_CACHE_SPEC_VERSION"
pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION :: Integral a => a
pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION = 1

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_VALIDATION_CACHE_EXT"
pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT :: VkObjectType
pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT = VkObjectType 1000160000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT = VkStructureType 1000160001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT = VkStructureType 1000160000
