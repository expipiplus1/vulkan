{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  ( VkValidationCacheHeaderVersionEXT(..)
  , pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT
  , VkValidationCacheCreateFlagsEXT(..)
  , pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT
  , pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION
  , pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT
  , VkValidationCacheEXT
  , vkCreateValidationCacheEXT
  , vkDestroyValidationCacheEXT
  , vkGetValidationCacheDataEXT
  , vkMergeValidationCachesEXT
  , VkValidationCacheCreateInfoEXT(..)
  , VkShaderModuleValidationCacheCreateInfoEXT(..)
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
  ( VkResult(..)
  , VkObjectType(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( VkDebugReportObjectTypeEXT(..)
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT
  )


-- ** VkValidationCacheHeaderVersionEXT

-- | 
newtype VkValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT Int32
  deriving (Eq, Ord, Storable)

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

-- | 
pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT :: VkValidationCacheHeaderVersionEXT
pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT = VkValidationCacheHeaderVersionEXT 1
-- ** VkValidationCacheCreateFlagsEXT

-- | 
newtype VkValidationCacheCreateFlagsEXT = VkValidationCacheCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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


-- | Nothing
pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT = VkStructureType 1000160000
-- | Nothing
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT = VkStructureType 1000160001
-- | Just "VkValidationCacheEXT"
pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT :: VkObjectType
pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT = VkObjectType 1000160000
pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION :: Integral a => a
pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION = 1
pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME = "VK_EXT_validation_cache"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT
-- |
data VkValidationCacheEXT_T
type VkValidationCacheEXT = Ptr VkValidationCacheEXT_T
-- | 
foreign import ccall "vkCreateValidationCacheEXT" vkCreateValidationCacheEXT :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pValidationCache" ::: Ptr VkValidationCacheEXT) -> IO VkResult
-- | 
foreign import ccall "vkDestroyValidationCacheEXT" vkDestroyValidationCacheEXT :: ("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | 
foreign import ccall "vkGetValidationCacheDataEXT" vkGetValidationCacheDataEXT :: ("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
-- | 
foreign import ccall "vkMergeValidationCachesEXT" vkMergeValidationCachesEXT :: ("device" ::: VkDevice) -> ("dstCache" ::: VkValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkValidationCacheEXT) -> IO VkResult
-- | TODO: Struct comments
data VkValidationCacheCreateInfoEXT = VkValidationCacheCreateInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkValidationCacheCreateFlagsEXT
  , vkInitialDataSize :: CSize
  , vkInitialData :: Ptr ()
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkInitialDataSize (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkInitialData (poked :: VkValidationCacheCreateInfoEXT))
-- | TODO: Struct comments
data VkShaderModuleValidationCacheCreateInfoEXT = VkShaderModuleValidationCacheCreateInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkValidationCache :: VkValidationCacheEXT
  }
  deriving (Eq, Show)

instance Storable VkShaderModuleValidationCacheCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkShaderModuleValidationCacheCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkShaderModuleValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkShaderModuleValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkValidationCache (poked :: VkShaderModuleValidationCacheCreateInfoEXT))
