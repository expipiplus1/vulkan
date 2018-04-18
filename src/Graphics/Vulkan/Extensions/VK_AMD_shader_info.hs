{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_AMD_shader_info
  ( VkShaderInfoTypeAMD(..)
  , pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD
  , pattern VK_SHADER_INFO_TYPE_BINARY_AMD
  , pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD
  , pattern VK_AMD_SHADER_INFO_SPEC_VERSION
  , pattern VK_AMD_SHADER_INFO_EXTENSION_NAME
  , vkGetShaderInfoAMD
  , VkShaderResourceUsageAMD(..)
  , VkShaderStatisticsInfoAMD(..)
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
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
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkShaderStageFlagBits(..)
  , VkPipeline
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkShaderStageFlags
  )


-- ** VkShaderInfoTypeAMD

-- | 
newtype VkShaderInfoTypeAMD = VkShaderInfoTypeAMD Int32
  deriving (Eq, Ord, Storable)

instance Show VkShaderInfoTypeAMD where
  showsPrec _ VK_SHADER_INFO_TYPE_STATISTICS_AMD = showString "VK_SHADER_INFO_TYPE_STATISTICS_AMD"
  showsPrec _ VK_SHADER_INFO_TYPE_BINARY_AMD = showString "VK_SHADER_INFO_TYPE_BINARY_AMD"
  showsPrec _ VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD = showString "VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD"
  showsPrec p (VkShaderInfoTypeAMD x) = showParen (p >= 11) (showString "VkShaderInfoTypeAMD " . showsPrec 11 x)

instance Read VkShaderInfoTypeAMD where
  readPrec = parens ( choose [ ("VK_SHADER_INFO_TYPE_STATISTICS_AMD",  pure VK_SHADER_INFO_TYPE_STATISTICS_AMD)
                             , ("VK_SHADER_INFO_TYPE_BINARY_AMD",      pure VK_SHADER_INFO_TYPE_BINARY_AMD)
                             , ("VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD", pure VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkShaderInfoTypeAMD")
                        v <- step readPrec
                        pure (VkShaderInfoTypeAMD v)
                        )
                    )

-- | 
pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD :: VkShaderInfoTypeAMD
pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD = VkShaderInfoTypeAMD 0

-- | 
pattern VK_SHADER_INFO_TYPE_BINARY_AMD :: VkShaderInfoTypeAMD
pattern VK_SHADER_INFO_TYPE_BINARY_AMD = VkShaderInfoTypeAMD 1

-- | 
pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD :: VkShaderInfoTypeAMD
pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD = VkShaderInfoTypeAMD 2
pattern VK_AMD_SHADER_INFO_SPEC_VERSION :: Integral a => a
pattern VK_AMD_SHADER_INFO_SPEC_VERSION = 1
pattern VK_AMD_SHADER_INFO_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_SHADER_INFO_EXTENSION_NAME = "VK_AMD_shader_info"
-- | 
foreign import ccall "vkGetShaderInfoAMD" vkGetShaderInfoAMD :: ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult
-- | TODO: Struct comments
data VkShaderResourceUsageAMD = VkShaderResourceUsageAMD
  { vkNumUsedVgprs :: Word32
  , vkNumUsedSgprs :: Word32
  , vkLdsSizePerLocalWorkGroup :: Word32
  , vkLdsUsageSizeInBytes :: CSize
  , vkScratchMemUsageInBytes :: CSize
  }
  deriving (Eq, Show)

instance Storable VkShaderResourceUsageAMD where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkShaderResourceUsageAMD <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkNumUsedVgprs (poked :: VkShaderResourceUsageAMD))
                *> poke (ptr `plusPtr` 4) (vkNumUsedSgprs (poked :: VkShaderResourceUsageAMD))
                *> poke (ptr `plusPtr` 8) (vkLdsSizePerLocalWorkGroup (poked :: VkShaderResourceUsageAMD))
                *> poke (ptr `plusPtr` 16) (vkLdsUsageSizeInBytes (poked :: VkShaderResourceUsageAMD))
                *> poke (ptr `plusPtr` 24) (vkScratchMemUsageInBytes (poked :: VkShaderResourceUsageAMD))
-- | TODO: Struct comments
data VkShaderStatisticsInfoAMD = VkShaderStatisticsInfoAMD
  { vkShaderStageMask :: VkShaderStageFlags
  , vkResourceUsage :: VkShaderResourceUsageAMD
  , vkNumPhysicalVgprs :: Word32
  , vkNumPhysicalSgprs :: Word32
  , vkNumAvailableVgprs :: Word32
  , vkNumAvailableSgprs :: Word32
  , vkComputeWorkGroupSize :: Vector 3 Word32
  }
  deriving (Eq, Show)

instance Storable VkShaderStatisticsInfoAMD where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkShaderStatisticsInfoAMD <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 40)
                                       <*> peek (ptr `plusPtr` 44)
                                       <*> peek (ptr `plusPtr` 48)
                                       <*> peek (ptr `plusPtr` 52)
                                       <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkShaderStageMask (poked :: VkShaderStatisticsInfoAMD))
                *> poke (ptr `plusPtr` 8) (vkResourceUsage (poked :: VkShaderStatisticsInfoAMD))
                *> poke (ptr `plusPtr` 40) (vkNumPhysicalVgprs (poked :: VkShaderStatisticsInfoAMD))
                *> poke (ptr `plusPtr` 44) (vkNumPhysicalSgprs (poked :: VkShaderStatisticsInfoAMD))
                *> poke (ptr `plusPtr` 48) (vkNumAvailableVgprs (poked :: VkShaderStatisticsInfoAMD))
                *> poke (ptr `plusPtr` 52) (vkNumAvailableSgprs (poked :: VkShaderStatisticsInfoAMD))
                *> poke (ptr `plusPtr` 56) (vkComputeWorkGroupSize (poked :: VkShaderStatisticsInfoAMD))
