{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_shader_info
  ( VkShaderInfoTypeAMD(..)
  , pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD
  , pattern VK_SHADER_INFO_TYPE_BINARY_AMD
  , pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD
  , VkShaderResourceUsageAMD(..)
  , VkShaderStatisticsInfoAMD(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetShaderInfoAMD
#endif
  , FN_vkGetShaderInfoAMD
  , PFN_vkGetShaderInfoAMD
  , pattern VK_AMD_SHADER_INFO_EXTENSION_NAME
  , pattern VK_AMD_SHADER_INFO_SPEC_VERSION
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
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkShaderStageFlagBits(..)
  , VkPipeline
  )
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkShaderStageFlags
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkShaderInfoTypeAMD

-- No documentation found for TopLevel "VkShaderInfoTypeAMD"
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

-- No documentation found for Nested "VkShaderInfoTypeAMD" "VK_SHADER_INFO_TYPE_STATISTICS_AMD"
pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD :: VkShaderInfoTypeAMD
pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD = VkShaderInfoTypeAMD 0

-- No documentation found for Nested "VkShaderInfoTypeAMD" "VK_SHADER_INFO_TYPE_BINARY_AMD"
pattern VK_SHADER_INFO_TYPE_BINARY_AMD :: VkShaderInfoTypeAMD
pattern VK_SHADER_INFO_TYPE_BINARY_AMD = VkShaderInfoTypeAMD 1

-- No documentation found for Nested "VkShaderInfoTypeAMD" "VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD"
pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD :: VkShaderInfoTypeAMD
pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD = VkShaderInfoTypeAMD 2
-- No documentation found for TopLevel "VkShaderResourceUsageAMD"
data VkShaderResourceUsageAMD = VkShaderResourceUsageAMD
  { -- No documentation found for Nested "VkShaderResourceUsageAMD" "numUsedVgprs"
  vkNumUsedVgprs :: Word32
  , -- No documentation found for Nested "VkShaderResourceUsageAMD" "numUsedSgprs"
  vkNumUsedSgprs :: Word32
  , -- No documentation found for Nested "VkShaderResourceUsageAMD" "ldsSizePerLocalWorkGroup"
  vkLdsSizePerLocalWorkGroup :: Word32
  , -- No documentation found for Nested "VkShaderResourceUsageAMD" "ldsUsageSizeInBytes"
  vkLdsUsageSizeInBytes :: CSize
  , -- No documentation found for Nested "VkShaderResourceUsageAMD" "scratchMemUsageInBytes"
  vkScratchMemUsageInBytes :: CSize
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
-- No documentation found for TopLevel "VkShaderStatisticsInfoAMD"
data VkShaderStatisticsInfoAMD = VkShaderStatisticsInfoAMD
  { -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "shaderStageMask"
  vkShaderStageMask :: VkShaderStageFlags
  , -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "resourceUsage"
  vkResourceUsage :: VkShaderResourceUsageAMD
  , -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "numPhysicalVgprs"
  vkNumPhysicalVgprs :: Word32
  , -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "numPhysicalSgprs"
  vkNumPhysicalSgprs :: Word32
  , -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "numAvailableVgprs"
  vkNumAvailableVgprs :: Word32
  , -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "numAvailableSgprs"
  vkNumAvailableSgprs :: Word32
  , -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "computeWorkGroupSize"
  vkComputeWorkGroupSize :: Vector 3 Word32
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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetShaderInfoAMD"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetShaderInfoAMD" vkGetShaderInfoAMD :: ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult

#endif
type FN_vkGetShaderInfoAMD = ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult
type PFN_vkGetShaderInfoAMD = FunPtr FN_vkGetShaderInfoAMD
-- No documentation found for TopLevel "VK_AMD_SHADER_INFO_EXTENSION_NAME"
pattern VK_AMD_SHADER_INFO_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_SHADER_INFO_EXTENSION_NAME = "VK_AMD_shader_info"
-- No documentation found for TopLevel "VK_AMD_SHADER_INFO_SPEC_VERSION"
pattern VK_AMD_SHADER_INFO_SPEC_VERSION :: Integral a => a
pattern VK_AMD_SHADER_INFO_SPEC_VERSION = 1
