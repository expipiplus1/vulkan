{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkDescriptorSetLayout
  , VkPipelineLayoutCreateFlags(..)
  , VkPipelineLayoutCreateInfo(..)
  , VkPushConstantRange(..)
  , VkShaderStageFlags
  , FN_vkCreatePipelineLayout
  , PFN_vkCreatePipelineLayout
  , vkCreatePipelineLayout
  , FN_vkDestroyPipelineLayout
  , PFN_vkDestroyPipelineLayout
  , vkDestroyPipelineLayout
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
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
  , pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkShaderStageFlagBits(..)
  , VkPipelineLayout
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkDescriptorSetLayout_T
-- No documentation found for TopLevel "VkDescriptorSetLayout"
type VkDescriptorSetLayout = Ptr VkDescriptorSetLayout_T

-- ** VkPipelineLayoutCreateFlags

-- No documentation found for TopLevel "VkPipelineLayoutCreateFlags"
newtype VkPipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkPipelineLayoutCreateFlags where
  
  showsPrec p (VkPipelineLayoutCreateFlags x) = showParen (p >= 11) (showString "VkPipelineLayoutCreateFlags " . showsPrec 11 x)

instance Read VkPipelineLayoutCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineLayoutCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineLayoutCreateFlags v)
                        )
                    )



-- No documentation found for TopLevel "VkPipelineLayoutCreateInfo"
data VkPipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo
  { -- No documentation found for Nested "VkPipelineLayoutCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineLayoutCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineLayoutCreateInfo" "flags"
  vkFlags :: VkPipelineLayoutCreateFlags
  , -- No documentation found for Nested "VkPipelineLayoutCreateInfo" "setLayoutCount"
  vkSetLayoutCount :: Word32
  , -- No documentation found for Nested "VkPipelineLayoutCreateInfo" "pSetLayouts"
  vkPSetLayouts :: Ptr VkDescriptorSetLayout
  , -- No documentation found for Nested "VkPipelineLayoutCreateInfo" "pushConstantRangeCount"
  vkPushConstantRangeCount :: Word32
  , -- No documentation found for Nested "VkPipelineLayoutCreateInfo" "pPushConstantRanges"
  vkPPushConstantRanges :: Ptr VkPushConstantRange
  }
  deriving (Eq, Show)

instance Storable VkPipelineLayoutCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPipelineLayoutCreateInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
                                        <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkSetLayoutCount (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPSetLayouts (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPushConstantRangeCount (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPPushConstantRanges (poked :: VkPipelineLayoutCreateInfo))

instance Zero VkPipelineLayoutCreateInfo where
  zero = VkPipelineLayoutCreateInfo VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero

-- No documentation found for TopLevel "VkPushConstantRange"
data VkPushConstantRange = VkPushConstantRange
  { -- No documentation found for Nested "VkPushConstantRange" "stageFlags"
  vkStageFlags :: VkShaderStageFlags
  , -- No documentation found for Nested "VkPushConstantRange" "offset"
  vkOffset :: Word32
  , -- No documentation found for Nested "VkPushConstantRange" "size"
  vkSize :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPushConstantRange where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkPushConstantRange <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
                                 <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkStageFlags (poked :: VkPushConstantRange))
                *> poke (ptr `plusPtr` 4) (vkOffset (poked :: VkPushConstantRange))
                *> poke (ptr `plusPtr` 8) (vkSize (poked :: VkPushConstantRange))

instance Zero VkPushConstantRange where
  zero = VkPushConstantRange zero
                             zero
                             zero

-- No documentation found for TopLevel "VkShaderStageFlags"
type VkShaderStageFlags = VkShaderStageFlagBits

-- No documentation found for TopLevel "vkCreatePipelineLayout"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreatePipelineLayout" vkCreatePipelineLayout :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult
#else
vkCreatePipelineLayout :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult
vkCreatePipelineLayout deviceCmds = mkVkCreatePipelineLayout (pVkCreatePipelineLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePipelineLayout
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult)
#endif

type FN_vkCreatePipelineLayout = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult
type PFN_vkCreatePipelineLayout = FunPtr FN_vkCreatePipelineLayout

-- No documentation found for TopLevel "vkDestroyPipelineLayout"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyPipelineLayout" vkDestroyPipelineLayout :: ("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyPipelineLayout :: DeviceCmds -> ("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyPipelineLayout deviceCmds = mkVkDestroyPipelineLayout (pVkDestroyPipelineLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipelineLayout
  :: FunPtr (("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyPipelineLayout = ("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyPipelineLayout = FunPtr FN_vkDestroyPipelineLayout
