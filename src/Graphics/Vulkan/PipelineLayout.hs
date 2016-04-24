{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.PipelineLayout where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.DescriptorSet( VkDescriptorSetLayout(..)
                                    )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkInternalAllocationType(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkSystemAllocationScope(..)
                             , PFN_vkFreeFunction
                             , PFN_vkInternalFreeNotification
                             )
import Graphics.Vulkan.Shader( VkShaderStageFlagBits(..)
                             , VkShaderStageFlags(..)
                             )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

-- ** VkPipelineLayoutCreateFlags
-- | Opaque flag
newtype VkPipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

newtype VkPipelineLayout = VkPipelineLayout Word64
  deriving (Eq, Ord, Storable)

-- ** vkDestroyPipelineLayout
foreign import ccall "vkDestroyPipelineLayout" vkDestroyPipelineLayout ::
  VkDevice -> VkPipelineLayout -> Ptr VkAllocationCallbacks -> IO ()


data VkPushConstantRange =
  VkPushConstantRange{ vkStageFlags :: VkShaderStageFlags 
                     , vkOffset :: Word32 
                     , vkSize :: Word32 
                     }
  deriving (Eq, Ord)

instance Storable VkPushConstantRange where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkPushConstantRange <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
                                 <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkStageFlags (poked :: VkPushConstantRange))
                *> poke (ptr `plusPtr` 4) (vkOffset (poked :: VkPushConstantRange))
                *> poke (ptr `plusPtr` 8) (vkSize (poked :: VkPushConstantRange))



data VkPipelineLayoutCreateInfo =
  VkPipelineLayoutCreateInfo{ vkSType :: VkStructureType 
                            , vkPNext :: Ptr Void 
                            , vkFlags :: VkPipelineLayoutCreateFlags 
                            , vkSetLayoutCount :: Word32 
                            , vkPSetLayouts :: Ptr VkDescriptorSetLayout 
                            , vkPushConstantRangeCount :: Word32 
                            , vkPPushConstantRanges :: Ptr VkPushConstantRange 
                            }
  deriving (Eq, Ord)

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


-- ** vkCreatePipelineLayout
foreign import ccall "vkCreatePipelineLayout" vkCreatePipelineLayout ::
  VkDevice ->
  Ptr VkPipelineLayoutCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkPipelineLayout -> IO VkResult

