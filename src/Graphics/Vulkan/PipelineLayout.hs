{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.PipelineLayout where

import Graphics.Vulkan.Device( Device(..)
                             )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.DescriptorSet( DescriptorSetLayout(..)
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
  deriving (Eq, Storable)

newtype PipelineLayout = PipelineLayout Word64
  deriving (Eq, Storable)

-- ** vkDestroyPipelineLayout
foreign import ccall "vkDestroyPipelineLayout" vkDestroyPipelineLayout ::
  Device -> PipelineLayout -> Ptr VkAllocationCallbacks -> IO ()


data VkPushConstantRange =
  VkPushConstantRange{ stageFlags :: VkShaderStageFlags 
                     , offset :: Word32 
                     , size :: Word32 
                     }
  deriving (Eq)

instance Storable VkPushConstantRange where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkPushConstantRange <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
                                 <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (stageFlags (poked :: VkPushConstantRange))
                *> poke (ptr `plusPtr` 4) (offset (poked :: VkPushConstantRange))
                *> poke (ptr `plusPtr` 8) (size (poked :: VkPushConstantRange))



data VkPipelineLayoutCreateInfo =
  VkPipelineLayoutCreateInfo{ sType :: VkStructureType 
                            , pNext :: Ptr Void 
                            , flags :: VkPipelineLayoutCreateFlags 
                            , setLayoutCount :: Word32 
                            , pSetLayouts :: Ptr DescriptorSetLayout 
                            , pushConstantRangeCount :: Word32 
                            , pPushConstantRanges :: Ptr VkPushConstantRange 
                            }
  deriving (Eq)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 20) (setLayoutCount (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 24) (pSetLayouts (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 32) (pushConstantRangeCount (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 40) (pPushConstantRanges (poked :: VkPipelineLayoutCreateInfo))


-- ** vkCreatePipelineLayout
foreign import ccall "vkCreatePipelineLayout" vkCreatePipelineLayout ::
  Device ->
  Ptr VkPipelineLayoutCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr PipelineLayout -> IO VkResult

