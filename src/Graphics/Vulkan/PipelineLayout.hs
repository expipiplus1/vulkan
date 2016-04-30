{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.PipelineLayout where

import Graphics.Vulkan.Device( Device(..)
                             )
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Graphics.Vulkan.DescriptorSet( DescriptorSetLayout(..)
                                    )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Graphics.Vulkan.Shader( ShaderStageFlags(..)
                             )
import Graphics.Vulkan.Core( StructureType(..)
                           , Result(..)
                           , Flags(..)
                           )

-- ** PipelineLayoutCreateFlags
-- | Opaque flag
newtype PipelineLayoutCreateFlags = PipelineLayoutCreateFlags Flags
  deriving (Eq, Ord, Storable)

newtype PipelineLayout = PipelineLayout Word64
  deriving (Eq, Ord, Storable)

-- ** destroyPipelineLayout
foreign import ccall "vkDestroyPipelineLayout" destroyPipelineLayout ::
  Device -> PipelineLayout -> Ptr AllocationCallbacks -> IO ()


data PushConstantRange =
  PushConstantRange{ stageFlags :: ShaderStageFlags 
                   , offset :: Word32 
                   , size :: Word32 
                   }
  deriving (Eq, Ord)

instance Storable PushConstantRange where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = PushConstantRange <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 4)
                               <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (stageFlags (poked :: PushConstantRange))
                *> poke (ptr `plusPtr` 4) (offset (poked :: PushConstantRange))
                *> poke (ptr `plusPtr` 8) (size (poked :: PushConstantRange))



data PipelineLayoutCreateInfo =
  PipelineLayoutCreateInfo{ sType :: StructureType 
                          , pNext :: Ptr Void 
                          , flags :: PipelineLayoutCreateFlags 
                          , setLayoutCount :: Word32 
                          , pSetLayouts :: Ptr DescriptorSetLayout 
                          , pushConstantRangeCount :: Word32 
                          , pPushConstantRanges :: Ptr PushConstantRange 
                          }
  deriving (Eq, Ord)

instance Storable PipelineLayoutCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = PipelineLayoutCreateInfo <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 20)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: PipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 20) (setLayoutCount (poked :: PipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 24) (pSetLayouts (poked :: PipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 32) (pushConstantRangeCount (poked :: PipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 40) (pPushConstantRanges (poked :: PipelineLayoutCreateInfo))


-- ** createPipelineLayout
foreign import ccall "vkCreatePipelineLayout" createPipelineLayout ::
  Device ->
  Ptr PipelineLayoutCreateInfo ->
    Ptr AllocationCallbacks -> Ptr PipelineLayout -> IO Result

