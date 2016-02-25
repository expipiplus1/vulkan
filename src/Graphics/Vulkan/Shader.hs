{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Shader where
import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
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
import Graphics.Vulkan.Core( VkResult(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize
                      , CSize(..)
                      )

data VkShaderModuleCreateInfo =
  VkShaderModuleCreateInfo{ vkSType :: VkStructureType 
                          , vkPNext :: Ptr Void 
                          , vkFlags :: VkShaderModuleCreateFlags 
                          , vkCodeSize :: CSize 
                          , vkPCode :: Ptr Word32 
                          }
  deriving (Eq)

instance Storable VkShaderModuleCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkShaderModuleCreateInfo <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkCodeSize (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPCode (poked :: VkShaderModuleCreateInfo))


-- ** vkDestroyShaderModule
foreign import ccall "vkDestroyShaderModule" vkDestroyShaderModule :: 
  VkDevice -> VkShaderModule -> Ptr VkAllocationCallbacks -> IO ()

-- ** VkShaderModuleCreateFlags
-- | Opaque flag
newtype VkShaderModuleCreateFlags = VkShaderModuleCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkShaderStageFlags

newtype VkShaderStageFlagBits = VkShaderStageFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkShaderStageFlagBits
type VkShaderStageFlags = VkShaderStageFlagBits

pattern VK_SHADER_STAGE_VERTEX_BIT = VkShaderStageFlagBits 0x1

pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT = VkShaderStageFlagBits 0x2

pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = VkShaderStageFlagBits 0x4

pattern VK_SHADER_STAGE_GEOMETRY_BIT = VkShaderStageFlagBits 0x8

pattern VK_SHADER_STAGE_FRAGMENT_BIT = VkShaderStageFlagBits 0x10

pattern VK_SHADER_STAGE_COMPUTE_BIT = VkShaderStageFlagBits 0x20

pattern VK_SHADER_STAGE_ALL_GRAPHICS = VkShaderStageFlagBits 0x1f

pattern VK_SHADER_STAGE_ALL = VkShaderStageFlagBits 0x7fffffff

newtype VkShaderModule = VkShaderModule Word64
  deriving (Eq, Storable)

-- ** vkCreateShaderModule
foreign import ccall "vkCreateShaderModule" vkCreateShaderModule :: 
  VkDevice ->
  Ptr VkShaderModuleCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkShaderModule -> IO VkResult

