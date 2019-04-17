{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Shader
  ( ShaderModule
  , ShaderModuleCreateFlags
  , withCStructShaderModuleCreateInfo
  , fromCStructShaderModuleCreateInfo
  , ShaderModuleCreateInfo(..)
  , createShaderModule
  , destroyShaderModule
  , withShaderModule
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createShaderModule
  , destroyShaderModule
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Shader
  ( VkShaderModuleCreateFlags(..)
  , VkShaderModuleCreateInfo(..)
  , VkShaderModule
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "ShaderModule"
type ShaderModule = VkShaderModule
-- No documentation found for TopLevel "ShaderModuleCreateFlags"
type ShaderModuleCreateFlags = VkShaderModuleCreateFlags
-- No documentation found for TopLevel "ShaderModuleCreateInfo"
data ShaderModuleCreateInfo = ShaderModuleCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ShaderModuleCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ShaderModuleCreateInfo" "flags"
  vkFlags :: ShaderModuleCreateFlags
  -- Length multiple valued member elided
  , -- No documentation found for Nested "ShaderModuleCreateInfo" "pCode"
  vkPCode :: Vector Word32
  }
  deriving (Show, Eq)
withCStructShaderModuleCreateInfo :: ShaderModuleCreateInfo -> (VkShaderModuleCreateInfo -> IO a) -> IO a
withCStructShaderModuleCreateInfo from cont = withVec (&) (vkPCode (from :: ShaderModuleCreateInfo)) (\pCode -> maybeWith withSomeVkStruct (vkPNext (from :: ShaderModuleCreateInfo)) (\pPNext -> cont (VkShaderModuleCreateInfo VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO pPNext (vkFlags (from :: ShaderModuleCreateInfo)) (4 * fromIntegral (Data.Vector.length (vkPCode (from :: ShaderModuleCreateInfo)))) pCode)))
fromCStructShaderModuleCreateInfo :: VkShaderModuleCreateInfo -> IO ShaderModuleCreateInfo
fromCStructShaderModuleCreateInfo c = ShaderModuleCreateInfo <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkShaderModuleCreateInfo)))
                                                             <*> pure (vkFlags (c :: VkShaderModuleCreateInfo))
                                                             -- Length multiple valued member elided
                                                             <*> (Data.Vector.generateM (fromIntegral (vkCodeSize (c :: VkShaderModuleCreateInfo)) `quot` 4) (peekElemOff (vkPCode (c :: VkShaderModuleCreateInfo))))
instance Zero ShaderModuleCreateInfo where
  zero = ShaderModuleCreateInfo Nothing
                                zero
                                Data.Vector.empty

-- | Wrapper for 'vkCreateShaderModule'
createShaderModule :: Device ->  ShaderModuleCreateInfo ->  Maybe AllocationCallbacks ->  IO (ShaderModule)
createShaderModule = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pShaderModule -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructShaderModuleCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createShaderModule commandTable device pCreateInfo pAllocator pShaderModule >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pShaderModule)))))

-- | Wrapper for 'vkDestroyShaderModule'
destroyShaderModule :: Device ->  ShaderModule ->  Maybe AllocationCallbacks ->  IO ()
destroyShaderModule = \(Device device commandTable) -> \shaderModule -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyShaderModule commandTable device shaderModule pAllocator *> (pure ()))
-- | Wrapper for 'createShaderModule' and 'destroyShaderModule' using 'bracket'
withShaderModule
  :: Device -> ShaderModuleCreateInfo -> Maybe (AllocationCallbacks) -> (ShaderModule -> IO a) -> IO a
withShaderModule device shaderModuleCreateInfo allocationCallbacks = bracket
  (createShaderModule device shaderModuleCreateInfo allocationCallbacks)
  (\o -> destroyShaderModule device o allocationCallbacks)
