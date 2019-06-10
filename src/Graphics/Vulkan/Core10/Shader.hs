{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Shader
  ( ShaderModule
  , ShaderModuleCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , ShaderModuleCreateInfo(..)
#endif
  , createShaderModule
  , destroyShaderModule
  , withShaderModule
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.ByteString
  ( ByteString
  )
#endif
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core10.Shader
  ( VkShaderModuleCreateFlags(..)
  , VkShaderModule
  , vkCreateShaderModule
  , vkDestroyShaderModule
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "ShaderModule"
type ShaderModule = VkShaderModule

-- No documentation found for TopLevel "ShaderModuleCreateFlags"
type ShaderModuleCreateFlags = VkShaderModuleCreateFlags


-- No complete pragma for ShaderModuleCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkShaderModuleCreateInfo"
data ShaderModuleCreateInfo = ShaderModuleCreateInfo
  { -- No documentation found for Nested "ShaderModuleCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ShaderModuleCreateInfo" "flags"
  flags :: ShaderModuleCreateFlags
  , -- No documentation found for Nested "ShaderModuleCreateInfo" "pCode"
  code :: ByteString
  }
  deriving (Show, Eq)

instance Zero ShaderModuleCreateInfo where
  zero = ShaderModuleCreateInfo Nothing
                                zero
                                mempty

#endif


-- No documentation found for TopLevel "vkCreateShaderModule"
createShaderModule :: Device ->  ShaderModuleCreateInfo ->  Maybe AllocationCallbacks ->  IO (ShaderModule)
createShaderModule = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyShaderModule"
destroyShaderModule :: Device ->  ShaderModule ->  Maybe AllocationCallbacks ->  IO ()
destroyShaderModule = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createShaderModule' and 'destroyShaderModule' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withShaderModule
  :: Device -> ShaderModuleCreateInfo -> Maybe AllocationCallbacks -> (ShaderModule -> IO a) -> IO a
withShaderModule device shaderModuleCreateInfo allocationCallbacks = bracket
  (createShaderModule device shaderModuleCreateInfo allocationCallbacks)
  (\o -> destroyShaderModule device o allocationCallbacks)
