{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Sampler
  ( BorderColor
  , Filter
  , Sampler
  , SamplerAddressMode
  , SamplerCreateFlagBits
  , SamplerCreateFlags
  , withCStructSamplerCreateInfo
  , fromCStructSamplerCreateInfo
  , SamplerCreateInfo(..)
  , SamplerMipmapMode
  , createSampler
  , destroySampler
  , withSampler
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Foreign.C.Types
  ( CFloat(..)
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
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createSampler
  , destroySampler
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkBorderColor(..)
  , VkFilter(..)
  , VkSamplerAddressMode(..)
  , VkSamplerCreateFlagBits(..)
  , VkSamplerCreateInfo(..)
  , VkSamplerMipmapMode(..)
  , VkSampler
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Pipeline
  ( CompareOp
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "BorderColor"
type BorderColor = VkBorderColor
-- No documentation found for TopLevel "Filter"
type Filter = VkFilter
-- No documentation found for TopLevel "Sampler"
type Sampler = VkSampler
-- No documentation found for TopLevel "SamplerAddressMode"
type SamplerAddressMode = VkSamplerAddressMode
-- No documentation found for TopLevel "SamplerCreateFlagBits"
type SamplerCreateFlagBits = VkSamplerCreateFlagBits
-- No documentation found for TopLevel "SamplerCreateFlags"
type SamplerCreateFlags = SamplerCreateFlagBits
-- No documentation found for TopLevel "SamplerCreateInfo"
data SamplerCreateInfo = SamplerCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "SamplerCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerCreateInfo" "flags"
  vkFlags :: SamplerCreateFlags
  , -- No documentation found for Nested "SamplerCreateInfo" "magFilter"
  vkMagFilter :: Filter
  , -- No documentation found for Nested "SamplerCreateInfo" "minFilter"
  vkMinFilter :: Filter
  , -- No documentation found for Nested "SamplerCreateInfo" "mipmapMode"
  vkMipmapMode :: SamplerMipmapMode
  , -- No documentation found for Nested "SamplerCreateInfo" "addressModeU"
  vkAddressModeU :: SamplerAddressMode
  , -- No documentation found for Nested "SamplerCreateInfo" "addressModeV"
  vkAddressModeV :: SamplerAddressMode
  , -- No documentation found for Nested "SamplerCreateInfo" "addressModeW"
  vkAddressModeW :: SamplerAddressMode
  , -- No documentation found for Nested "SamplerCreateInfo" "mipLodBias"
  vkMipLodBias :: CFloat
  , -- No documentation found for Nested "SamplerCreateInfo" "anisotropyEnable"
  vkAnisotropyEnable :: Bool
  , -- No documentation found for Nested "SamplerCreateInfo" "maxAnisotropy"
  vkMaxAnisotropy :: CFloat
  , -- No documentation found for Nested "SamplerCreateInfo" "compareEnable"
  vkCompareEnable :: Bool
  , -- No documentation found for Nested "SamplerCreateInfo" "compareOp"
  vkCompareOp :: CompareOp
  , -- No documentation found for Nested "SamplerCreateInfo" "minLod"
  vkMinLod :: CFloat
  , -- No documentation found for Nested "SamplerCreateInfo" "maxLod"
  vkMaxLod :: CFloat
  , -- No documentation found for Nested "SamplerCreateInfo" "borderColor"
  vkBorderColor :: BorderColor
  , -- No documentation found for Nested "SamplerCreateInfo" "unnormalizedCoordinates"
  vkUnnormalizedCoordinates :: Bool
  }
  deriving (Show, Eq)
withCStructSamplerCreateInfo :: SamplerCreateInfo -> (VkSamplerCreateInfo -> IO a) -> IO a
withCStructSamplerCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: SamplerCreateInfo)) (\pPNext -> cont (VkSamplerCreateInfo VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO pPNext (vkFlags (from :: SamplerCreateInfo)) (vkMagFilter (from :: SamplerCreateInfo)) (vkMinFilter (from :: SamplerCreateInfo)) (vkMipmapMode (from :: SamplerCreateInfo)) (vkAddressModeU (from :: SamplerCreateInfo)) (vkAddressModeV (from :: SamplerCreateInfo)) (vkAddressModeW (from :: SamplerCreateInfo)) (vkMipLodBias (from :: SamplerCreateInfo)) (boolToBool32 (vkAnisotropyEnable (from :: SamplerCreateInfo))) (vkMaxAnisotropy (from :: SamplerCreateInfo)) (boolToBool32 (vkCompareEnable (from :: SamplerCreateInfo))) (vkCompareOp (from :: SamplerCreateInfo)) (vkMinLod (from :: SamplerCreateInfo)) (vkMaxLod (from :: SamplerCreateInfo)) (vkBorderColor (from :: SamplerCreateInfo)) (boolToBool32 (vkUnnormalizedCoordinates (from :: SamplerCreateInfo)))))
fromCStructSamplerCreateInfo :: VkSamplerCreateInfo -> IO SamplerCreateInfo
fromCStructSamplerCreateInfo c = SamplerCreateInfo <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSamplerCreateInfo)))
                                                   <*> pure (vkFlags (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkMagFilter (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkMinFilter (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkMipmapMode (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkAddressModeU (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkAddressModeV (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkAddressModeW (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkMipLodBias (c :: VkSamplerCreateInfo))
                                                   <*> pure (bool32ToBool (vkAnisotropyEnable (c :: VkSamplerCreateInfo)))
                                                   <*> pure (vkMaxAnisotropy (c :: VkSamplerCreateInfo))
                                                   <*> pure (bool32ToBool (vkCompareEnable (c :: VkSamplerCreateInfo)))
                                                   <*> pure (vkCompareOp (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkMinLod (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkMaxLod (c :: VkSamplerCreateInfo))
                                                   <*> pure (vkBorderColor (c :: VkSamplerCreateInfo))
                                                   <*> pure (bool32ToBool (vkUnnormalizedCoordinates (c :: VkSamplerCreateInfo)))
instance Zero SamplerCreateInfo where
  zero = SamplerCreateInfo Nothing
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           False
                           zero
                           False
                           zero
                           zero
                           zero
                           zero
                           False
-- No documentation found for TopLevel "SamplerMipmapMode"
type SamplerMipmapMode = VkSamplerMipmapMode

-- | Wrapper for 'vkCreateSampler'
createSampler :: Device ->  SamplerCreateInfo ->  Maybe AllocationCallbacks ->  IO (Sampler)
createSampler = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pSampler -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructSamplerCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createSampler commandTable device pCreateInfo pAllocator pSampler >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSampler)))))

-- | Wrapper for 'vkDestroySampler'
destroySampler :: Device ->  Sampler ->  Maybe AllocationCallbacks ->  IO ()
destroySampler = \(Device device commandTable) -> \sampler -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroySampler commandTable device sampler pAllocator *> (pure ()))
-- | Wrapper for 'createSampler' and 'destroySampler' using 'bracket'
withSampler
  :: Device -> SamplerCreateInfo -> Maybe (AllocationCallbacks) -> (Sampler -> IO a) -> IO a
withSampler device samplerCreateInfo allocationCallbacks = bracket
  (createSampler device samplerCreateInfo allocationCallbacks)
  (\o -> destroySampler device o allocationCallbacks)
