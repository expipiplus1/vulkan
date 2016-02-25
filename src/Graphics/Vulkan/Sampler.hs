{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Sampler where
import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Int( Int32
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
                           , VkBool32(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CFloat
                      , CFloat(..)
                      , CSize(..)
                      )
-- ** VkSamplerAddressMode

newtype VkSamplerAddressMode = VkSamplerAddressMode Int32
  deriving (Eq, Storable)

pattern VK_SAMPLER_ADDRESS_MODE_REPEAT = VkSamplerAddressMode 0

pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = VkSamplerAddressMode 1

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = VkSamplerAddressMode 2

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = VkSamplerAddressMode 3

pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE = VkSamplerAddressMode 4

-- ** VkFilter

newtype VkFilter = VkFilter Int32
  deriving (Eq, Storable)

pattern VK_FILTER_NEAREST = VkFilter 0

pattern VK_FILTER_LINEAR = VkFilter 1

-- ** VkBorderColor

newtype VkBorderColor = VkBorderColor Int32
  deriving (Eq, Storable)

pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = VkBorderColor 0

pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = VkBorderColor 1

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = VkBorderColor 2

pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK = VkBorderColor 3

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = VkBorderColor 4

pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE = VkBorderColor 5

-- ** VkCompareOp

newtype VkCompareOp = VkCompareOp Int32
  deriving (Eq, Storable)

pattern VK_COMPARE_OP_NEVER = VkCompareOp 0

pattern VK_COMPARE_OP_LESS = VkCompareOp 1

pattern VK_COMPARE_OP_EQUAL = VkCompareOp 2

pattern VK_COMPARE_OP_LESS_OR_EQUAL = VkCompareOp 3

pattern VK_COMPARE_OP_GREATER = VkCompareOp 4

pattern VK_COMPARE_OP_NOT_EQUAL = VkCompareOp 5

pattern VK_COMPARE_OP_GREATER_OR_EQUAL = VkCompareOp 6

pattern VK_COMPARE_OP_ALWAYS = VkCompareOp 7

newtype VkSampler = VkSampler Word64
  deriving (Eq, Storable)


data VkSamplerCreateInfo =
  VkSamplerCreateInfo{ vkSType :: VkStructureType 
                     , vkPNext :: Ptr Void 
                     , vkFlags :: VkSamplerCreateFlags 
                     , vkMagFilter :: VkFilter 
                     , vkMinFilter :: VkFilter 
                     , vkMipmapMode :: VkSamplerMipmapMode 
                     , vkAddressModeU :: VkSamplerAddressMode 
                     , vkAddressModeV :: VkSamplerAddressMode 
                     , vkAddressModeW :: VkSamplerAddressMode 
                     , vkMipLodBias :: CFloat 
                     , vkAnisotropyEnable :: VkBool32 
                     , vkMaxAnisotropy :: CFloat 
                     , vkCompareEnable :: VkBool32 
                     , vkCompareOp :: VkCompareOp 
                     , vkMinLod :: CFloat 
                     , vkMaxLod :: CFloat 
                     , vkBorderColor :: VkBorderColor 
                     , vkUnnormalizedCoordinates :: VkBool32 
                     }
  deriving (Eq)

instance Storable VkSamplerCreateInfo where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek ptr = VkSamplerCreateInfo <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 20)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 28)
                                 <*> peek (ptr `plusPtr` 32)
                                 <*> peek (ptr `plusPtr` 36)
                                 <*> peek (ptr `plusPtr` 40)
                                 <*> peek (ptr `plusPtr` 44)
                                 <*> peek (ptr `plusPtr` 48)
                                 <*> peek (ptr `plusPtr` 52)
                                 <*> peek (ptr `plusPtr` 56)
                                 <*> peek (ptr `plusPtr` 60)
                                 <*> peek (ptr `plusPtr` 64)
                                 <*> peek (ptr `plusPtr` 68)
                                 <*> peek (ptr `plusPtr` 72)
                                 <*> peek (ptr `plusPtr` 76)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkMagFilter (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkMinFilter (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkMipmapMode (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkAddressModeU (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkAddressModeV (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkAddressModeW (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkMipLodBias (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkAnisotropyEnable (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkMaxAnisotropy (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkCompareEnable (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 60) (vkCompareOp (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkMinLod (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 68) (vkMaxLod (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkBorderColor (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 76) (vkUnnormalizedCoordinates (poked :: VkSamplerCreateInfo))


-- ** VkSamplerCreateFlags
-- | Opaque flag
newtype VkSamplerCreateFlags = VkSamplerCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkSamplerMipmapMode

newtype VkSamplerMipmapMode = VkSamplerMipmapMode Int32
  deriving (Eq, Storable)
-- | Choose nearest mip level
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = VkSamplerMipmapMode 0
-- | Linear filter between mip levels
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR = VkSamplerMipmapMode 1

-- ** vkCreateSampler
foreign import ccall "vkCreateSampler" vkCreateSampler :: 
  VkDevice ->
  Ptr VkSamplerCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkSampler -> IO VkResult

-- ** VkSampleCountFlags

newtype VkSampleCountFlagBits = VkSampleCountFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkSampleCountFlagBits
type VkSampleCountFlags = VkSampleCountFlagBits
-- | Sample count 1 supported
pattern VK_SAMPLE_COUNT_1_BIT = VkSampleCountFlagBits 0x1
-- | Sample count 2 supported
pattern VK_SAMPLE_COUNT_2_BIT = VkSampleCountFlagBits 0x2
-- | Sample count 4 supported
pattern VK_SAMPLE_COUNT_4_BIT = VkSampleCountFlagBits 0x4
-- | Sample count 8 supported
pattern VK_SAMPLE_COUNT_8_BIT = VkSampleCountFlagBits 0x8
-- | Sample count 16 supported
pattern VK_SAMPLE_COUNT_16_BIT = VkSampleCountFlagBits 0x10
-- | Sample count 32 supported
pattern VK_SAMPLE_COUNT_32_BIT = VkSampleCountFlagBits 0x20
-- | Sample count 64 supported
pattern VK_SAMPLE_COUNT_64_BIT = VkSampleCountFlagBits 0x40


-- ** vkDestroySampler
foreign import ccall "vkDestroySampler" vkDestroySampler :: 
  VkDevice -> VkSampler -> Ptr VkAllocationCallbacks -> IO ()

