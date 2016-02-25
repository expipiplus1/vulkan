{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.Surface where
import Data.Word( Word64
                , Word32
                )
import Data.Int( Int32
               )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Core( VkFlags(..)
                           )
-- ** VkCompositeAlphaFlagsKHR
-- | Opaque flag
newtype VkCompositeAlphaFlagsKHR = VkCompositeAlphaFlagsKHR VkFlags
  deriving (Eq, Storable)

-- ** VkPresentModeKHR

newtype VkPresentModeKHR = VkPresentModeKHR Int32
  deriving (Eq, Storable)

pattern VK_PRESENT_MODE_IMMEDIATE_KHR = VkPresentModeKHR 0

pattern VK_PRESENT_MODE_MAILBOX_KHR = VkPresentModeKHR 1

pattern VK_PRESENT_MODE_FIFO_KHR = VkPresentModeKHR 2

pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR = VkPresentModeKHR 3

newtype VkSurfaceKHR = VkSurfaceKHR Word64
  deriving (Eq, Storable)

-- ** VkColorSpaceKHR

newtype VkColorSpaceKHR = VkColorSpaceKHR Int32
  deriving (Eq, Storable)

pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR = VkColorSpaceKHR 0

-- ** VkSurfaceTransformFlagsKHR
-- | Opaque flag
newtype VkSurfaceTransformFlagsKHR = VkSurfaceTransformFlagsKHR VkFlags
  deriving (Eq, Storable)

