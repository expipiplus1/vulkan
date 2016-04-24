{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Pass where

import Data.Word( Word64
                )
import Foreign.Storable( Storable(..)
                       )

newtype VkRenderPass = VkRenderPass Word64
  
instance Eq VkRenderPass
instance Ord VkRenderPass
instance Storable VkRenderPass

