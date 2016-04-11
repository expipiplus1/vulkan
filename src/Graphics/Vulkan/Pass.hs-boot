{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Pass where

import Data.Word( Word64
                )
import Foreign.Storable( Storable(..)
                       )

newtype RenderPass = RenderPass Word64
  
instance Eq RenderPass
instance Storable RenderPass

