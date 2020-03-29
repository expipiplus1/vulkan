{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_GGP_frame_token  (PresentFrameTokenGGP) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PresentFrameTokenGGP

instance ToCStruct PresentFrameTokenGGP
instance Show PresentFrameTokenGGP

instance FromCStruct PresentFrameTokenGGP

