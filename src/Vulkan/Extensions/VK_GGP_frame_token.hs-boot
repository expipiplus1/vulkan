{-# language CPP #-}
module Vulkan.Extensions.VK_GGP_frame_token  (PresentFrameTokenGGP) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PresentFrameTokenGGP

instance ToCStruct PresentFrameTokenGGP
instance Show PresentFrameTokenGGP

instance FromCStruct PresentFrameTokenGGP

