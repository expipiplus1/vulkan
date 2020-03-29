{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform  ( CommandBufferInheritanceRenderPassTransformInfoQCOM
                                                                 , RenderPassTransformBeginInfoQCOM
                                                                 ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data CommandBufferInheritanceRenderPassTransformInfoQCOM

instance ToCStruct CommandBufferInheritanceRenderPassTransformInfoQCOM
instance Show CommandBufferInheritanceRenderPassTransformInfoQCOM

instance FromCStruct CommandBufferInheritanceRenderPassTransformInfoQCOM


data RenderPassTransformBeginInfoQCOM

instance ToCStruct RenderPassTransformBeginInfoQCOM
instance Show RenderPassTransformBeginInfoQCOM

instance FromCStruct RenderPassTransformBeginInfoQCOM

