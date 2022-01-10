{-# language CPP #-}
-- | = Name
--
-- VK_NV_clip_space_w_scaling - device extension
--
-- == VK_NV_clip_space_w_scaling
--
-- [__Name String__]
--     @VK_NV_clip_space_w_scaling@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     88
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Eric Werness
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_clip_space_w_scaling] @ewerness-nv%0A<<Here describe the issue or question you have about the VK_NV_clip_space_w_scaling extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-02-15
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Kedarnath Thangudu, NVIDIA
--
-- == Description
--
-- Virtual Reality (VR) applications often involve a post-processing step
-- to apply a “barrel” distortion to the rendered image to correct the
-- “pincushion” distortion introduced by the optics in a VR device. The
-- barrel distorted image has lower resolution along the edges compared to
-- the center. Since the original image is rendered at high resolution,
-- which is uniform across the complete image, a lot of pixels towards the
-- edges do not make it to the final post-processed image.
--
-- This extension provides a mechanism to render VR scenes at a non-uniform
-- resolution, in particular a resolution that falls linearly from the
-- center towards the edges. This is achieved by scaling the w coordinate
-- of the vertices in the clip space before perspective divide. The clip
-- space w coordinate of the vertices /can/ be offset as of a function of x
-- and y coordinates as follows:
--
-- w\' = w + Ax + By
--
-- In the intended use case for viewport position scaling, an application
-- should use a set of four viewports, one for each of the four quadrants
-- of a Cartesian coordinate system. Each viewport is set to the dimension
-- of the image, but is scissored to the quadrant it represents. The
-- application should specify A and B coefficients of the w-scaling
-- equation above, that have the same value, but different signs, for each
-- of the viewports. The signs of A and B should match the signs of x and y
-- for the quadrant that they represent such that the value of w\' will
-- always be greater than or equal to the original w value for the entire
-- image. Since the offset to w, (Ax + By), is always positive, and
-- increases with the absolute values of x and y, the effective resolution
-- will fall off linearly from the center of the image to its edges.
--
-- == New Commands
--
-- -   'cmdSetViewportWScalingNV'
--
-- == New Structures
--
-- -   'ViewportWScalingNV'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo':
--
--     -   'PipelineViewportWScalingStateCreateInfoNV'
--
-- == New Enum Constants
--
-- -   'NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME'
--
-- -   'NV_CLIP_SPACE_W_SCALING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV'
--
-- == Issues
--
-- 1) Is the pipeline struct name too long?
--
-- __RESOLVED__: It fits with the naming convention.
--
-- 2) Separate W scaling section or fold into coordinate transformations?
--
-- __RESOLVED__: Leaving it as its own section for now.
--
-- == Examples
--
-- > VkViewport viewports[4];
-- > VkRect2D scissors[4];
-- > VkViewportWScalingNV scalings[4];
-- >
-- > for (int i = 0; i < 4; i++) {
-- >     int x = (i & 2) ? 0 : currentWindowWidth / 2;
-- >     int y = (i & 1) ? 0 : currentWindowHeight / 2;
-- >
-- >     viewports[i].x = 0;
-- >     viewports[i].y = 0;
-- >     viewports[i].width = currentWindowWidth;
-- >     viewports[i].height = currentWindowHeight;
-- >     viewports[i].minDepth = 0.0f;
-- >     viewports[i].maxDepth = 1.0f;
-- >
-- >     scissors[i].offset.x = x;
-- >     scissors[i].offset.y = y;
-- >     scissors[i].extent.width = currentWindowWidth/2;
-- >     scissors[i].extent.height = currentWindowHeight/2;
-- >
-- >     const float factor = 0.15;
-- >     scalings[i].xcoeff = ((i & 2) ? -1.0 : 1.0) * factor;
-- >     scalings[i].ycoeff = ((i & 1) ? -1.0 : 1.0) * factor;
-- > }
-- >
-- > VkPipelineViewportWScalingStateCreateInfoNV vpWScalingStateInfo = { VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV };
-- >
-- > vpWScalingStateInfo.viewportWScalingEnable = VK_TRUE;
-- > vpWScalingStateInfo.viewportCount = 4;
-- > vpWScalingStateInfo.pViewportWScalings = &scalings[0];
-- >
-- > VkPipelineViewportStateCreateInfo vpStateInfo = { VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO };
-- > vpStateInfo.viewportCount = 4;
-- > vpStateInfo.pViewports = &viewports[0];
-- > vpStateInfo.scissorCount = 4;
-- > vpStateInfo.pScissors = &scissors[0];
-- > vpStateInfo.pNext = &vpWScalingStateInfo;
--
-- Example shader to read from a w-scaled texture:
--
-- > // Vertex Shader
-- > // Draw a triangle that covers the whole screen
-- > const vec4 positions[3] = vec4[3](vec4(-1, -1, 0, 1),
-- >                                   vec4( 3, -1, 0, 1),
-- >                                   vec4(-1,  3, 0, 1));
-- > out vec2 uv;
-- > void main()
-- > {
-- >     vec4 pos = positions[ gl_VertexID ];
-- >     gl_Position = pos;
-- >     uv = pos.xy;
-- > }
-- >
-- > // Fragment Shader
-- > uniform sampler2D tex;
-- > uniform float xcoeff;
-- > uniform float ycoeff;
-- > out vec4 Color;
-- > in vec2 uv;
-- >
-- > void main()
-- > {
-- >     // Handle uv as if upper right quadrant
-- >     vec2 uvabs = abs(uv);
-- >
-- >     // unscale: transform w-scaled image into an unscaled image
-- >     //   scale: transform unscaled image int a w-scaled image
-- >     float unscale = 1.0 / (1 + xcoeff * uvabs.x + xcoeff * uvabs.y);
-- >     //float scale = 1.0 / (1 - xcoeff * uvabs.x - xcoeff * uvabs.y);
-- >
-- >     vec2 P = vec2(unscale * uvabs.x, unscale * uvabs.y);
-- >
-- >     // Go back to the right quadrant
-- >     P *= sign(uv);
-- >
-- >     Color = texture(tex, P * 0.5 + 0.5);
-- > }
--
-- == Version History
--
-- -   Revision 1, 2017-02-15 (Eric Werness)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PipelineViewportWScalingStateCreateInfoNV', 'ViewportWScalingNV',
-- 'cmdSetViewportWScalingNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_clip_space_w_scaling Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_clip_space_w_scaling  ( PipelineViewportWScalingStateCreateInfoNV
                                                     , ViewportWScalingNV
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PipelineViewportWScalingStateCreateInfoNV

instance ToCStruct PipelineViewportWScalingStateCreateInfoNV
instance Show PipelineViewportWScalingStateCreateInfoNV

instance FromCStruct PipelineViewportWScalingStateCreateInfoNV


data ViewportWScalingNV

instance ToCStruct ViewportWScalingNV
instance Show ViewportWScalingNV

instance FromCStruct ViewportWScalingNV

