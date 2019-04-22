{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced
  ( BlendOverlapEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced
  ( VkBlendOverlapEXT
  )


-- | VkBlendOverlapEXT - Enumerant specifying the blend overlap parameter
--
-- = Description
--
-- \'
--
-- > +-----------------------------------+-----------------------------------+
-- > | Overlap Mode                      | Weighting Equations               |
-- > +===================================+===================================+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | \[                                |
-- > | EXT_blend_operation_advanced.VK_B |                \begin{aligned}    |
-- > | LEND_OVERLAP_UNCORRELATED_EXT'    |                                   |
-- > |                                   |                p_0(A_s,A_d) & = A |
-- > |                                   | _sA_d \\                          |
-- > |                                   |                                   |
-- > |                                   |                p_1(A_s,A_d) & = A |
-- > |                                   | _s(1-A_d) \\                      |
-- > |                                   |                                   |
-- > |                                   |                p_2(A_s,A_d) & = A |
-- > |                                   | _d(1-A_s) \\                      |
-- > |                                   |                                   |
-- > |                                   |              \end{aligned}\]      |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | \[                                |
-- > | EXT_blend_operation_advanced.VK_B |                \begin{aligned}    |
-- > | LEND_OVERLAP_CONJOINT_EXT'        |                                   |
-- > |                                   |                p_0(A_s,A_d) & = m |
-- > |                                   | in(A_s,A_d) \\                    |
-- > |                                   |                                   |
-- > |                                   |                p_1(A_s,A_d) & = m |
-- > |                                   | ax(A_s-A_d,0) \\                  |
-- > |                                   |                                   |
-- > |                                   |                p_2(A_s,A_d) & = m |
-- > |                                   | ax(A_d-A_s,0) \\                  |
-- > |                                   |                                   |
-- > |                                   |              \end{aligned}\]      |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | \[                                |
-- > | EXT_blend_operation_advanced.VK_B |                \begin{aligned}    |
-- > | LEND_OVERLAP_DISJOINT_EXT'        |                                   |
-- > |                                   |                p_0(A_s,A_d) & = m |
-- > |                                   | ax(A_s+A_d-1,0) \\                |
-- > |                                   |                                   |
-- > |                                   |                p_1(A_s,A_d) & = m |
-- > |                                   | in(A_s,1-A_d) \\                  |
-- > |                                   |                                   |
-- > |                                   |                p_2(A_s,A_d) & = m |
-- > |                                   | in(A_d,1-A_s) \\                  |
-- > |                                   |                                   |
-- > |                                   |              \end{aligned}\]      |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Advanced Blend Overlap Modes
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VkPipelineColorBlendAdvancedStateCreateInfoEXT'
type BlendOverlapEXT = VkBlendOverlapEXT
