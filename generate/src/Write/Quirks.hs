module Write.Quirks where

import Data.HashMap.Strict as M
import Write.Utils

-- | Entities which must be put in hs-boot files to break dependency cycles
--
-- Only handles are allowed in here, this isn't checked
cycleBreakers :: HashMap ModuleName [String]
cycleBreakers = M.fromList [ (ModuleName "Graphics.Vulkan.Device", ["VkDevice"])
                           , (ModuleName "Graphics.Vulkan.Pass", ["VkRenderPass"])
                           ]

sourceImports = M.fromList [ ( ModuleName "Graphics.Vulkan.Memory"
                             , [ModuleName "Graphics.Vulkan.Device"]
                             )
                           , ( ModuleName "Graphics.Vulkan.Pipeline"
                             , [ModuleName "Graphics.Vulkan.Pass"]
                             )
                           ]


