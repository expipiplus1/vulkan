
module Graphics.Vulkan.Device where

import Foreign.Ptr( Ptr
                  )

data VkDevice_T
type Device = Ptr VkDevice_T

