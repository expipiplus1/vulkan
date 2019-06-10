{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.DeviceInitialization
  ( enumerateInstanceVersion
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core11.DeviceInitialization
  ( vkEnumerateInstanceVersion
  )



-- No documentation found for TopLevel "vkEnumerateInstanceVersion"
enumerateInstanceVersion :: IO (Word32)
enumerateInstanceVersion = undefined {- {wrapped (pretty cName) :: Doc ()} -}
