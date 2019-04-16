{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core10.Version
  ( pattern VK_MAKE_VERSION
  , pattern VK_API_VERSION_1_0
  , _VK_VERSION_MAJOR
  , _VK_VERSION_MINOR
  , _VK_VERSION_PATCH
  ) where




import Graphics.Vulkan.C.Core10.Version
  ( _VK_VERSION_MAJOR
  , _VK_VERSION_MINOR
  , _VK_VERSION_PATCH
  , pattern VK_API_VERSION_1_0
  , pattern VK_MAKE_VERSION
  )

