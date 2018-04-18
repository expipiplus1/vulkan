{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_mir_surface
  ( MirConnection
  , MirSurface
  , VkMirSurfaceCreateFlagsKHR(..)
  , pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR
  , pattern VK_KHR_MIR_SURFACE_SPEC_VERSION
  , pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME
  , vkCreateMirSurfaceKHR
  , vkGetPhysicalDeviceMirPresentationSupportKHR
  , VkMirSurfaceCreateInfoKHR(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkPhysicalDevice
  , VkAllocationCallbacks(..)
  , VkInstance
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )


data MirConnection
data MirSurface
-- ** VkMirSurfaceCreateFlagsKHR

-- | 
newtype VkMirSurfaceCreateFlagsKHR = VkMirSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkMirSurfaceCreateFlagsKHR where
  
  showsPrec p (VkMirSurfaceCreateFlagsKHR x) = showParen (p >= 11) (showString "VkMirSurfaceCreateFlagsKHR " . showsPrec 11 x)

instance Read VkMirSurfaceCreateFlagsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMirSurfaceCreateFlagsKHR")
                        v <- step readPrec
                        pure (VkMirSurfaceCreateFlagsKHR v)
                        )
                    )


-- | Nothing
pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR = VkStructureType 1000007000
pattern VK_KHR_MIR_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_MIR_SURFACE_SPEC_VERSION = 4
pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME = "VK_KHR_mir_surface"
-- | 
foreign import ccall "vkCreateMirSurfaceKHR" vkCreateMirSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMirSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | 
foreign import ccall "vkGetPhysicalDeviceMirPresentationSupportKHR" vkGetPhysicalDeviceMirPresentationSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr MirConnection) -> IO VkBool32
-- | TODO: Struct comments
data VkMirSurfaceCreateInfoKHR = VkMirSurfaceCreateInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkMirSurfaceCreateFlagsKHR
  , vkConnection :: Ptr MirConnection
  , vkMirSurface :: Ptr MirSurface
  }
  deriving (Eq, Show)

instance Storable VkMirSurfaceCreateInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkMirSurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMirSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkMirSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkMirSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkConnection (poked :: VkMirSurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkMirSurface (poked :: VkMirSurfaceCreateInfoKHR))
