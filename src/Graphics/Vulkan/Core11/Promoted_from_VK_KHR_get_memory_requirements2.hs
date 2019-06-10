{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  BufferMemoryRequirementsInfo2(..)
  , 
  ImageMemoryRequirementsInfo2(..)
  , ImageSparseMemoryRequirementsInfo2(..)
  , MemoryRequirements2(..)
#endif
  , MemoryRequirements2KHR
#if defined(VK_USE_PLATFORM_GGP)
  , SparseImageMemoryRequirements2(..)
  , getBufferMemoryRequirements2
  , getImageMemoryRequirements2
  , getNumImageSparseMemoryRequirements2
  , getImageSparseMemoryRequirements2
  , getAllImageSparseMemoryRequirements2
#endif
  , pattern STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import qualified Data.Vector
  ( generateM
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Array
  ( allocaArray
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Utils
  ( with
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( nullPtr
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peekElemOff
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( vkGetBufferMemoryRequirements2
  , vkGetImageMemoryRequirements2
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( vkGetImageSparseMemoryRequirements2
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.MemoryManagement
  ( MemoryRequirements(..)
  , Buffer
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.MemoryManagement
  ( Image
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( SparseImageMemoryRequirements(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBufferMemoryRequirementsInfo2"
data BufferMemoryRequirementsInfo2 = BufferMemoryRequirementsInfo2
  { -- No documentation found for Nested "BufferMemoryRequirementsInfo2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferMemoryRequirementsInfo2" "buffer"
  buffer :: Buffer
  }
  deriving (Show, Eq)

instance Zero BufferMemoryRequirementsInfo2 where
  zero = BufferMemoryRequirementsInfo2 Nothing
                                       zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageMemoryRequirementsInfo2"
data ImageMemoryRequirementsInfo2 = ImageMemoryRequirementsInfo2
  { -- No documentation found for Nested "ImageMemoryRequirementsInfo2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageMemoryRequirementsInfo2" "image"
  image :: Image
  }
  deriving (Show, Eq)

instance Zero ImageMemoryRequirementsInfo2 where
  zero = ImageMemoryRequirementsInfo2 Nothing
                                      zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageSparseMemoryRequirementsInfo2"
data ImageSparseMemoryRequirementsInfo2 = ImageSparseMemoryRequirementsInfo2
  { -- No documentation found for Nested "ImageSparseMemoryRequirementsInfo2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageSparseMemoryRequirementsInfo2" "image"
  image :: Image
  }
  deriving (Show, Eq)

instance Zero ImageSparseMemoryRequirementsInfo2 where
  zero = ImageSparseMemoryRequirementsInfo2 Nothing
                                            zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryRequirements2"
data MemoryRequirements2 = MemoryRequirements2
  { -- No documentation found for Nested "MemoryRequirements2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryRequirements2" "memoryRequirements"
  memoryRequirements :: MemoryRequirements
  }
  deriving (Show, Eq)

instance Zero MemoryRequirements2 where
  zero = MemoryRequirements2 Nothing
                             zero

#endif

type MemoryRequirements2KHR = MemoryRequirements2
-- TODO: Pattern constructor alias)


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSparseImageMemoryRequirements2"
data SparseImageMemoryRequirements2 = SparseImageMemoryRequirements2
  { -- No documentation found for Nested "SparseImageMemoryRequirements2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SparseImageMemoryRequirements2" "memoryRequirements"
  memoryRequirements :: SparseImageMemoryRequirements
  }
  deriving (Show, Eq)

instance Zero SparseImageMemoryRequirements2 where
  zero = SparseImageMemoryRequirements2 Nothing
                                        zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetBufferMemoryRequirements2"
getBufferMemoryRequirements2 :: Device ->  BufferMemoryRequirementsInfo2 ->  IO (MemoryRequirements2)
getBufferMemoryRequirements2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetImageMemoryRequirements2"
getImageMemoryRequirements2 :: Device ->  ImageMemoryRequirementsInfo2 ->  IO (MemoryRequirements2)
getImageMemoryRequirements2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetImageSparseMemoryRequirements2"
getNumImageSparseMemoryRequirements2 :: Device ->  ImageSparseMemoryRequirementsInfo2 ->  IO (Word32)
getNumImageSparseMemoryRequirements2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetImageSparseMemoryRequirements2"
getImageSparseMemoryRequirements2 :: Device ->  ImageSparseMemoryRequirementsInfo2 ->  Word32 ->  IO (Vector SparseImageMemoryRequirements2)
getImageSparseMemoryRequirements2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getImageSparseMemoryRequirements2'.
getAllImageSparseMemoryRequirements2 :: Device ->  ImageSparseMemoryRequirementsInfo2 ->  IO (Vector SparseImageMemoryRequirements2)
getAllImageSparseMemoryRequirements2 device' pInfo' =
  getNumImageSparseMemoryRequirements2 device' pInfo'
    >>= \num -> getImageSparseMemoryRequirements2 device' pInfo' num

#endif
