{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( withCStructBufferMemoryRequirementsInfo2
  , fromCStructBufferMemoryRequirementsInfo2
  , BufferMemoryRequirementsInfo2(..)
  , withCStructImageMemoryRequirementsInfo2
  , fromCStructImageMemoryRequirementsInfo2
  , ImageMemoryRequirementsInfo2(..)
  , withCStructImageSparseMemoryRequirementsInfo2
  , fromCStructImageSparseMemoryRequirementsInfo2
  , ImageSparseMemoryRequirementsInfo2(..)
  , withCStructMemoryRequirements2
  , fromCStructMemoryRequirements2
  , MemoryRequirements2(..)
  , MemoryRequirements2KHR
  , withCStructSparseImageMemoryRequirements2
  , fromCStructSparseImageMemoryRequirements2
  , SparseImageMemoryRequirements2(..)
  , getBufferMemoryRequirements2
  , getImageMemoryRequirements2
  , getNumImageSparseMemoryRequirements2
  , getImageSparseMemoryRequirements2
  , getAllImageSparseMemoryRequirements2
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getBufferMemoryRequirements2
  , getImageMemoryRequirements2
  , getImageSparseMemoryRequirements2
  )


import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( VkBufferMemoryRequirementsInfo2(..)
  , VkImageMemoryRequirementsInfo2(..)
  , VkImageSparseMemoryRequirementsInfo2(..)
  , VkMemoryRequirements2(..)
  , VkSparseImageMemoryRequirements2(..)
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( MemoryRequirements(..)
  , Buffer
  , Image
  , fromCStructMemoryRequirements
  , withCStructMemoryRequirements
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( SparseImageMemoryRequirements(..)
  , fromCStructSparseImageMemoryRequirements
  , withCStructSparseImageMemoryRequirements
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "BufferMemoryRequirementsInfo2"
data BufferMemoryRequirementsInfo2 = BufferMemoryRequirementsInfo2
  { -- Univalued Member elided
  -- No documentation found for Nested "BufferMemoryRequirementsInfo2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferMemoryRequirementsInfo2" "buffer"
  vkBuffer :: Buffer
  }
  deriving (Show, Eq)
withCStructBufferMemoryRequirementsInfo2 :: BufferMemoryRequirementsInfo2 -> (VkBufferMemoryRequirementsInfo2 -> IO a) -> IO a
withCStructBufferMemoryRequirementsInfo2 from cont = maybeWith withSomeVkStruct (vkPNext (from :: BufferMemoryRequirementsInfo2)) (\pPNext -> cont (VkBufferMemoryRequirementsInfo2 VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 pPNext (vkBuffer (from :: BufferMemoryRequirementsInfo2))))
fromCStructBufferMemoryRequirementsInfo2 :: VkBufferMemoryRequirementsInfo2 -> IO BufferMemoryRequirementsInfo2
fromCStructBufferMemoryRequirementsInfo2 c = BufferMemoryRequirementsInfo2 <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBufferMemoryRequirementsInfo2)))
                                                                           <*> pure (vkBuffer (c :: VkBufferMemoryRequirementsInfo2))
-- No documentation found for TopLevel "ImageMemoryRequirementsInfo2"
data ImageMemoryRequirementsInfo2 = ImageMemoryRequirementsInfo2
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageMemoryRequirementsInfo2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageMemoryRequirementsInfo2" "image"
  vkImage :: Image
  }
  deriving (Show, Eq)
withCStructImageMemoryRequirementsInfo2 :: ImageMemoryRequirementsInfo2 -> (VkImageMemoryRequirementsInfo2 -> IO a) -> IO a
withCStructImageMemoryRequirementsInfo2 from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImageMemoryRequirementsInfo2)) (\pPNext -> cont (VkImageMemoryRequirementsInfo2 VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 pPNext (vkImage (from :: ImageMemoryRequirementsInfo2))))
fromCStructImageMemoryRequirementsInfo2 :: VkImageMemoryRequirementsInfo2 -> IO ImageMemoryRequirementsInfo2
fromCStructImageMemoryRequirementsInfo2 c = ImageMemoryRequirementsInfo2 <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageMemoryRequirementsInfo2)))
                                                                         <*> pure (vkImage (c :: VkImageMemoryRequirementsInfo2))
-- No documentation found for TopLevel "ImageSparseMemoryRequirementsInfo2"
data ImageSparseMemoryRequirementsInfo2 = ImageSparseMemoryRequirementsInfo2
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageSparseMemoryRequirementsInfo2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageSparseMemoryRequirementsInfo2" "image"
  vkImage :: Image
  }
  deriving (Show, Eq)
withCStructImageSparseMemoryRequirementsInfo2 :: ImageSparseMemoryRequirementsInfo2 -> (VkImageSparseMemoryRequirementsInfo2 -> IO a) -> IO a
withCStructImageSparseMemoryRequirementsInfo2 from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImageSparseMemoryRequirementsInfo2)) (\pPNext -> cont (VkImageSparseMemoryRequirementsInfo2 VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 pPNext (vkImage (from :: ImageSparseMemoryRequirementsInfo2))))
fromCStructImageSparseMemoryRequirementsInfo2 :: VkImageSparseMemoryRequirementsInfo2 -> IO ImageSparseMemoryRequirementsInfo2
fromCStructImageSparseMemoryRequirementsInfo2 c = ImageSparseMemoryRequirementsInfo2 <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageSparseMemoryRequirementsInfo2)))
                                                                                     <*> pure (vkImage (c :: VkImageSparseMemoryRequirementsInfo2))
-- No documentation found for TopLevel "MemoryRequirements2"
data MemoryRequirements2 = MemoryRequirements2
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryRequirements2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryRequirements2" "memoryRequirements"
  vkMemoryRequirements :: MemoryRequirements
  }
  deriving (Show, Eq)
withCStructMemoryRequirements2 :: MemoryRequirements2 -> (VkMemoryRequirements2 -> IO a) -> IO a
withCStructMemoryRequirements2 from cont = withCStructMemoryRequirements (vkMemoryRequirements (from :: MemoryRequirements2)) (\memoryRequirements -> maybeWith withSomeVkStruct (vkPNext (from :: MemoryRequirements2)) (\pPNext -> cont (VkMemoryRequirements2 VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 pPNext memoryRequirements)))
fromCStructMemoryRequirements2 :: VkMemoryRequirements2 -> IO MemoryRequirements2
fromCStructMemoryRequirements2 c = MemoryRequirements2 <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryRequirements2)))
                                                       <*> (fromCStructMemoryRequirements (vkMemoryRequirements (c :: VkMemoryRequirements2)))
type MemoryRequirements2KHR = MemoryRequirements2
-- TODO: Pattern constructor alias)
-- No documentation found for TopLevel "SparseImageMemoryRequirements2"
data SparseImageMemoryRequirements2 = SparseImageMemoryRequirements2
  { -- Univalued Member elided
  -- No documentation found for Nested "SparseImageMemoryRequirements2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SparseImageMemoryRequirements2" "memoryRequirements"
  vkMemoryRequirements :: SparseImageMemoryRequirements
  }
  deriving (Show, Eq)
withCStructSparseImageMemoryRequirements2 :: SparseImageMemoryRequirements2 -> (VkSparseImageMemoryRequirements2 -> IO a) -> IO a
withCStructSparseImageMemoryRequirements2 from cont = withCStructSparseImageMemoryRequirements (vkMemoryRequirements (from :: SparseImageMemoryRequirements2)) (\memoryRequirements -> maybeWith withSomeVkStruct (vkPNext (from :: SparseImageMemoryRequirements2)) (\pPNext -> cont (VkSparseImageMemoryRequirements2 VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 pPNext memoryRequirements)))
fromCStructSparseImageMemoryRequirements2 :: VkSparseImageMemoryRequirements2 -> IO SparseImageMemoryRequirements2
fromCStructSparseImageMemoryRequirements2 c = SparseImageMemoryRequirements2 <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSparseImageMemoryRequirements2)))
                                                                             <*> (fromCStructSparseImageMemoryRequirements (vkMemoryRequirements (c :: VkSparseImageMemoryRequirements2)))

-- | Wrapper for 'vkGetBufferMemoryRequirements2'
getBufferMemoryRequirements2 :: Device ->  BufferMemoryRequirementsInfo2 ->  IO (MemoryRequirements2)
getBufferMemoryRequirements2 = \(Device device commandTable) -> \info -> alloca (\pMemoryRequirements -> (\a -> withCStructBufferMemoryRequirementsInfo2 a . flip with) info (\pInfo -> Graphics.Vulkan.C.Dynamic.getBufferMemoryRequirements2 commandTable device pInfo pMemoryRequirements *> ((fromCStructMemoryRequirements2 <=< peek) pMemoryRequirements)))

-- | Wrapper for 'vkGetImageMemoryRequirements2'
getImageMemoryRequirements2 :: Device ->  ImageMemoryRequirementsInfo2 ->  IO (MemoryRequirements2)
getImageMemoryRequirements2 = \(Device device commandTable) -> \info -> alloca (\pMemoryRequirements -> (\a -> withCStructImageMemoryRequirementsInfo2 a . flip with) info (\pInfo -> Graphics.Vulkan.C.Dynamic.getImageMemoryRequirements2 commandTable device pInfo pMemoryRequirements *> ((fromCStructMemoryRequirements2 <=< peek) pMemoryRequirements)))

-- | Wrapper for 'vkGetImageSparseMemoryRequirements2'
getNumImageSparseMemoryRequirements2 :: Device ->  ImageSparseMemoryRequirementsInfo2 ->  IO (Word32)
getNumImageSparseMemoryRequirements2 = \(Device device commandTable) -> \info -> alloca (\pSparseMemoryRequirementCount -> (\a -> withCStructImageSparseMemoryRequirementsInfo2 a . flip with) info (\pInfo -> Graphics.Vulkan.C.Dynamic.getImageSparseMemoryRequirements2 commandTable device pInfo pSparseMemoryRequirementCount nullPtr *> (peek pSparseMemoryRequirementCount)))

-- | Wrapper for 'vkGetImageSparseMemoryRequirements2'
getImageSparseMemoryRequirements2 :: Device ->  ImageSparseMemoryRequirementsInfo2 ->  Word32 ->  IO ( Vector SparseImageMemoryRequirements2 )
getImageSparseMemoryRequirements2 = \(Device device commandTable) -> \info -> \sparseMemoryRequirementCount -> allocaArray (fromIntegral sparseMemoryRequirementCount) (\pSparseMemoryRequirements -> with sparseMemoryRequirementCount (\pSparseMemoryRequirementCount -> (\a -> withCStructImageSparseMemoryRequirementsInfo2 a . flip with) info (\pInfo -> Graphics.Vulkan.C.Dynamic.getImageSparseMemoryRequirements2 commandTable device pInfo pSparseMemoryRequirementCount pSparseMemoryRequirements *> ((flip Data.Vector.generateM ((\p -> fromCStructSparseImageMemoryRequirements2 <=< peekElemOff p) pSparseMemoryRequirements) =<< (fromIntegral <$> (peek pSparseMemoryRequirementCount)))))))
-- | Call 'getNumImageSparseMemoryRequirements2' to get the number of return values, then use that
-- number to call 'getImageSparseMemoryRequirements2' to get all the values.
getAllImageSparseMemoryRequirements2 :: Device ->  ImageSparseMemoryRequirementsInfo2 ->  IO (Vector SparseImageMemoryRequirements2)
getAllImageSparseMemoryRequirements2 device pInfo =
  getNumImageSparseMemoryRequirements2 device pInfo
    >>= \num -> getImageSparseMemoryRequirements2 device pInfo num

