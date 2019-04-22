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
  , pattern STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( VkBufferMemoryRequirementsInfo2(..)
  , VkImageMemoryRequirementsInfo2(..)
  , VkImageSparseMemoryRequirementsInfo2(..)
  , VkMemoryRequirements2(..)
  , VkSparseImageMemoryRequirements2(..)
  , vkGetBufferMemoryRequirements2
  , vkGetImageMemoryRequirements2
  , vkGetImageSparseMemoryRequirements2
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
  )



-- | VkBufferMemoryRequirementsInfo2 - (None)
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2KHR'
data BufferMemoryRequirementsInfo2 = BufferMemoryRequirementsInfo2
  { -- Univalued member elided
  -- No documentation found for Nested "BufferMemoryRequirementsInfo2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferMemoryRequirementsInfo2" "buffer"
  buffer :: Buffer
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBufferMemoryRequirementsInfo2' and
-- marshal a 'BufferMemoryRequirementsInfo2' into it. The 'VkBufferMemoryRequirementsInfo2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBufferMemoryRequirementsInfo2 :: BufferMemoryRequirementsInfo2 -> (VkBufferMemoryRequirementsInfo2 -> IO a) -> IO a
withCStructBufferMemoryRequirementsInfo2 marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: BufferMemoryRequirementsInfo2)) (\pPNext -> cont (VkBufferMemoryRequirementsInfo2 VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 pPNext (buffer (marshalled :: BufferMemoryRequirementsInfo2))))

-- | A function to read a 'VkBufferMemoryRequirementsInfo2' and all additional
-- structures in the pointer chain into a 'BufferMemoryRequirementsInfo2'.
fromCStructBufferMemoryRequirementsInfo2 :: VkBufferMemoryRequirementsInfo2 -> IO BufferMemoryRequirementsInfo2
fromCStructBufferMemoryRequirementsInfo2 c = BufferMemoryRequirementsInfo2 <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBufferMemoryRequirementsInfo2)))
                                                                           <*> pure (vkBuffer (c :: VkBufferMemoryRequirementsInfo2))

instance Zero BufferMemoryRequirementsInfo2 where
  zero = BufferMemoryRequirementsInfo2 Nothing
                                       zero



-- | VkImageMemoryRequirementsInfo2 - (None)
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2KHR'
data ImageMemoryRequirementsInfo2 = ImageMemoryRequirementsInfo2
  { -- Univalued member elided
  -- No documentation found for Nested "ImageMemoryRequirementsInfo2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageMemoryRequirementsInfo2" "image"
  image :: Image
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageMemoryRequirementsInfo2' and
-- marshal a 'ImageMemoryRequirementsInfo2' into it. The 'VkImageMemoryRequirementsInfo2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageMemoryRequirementsInfo2 :: ImageMemoryRequirementsInfo2 -> (VkImageMemoryRequirementsInfo2 -> IO a) -> IO a
withCStructImageMemoryRequirementsInfo2 marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImageMemoryRequirementsInfo2)) (\pPNext -> cont (VkImageMemoryRequirementsInfo2 VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 pPNext (image (marshalled :: ImageMemoryRequirementsInfo2))))

-- | A function to read a 'VkImageMemoryRequirementsInfo2' and all additional
-- structures in the pointer chain into a 'ImageMemoryRequirementsInfo2'.
fromCStructImageMemoryRequirementsInfo2 :: VkImageMemoryRequirementsInfo2 -> IO ImageMemoryRequirementsInfo2
fromCStructImageMemoryRequirementsInfo2 c = ImageMemoryRequirementsInfo2 <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageMemoryRequirementsInfo2)))
                                                                         <*> pure (vkImage (c :: VkImageMemoryRequirementsInfo2))

instance Zero ImageMemoryRequirementsInfo2 where
  zero = ImageMemoryRequirementsInfo2 Nothing
                                      zero



-- | VkImageSparseMemoryRequirementsInfo2 - (None)
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageSparseMemoryRequirements2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_memory_requirements2.vkGetImageSparseMemoryRequirements2KHR'
data ImageSparseMemoryRequirementsInfo2 = ImageSparseMemoryRequirementsInfo2
  { -- Univalued member elided
  -- No documentation found for Nested "ImageSparseMemoryRequirementsInfo2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageSparseMemoryRequirementsInfo2" "image"
  image :: Image
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageSparseMemoryRequirementsInfo2' and
-- marshal a 'ImageSparseMemoryRequirementsInfo2' into it. The 'VkImageSparseMemoryRequirementsInfo2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageSparseMemoryRequirementsInfo2 :: ImageSparseMemoryRequirementsInfo2 -> (VkImageSparseMemoryRequirementsInfo2 -> IO a) -> IO a
withCStructImageSparseMemoryRequirementsInfo2 marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImageSparseMemoryRequirementsInfo2)) (\pPNext -> cont (VkImageSparseMemoryRequirementsInfo2 VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 pPNext (image (marshalled :: ImageSparseMemoryRequirementsInfo2))))

-- | A function to read a 'VkImageSparseMemoryRequirementsInfo2' and all additional
-- structures in the pointer chain into a 'ImageSparseMemoryRequirementsInfo2'.
fromCStructImageSparseMemoryRequirementsInfo2 :: VkImageSparseMemoryRequirementsInfo2 -> IO ImageSparseMemoryRequirementsInfo2
fromCStructImageSparseMemoryRequirementsInfo2 c = ImageSparseMemoryRequirementsInfo2 <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageSparseMemoryRequirementsInfo2)))
                                                                                     <*> pure (vkImage (c :: VkImageSparseMemoryRequirementsInfo2))

instance Zero ImageSparseMemoryRequirementsInfo2 where
  zero = ImageSparseMemoryRequirementsInfo2 Nothing
                                            zero



-- | VkMemoryRequirements2 - Structure specifying memory requirements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2KHR',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2KHR'
data MemoryRequirements2 = MemoryRequirements2
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryRequirements2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryRequirements2" "memoryRequirements"
  memoryRequirements :: MemoryRequirements
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryRequirements2' and
-- marshal a 'MemoryRequirements2' into it. The 'VkMemoryRequirements2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryRequirements2 :: MemoryRequirements2 -> (VkMemoryRequirements2 -> IO a) -> IO a
withCStructMemoryRequirements2 marshalled cont = withCStructMemoryRequirements (memoryRequirements (marshalled :: MemoryRequirements2)) (\memoryRequirements'' -> maybeWith withSomeVkStruct (next (marshalled :: MemoryRequirements2)) (\pPNext -> cont (VkMemoryRequirements2 VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 pPNext memoryRequirements'')))

-- | A function to read a 'VkMemoryRequirements2' and all additional
-- structures in the pointer chain into a 'MemoryRequirements2'.
fromCStructMemoryRequirements2 :: VkMemoryRequirements2 -> IO MemoryRequirements2
fromCStructMemoryRequirements2 c = MemoryRequirements2 <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryRequirements2)))
                                                       <*> (fromCStructMemoryRequirements (vkMemoryRequirements (c :: VkMemoryRequirements2)))

instance Zero MemoryRequirements2 where
  zero = MemoryRequirements2 Nothing
                             zero


type MemoryRequirements2KHR = MemoryRequirements2
-- TODO: Pattern constructor alias)


-- | VkSparseImageMemoryRequirements2 - (None)
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageSparseMemoryRequirements2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_memory_requirements2.vkGetImageSparseMemoryRequirements2KHR'
data SparseImageMemoryRequirements2 = SparseImageMemoryRequirements2
  { -- Univalued member elided
  -- No documentation found for Nested "SparseImageMemoryRequirements2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SparseImageMemoryRequirements2" "memoryRequirements"
  memoryRequirements :: SparseImageMemoryRequirements
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSparseImageMemoryRequirements2' and
-- marshal a 'SparseImageMemoryRequirements2' into it. The 'VkSparseImageMemoryRequirements2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSparseImageMemoryRequirements2 :: SparseImageMemoryRequirements2 -> (VkSparseImageMemoryRequirements2 -> IO a) -> IO a
withCStructSparseImageMemoryRequirements2 marshalled cont = withCStructSparseImageMemoryRequirements (memoryRequirements (marshalled :: SparseImageMemoryRequirements2)) (\memoryRequirements'' -> maybeWith withSomeVkStruct (next (marshalled :: SparseImageMemoryRequirements2)) (\pPNext -> cont (VkSparseImageMemoryRequirements2 VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 pPNext memoryRequirements'')))

-- | A function to read a 'VkSparseImageMemoryRequirements2' and all additional
-- structures in the pointer chain into a 'SparseImageMemoryRequirements2'.
fromCStructSparseImageMemoryRequirements2 :: VkSparseImageMemoryRequirements2 -> IO SparseImageMemoryRequirements2
fromCStructSparseImageMemoryRequirements2 c = SparseImageMemoryRequirements2 <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSparseImageMemoryRequirements2)))
                                                                             <*> (fromCStructSparseImageMemoryRequirements (vkMemoryRequirements (c :: VkSparseImageMemoryRequirements2)))

instance Zero SparseImageMemoryRequirements2 where
  zero = SparseImageMemoryRequirements2 Nothing
                                        zero



-- | vkGetBufferMemoryRequirements2 - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the buffer.
--
-- -   @pInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkBufferMemoryRequirementsInfo2'
--     structure containing parameters required for the memory requirements
--     query.
--
-- -   @pMemoryRequirements@ points to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
--     structure in which the memory requirements of the buffer object are
--     returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkBufferMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
getBufferMemoryRequirements2 :: Device ->  BufferMemoryRequirementsInfo2 ->  IO (MemoryRequirements2)
getBufferMemoryRequirements2 = \(Device device' commandTable) -> \info' -> alloca (\pMemoryRequirements' -> (\marshalled -> withCStructBufferMemoryRequirementsInfo2 marshalled . flip with) info' (\pInfo' -> vkGetBufferMemoryRequirements2 commandTable device' pInfo' pMemoryRequirements' *> ((fromCStructMemoryRequirements2 <=< peek) pMemoryRequirements')))


-- | vkGetImageMemoryRequirements2 - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @pInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageMemoryRequirementsInfo2'
--     structure containing parameters required for the memory requirements
--     query.
--
-- -   @pMemoryRequirements@ points to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
--     structure in which the memory requirements of the image object are
--     returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
getImageMemoryRequirements2 :: Device ->  ImageMemoryRequirementsInfo2 ->  IO (MemoryRequirements2)
getImageMemoryRequirements2 = \(Device device' commandTable) -> \info' -> alloca (\pMemoryRequirements' -> (\marshalled -> withCStructImageMemoryRequirementsInfo2 marshalled . flip with) info' (\pInfo' -> vkGetImageMemoryRequirements2 commandTable device' pInfo' pMemoryRequirements' *> ((fromCStructMemoryRequirements2 <=< peek) pMemoryRequirements')))


-- | vkGetImageSparseMemoryRequirements2 - Query the memory requirements for
-- a sparse image
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @pInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageSparseMemoryRequirementsInfo2'
--     structure containing parameters required for the memory requirements
--     query.
--
-- -   @pSparseMemoryRequirementCount@ is a pointer to an integer related
--     to the number of sparse memory requirements available or queried, as
--     described below.
--
-- -   @pSparseMemoryRequirements@ is either @NULL@ or a pointer to an
--     array of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkSparseImageMemoryRequirements2'
--     structures.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageSparseMemoryRequirementsInfo2'
--     structure
--
-- -   @pSparseMemoryRequirementCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pSparseMemoryRequirementCount@ is not
--     @0@, and @pSparseMemoryRequirements@ is not @NULL@,
--     @pSparseMemoryRequirements@ /must/ be a valid pointer to an array of
--     @pSparseMemoryRequirementCount@
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkSparseImageMemoryRequirements2'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageSparseMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkSparseImageMemoryRequirements2'
getNumImageSparseMemoryRequirements2 :: Device ->  ImageSparseMemoryRequirementsInfo2 ->  IO (Word32)
getNumImageSparseMemoryRequirements2 = \(Device device' commandTable) -> \info' -> alloca (\pSparseMemoryRequirementCount' -> (\marshalled -> withCStructImageSparseMemoryRequirementsInfo2 marshalled . flip with) info' (\pInfo' -> vkGetImageSparseMemoryRequirements2 commandTable device' pInfo' pSparseMemoryRequirementCount' nullPtr *> (peek pSparseMemoryRequirementCount')))

-- | vkGetImageSparseMemoryRequirements2 - Query the memory requirements for
-- a sparse image
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @pInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageSparseMemoryRequirementsInfo2'
--     structure containing parameters required for the memory requirements
--     query.
--
-- -   @pSparseMemoryRequirementCount@ is a pointer to an integer related
--     to the number of sparse memory requirements available or queried, as
--     described below.
--
-- -   @pSparseMemoryRequirements@ is either @NULL@ or a pointer to an
--     array of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkSparseImageMemoryRequirements2'
--     structures.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageSparseMemoryRequirementsInfo2'
--     structure
--
-- -   @pSparseMemoryRequirementCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pSparseMemoryRequirementCount@ is not
--     @0@, and @pSparseMemoryRequirements@ is not @NULL@,
--     @pSparseMemoryRequirements@ /must/ be a valid pointer to an array of
--     @pSparseMemoryRequirementCount@
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkSparseImageMemoryRequirements2'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageSparseMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkSparseImageMemoryRequirements2'
getImageSparseMemoryRequirements2 :: Device ->  ImageSparseMemoryRequirementsInfo2 ->  Word32 ->  IO (Vector SparseImageMemoryRequirements2)
getImageSparseMemoryRequirements2 = \(Device device' commandTable) -> \info' -> \sparseMemoryRequirementCount' -> allocaArray (fromIntegral sparseMemoryRequirementCount') (\pSparseMemoryRequirements' -> with sparseMemoryRequirementCount' (\pSparseMemoryRequirementCount' -> (\marshalled -> withCStructImageSparseMemoryRequirementsInfo2 marshalled . flip with) info' (\pInfo' -> vkGetImageSparseMemoryRequirements2 commandTable device' pInfo' pSparseMemoryRequirementCount' pSparseMemoryRequirements' *> ((flip Data.Vector.generateM ((\p -> fromCStructSparseImageMemoryRequirements2 <=< peekElemOff p) pSparseMemoryRequirements') =<< (fromIntegral <$> (peek pSparseMemoryRequirementCount')))))))
-- | Returns all the values available from 'getImageSparseMemoryRequirements2'.
getAllImageSparseMemoryRequirements2 :: Device ->  ImageSparseMemoryRequirementsInfo2 ->  IO (Vector SparseImageMemoryRequirements2)
getAllImageSparseMemoryRequirements2 device' pInfo' =
  getNumImageSparseMemoryRequirements2 device' pInfo'
    >>= \num -> getImageSparseMemoryRequirements2 device' pInfo' num

