{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Memory where

import {-# SOURCE #-} Graphics.Vulkan.Device( Device(..)
                                            )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , FunPtr(..)
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Core( VkFlags(..)
                           , StructureType(..)
                           , Result(..)
                           , VkDeviceSize(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

newtype DeviceMemory = DeviceMemory Word64
  deriving (Eq, Storable)

-- ** vkMapMemory
foreign import ccall "vkMapMemory" vkMapMemory ::
  Device ->
  DeviceMemory ->
    VkDeviceSize ->
      VkDeviceSize -> MemoryMapFlags -> Ptr (Ptr Void) -> IO Result

type PFN_vkInternalFreeNotification = FunPtr
  (Ptr Void ->
     CSize -> InternalAllocationType -> SystemAllocationScope -> IO ())


data AllocationCallbacks =
  AllocationCallbacks{ pUserData :: Ptr Void 
                     , pfnAllocation :: PFN_vkAllocationFunction 
                     , pfnReallocation :: PFN_vkReallocationFunction 
                     , pfnFree :: PFN_vkFreeFunction 
                     , pfnInternalAllocation :: PFN_vkInternalAllocationNotification 
                     , pfnInternalFree :: PFN_vkInternalFreeNotification 
                     }
  deriving (Eq)

instance Storable AllocationCallbacks where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = AllocationCallbacks <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
                                 <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (pUserData (poked :: AllocationCallbacks))
                *> poke (ptr `plusPtr` 8) (pfnAllocation (poked :: AllocationCallbacks))
                *> poke (ptr `plusPtr` 16) (pfnReallocation (poked :: AllocationCallbacks))
                *> poke (ptr `plusPtr` 24) (pfnFree (poked :: AllocationCallbacks))
                *> poke (ptr `plusPtr` 32) (pfnInternalAllocation (poked :: AllocationCallbacks))
                *> poke (ptr `plusPtr` 40) (pfnInternalFree (poked :: AllocationCallbacks))


-- ** vkInvalidateMappedMemoryRanges
foreign import ccall "vkInvalidateMappedMemoryRanges" vkInvalidateMappedMemoryRanges ::
  Device -> Word32 -> Ptr MappedMemoryRange -> IO Result

-- ** SystemAllocationScope

newtype SystemAllocationScope = SystemAllocationScope Int32
  deriving (Eq, Storable)

instance Show SystemAllocationScope where
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_COMMAND = showString "VK_SYSTEM_ALLOCATION_SCOPE_COMMAND"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_OBJECT = showString "VK_SYSTEM_ALLOCATION_SCOPE_OBJECT"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_CACHE = showString "VK_SYSTEM_ALLOCATION_SCOPE_CACHE"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_DEVICE = showString "VK_SYSTEM_ALLOCATION_SCOPE_DEVICE"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = showString "VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE"
  showsPrec p (SystemAllocationScope x) = showParen (p >= 11) (showString "SystemAllocationScope " . showsPrec 11 x)

instance Read SystemAllocationScope where
  readPrec = parens ( choose [ ("VK_SYSTEM_ALLOCATION_SCOPE_COMMAND", pure VK_SYSTEM_ALLOCATION_SCOPE_COMMAND)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_OBJECT", pure VK_SYSTEM_ALLOCATION_SCOPE_OBJECT)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_CACHE", pure VK_SYSTEM_ALLOCATION_SCOPE_CACHE)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_DEVICE", pure VK_SYSTEM_ALLOCATION_SCOPE_DEVICE)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE", pure VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SystemAllocationScope")
                        v <- step readPrec
                        pure (SystemAllocationScope v)
                        )
                    )


pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND = SystemAllocationScope 0

pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT = SystemAllocationScope 1

pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE = SystemAllocationScope 2

pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE = SystemAllocationScope 3

pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = SystemAllocationScope 4

-- ** vkFlushMappedMemoryRanges
foreign import ccall "vkFlushMappedMemoryRanges" vkFlushMappedMemoryRanges ::
  Device -> Word32 -> Ptr MappedMemoryRange -> IO Result

-- ** MemoryMapFlags
-- | Opaque flag
newtype MemoryMapFlags = MemoryMapFlags VkFlags
  deriving (Eq, Storable)

type PFN_vkInternalAllocationNotification = FunPtr
  (Ptr Void ->
     CSize -> InternalAllocationType -> SystemAllocationScope -> IO ())

-- ** vkFreeMemory
foreign import ccall "vkFreeMemory" vkFreeMemory ::
  Device -> DeviceMemory -> Ptr AllocationCallbacks -> IO ()

type PFN_vkReallocationFunction = FunPtr
  (Ptr Void ->
     Ptr Void ->
       CSize -> CSize -> SystemAllocationScope -> IO (Ptr Void))

-- ** vkUnmapMemory
foreign import ccall "vkUnmapMemory" vkUnmapMemory ::
  Device -> DeviceMemory -> IO ()

type PFN_vkAllocationFunction = FunPtr
  (Ptr Void ->
     CSize -> CSize -> SystemAllocationScope -> IO (Ptr Void))

-- ** InternalAllocationType

newtype InternalAllocationType = InternalAllocationType Int32
  deriving (Eq, Storable)

instance Show InternalAllocationType where
  showsPrec _ VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = showString "VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE"
  showsPrec p (InternalAllocationType x) = showParen (p >= 11) (showString "InternalAllocationType " . showsPrec 11 x)

instance Read InternalAllocationType where
  readPrec = parens ( choose [ ("VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE", pure VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "InternalAllocationType")
                        v <- step readPrec
                        pure (InternalAllocationType v)
                        )
                    )


pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = InternalAllocationType 0

type PFN_vkFreeFunction = FunPtr (Ptr Void -> Ptr Void -> IO ())

-- ** vkGetDeviceMemoryCommitment
foreign import ccall "vkGetDeviceMemoryCommitment" vkGetDeviceMemoryCommitment ::
  Device -> DeviceMemory -> Ptr VkDeviceSize -> IO ()

-- ** vkAllocateMemory
foreign import ccall "vkAllocateMemory" vkAllocateMemory ::
  Device ->
  Ptr MemoryAllocateInfo ->
    Ptr AllocationCallbacks -> Ptr DeviceMemory -> IO Result


data MappedMemoryRange =
  MappedMemoryRange{ sType :: StructureType 
                   , pNext :: Ptr Void 
                   , memory :: DeviceMemory 
                   , offset :: VkDeviceSize 
                   , size :: VkDeviceSize 
                   }
  deriving (Eq)

instance Storable MappedMemoryRange where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = MappedMemoryRange <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 24)
                               <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: MappedMemoryRange))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: MappedMemoryRange))
                *> poke (ptr `plusPtr` 16) (memory (poked :: MappedMemoryRange))
                *> poke (ptr `plusPtr` 24) (offset (poked :: MappedMemoryRange))
                *> poke (ptr `plusPtr` 32) (size (poked :: MappedMemoryRange))



data MemoryAllocateInfo =
  MemoryAllocateInfo{ sType :: StructureType 
                    , pNext :: Ptr Void 
                    , allocationSize :: VkDeviceSize 
                    , memoryTypeIndex :: Word32 
                    }
  deriving (Eq)

instance Storable MemoryAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = MemoryAllocateInfo <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: MemoryAllocateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: MemoryAllocateInfo))
                *> poke (ptr `plusPtr` 16) (allocationSize (poked :: MemoryAllocateInfo))
                *> poke (ptr `plusPtr` 24) (memoryTypeIndex (poked :: MemoryAllocateInfo))


