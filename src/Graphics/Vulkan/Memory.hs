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
import Graphics.Vulkan.Core( StructureType(..)
                           , Result(..)
                           , DeviceSize(..)
                           , Flags(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

newtype DeviceMemory = DeviceMemory Word64
  deriving (Eq, Storable)

-- ** mapMemory
foreign import ccall "vkMapMemory" mapMemory ::
  Device ->
  DeviceMemory ->
    DeviceSize ->
      DeviceSize -> MemoryMapFlags -> Ptr (Ptr Void) -> IO Result

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


-- ** invalidateMappedMemoryRanges
foreign import ccall "vkInvalidateMappedMemoryRanges" invalidateMappedMemoryRanges ::
  Device -> Word32 -> Ptr MappedMemoryRange -> IO Result

-- ** SystemAllocationScope

newtype SystemAllocationScope = SystemAllocationScope Int32
  deriving (Eq, Storable)

instance Show SystemAllocationScope where
  showsPrec _ SystemAllocationScopeCommand = showString "SystemAllocationScopeCommand"
  showsPrec _ SystemAllocationScopeObject = showString "SystemAllocationScopeObject"
  showsPrec _ SystemAllocationScopeCache = showString "SystemAllocationScopeCache"
  showsPrec _ SystemAllocationScopeDevice = showString "SystemAllocationScopeDevice"
  showsPrec _ SystemAllocationScopeInstance = showString "SystemAllocationScopeInstance"
  showsPrec p (SystemAllocationScope x) = showParen (p >= 11) (showString "SystemAllocationScope " . showsPrec 11 x)

instance Read SystemAllocationScope where
  readPrec = parens ( choose [ ("SystemAllocationScopeCommand", pure SystemAllocationScopeCommand)
                             , ("SystemAllocationScopeObject", pure SystemAllocationScopeObject)
                             , ("SystemAllocationScopeCache", pure SystemAllocationScopeCache)
                             , ("SystemAllocationScopeDevice", pure SystemAllocationScopeDevice)
                             , ("SystemAllocationScopeInstance", pure SystemAllocationScopeInstance)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SystemAllocationScope")
                        v <- step readPrec
                        pure (SystemAllocationScope v)
                        )
                    )


pattern SystemAllocationScopeCommand = SystemAllocationScope 0

pattern SystemAllocationScopeObject = SystemAllocationScope 1

pattern SystemAllocationScopeCache = SystemAllocationScope 2

pattern SystemAllocationScopeDevice = SystemAllocationScope 3

pattern SystemAllocationScopeInstance = SystemAllocationScope 4

-- ** flushMappedMemoryRanges
foreign import ccall "vkFlushMappedMemoryRanges" flushMappedMemoryRanges ::
  Device -> Word32 -> Ptr MappedMemoryRange -> IO Result

-- ** MemoryMapFlags
-- | Opaque flag
newtype MemoryMapFlags = MemoryMapFlags Flags
  deriving (Eq, Storable)

type PFN_vkInternalAllocationNotification = FunPtr
  (Ptr Void ->
     CSize -> InternalAllocationType -> SystemAllocationScope -> IO ())

-- ** freeMemory
foreign import ccall "vkFreeMemory" freeMemory ::
  Device -> DeviceMemory -> Ptr AllocationCallbacks -> IO ()

type PFN_vkReallocationFunction = FunPtr
  (Ptr Void ->
     Ptr Void ->
       CSize -> CSize -> SystemAllocationScope -> IO (Ptr Void))

-- ** unmapMemory
foreign import ccall "vkUnmapMemory" unmapMemory ::
  Device -> DeviceMemory -> IO ()

type PFN_vkAllocationFunction = FunPtr
  (Ptr Void ->
     CSize -> CSize -> SystemAllocationScope -> IO (Ptr Void))

-- ** InternalAllocationType

newtype InternalAllocationType = InternalAllocationType Int32
  deriving (Eq, Storable)

instance Show InternalAllocationType where
  showsPrec _ InternalAllocationTypeExecutable = showString "InternalAllocationTypeExecutable"
  showsPrec p (InternalAllocationType x) = showParen (p >= 11) (showString "InternalAllocationType " . showsPrec 11 x)

instance Read InternalAllocationType where
  readPrec = parens ( choose [ ("InternalAllocationTypeExecutable", pure InternalAllocationTypeExecutable)
                             ] +++
                      prec 10 (do
                        expectP (Ident "InternalAllocationType")
                        v <- step readPrec
                        pure (InternalAllocationType v)
                        )
                    )


pattern InternalAllocationTypeExecutable = InternalAllocationType 0

type PFN_vkFreeFunction = FunPtr (Ptr Void -> Ptr Void -> IO ())

-- ** getDeviceMemoryCommitment
foreign import ccall "vkGetDeviceMemoryCommitment" getDeviceMemoryCommitment ::
  Device -> DeviceMemory -> Ptr DeviceSize -> IO ()

-- ** allocateMemory
foreign import ccall "vkAllocateMemory" allocateMemory ::
  Device ->
  Ptr MemoryAllocateInfo ->
    Ptr AllocationCallbacks -> Ptr DeviceMemory -> IO Result


data MappedMemoryRange =
  MappedMemoryRange{ sType :: StructureType 
                   , pNext :: Ptr Void 
                   , memory :: DeviceMemory 
                   , offset :: DeviceSize 
                   , size :: DeviceSize 
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
                    , allocationSize :: DeviceSize 
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


