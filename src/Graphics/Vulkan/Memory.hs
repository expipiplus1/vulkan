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
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , FunPtr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkDeviceSize(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize
                      , CSize(..)
                      )

newtype DeviceMemory = DeviceMemory Word64
  deriving (Eq, Storable)

-- ** vkMapMemory
foreign import ccall "vkMapMemory" vkMapMemory ::
  Device ->
  DeviceMemory ->
    VkDeviceSize ->
      VkDeviceSize -> VkMemoryMapFlags -> Ptr (Ptr Void) -> IO VkResult

type PFN_vkInternalFreeNotification = FunPtr
  (Ptr Void ->
     CSize ->
       VkInternalAllocationType -> VkSystemAllocationScope -> IO ())


data VkAllocationCallbacks =
  VkAllocationCallbacks{ pUserData :: Ptr Void 
                       , pfnAllocation :: PFN_vkAllocationFunction 
                       , pfnReallocation :: PFN_vkReallocationFunction 
                       , pfnFree :: PFN_vkFreeFunction 
                       , pfnInternalAllocation :: PFN_vkInternalAllocationNotification 
                       , pfnInternalFree :: PFN_vkInternalFreeNotification 
                       }
  deriving (Eq)

instance Storable VkAllocationCallbacks where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkAllocationCallbacks <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (pUserData (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 8) (pfnAllocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 16) (pfnReallocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 24) (pfnFree (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 32) (pfnInternalAllocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 40) (pfnInternalFree (poked :: VkAllocationCallbacks))


-- ** vkInvalidateMappedMemoryRanges
foreign import ccall "vkInvalidateMappedMemoryRanges" vkInvalidateMappedMemoryRanges ::
  Device -> Word32 -> Ptr VkMappedMemoryRange -> IO VkResult

-- ** VkSystemAllocationScope

newtype VkSystemAllocationScope = VkSystemAllocationScope Int32
  deriving (Eq, Storable)

instance Show VkSystemAllocationScope where
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_COMMAND = showString "VK_SYSTEM_ALLOCATION_SCOPE_COMMAND"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_OBJECT = showString "VK_SYSTEM_ALLOCATION_SCOPE_OBJECT"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_CACHE = showString "VK_SYSTEM_ALLOCATION_SCOPE_CACHE"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_DEVICE = showString "VK_SYSTEM_ALLOCATION_SCOPE_DEVICE"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = showString "VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE"
  showsPrec p (VkSystemAllocationScope x) = showParen (p >= 11) (showString "VkSystemAllocationScope " . showsPrec 11 x)

instance Read VkSystemAllocationScope where
  readPrec = parens ( choose [ ("VK_SYSTEM_ALLOCATION_SCOPE_COMMAND", pure VK_SYSTEM_ALLOCATION_SCOPE_COMMAND)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_OBJECT", pure VK_SYSTEM_ALLOCATION_SCOPE_OBJECT)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_CACHE", pure VK_SYSTEM_ALLOCATION_SCOPE_CACHE)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_DEVICE", pure VK_SYSTEM_ALLOCATION_SCOPE_DEVICE)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE", pure VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSystemAllocationScope")
                        v <- step readPrec
                        pure (VkSystemAllocationScope v)
                        )
                    )


pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND = VkSystemAllocationScope 0

pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT = VkSystemAllocationScope 1

pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE = VkSystemAllocationScope 2

pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE = VkSystemAllocationScope 3

pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = VkSystemAllocationScope 4

-- ** vkFlushMappedMemoryRanges
foreign import ccall "vkFlushMappedMemoryRanges" vkFlushMappedMemoryRanges ::
  Device -> Word32 -> Ptr VkMappedMemoryRange -> IO VkResult

-- ** VkMemoryMapFlags
-- | Opaque flag
newtype VkMemoryMapFlags = VkMemoryMapFlags VkFlags
  deriving (Eq, Storable)

type PFN_vkInternalAllocationNotification = FunPtr
  (Ptr Void ->
     CSize ->
       VkInternalAllocationType -> VkSystemAllocationScope -> IO ())

-- ** vkFreeMemory
foreign import ccall "vkFreeMemory" vkFreeMemory ::
  Device -> DeviceMemory -> Ptr VkAllocationCallbacks -> IO ()

type PFN_vkReallocationFunction = FunPtr
  (Ptr Void ->
     Ptr Void ->
       CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))

-- ** vkUnmapMemory
foreign import ccall "vkUnmapMemory" vkUnmapMemory ::
  Device -> DeviceMemory -> IO ()

type PFN_vkAllocationFunction = FunPtr
  (Ptr Void ->
     CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))

-- ** VkInternalAllocationType

newtype VkInternalAllocationType = VkInternalAllocationType Int32
  deriving (Eq, Storable)

instance Show VkInternalAllocationType where
  showsPrec _ VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = showString "VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE"
  showsPrec p (VkInternalAllocationType x) = showParen (p >= 11) (showString "VkInternalAllocationType " . showsPrec 11 x)

instance Read VkInternalAllocationType where
  readPrec = parens ( choose [ ("VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE", pure VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkInternalAllocationType")
                        v <- step readPrec
                        pure (VkInternalAllocationType v)
                        )
                    )


pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = VkInternalAllocationType 0

type PFN_vkFreeFunction = FunPtr (Ptr Void -> Ptr Void -> IO ())

-- ** vkGetDeviceMemoryCommitment
foreign import ccall "vkGetDeviceMemoryCommitment" vkGetDeviceMemoryCommitment ::
  Device -> DeviceMemory -> Ptr VkDeviceSize -> IO ()

-- ** vkAllocateMemory
foreign import ccall "vkAllocateMemory" vkAllocateMemory ::
  Device ->
  Ptr VkMemoryAllocateInfo ->
    Ptr VkAllocationCallbacks -> Ptr DeviceMemory -> IO VkResult


data VkMappedMemoryRange =
  VkMappedMemoryRange{ sType :: VkStructureType 
                     , pNext :: Ptr Void 
                     , memory :: DeviceMemory 
                     , offset :: VkDeviceSize 
                     , size :: VkDeviceSize 
                     }
  deriving (Eq)

instance Storable VkMappedMemoryRange where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkMappedMemoryRange <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 16) (memory (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 24) (offset (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 32) (size (poked :: VkMappedMemoryRange))



data VkMemoryAllocateInfo =
  VkMemoryAllocateInfo{ sType :: VkStructureType 
                      , pNext :: Ptr Void 
                      , allocationSize :: VkDeviceSize 
                      , memoryTypeIndex :: Word32 
                      }
  deriving (Eq)

instance Storable VkMemoryAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMemoryAllocateInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 16) (allocationSize (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 24) (memoryTypeIndex (poked :: VkMemoryAllocateInfo))


