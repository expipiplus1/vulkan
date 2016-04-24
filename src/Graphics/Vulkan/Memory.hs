{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Memory where

import {-# SOURCE #-} Graphics.Vulkan.Device( VkDevice(..)
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
import Data.Bits( Bits
                , FiniteBits
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

newtype VkDeviceMemory = VkDeviceMemory Word64
  deriving (Eq, Ord, Storable)

-- ** vkMapMemory
foreign import ccall "vkMapMemory" vkMapMemory ::
  VkDevice ->
  VkDeviceMemory ->
    VkDeviceSize ->
      VkDeviceSize -> VkMemoryMapFlags -> Ptr (Ptr Void) -> IO VkResult

type PFN_vkInternalFreeNotification = FunPtr
  (Ptr Void ->
     CSize ->
       VkInternalAllocationType -> VkSystemAllocationScope -> IO ())


data VkAllocationCallbacks =
  VkAllocationCallbacks{ vkPUserData :: Ptr Void 
                       , vkPfnAllocation :: PFN_vkAllocationFunction 
                       , vkPfnReallocation :: PFN_vkReallocationFunction 
                       , vkPfnFree :: PFN_vkFreeFunction 
                       , vkPfnInternalAllocation :: PFN_vkInternalAllocationNotification 
                       , vkPfnInternalFree :: PFN_vkInternalFreeNotification 
                       }
  deriving (Eq, Ord)

instance Storable VkAllocationCallbacks where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkAllocationCallbacks <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPUserData (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 8) (vkPfnAllocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 16) (vkPfnReallocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 24) (vkPfnFree (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 32) (vkPfnInternalAllocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 40) (vkPfnInternalFree (poked :: VkAllocationCallbacks))


-- ** vkInvalidateMappedMemoryRanges
foreign import ccall "vkInvalidateMappedMemoryRanges" vkInvalidateMappedMemoryRanges ::
  VkDevice -> Word32 -> Ptr VkMappedMemoryRange -> IO VkResult

-- ** VkSystemAllocationScope

newtype VkSystemAllocationScope = VkSystemAllocationScope Int32
  deriving (Eq, Ord, Storable)

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
  VkDevice -> Word32 -> Ptr VkMappedMemoryRange -> IO VkResult

-- ** VkMemoryMapFlags
-- | Opaque flag
newtype VkMemoryMapFlags = VkMemoryMapFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

type PFN_vkInternalAllocationNotification = FunPtr
  (Ptr Void ->
     CSize ->
       VkInternalAllocationType -> VkSystemAllocationScope -> IO ())

-- ** vkFreeMemory
foreign import ccall "vkFreeMemory" vkFreeMemory ::
  VkDevice -> VkDeviceMemory -> Ptr VkAllocationCallbacks -> IO ()

type PFN_vkReallocationFunction = FunPtr
  (Ptr Void ->
     Ptr Void ->
       CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))

-- ** vkUnmapMemory
foreign import ccall "vkUnmapMemory" vkUnmapMemory ::
  VkDevice -> VkDeviceMemory -> IO ()

type PFN_vkAllocationFunction = FunPtr
  (Ptr Void ->
     CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))

-- ** VkInternalAllocationType

newtype VkInternalAllocationType = VkInternalAllocationType Int32
  deriving (Eq, Ord, Storable)

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
  VkDevice -> VkDeviceMemory -> Ptr VkDeviceSize -> IO ()

-- ** vkAllocateMemory
foreign import ccall "vkAllocateMemory" vkAllocateMemory ::
  VkDevice ->
  Ptr VkMemoryAllocateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkDeviceMemory -> IO VkResult


data VkMappedMemoryRange =
  VkMappedMemoryRange{ vkSType :: VkStructureType 
                     , vkPNext :: Ptr Void 
                     , vkMemory :: VkDeviceMemory 
                     , vkOffset :: VkDeviceSize 
                     , vkSize :: VkDeviceSize 
                     }
  deriving (Eq, Ord)

instance Storable VkMappedMemoryRange where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkMappedMemoryRange <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 24) (vkOffset (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 32) (vkSize (poked :: VkMappedMemoryRange))



data VkMemoryAllocateInfo =
  VkMemoryAllocateInfo{ vkSType :: VkStructureType 
                      , vkPNext :: Ptr Void 
                      , vkAllocationSize :: VkDeviceSize 
                      , vkMemoryTypeIndex :: Word32 
                      }
  deriving (Eq, Ord)

instance Storable VkMemoryAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMemoryAllocateInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkAllocationSize (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkMemoryTypeIndex (poked :: VkMemoryAllocateInfo))


