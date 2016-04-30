{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Buffer where

import Graphics.Vulkan.Device( Device(..)
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
                  , plusPtr
                  )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Core( SharingMode(..)
                           , StructureType(..)
                           , Result(..)
                           , DeviceSize(..)
                           , Flags(..)
                           )

-- ** createBuffer
foreign import ccall "vkCreateBuffer" createBuffer ::
  Device ->
  Ptr BufferCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Buffer -> IO Result

-- ** BufferCreateFlags

newtype BufferCreateFlags = BufferCreateFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show BufferCreateFlags where
  showsPrec _ BufferCreateSparseBindingBit = showString "BufferCreateSparseBindingBit"
  showsPrec _ BufferCreateSparseResidencyBit = showString "BufferCreateSparseResidencyBit"
  showsPrec _ BufferCreateSparseAliasedBit = showString "BufferCreateSparseAliasedBit"
  
  showsPrec p (BufferCreateFlags x) = showParen (p >= 11) (showString "BufferCreateFlags " . showsPrec 11 x)

instance Read BufferCreateFlags where
  readPrec = parens ( choose [ ("BufferCreateSparseBindingBit", pure BufferCreateSparseBindingBit)
                             , ("BufferCreateSparseResidencyBit", pure BufferCreateSparseResidencyBit)
                             , ("BufferCreateSparseAliasedBit", pure BufferCreateSparseAliasedBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "BufferCreateFlags")
                        v <- step readPrec
                        pure (BufferCreateFlags v)
                        )
                    )

-- | Buffer should support sparse backing
pattern BufferCreateSparseBindingBit = BufferCreateFlags 0x1
-- | Buffer should support sparse backing with partial residency
pattern BufferCreateSparseResidencyBit = BufferCreateFlags 0x2
-- | Buffer should support constent data access to physical memory blocks mapped into multiple locations of sparse buffers
pattern BufferCreateSparseAliasedBit = BufferCreateFlags 0x4


-- ** BufferUsageFlags

newtype BufferUsageFlags = BufferUsageFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show BufferUsageFlags where
  showsPrec _ BufferUsageTransferSrcBit = showString "BufferUsageTransferSrcBit"
  showsPrec _ BufferUsageTransferDstBit = showString "BufferUsageTransferDstBit"
  showsPrec _ BufferUsageUniformTexelBufferBit = showString "BufferUsageUniformTexelBufferBit"
  showsPrec _ BufferUsageStorageTexelBufferBit = showString "BufferUsageStorageTexelBufferBit"
  showsPrec _ BufferUsageUniformBufferBit = showString "BufferUsageUniformBufferBit"
  showsPrec _ BufferUsageStorageBufferBit = showString "BufferUsageStorageBufferBit"
  showsPrec _ BufferUsageIndexBufferBit = showString "BufferUsageIndexBufferBit"
  showsPrec _ BufferUsageVertexBufferBit = showString "BufferUsageVertexBufferBit"
  showsPrec _ BufferUsageIndirectBufferBit = showString "BufferUsageIndirectBufferBit"
  
  showsPrec p (BufferUsageFlags x) = showParen (p >= 11) (showString "BufferUsageFlags " . showsPrec 11 x)

instance Read BufferUsageFlags where
  readPrec = parens ( choose [ ("BufferUsageTransferSrcBit", pure BufferUsageTransferSrcBit)
                             , ("BufferUsageTransferDstBit", pure BufferUsageTransferDstBit)
                             , ("BufferUsageUniformTexelBufferBit", pure BufferUsageUniformTexelBufferBit)
                             , ("BufferUsageStorageTexelBufferBit", pure BufferUsageStorageTexelBufferBit)
                             , ("BufferUsageUniformBufferBit", pure BufferUsageUniformBufferBit)
                             , ("BufferUsageStorageBufferBit", pure BufferUsageStorageBufferBit)
                             , ("BufferUsageIndexBufferBit", pure BufferUsageIndexBufferBit)
                             , ("BufferUsageVertexBufferBit", pure BufferUsageVertexBufferBit)
                             , ("BufferUsageIndirectBufferBit", pure BufferUsageIndirectBufferBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "BufferUsageFlags")
                        v <- step readPrec
                        pure (BufferUsageFlags v)
                        )
                    )

-- | Can be used as a source of transfer operations
pattern BufferUsageTransferSrcBit = BufferUsageFlags 0x1
-- | Can be used as a destination of transfer operations
pattern BufferUsageTransferDstBit = BufferUsageFlags 0x2
-- | Can be used as TBO
pattern BufferUsageUniformTexelBufferBit = BufferUsageFlags 0x4
-- | Can be used as IBO
pattern BufferUsageStorageTexelBufferBit = BufferUsageFlags 0x8
-- | Can be used as UBO
pattern BufferUsageUniformBufferBit = BufferUsageFlags 0x10
-- | Can be used as SSBO
pattern BufferUsageStorageBufferBit = BufferUsageFlags 0x20
-- | Can be used as source of fixed-function index fetch (index buffer)
pattern BufferUsageIndexBufferBit = BufferUsageFlags 0x40
-- | Can be used as source of fixed-function vertex fetch (VBO)
pattern BufferUsageVertexBufferBit = BufferUsageFlags 0x80
-- | Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)
pattern BufferUsageIndirectBufferBit = BufferUsageFlags 0x100


-- ** destroyBuffer
foreign import ccall "vkDestroyBuffer" destroyBuffer ::
  Device -> Buffer -> Ptr AllocationCallbacks -> IO ()

newtype Buffer = Buffer Word64
  deriving (Eq, Ord, Storable)


data BufferCreateInfo =
  BufferCreateInfo{ sType :: StructureType 
                  , pNext :: Ptr Void 
                  , flags :: BufferCreateFlags 
                  , size :: DeviceSize 
                  , usage :: BufferUsageFlags 
                  , sharingMode :: SharingMode 
                  , queueFamilyIndexCount :: Word32 
                  , pQueueFamilyIndices :: Ptr Word32 
                  }
  deriving (Eq, Ord)

instance Storable BufferCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = BufferCreateInfo <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 32)
                              <*> peek (ptr `plusPtr` 36)
                              <*> peek (ptr `plusPtr` 40)
                              <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 24) (size (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 32) (usage (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 36) (sharingMode (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 40) (queueFamilyIndexCount (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 48) (pQueueFamilyIndices (poked :: BufferCreateInfo))


