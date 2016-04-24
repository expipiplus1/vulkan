{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.SparseResourceMemoryManagement where

import Graphics.Vulkan.Device( Device(..)
                             , PhysicalDevice(..)
                             )
import Graphics.Vulkan.Buffer( Buffer(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Graphics.Vulkan.Queue( Queue(..)
                            )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Fence( Fence(..)
                            )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( DeviceMemory(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Sampler( SampleCountFlags(..)
                              )
import Graphics.Vulkan.Image( ImageAspectFlags(..)
                            , Image(..)
                            , ImageUsageFlags(..)
                            , ImageTiling(..)
                            , ImageType(..)
                            , ImageSubresource(..)
                            )
import Graphics.Vulkan.QueueSemaphore( Semaphore(..)
                                     )
import Graphics.Vulkan.Core( Offset3D(..)
                           , StructureType(..)
                           , Format(..)
                           , Extent3D(..)
                           , Result(..)
                           , DeviceSize(..)
                           , Flags(..)
                           )


data SparseImageMemoryRequirements =
  SparseImageMemoryRequirements{ formatProperties :: SparseImageFormatProperties 
                               , imageMipTailFirstLod :: Word32 
                               , imageMipTailSize :: DeviceSize 
                               , imageMipTailOffset :: DeviceSize 
                               , imageMipTailStride :: DeviceSize 
                               }
  deriving (Eq, Ord)

instance Storable SparseImageMemoryRequirements where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = SparseImageMemoryRequirements <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 20)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 32)
                                           <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (formatProperties (poked :: SparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 20) (imageMipTailFirstLod (poked :: SparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 24) (imageMipTailSize (poked :: SparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 32) (imageMipTailOffset (poked :: SparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 40) (imageMipTailStride (poked :: SparseImageMemoryRequirements))



data SparseMemoryBind =
  SparseMemoryBind{ resourceOffset :: DeviceSize 
                  , size :: DeviceSize 
                  , memory :: DeviceMemory 
                  , memoryOffset :: DeviceSize 
                  , flags :: SparseMemoryBindFlags 
                  }
  deriving (Eq, Ord)

instance Storable SparseMemoryBind where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = SparseMemoryBind <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (resourceOffset (poked :: SparseMemoryBind))
                *> poke (ptr `plusPtr` 8) (size (poked :: SparseMemoryBind))
                *> poke (ptr `plusPtr` 16) (memory (poked :: SparseMemoryBind))
                *> poke (ptr `plusPtr` 24) (memoryOffset (poked :: SparseMemoryBind))
                *> poke (ptr `plusPtr` 32) (flags (poked :: SparseMemoryBind))



data SparseImageMemoryBind =
  SparseImageMemoryBind{ subresource :: ImageSubresource 
                       , offset :: Offset3D 
                       , extent :: Extent3D 
                       , memory :: DeviceMemory 
                       , memoryOffset :: DeviceSize 
                       , flags :: SparseMemoryBindFlags 
                       }
  deriving (Eq, Ord)

instance Storable SparseImageMemoryBind where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = SparseImageMemoryBind <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 12)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 40)
                                   <*> peek (ptr `plusPtr` 48)
                                   <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (subresource (poked :: SparseImageMemoryBind))
                *> poke (ptr `plusPtr` 12) (offset (poked :: SparseImageMemoryBind))
                *> poke (ptr `plusPtr` 24) (extent (poked :: SparseImageMemoryBind))
                *> poke (ptr `plusPtr` 40) (memory (poked :: SparseImageMemoryBind))
                *> poke (ptr `plusPtr` 48) (memoryOffset (poked :: SparseImageMemoryBind))
                *> poke (ptr `plusPtr` 56) (flags (poked :: SparseImageMemoryBind))



data SparseImageMemoryBindInfo =
  SparseImageMemoryBindInfo{ image :: Image 
                           , bindCount :: Word32 
                           , pBinds :: Ptr SparseImageMemoryBind 
                           }
  deriving (Eq, Ord)

instance Storable SparseImageMemoryBindInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = SparseImageMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (image (poked :: SparseImageMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (bindCount (poked :: SparseImageMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (pBinds (poked :: SparseImageMemoryBindInfo))


-- ** getImageSparseMemoryRequirements
foreign import ccall "vkGetImageSparseMemoryRequirements" getImageSparseMemoryRequirements ::
  Device ->
  Image -> Ptr Word32 -> Ptr SparseImageMemoryRequirements -> IO ()

-- ** queueBindSparse
foreign import ccall "vkQueueBindSparse" queueBindSparse ::
  Queue -> Word32 -> Ptr BindSparseInfo -> Fence -> IO Result


data BindSparseInfo =
  BindSparseInfo{ sType :: StructureType 
                , pNext :: Ptr Void 
                , waitSemaphoreCount :: Word32 
                , pWaitSemaphores :: Ptr Semaphore 
                , bufferBindCount :: Word32 
                , pBufferBinds :: Ptr SparseBufferMemoryBindInfo 
                , imageOpaqueBindCount :: Word32 
                , pImageOpaqueBinds :: Ptr SparseImageOpaqueMemoryBindInfo 
                , imageBindCount :: Word32 
                , pImageBinds :: Ptr SparseImageMemoryBindInfo 
                , signalSemaphoreCount :: Word32 
                , pSignalSemaphores :: Ptr Semaphore 
                }
  deriving (Eq, Ord)

instance Storable BindSparseInfo where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = BindSparseInfo <$> peek (ptr `plusPtr` 0)
                            <*> peek (ptr `plusPtr` 8)
                            <*> peek (ptr `plusPtr` 16)
                            <*> peek (ptr `plusPtr` 24)
                            <*> peek (ptr `plusPtr` 32)
                            <*> peek (ptr `plusPtr` 40)
                            <*> peek (ptr `plusPtr` 48)
                            <*> peek (ptr `plusPtr` 56)
                            <*> peek (ptr `plusPtr` 64)
                            <*> peek (ptr `plusPtr` 72)
                            <*> peek (ptr `plusPtr` 80)
                            <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: BindSparseInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: BindSparseInfo))
                *> poke (ptr `plusPtr` 16) (waitSemaphoreCount (poked :: BindSparseInfo))
                *> poke (ptr `plusPtr` 24) (pWaitSemaphores (poked :: BindSparseInfo))
                *> poke (ptr `plusPtr` 32) (bufferBindCount (poked :: BindSparseInfo))
                *> poke (ptr `plusPtr` 40) (pBufferBinds (poked :: BindSparseInfo))
                *> poke (ptr `plusPtr` 48) (imageOpaqueBindCount (poked :: BindSparseInfo))
                *> poke (ptr `plusPtr` 56) (pImageOpaqueBinds (poked :: BindSparseInfo))
                *> poke (ptr `plusPtr` 64) (imageBindCount (poked :: BindSparseInfo))
                *> poke (ptr `plusPtr` 72) (pImageBinds (poked :: BindSparseInfo))
                *> poke (ptr `plusPtr` 80) (signalSemaphoreCount (poked :: BindSparseInfo))
                *> poke (ptr `plusPtr` 88) (pSignalSemaphores (poked :: BindSparseInfo))



data SparseBufferMemoryBindInfo =
  SparseBufferMemoryBindInfo{ buffer :: Buffer 
                            , bindCount :: Word32 
                            , pBinds :: Ptr SparseMemoryBind 
                            }
  deriving (Eq, Ord)

instance Storable SparseBufferMemoryBindInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = SparseBufferMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (buffer (poked :: SparseBufferMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (bindCount (poked :: SparseBufferMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (pBinds (poked :: SparseBufferMemoryBindInfo))


-- ** SparseImageFormatFlags

newtype SparseImageFormatFlags = SparseImageFormatFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show SparseImageFormatFlags where
  showsPrec _ SparseImageFormatSingleMiptailBit = showString "SparseImageFormatSingleMiptailBit"
  showsPrec _ SparseImageFormatAlignedMipSizeBit = showString "SparseImageFormatAlignedMipSizeBit"
  showsPrec _ SparseImageFormatNonstandardBlockSizeBit = showString "SparseImageFormatNonstandardBlockSizeBit"
  
  showsPrec p (SparseImageFormatFlags x) = showParen (p >= 11) (showString "SparseImageFormatFlags " . showsPrec 11 x)

instance Read SparseImageFormatFlags where
  readPrec = parens ( choose [ ("SparseImageFormatSingleMiptailBit", pure SparseImageFormatSingleMiptailBit)
                             , ("SparseImageFormatAlignedMipSizeBit", pure SparseImageFormatAlignedMipSizeBit)
                             , ("SparseImageFormatNonstandardBlockSizeBit", pure SparseImageFormatNonstandardBlockSizeBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SparseImageFormatFlags")
                        v <- step readPrec
                        pure (SparseImageFormatFlags v)
                        )
                    )

-- | Image uses a single miptail region for all array layers
pattern SparseImageFormatSingleMiptailBit = SparseImageFormatFlags 0x1
-- | Image requires mip levels to be an exact multiple of the sparse image block size for non-miptail levels.
pattern SparseImageFormatAlignedMipSizeBit = SparseImageFormatFlags 0x2
-- | Image uses a non-standard sparse block size
pattern SparseImageFormatNonstandardBlockSizeBit = SparseImageFormatFlags 0x4


-- ** getPhysicalDeviceSparseImageFormatProperties
foreign import ccall "vkGetPhysicalDeviceSparseImageFormatProperties" getPhysicalDeviceSparseImageFormatProperties ::
  PhysicalDevice ->
  Format ->
    ImageType ->
      SampleCountFlags ->
        ImageUsageFlags ->
          ImageTiling ->
            Ptr Word32 -> Ptr SparseImageFormatProperties -> IO ()

-- ** SparseMemoryBindFlags

newtype SparseMemoryBindFlags = SparseMemoryBindFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show SparseMemoryBindFlags where
  showsPrec _ SparseMemoryBindMetadataBit = showString "SparseMemoryBindMetadataBit"
  
  showsPrec p (SparseMemoryBindFlags x) = showParen (p >= 11) (showString "SparseMemoryBindFlags " . showsPrec 11 x)

instance Read SparseMemoryBindFlags where
  readPrec = parens ( choose [ ("SparseMemoryBindMetadataBit", pure SparseMemoryBindMetadataBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SparseMemoryBindFlags")
                        v <- step readPrec
                        pure (SparseMemoryBindFlags v)
                        )
                    )

-- | Operation binds resource metadata to memory
pattern SparseMemoryBindMetadataBit = SparseMemoryBindFlags 0x1



data SparseImageOpaqueMemoryBindInfo =
  SparseImageOpaqueMemoryBindInfo{ image :: Image 
                                 , bindCount :: Word32 
                                 , pBinds :: Ptr SparseMemoryBind 
                                 }
  deriving (Eq, Ord)

instance Storable SparseImageOpaqueMemoryBindInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = SparseImageOpaqueMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (image (poked :: SparseImageOpaqueMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (bindCount (poked :: SparseImageOpaqueMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (pBinds (poked :: SparseImageOpaqueMemoryBindInfo))



data SparseImageFormatProperties =
  SparseImageFormatProperties{ aspectMask :: ImageAspectFlags 
                             , imageGranularity :: Extent3D 
                             , flags :: SparseImageFormatFlags 
                             }
  deriving (Eq, Ord)

instance Storable SparseImageFormatProperties where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = SparseImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 4)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (aspectMask (poked :: SparseImageFormatProperties))
                *> poke (ptr `plusPtr` 4) (imageGranularity (poked :: SparseImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (flags (poked :: SparseImageFormatProperties))


