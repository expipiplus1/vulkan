{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_copy_commands2"
module Vulkan.Extensions.VK_KHR_copy_commands2  ( cmdCopyBuffer2KHR
                                                , cmdCopyImage2KHR
                                                , cmdBlitImage2KHR
                                                , cmdCopyBufferToImage2KHR
                                                , cmdCopyImageToBuffer2KHR
                                                , cmdResolveImage2KHR
                                                , BufferCopy2KHR(..)
                                                , ImageCopy2KHR(..)
                                                , ImageBlit2KHR(..)
                                                , BufferImageCopy2KHR(..)
                                                , ImageResolve2KHR(..)
                                                , CopyBufferInfo2KHR(..)
                                                , CopyImageInfo2KHR(..)
                                                , BlitImageInfo2KHR(..)
                                                , CopyBufferToImageInfo2KHR(..)
                                                , CopyImageToBufferInfo2KHR(..)
                                                , ResolveImageInfo2KHR(..)
                                                , KHR_COPY_COMMANDS_2_SPEC_VERSION
                                                , pattern KHR_COPY_COMMANDS_2_SPEC_VERSION
                                                , KHR_COPY_COMMANDS_2_EXTENSION_NAME
                                                , pattern KHR_COPY_COMMANDS_2_EXTENSION_NAME
                                                ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_rotated_copy_commands (CopyCommandTransformInfoQCOM)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBlitImage2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyBuffer2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyBufferToImage2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyImage2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyImageToBuffer2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdResolveImage2KHR))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core10.Enums.Filter (Filter)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.CommandBufferBuilding (ImageSubresourceLayers)
import Vulkan.Core10.FundamentalTypes (Offset3D)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_COPY_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_BLIT_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_COPY_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBuffer2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyBufferInfo2KHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyBufferInfo2KHR -> IO ()

-- No documentation found for TopLevel "vkCmdCopyBuffer2KHR"
cmdCopyBuffer2KHR :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkCmdCopyBuffer2KHR" "commandBuffer"
                     CommandBuffer
                  -> -- No documentation found for Nested "vkCmdCopyBuffer2KHR" "pCopyBufferInfo"
                     CopyBufferInfo2KHR
                  -> io ()
cmdCopyBuffer2KHR commandBuffer copyBufferInfo = liftIO . evalContT $ do
  let vkCmdCopyBuffer2KHRPtr = pVkCmdCopyBuffer2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyBuffer2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyBuffer2KHR is null" Nothing Nothing
  let vkCmdCopyBuffer2KHR' = mkVkCmdCopyBuffer2KHR vkCmdCopyBuffer2KHRPtr
  pCopyBufferInfo <- ContT $ withCStruct (copyBufferInfo)
  lift $ vkCmdCopyBuffer2KHR' (commandBufferHandle (commandBuffer)) pCopyBufferInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImage2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyImageInfo2KHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyImageInfo2KHR -> IO ()

-- No documentation found for TopLevel "vkCmdCopyImage2KHR"
cmdCopyImage2KHR :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkCmdCopyImage2KHR" "commandBuffer"
                    CommandBuffer
                 -> -- No documentation found for Nested "vkCmdCopyImage2KHR" "pCopyImageInfo"
                    CopyImageInfo2KHR
                 -> io ()
cmdCopyImage2KHR commandBuffer copyImageInfo = liftIO . evalContT $ do
  let vkCmdCopyImage2KHRPtr = pVkCmdCopyImage2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyImage2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyImage2KHR is null" Nothing Nothing
  let vkCmdCopyImage2KHR' = mkVkCmdCopyImage2KHR vkCmdCopyImage2KHRPtr
  pCopyImageInfo <- ContT $ withCStruct (copyImageInfo)
  lift $ vkCmdCopyImage2KHR' (commandBufferHandle (commandBuffer)) pCopyImageInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBlitImage2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr BlitImageInfo2KHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr BlitImageInfo2KHR -> IO ()

-- No documentation found for TopLevel "vkCmdBlitImage2KHR"
cmdBlitImage2KHR :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkCmdBlitImage2KHR" "commandBuffer"
                    CommandBuffer
                 -> -- No documentation found for Nested "vkCmdBlitImage2KHR" "pBlitImageInfo"
                    BlitImageInfo2KHR
                 -> io ()
cmdBlitImage2KHR commandBuffer blitImageInfo = liftIO . evalContT $ do
  let vkCmdBlitImage2KHRPtr = pVkCmdBlitImage2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBlitImage2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBlitImage2KHR is null" Nothing Nothing
  let vkCmdBlitImage2KHR' = mkVkCmdBlitImage2KHR vkCmdBlitImage2KHRPtr
  pBlitImageInfo <- ContT $ withCStruct (blitImageInfo)
  lift $ vkCmdBlitImage2KHR' (commandBufferHandle (commandBuffer)) pBlitImageInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBufferToImage2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyBufferToImageInfo2KHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyBufferToImageInfo2KHR -> IO ()

-- No documentation found for TopLevel "vkCmdCopyBufferToImage2KHR"
cmdCopyBufferToImage2KHR :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkCmdCopyBufferToImage2KHR" "commandBuffer"
                            CommandBuffer
                         -> -- No documentation found for Nested "vkCmdCopyBufferToImage2KHR" "pCopyBufferToImageInfo"
                            CopyBufferToImageInfo2KHR
                         -> io ()
cmdCopyBufferToImage2KHR commandBuffer copyBufferToImageInfo = liftIO . evalContT $ do
  let vkCmdCopyBufferToImage2KHRPtr = pVkCmdCopyBufferToImage2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyBufferToImage2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyBufferToImage2KHR is null" Nothing Nothing
  let vkCmdCopyBufferToImage2KHR' = mkVkCmdCopyBufferToImage2KHR vkCmdCopyBufferToImage2KHRPtr
  pCopyBufferToImageInfo <- ContT $ withCStruct (copyBufferToImageInfo)
  lift $ vkCmdCopyBufferToImage2KHR' (commandBufferHandle (commandBuffer)) pCopyBufferToImageInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImageToBuffer2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyImageToBufferInfo2KHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyImageToBufferInfo2KHR -> IO ()

-- No documentation found for TopLevel "vkCmdCopyImageToBuffer2KHR"
cmdCopyImageToBuffer2KHR :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkCmdCopyImageToBuffer2KHR" "commandBuffer"
                            CommandBuffer
                         -> -- No documentation found for Nested "vkCmdCopyImageToBuffer2KHR" "pCopyImageToBufferInfo"
                            CopyImageToBufferInfo2KHR
                         -> io ()
cmdCopyImageToBuffer2KHR commandBuffer copyImageToBufferInfo = liftIO . evalContT $ do
  let vkCmdCopyImageToBuffer2KHRPtr = pVkCmdCopyImageToBuffer2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyImageToBuffer2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyImageToBuffer2KHR is null" Nothing Nothing
  let vkCmdCopyImageToBuffer2KHR' = mkVkCmdCopyImageToBuffer2KHR vkCmdCopyImageToBuffer2KHRPtr
  pCopyImageToBufferInfo <- ContT $ withCStruct (copyImageToBufferInfo)
  lift $ vkCmdCopyImageToBuffer2KHR' (commandBufferHandle (commandBuffer)) pCopyImageToBufferInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResolveImage2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr ResolveImageInfo2KHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr ResolveImageInfo2KHR -> IO ()

-- No documentation found for TopLevel "vkCmdResolveImage2KHR"
cmdResolveImage2KHR :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkCmdResolveImage2KHR" "commandBuffer"
                       CommandBuffer
                    -> -- No documentation found for Nested "vkCmdResolveImage2KHR" "pResolveImageInfo"
                       ResolveImageInfo2KHR
                    -> io ()
cmdResolveImage2KHR commandBuffer resolveImageInfo = liftIO . evalContT $ do
  let vkCmdResolveImage2KHRPtr = pVkCmdResolveImage2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdResolveImage2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdResolveImage2KHR is null" Nothing Nothing
  let vkCmdResolveImage2KHR' = mkVkCmdResolveImage2KHR vkCmdResolveImage2KHRPtr
  pResolveImageInfo <- ContT $ withCStruct (resolveImageInfo)
  lift $ vkCmdResolveImage2KHR' (commandBufferHandle (commandBuffer)) pResolveImageInfo
  pure $ ()



-- No documentation found for TopLevel "VkBufferCopy2KHR"
data BufferCopy2KHR = BufferCopy2KHR
  { -- No documentation found for Nested "VkBufferCopy2KHR" "srcOffset"
    srcOffset :: DeviceSize
  , -- No documentation found for Nested "VkBufferCopy2KHR" "dstOffset"
    dstOffset :: DeviceSize
  , -- No documentation found for Nested "VkBufferCopy2KHR" "size"
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferCopy2KHR)
#endif
deriving instance Show BufferCopy2KHR

instance ToCStruct BufferCopy2KHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCopy2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COPY_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (srcOffset)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (dstOffset)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COPY_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BufferCopy2KHR where
  peekCStruct p = do
    srcOffset <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    dstOffset <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ BufferCopy2KHR
             srcOffset dstOffset size


instance Storable BufferCopy2KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferCopy2KHR where
  zero = BufferCopy2KHR
           zero
           zero
           zero



-- No documentation found for TopLevel "VkImageCopy2KHR"
data ImageCopy2KHR = ImageCopy2KHR
  { -- No documentation found for Nested "VkImageCopy2KHR" "srcSubresource"
    srcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageCopy2KHR" "srcOffset"
    srcOffset :: Offset3D
  , -- No documentation found for Nested "VkImageCopy2KHR" "dstSubresource"
    dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageCopy2KHR" "dstOffset"
    dstOffset :: Offset3D
  , -- No documentation found for Nested "VkImageCopy2KHR" "extent"
    extent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCopy2KHR)
#endif
deriving instance Show ImageCopy2KHR

instance ToCStruct ImageCopy2KHR where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCopy2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COPY_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (srcSubresource)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (srcOffset)
    poke ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers)) (dstSubresource)
    poke ((p `plusPtr` 60 :: Ptr Offset3D)) (dstOffset)
    poke ((p `plusPtr` 72 :: Ptr Extent3D)) (extent)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_COPY_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct ImageCopy2KHR where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers))
    srcOffset <- peekCStruct @Offset3D ((p `plusPtr` 32 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers))
    dstOffset <- peekCStruct @Offset3D ((p `plusPtr` 60 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 72 :: Ptr Extent3D))
    pure $ ImageCopy2KHR
             srcSubresource srcOffset dstSubresource dstOffset extent


instance Storable ImageCopy2KHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageCopy2KHR where
  zero = ImageCopy2KHR
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkImageBlit2KHR"
data ImageBlit2KHR (es :: [Type]) = ImageBlit2KHR
  { -- No documentation found for Nested "VkImageBlit2KHR" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkImageBlit2KHR" "srcSubresource"
    srcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageBlit2KHR" "srcOffsets"
    srcOffsets :: (Offset3D, Offset3D)
  , -- No documentation found for Nested "VkImageBlit2KHR" "dstSubresource"
    dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageBlit2KHR" "dstOffsets"
    dstOffsets :: (Offset3D, Offset3D)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageBlit2KHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageBlit2KHR es)

instance Extensible ImageBlit2KHR where
  extensibleType = STRUCTURE_TYPE_IMAGE_BLIT_2_KHR
  setNext x next = x{next = next}
  getNext ImageBlit2KHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageBlit2KHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @CopyCommandTransformInfoQCOM = Just f
    | otherwise = Nothing

instance (Extendss ImageBlit2KHR es, PokeChain es) => ToCStruct (ImageBlit2KHR es) where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageBlit2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_BLIT_2_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (srcSubresource)
    let pSrcOffsets' = lowerArrayPtr ((p `plusPtr` 32 :: Ptr (FixedArray 2 Offset3D)))
    lift $ case (srcOffsets) of
      (e0, e1) -> do
        poke (pSrcOffsets' :: Ptr Offset3D) (e0)
        poke (pSrcOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageSubresourceLayers)) (dstSubresource)
    let pDstOffsets' = lowerArrayPtr ((p `plusPtr` 72 :: Ptr (FixedArray 2 Offset3D)))
    lift $ case (dstOffsets) of
      (e0, e1) -> do
        poke (pDstOffsets' :: Ptr Offset3D) (e0)
        poke (pDstOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_BLIT_2_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (zero)
    let pSrcOffsets' = lowerArrayPtr ((p `plusPtr` 32 :: Ptr (FixedArray 2 Offset3D)))
    lift $ case ((zero, zero)) of
      (e0, e1) -> do
        poke (pSrcOffsets' :: Ptr Offset3D) (e0)
        poke (pSrcOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageSubresourceLayers)) (zero)
    let pDstOffsets' = lowerArrayPtr ((p `plusPtr` 72 :: Ptr (FixedArray 2 Offset3D)))
    lift $ case ((zero, zero)) of
      (e0, e1) -> do
        poke (pDstOffsets' :: Ptr Offset3D) (e0)
        poke (pDstOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    lift $ f

instance (Extendss ImageBlit2KHR es, PeekChain es) => FromCStruct (ImageBlit2KHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers))
    let psrcOffsets = lowerArrayPtr @Offset3D ((p `plusPtr` 32 :: Ptr (FixedArray 2 Offset3D)))
    srcOffsets0 <- peekCStruct @Offset3D ((psrcOffsets `advancePtrBytes` 0 :: Ptr Offset3D))
    srcOffsets1 <- peekCStruct @Offset3D ((psrcOffsets `advancePtrBytes` 12 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 56 :: Ptr ImageSubresourceLayers))
    let pdstOffsets = lowerArrayPtr @Offset3D ((p `plusPtr` 72 :: Ptr (FixedArray 2 Offset3D)))
    dstOffsets0 <- peekCStruct @Offset3D ((pdstOffsets `advancePtrBytes` 0 :: Ptr Offset3D))
    dstOffsets1 <- peekCStruct @Offset3D ((pdstOffsets `advancePtrBytes` 12 :: Ptr Offset3D))
    pure $ ImageBlit2KHR
             next srcSubresource ((srcOffsets0, srcOffsets1)) dstSubresource ((dstOffsets0, dstOffsets1))

instance es ~ '[] => Zero (ImageBlit2KHR es) where
  zero = ImageBlit2KHR
           ()
           zero
           (zero, zero)
           zero
           (zero, zero)



-- No documentation found for TopLevel "VkBufferImageCopy2KHR"
data BufferImageCopy2KHR (es :: [Type]) = BufferImageCopy2KHR
  { -- No documentation found for Nested "VkBufferImageCopy2KHR" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkBufferImageCopy2KHR" "bufferOffset"
    bufferOffset :: DeviceSize
  , -- No documentation found for Nested "VkBufferImageCopy2KHR" "bufferRowLength"
    bufferRowLength :: Word32
  , -- No documentation found for Nested "VkBufferImageCopy2KHR" "bufferImageHeight"
    bufferImageHeight :: Word32
  , -- No documentation found for Nested "VkBufferImageCopy2KHR" "imageSubresource"
    imageSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkBufferImageCopy2KHR" "imageOffset"
    imageOffset :: Offset3D
  , -- No documentation found for Nested "VkBufferImageCopy2KHR" "imageExtent"
    imageExtent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferImageCopy2KHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BufferImageCopy2KHR es)

instance Extensible BufferImageCopy2KHR where
  extensibleType = STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR
  setNext x next = x{next = next}
  getNext BufferImageCopy2KHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BufferImageCopy2KHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @CopyCommandTransformInfoQCOM = Just f
    | otherwise = Nothing

instance (Extendss BufferImageCopy2KHR es, PokeChain es) => ToCStruct (BufferImageCopy2KHR es) where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferImageCopy2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (bufferOffset)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (bufferRowLength)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (bufferImageHeight)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (imageSubresource)
    lift $ poke ((p `plusPtr` 48 :: Ptr Offset3D)) (imageOffset)
    lift $ poke ((p `plusPtr` 60 :: Ptr Extent3D)) (imageExtent)
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr Offset3D)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr Extent3D)) (zero)
    lift $ f

instance (Extendss BufferImageCopy2KHR es, PeekChain es) => FromCStruct (BufferImageCopy2KHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    bufferOffset <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    bufferRowLength <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    bufferImageHeight <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    imageSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers))
    imageOffset <- peekCStruct @Offset3D ((p `plusPtr` 48 :: Ptr Offset3D))
    imageExtent <- peekCStruct @Extent3D ((p `plusPtr` 60 :: Ptr Extent3D))
    pure $ BufferImageCopy2KHR
             next bufferOffset bufferRowLength bufferImageHeight imageSubresource imageOffset imageExtent

instance es ~ '[] => Zero (BufferImageCopy2KHR es) where
  zero = BufferImageCopy2KHR
           ()
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkImageResolve2KHR"
data ImageResolve2KHR = ImageResolve2KHR
  { -- No documentation found for Nested "VkImageResolve2KHR" "srcSubresource"
    srcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageResolve2KHR" "srcOffset"
    srcOffset :: Offset3D
  , -- No documentation found for Nested "VkImageResolve2KHR" "dstSubresource"
    dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageResolve2KHR" "dstOffset"
    dstOffset :: Offset3D
  , -- No documentation found for Nested "VkImageResolve2KHR" "extent"
    extent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageResolve2KHR)
#endif
deriving instance Show ImageResolve2KHR

instance ToCStruct ImageResolve2KHR where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageResolve2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (srcSubresource)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (srcOffset)
    poke ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers)) (dstSubresource)
    poke ((p `plusPtr` 60 :: Ptr Offset3D)) (dstOffset)
    poke ((p `plusPtr` 72 :: Ptr Extent3D)) (extent)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct ImageResolve2KHR where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers))
    srcOffset <- peekCStruct @Offset3D ((p `plusPtr` 32 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 44 :: Ptr ImageSubresourceLayers))
    dstOffset <- peekCStruct @Offset3D ((p `plusPtr` 60 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 72 :: Ptr Extent3D))
    pure $ ImageResolve2KHR
             srcSubresource srcOffset dstSubresource dstOffset extent


instance Storable ImageResolve2KHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageResolve2KHR where
  zero = ImageResolve2KHR
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkCopyBufferInfo2KHR"
data CopyBufferInfo2KHR = CopyBufferInfo2KHR
  { -- No documentation found for Nested "VkCopyBufferInfo2KHR" "srcBuffer"
    srcBuffer :: Buffer
  , -- No documentation found for Nested "VkCopyBufferInfo2KHR" "dstBuffer"
    dstBuffer :: Buffer
  , -- No documentation found for Nested "VkCopyBufferInfo2KHR" "pRegions"
    regions :: Vector BufferCopy2KHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyBufferInfo2KHR)
#endif
deriving instance Show CopyBufferInfo2KHR

instance ToCStruct CopyBufferInfo2KHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyBufferInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Buffer)) (srcBuffer)
    lift $ poke ((p `plusPtr` 24 :: Ptr Buffer)) (dstBuffer)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytesAligned @BufferCopy2KHR ((Data.Vector.length (regions)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (40 * (i)) :: Ptr BufferCopy2KHR) (e)) (regions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr BufferCopy2KHR))) (pPRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Buffer)) (zero)
    pPRegions' <- ContT $ allocaBytesAligned @BufferCopy2KHR ((Data.Vector.length (mempty)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (40 * (i)) :: Ptr BufferCopy2KHR) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr BufferCopy2KHR))) (pPRegions')
    lift $ f

instance FromCStruct CopyBufferInfo2KHR where
  peekCStruct p = do
    srcBuffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    dstBuffer <- peek @Buffer ((p `plusPtr` 24 :: Ptr Buffer))
    regionCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pRegions <- peek @(Ptr BufferCopy2KHR) ((p `plusPtr` 40 :: Ptr (Ptr BufferCopy2KHR)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @BufferCopy2KHR ((pRegions `advancePtrBytes` (40 * (i)) :: Ptr BufferCopy2KHR)))
    pure $ CopyBufferInfo2KHR
             srcBuffer dstBuffer pRegions'

instance Zero CopyBufferInfo2KHR where
  zero = CopyBufferInfo2KHR
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkCopyImageInfo2KHR"
data CopyImageInfo2KHR = CopyImageInfo2KHR
  { -- No documentation found for Nested "VkCopyImageInfo2KHR" "srcImage"
    srcImage :: Image
  , -- No documentation found for Nested "VkCopyImageInfo2KHR" "srcImageLayout"
    srcImageLayout :: ImageLayout
  , -- No documentation found for Nested "VkCopyImageInfo2KHR" "dstImage"
    dstImage :: Image
  , -- No documentation found for Nested "VkCopyImageInfo2KHR" "dstImageLayout"
    dstImageLayout :: ImageLayout
  , -- No documentation found for Nested "VkCopyImageInfo2KHR" "pRegions"
    regions :: Vector ImageCopy2KHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyImageInfo2KHR)
#endif
deriving instance Show CopyImageInfo2KHR

instance ToCStruct CopyImageInfo2KHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyImageInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytesAligned @ImageCopy2KHR ((Data.Vector.length (regions)) * 88) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (88 * (i)) :: Ptr ImageCopy2KHR) (e)) (regions)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr ImageCopy2KHR))) (pPRegions')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (zero)
    pPRegions' <- ContT $ allocaBytesAligned @ImageCopy2KHR ((Data.Vector.length (mempty)) * 88) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (88 * (i)) :: Ptr ImageCopy2KHR) (e)) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr ImageCopy2KHR))) (pPRegions')
    lift $ f

instance FromCStruct CopyImageInfo2KHR where
  peekCStruct p = do
    srcImage <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    dstImage <- peek @Image ((p `plusPtr` 32 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 40 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pRegions <- peek @(Ptr ImageCopy2KHR) ((p `plusPtr` 48 :: Ptr (Ptr ImageCopy2KHR)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @ImageCopy2KHR ((pRegions `advancePtrBytes` (88 * (i)) :: Ptr ImageCopy2KHR)))
    pure $ CopyImageInfo2KHR
             srcImage srcImageLayout dstImage dstImageLayout pRegions'

instance Zero CopyImageInfo2KHR where
  zero = CopyImageInfo2KHR
           zero
           zero
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkBlitImageInfo2KHR"
data BlitImageInfo2KHR = BlitImageInfo2KHR
  { -- No documentation found for Nested "VkBlitImageInfo2KHR" "srcImage"
    srcImage :: Image
  , -- No documentation found for Nested "VkBlitImageInfo2KHR" "srcImageLayout"
    srcImageLayout :: ImageLayout
  , -- No documentation found for Nested "VkBlitImageInfo2KHR" "dstImage"
    dstImage :: Image
  , -- No documentation found for Nested "VkBlitImageInfo2KHR" "dstImageLayout"
    dstImageLayout :: ImageLayout
  , -- No documentation found for Nested "VkBlitImageInfo2KHR" "pRegions"
    regions :: Vector (SomeStruct ImageBlit2KHR)
  , -- No documentation found for Nested "VkBlitImageInfo2KHR" "filter"
    filter' :: Filter
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BlitImageInfo2KHR)
#endif
deriving instance Show BlitImageInfo2KHR

instance ToCStruct BlitImageInfo2KHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BlitImageInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytesAligned @(ImageBlit2KHR _) ((Data.Vector.length (regions)) * 96) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPRegions' `plusPtr` (96 * (i)) :: Ptr (ImageBlit2KHR _))) (e) . ($ ())) (regions)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (ImageBlit2KHR _)))) (pPRegions')
    lift $ poke ((p `plusPtr` 56 :: Ptr Filter)) (filter')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (zero)
    pPRegions' <- ContT $ allocaBytesAligned @(ImageBlit2KHR _) ((Data.Vector.length (mempty)) * 96) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPRegions' `plusPtr` (96 * (i)) :: Ptr (ImageBlit2KHR _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (ImageBlit2KHR _)))) (pPRegions')
    lift $ poke ((p `plusPtr` 56 :: Ptr Filter)) (zero)
    lift $ f

instance FromCStruct BlitImageInfo2KHR where
  peekCStruct p = do
    srcImage <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    dstImage <- peek @Image ((p `plusPtr` 32 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 40 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pRegions <- peek @(Ptr (ImageBlit2KHR _)) ((p `plusPtr` 48 :: Ptr (Ptr (ImageBlit2KHR a))))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekSomeCStruct (forgetExtensions ((pRegions `advancePtrBytes` (96 * (i)) :: Ptr (ImageBlit2KHR _)))))
    filter' <- peek @Filter ((p `plusPtr` 56 :: Ptr Filter))
    pure $ BlitImageInfo2KHR
             srcImage srcImageLayout dstImage dstImageLayout pRegions' filter'

instance Zero BlitImageInfo2KHR where
  zero = BlitImageInfo2KHR
           zero
           zero
           zero
           zero
           mempty
           zero



-- No documentation found for TopLevel "VkCopyBufferToImageInfo2KHR"
data CopyBufferToImageInfo2KHR = CopyBufferToImageInfo2KHR
  { -- No documentation found for Nested "VkCopyBufferToImageInfo2KHR" "srcBuffer"
    srcBuffer :: Buffer
  , -- No documentation found for Nested "VkCopyBufferToImageInfo2KHR" "dstImage"
    dstImage :: Image
  , -- No documentation found for Nested "VkCopyBufferToImageInfo2KHR" "dstImageLayout"
    dstImageLayout :: ImageLayout
  , -- No documentation found for Nested "VkCopyBufferToImageInfo2KHR" "pRegions"
    regions :: Vector (SomeStruct BufferImageCopy2KHR)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyBufferToImageInfo2KHR)
#endif
deriving instance Show CopyBufferToImageInfo2KHR

instance ToCStruct CopyBufferToImageInfo2KHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyBufferToImageInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Buffer)) (srcBuffer)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytesAligned @(BufferImageCopy2KHR _) ((Data.Vector.length (regions)) * 72) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPRegions' `plusPtr` (72 * (i)) :: Ptr (BufferImageCopy2KHR _))) (e) . ($ ())) (regions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (BufferImageCopy2KHR _)))) (pPRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (zero)
    pPRegions' <- ContT $ allocaBytesAligned @(BufferImageCopy2KHR _) ((Data.Vector.length (mempty)) * 72) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPRegions' `plusPtr` (72 * (i)) :: Ptr (BufferImageCopy2KHR _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (BufferImageCopy2KHR _)))) (pPRegions')
    lift $ f

instance FromCStruct CopyBufferToImageInfo2KHR where
  peekCStruct p = do
    srcBuffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    dstImage <- peek @Image ((p `plusPtr` 24 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 32 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pRegions <- peek @(Ptr (BufferImageCopy2KHR _)) ((p `plusPtr` 40 :: Ptr (Ptr (BufferImageCopy2KHR a))))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekSomeCStruct (forgetExtensions ((pRegions `advancePtrBytes` (72 * (i)) :: Ptr (BufferImageCopy2KHR _)))))
    pure $ CopyBufferToImageInfo2KHR
             srcBuffer dstImage dstImageLayout pRegions'

instance Zero CopyBufferToImageInfo2KHR where
  zero = CopyBufferToImageInfo2KHR
           zero
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkCopyImageToBufferInfo2KHR"
data CopyImageToBufferInfo2KHR = CopyImageToBufferInfo2KHR
  { -- No documentation found for Nested "VkCopyImageToBufferInfo2KHR" "srcImage"
    srcImage :: Image
  , -- No documentation found for Nested "VkCopyImageToBufferInfo2KHR" "srcImageLayout"
    srcImageLayout :: ImageLayout
  , -- No documentation found for Nested "VkCopyImageToBufferInfo2KHR" "dstBuffer"
    dstBuffer :: Buffer
  , -- No documentation found for Nested "VkCopyImageToBufferInfo2KHR" "pRegions"
    regions :: Vector (SomeStruct BufferImageCopy2KHR)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyImageToBufferInfo2KHR)
#endif
deriving instance Show CopyImageToBufferInfo2KHR

instance ToCStruct CopyImageToBufferInfo2KHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyImageToBufferInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Buffer)) (dstBuffer)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytesAligned @(BufferImageCopy2KHR _) ((Data.Vector.length (regions)) * 72) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPRegions' `plusPtr` (72 * (i)) :: Ptr (BufferImageCopy2KHR _))) (e) . ($ ())) (regions)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (BufferImageCopy2KHR _)))) (pPRegions')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Buffer)) (zero)
    pPRegions' <- ContT $ allocaBytesAligned @(BufferImageCopy2KHR _) ((Data.Vector.length (mempty)) * 72) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPRegions' `plusPtr` (72 * (i)) :: Ptr (BufferImageCopy2KHR _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (BufferImageCopy2KHR _)))) (pPRegions')
    lift $ f

instance FromCStruct CopyImageToBufferInfo2KHR where
  peekCStruct p = do
    srcImage <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    dstBuffer <- peek @Buffer ((p `plusPtr` 32 :: Ptr Buffer))
    regionCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pRegions <- peek @(Ptr (BufferImageCopy2KHR _)) ((p `plusPtr` 48 :: Ptr (Ptr (BufferImageCopy2KHR a))))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekSomeCStruct (forgetExtensions ((pRegions `advancePtrBytes` (72 * (i)) :: Ptr (BufferImageCopy2KHR _)))))
    pure $ CopyImageToBufferInfo2KHR
             srcImage srcImageLayout dstBuffer pRegions'

instance Zero CopyImageToBufferInfo2KHR where
  zero = CopyImageToBufferInfo2KHR
           zero
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkResolveImageInfo2KHR"
data ResolveImageInfo2KHR = ResolveImageInfo2KHR
  { -- No documentation found for Nested "VkResolveImageInfo2KHR" "srcImage"
    srcImage :: Image
  , -- No documentation found for Nested "VkResolveImageInfo2KHR" "srcImageLayout"
    srcImageLayout :: ImageLayout
  , -- No documentation found for Nested "VkResolveImageInfo2KHR" "dstImage"
    dstImage :: Image
  , -- No documentation found for Nested "VkResolveImageInfo2KHR" "dstImageLayout"
    dstImageLayout :: ImageLayout
  , -- No documentation found for Nested "VkResolveImageInfo2KHR" "pRegions"
    regions :: Vector ImageResolve2KHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ResolveImageInfo2KHR)
#endif
deriving instance Show ResolveImageInfo2KHR

instance ToCStruct ResolveImageInfo2KHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ResolveImageInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytesAligned @ImageResolve2KHR ((Data.Vector.length (regions)) * 88) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (88 * (i)) :: Ptr ImageResolve2KHR) (e)) (regions)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr ImageResolve2KHR))) (pPRegions')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr ImageLayout)) (zero)
    pPRegions' <- ContT $ allocaBytesAligned @ImageResolve2KHR ((Data.Vector.length (mempty)) * 88) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (88 * (i)) :: Ptr ImageResolve2KHR) (e)) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr ImageResolve2KHR))) (pPRegions')
    lift $ f

instance FromCStruct ResolveImageInfo2KHR where
  peekCStruct p = do
    srcImage <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    dstImage <- peek @Image ((p `plusPtr` 32 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 40 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pRegions <- peek @(Ptr ImageResolve2KHR) ((p `plusPtr` 48 :: Ptr (Ptr ImageResolve2KHR)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @ImageResolve2KHR ((pRegions `advancePtrBytes` (88 * (i)) :: Ptr ImageResolve2KHR)))
    pure $ ResolveImageInfo2KHR
             srcImage srcImageLayout dstImage dstImageLayout pRegions'

instance Zero ResolveImageInfo2KHR where
  zero = ResolveImageInfo2KHR
           zero
           zero
           zero
           zero
           mempty


type KHR_COPY_COMMANDS_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_COPY_COMMANDS_2_SPEC_VERSION"
pattern KHR_COPY_COMMANDS_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_COPY_COMMANDS_2_SPEC_VERSION = 1


type KHR_COPY_COMMANDS_2_EXTENSION_NAME = "VK_KHR_copy_commands2"

-- No documentation found for TopLevel "VK_KHR_COPY_COMMANDS_2_EXTENSION_NAME"
pattern KHR_COPY_COMMANDS_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_COPY_COMMANDS_2_EXTENSION_NAME = "VK_KHR_copy_commands2"

