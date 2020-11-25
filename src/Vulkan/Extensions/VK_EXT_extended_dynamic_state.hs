{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_extended_dynamic_state"
module Vulkan.Extensions.VK_EXT_extended_dynamic_state  ( cmdSetCullModeEXT
                                                        , cmdSetFrontFaceEXT
                                                        , cmdSetPrimitiveTopologyEXT
                                                        , cmdSetViewportWithCountEXT
                                                        , cmdSetScissorWithCountEXT
                                                        , cmdBindVertexBuffers2EXT
                                                        , cmdSetDepthTestEnableEXT
                                                        , cmdSetDepthWriteEnableEXT
                                                        , cmdSetDepthCompareOpEXT
                                                        , cmdSetDepthBoundsTestEnableEXT
                                                        , cmdSetStencilTestEnableEXT
                                                        , cmdSetStencilOpEXT
                                                        , PhysicalDeviceExtendedDynamicStateFeaturesEXT(..)
                                                        , EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION
                                                        , pattern EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION
                                                        , EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME
                                                        , pattern EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME
                                                        ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Enums.CompareOp (CompareOp)
import Vulkan.Core10.Enums.CompareOp (CompareOp(..))
import Vulkan.Core10.Enums.CullModeFlagBits (CullModeFlagBits(..))
import Vulkan.Core10.Enums.CullModeFlagBits (CullModeFlags)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindVertexBuffers2EXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetCullModeEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthBoundsTestEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthCompareOpEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthTestEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthWriteEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetFrontFaceEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetPrimitiveTopologyEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetScissorWithCountEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilOpEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilTestEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewportWithCountEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.FrontFace (FrontFace)
import Vulkan.Core10.Enums.FrontFace (FrontFace(..))
import Vulkan.Core10.Enums.PrimitiveTopology (PrimitiveTopology)
import Vulkan.Core10.Enums.PrimitiveTopology (PrimitiveTopology(..))
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlagBits(..))
import Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlags)
import Vulkan.Core10.Enums.StencilOp (StencilOp)
import Vulkan.Core10.Enums.StencilOp (StencilOp(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Core10.Pipeline (Viewport)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCullModeEXT
  :: FunPtr (Ptr CommandBuffer_T -> CullModeFlags -> IO ()) -> Ptr CommandBuffer_T -> CullModeFlags -> IO ()

-- No documentation found for TopLevel "vkCmdSetCullModeEXT"
cmdSetCullModeEXT :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkCmdSetCullModeEXT" "commandBuffer"
                     CommandBuffer
                  -> -- No documentation found for Nested "vkCmdSetCullModeEXT" "cullMode"
                     CullModeFlags
                  -> io ()
cmdSetCullModeEXT commandBuffer cullMode = liftIO $ do
  let vkCmdSetCullModeEXTPtr = pVkCmdSetCullModeEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetCullModeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCullModeEXT is null" Nothing Nothing
  let vkCmdSetCullModeEXT' = mkVkCmdSetCullModeEXT vkCmdSetCullModeEXTPtr
  vkCmdSetCullModeEXT' (commandBufferHandle (commandBuffer)) (cullMode)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetFrontFaceEXT
  :: FunPtr (Ptr CommandBuffer_T -> FrontFace -> IO ()) -> Ptr CommandBuffer_T -> FrontFace -> IO ()

-- No documentation found for TopLevel "vkCmdSetFrontFaceEXT"
cmdSetFrontFaceEXT :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkCmdSetFrontFaceEXT" "commandBuffer"
                      CommandBuffer
                   -> -- No documentation found for Nested "vkCmdSetFrontFaceEXT" "frontFace"
                      FrontFace
                   -> io ()
cmdSetFrontFaceEXT commandBuffer frontFace = liftIO $ do
  let vkCmdSetFrontFaceEXTPtr = pVkCmdSetFrontFaceEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetFrontFaceEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetFrontFaceEXT is null" Nothing Nothing
  let vkCmdSetFrontFaceEXT' = mkVkCmdSetFrontFaceEXT vkCmdSetFrontFaceEXTPtr
  vkCmdSetFrontFaceEXT' (commandBufferHandle (commandBuffer)) (frontFace)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetPrimitiveTopologyEXT
  :: FunPtr (Ptr CommandBuffer_T -> PrimitiveTopology -> IO ()) -> Ptr CommandBuffer_T -> PrimitiveTopology -> IO ()

-- No documentation found for TopLevel "vkCmdSetPrimitiveTopologyEXT"
cmdSetPrimitiveTopologyEXT :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkCmdSetPrimitiveTopologyEXT" "commandBuffer"
                              CommandBuffer
                           -> -- No documentation found for Nested "vkCmdSetPrimitiveTopologyEXT" "primitiveTopology"
                              PrimitiveTopology
                           -> io ()
cmdSetPrimitiveTopologyEXT commandBuffer primitiveTopology = liftIO $ do
  let vkCmdSetPrimitiveTopologyEXTPtr = pVkCmdSetPrimitiveTopologyEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetPrimitiveTopologyEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetPrimitiveTopologyEXT is null" Nothing Nothing
  let vkCmdSetPrimitiveTopologyEXT' = mkVkCmdSetPrimitiveTopologyEXT vkCmdSetPrimitiveTopologyEXTPtr
  vkCmdSetPrimitiveTopologyEXT' (commandBufferHandle (commandBuffer)) (primitiveTopology)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportWithCountEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Viewport -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Viewport -> IO ()

-- No documentation found for TopLevel "vkCmdSetViewportWithCountEXT"
cmdSetViewportWithCountEXT :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkCmdSetViewportWithCountEXT" "commandBuffer"
                              CommandBuffer
                           -> -- No documentation found for Nested "vkCmdSetViewportWithCountEXT" "pViewports"
                              ("viewports" ::: Vector Viewport)
                           -> io ()
cmdSetViewportWithCountEXT commandBuffer viewports = liftIO . evalContT $ do
  let vkCmdSetViewportWithCountEXTPtr = pVkCmdSetViewportWithCountEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetViewportWithCountEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetViewportWithCountEXT is null" Nothing Nothing
  let vkCmdSetViewportWithCountEXT' = mkVkCmdSetViewportWithCountEXT vkCmdSetViewportWithCountEXTPtr
  pPViewports <- ContT $ allocaBytesAligned @Viewport ((Data.Vector.length (viewports)) * 24) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPViewports `plusPtr` (24 * (i)) :: Ptr Viewport) (e)) (viewports)
  lift $ vkCmdSetViewportWithCountEXT' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (viewports)) :: Word32)) (pPViewports)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetScissorWithCountEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Rect2D -> IO ()

-- No documentation found for TopLevel "vkCmdSetScissorWithCountEXT"
cmdSetScissorWithCountEXT :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkCmdSetScissorWithCountEXT" "commandBuffer"
                             CommandBuffer
                          -> -- No documentation found for Nested "vkCmdSetScissorWithCountEXT" "pScissors"
                             ("scissors" ::: Vector Rect2D)
                          -> io ()
cmdSetScissorWithCountEXT commandBuffer scissors = liftIO . evalContT $ do
  let vkCmdSetScissorWithCountEXTPtr = pVkCmdSetScissorWithCountEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetScissorWithCountEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetScissorWithCountEXT is null" Nothing Nothing
  let vkCmdSetScissorWithCountEXT' = mkVkCmdSetScissorWithCountEXT vkCmdSetScissorWithCountEXTPtr
  pPScissors <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (scissors)) * 16) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (scissors)
  lift $ vkCmdSetScissorWithCountEXT' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (scissors)) :: Word32)) (pPScissors)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindVertexBuffers2EXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()

-- No documentation found for TopLevel "vkCmdBindVertexBuffers2EXT"
cmdBindVertexBuffers2EXT :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkCmdBindVertexBuffers2EXT" "commandBuffer"
                            CommandBuffer
                         -> -- No documentation found for Nested "vkCmdBindVertexBuffers2EXT" "firstBinding"
                            ("firstBinding" ::: Word32)
                         -> -- No documentation found for Nested "vkCmdBindVertexBuffers2EXT" "pBuffers"
                            ("buffers" ::: Vector Buffer)
                         -> -- No documentation found for Nested "vkCmdBindVertexBuffers2EXT" "pOffsets"
                            ("offsets" ::: Vector DeviceSize)
                         -> -- No documentation found for Nested "vkCmdBindVertexBuffers2EXT" "pSizes"
                            ("sizes" ::: Vector DeviceSize)
                         -> -- No documentation found for Nested "vkCmdBindVertexBuffers2EXT" "pStrides"
                            ("strides" ::: Vector DeviceSize)
                         -> io ()
cmdBindVertexBuffers2EXT commandBuffer firstBinding buffers offsets sizes strides = liftIO . evalContT $ do
  let vkCmdBindVertexBuffers2EXTPtr = pVkCmdBindVertexBuffers2EXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBindVertexBuffers2EXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindVertexBuffers2EXT is null" Nothing Nothing
  let vkCmdBindVertexBuffers2EXT' = mkVkCmdBindVertexBuffers2EXT vkCmdBindVertexBuffers2EXTPtr
  let pBuffersLength = Data.Vector.length $ (buffers)
  lift $ unless ((Data.Vector.length $ (offsets)) == pBuffersLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pOffsets and pBuffers must have the same length" Nothing Nothing
  let pSizesLength = Data.Vector.length $ (sizes)
  lift $ unless (fromIntegral pSizesLength == pBuffersLength || pSizesLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pSizes and pBuffers must have the same length" Nothing Nothing
  let pStridesLength = Data.Vector.length $ (strides)
  lift $ unless (fromIntegral pStridesLength == pBuffersLength || pStridesLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pStrides and pBuffers must have the same length" Nothing Nothing
  pPBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (buffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (buffers)
  pPOffsets <- ContT $ allocaBytesAligned @DeviceSize ((Data.Vector.length (offsets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (offsets)
  pSizes <- if Data.Vector.null (sizes)
    then pure nullPtr
    else do
      pPSizes <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (sizes))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPSizes `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((sizes))
      pure $ pPSizes
  pStrides <- if Data.Vector.null (strides)
    then pure nullPtr
    else do
      pPStrides <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (strides))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPStrides `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((strides))
      pure $ pPStrides
  lift $ vkCmdBindVertexBuffers2EXT' (commandBufferHandle (commandBuffer)) (firstBinding) ((fromIntegral pBuffersLength :: Word32)) (pPBuffers) (pPOffsets) pSizes pStrides
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthTestEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- No documentation found for TopLevel "vkCmdSetDepthTestEnableEXT"
cmdSetDepthTestEnableEXT :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkCmdSetDepthTestEnableEXT" "commandBuffer"
                            CommandBuffer
                         -> -- No documentation found for Nested "vkCmdSetDepthTestEnableEXT" "depthTestEnable"
                            ("depthTestEnable" ::: Bool)
                         -> io ()
cmdSetDepthTestEnableEXT commandBuffer depthTestEnable = liftIO $ do
  let vkCmdSetDepthTestEnableEXTPtr = pVkCmdSetDepthTestEnableEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDepthTestEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthTestEnableEXT is null" Nothing Nothing
  let vkCmdSetDepthTestEnableEXT' = mkVkCmdSetDepthTestEnableEXT vkCmdSetDepthTestEnableEXTPtr
  vkCmdSetDepthTestEnableEXT' (commandBufferHandle (commandBuffer)) (boolToBool32 (depthTestEnable))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthWriteEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- No documentation found for TopLevel "vkCmdSetDepthWriteEnableEXT"
cmdSetDepthWriteEnableEXT :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkCmdSetDepthWriteEnableEXT" "commandBuffer"
                             CommandBuffer
                          -> -- No documentation found for Nested "vkCmdSetDepthWriteEnableEXT" "depthWriteEnable"
                             ("depthWriteEnable" ::: Bool)
                          -> io ()
cmdSetDepthWriteEnableEXT commandBuffer depthWriteEnable = liftIO $ do
  let vkCmdSetDepthWriteEnableEXTPtr = pVkCmdSetDepthWriteEnableEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDepthWriteEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthWriteEnableEXT is null" Nothing Nothing
  let vkCmdSetDepthWriteEnableEXT' = mkVkCmdSetDepthWriteEnableEXT vkCmdSetDepthWriteEnableEXTPtr
  vkCmdSetDepthWriteEnableEXT' (commandBufferHandle (commandBuffer)) (boolToBool32 (depthWriteEnable))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthCompareOpEXT
  :: FunPtr (Ptr CommandBuffer_T -> CompareOp -> IO ()) -> Ptr CommandBuffer_T -> CompareOp -> IO ()

-- No documentation found for TopLevel "vkCmdSetDepthCompareOpEXT"
cmdSetDepthCompareOpEXT :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkCmdSetDepthCompareOpEXT" "commandBuffer"
                           CommandBuffer
                        -> -- No documentation found for Nested "vkCmdSetDepthCompareOpEXT" "depthCompareOp"
                           ("depthCompareOp" ::: CompareOp)
                        -> io ()
cmdSetDepthCompareOpEXT commandBuffer depthCompareOp = liftIO $ do
  let vkCmdSetDepthCompareOpEXTPtr = pVkCmdSetDepthCompareOpEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDepthCompareOpEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthCompareOpEXT is null" Nothing Nothing
  let vkCmdSetDepthCompareOpEXT' = mkVkCmdSetDepthCompareOpEXT vkCmdSetDepthCompareOpEXTPtr
  vkCmdSetDepthCompareOpEXT' (commandBufferHandle (commandBuffer)) (depthCompareOp)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBoundsTestEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- No documentation found for TopLevel "vkCmdSetDepthBoundsTestEnableEXT"
cmdSetDepthBoundsTestEnableEXT :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "vkCmdSetDepthBoundsTestEnableEXT" "commandBuffer"
                                  CommandBuffer
                               -> -- No documentation found for Nested "vkCmdSetDepthBoundsTestEnableEXT" "depthBoundsTestEnable"
                                  ("depthBoundsTestEnable" ::: Bool)
                               -> io ()
cmdSetDepthBoundsTestEnableEXT commandBuffer depthBoundsTestEnable = liftIO $ do
  let vkCmdSetDepthBoundsTestEnableEXTPtr = pVkCmdSetDepthBoundsTestEnableEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDepthBoundsTestEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthBoundsTestEnableEXT is null" Nothing Nothing
  let vkCmdSetDepthBoundsTestEnableEXT' = mkVkCmdSetDepthBoundsTestEnableEXT vkCmdSetDepthBoundsTestEnableEXTPtr
  vkCmdSetDepthBoundsTestEnableEXT' (commandBufferHandle (commandBuffer)) (boolToBool32 (depthBoundsTestEnable))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilTestEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- No documentation found for TopLevel "vkCmdSetStencilTestEnableEXT"
cmdSetStencilTestEnableEXT :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkCmdSetStencilTestEnableEXT" "commandBuffer"
                              CommandBuffer
                           -> -- No documentation found for Nested "vkCmdSetStencilTestEnableEXT" "stencilTestEnable"
                              ("stencilTestEnable" ::: Bool)
                           -> io ()
cmdSetStencilTestEnableEXT commandBuffer stencilTestEnable = liftIO $ do
  let vkCmdSetStencilTestEnableEXTPtr = pVkCmdSetStencilTestEnableEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetStencilTestEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetStencilTestEnableEXT is null" Nothing Nothing
  let vkCmdSetStencilTestEnableEXT' = mkVkCmdSetStencilTestEnableEXT vkCmdSetStencilTestEnableEXTPtr
  vkCmdSetStencilTestEnableEXT' (commandBufferHandle (commandBuffer)) (boolToBool32 (stencilTestEnable))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilOpEXT
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> StencilOp -> StencilOp -> StencilOp -> CompareOp -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> StencilOp -> StencilOp -> StencilOp -> CompareOp -> IO ()

-- No documentation found for TopLevel "vkCmdSetStencilOpEXT"
cmdSetStencilOpEXT :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkCmdSetStencilOpEXT" "commandBuffer"
                      CommandBuffer
                   -> -- No documentation found for Nested "vkCmdSetStencilOpEXT" "faceMask"
                      ("faceMask" ::: StencilFaceFlags)
                   -> -- No documentation found for Nested "vkCmdSetStencilOpEXT" "failOp"
                      ("failOp" ::: StencilOp)
                   -> -- No documentation found for Nested "vkCmdSetStencilOpEXT" "passOp"
                      ("passOp" ::: StencilOp)
                   -> -- No documentation found for Nested "vkCmdSetStencilOpEXT" "depthFailOp"
                      ("depthFailOp" ::: StencilOp)
                   -> -- No documentation found for Nested "vkCmdSetStencilOpEXT" "compareOp"
                      CompareOp
                   -> io ()
cmdSetStencilOpEXT commandBuffer faceMask failOp passOp depthFailOp compareOp = liftIO $ do
  let vkCmdSetStencilOpEXTPtr = pVkCmdSetStencilOpEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetStencilOpEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetStencilOpEXT is null" Nothing Nothing
  let vkCmdSetStencilOpEXT' = mkVkCmdSetStencilOpEXT vkCmdSetStencilOpEXTPtr
  vkCmdSetStencilOpEXT' (commandBufferHandle (commandBuffer)) (faceMask) (failOp) (passOp) (depthFailOp) (compareOp)
  pure $ ()



-- No documentation found for TopLevel "VkPhysicalDeviceExtendedDynamicStateFeaturesEXT"
data PhysicalDeviceExtendedDynamicStateFeaturesEXT = PhysicalDeviceExtendedDynamicStateFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceExtendedDynamicStateFeaturesEXT" "extendedDynamicState"
    extendedDynamicState :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExtendedDynamicStateFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceExtendedDynamicStateFeaturesEXT

instance ToCStruct PhysicalDeviceExtendedDynamicStateFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExtendedDynamicStateFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExtendedDynamicStateFeaturesEXT where
  peekCStruct p = do
    extendedDynamicState <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceExtendedDynamicStateFeaturesEXT
             (bool32ToBool extendedDynamicState)


instance Storable PhysicalDeviceExtendedDynamicStateFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExtendedDynamicStateFeaturesEXT where
  zero = PhysicalDeviceExtendedDynamicStateFeaturesEXT
           zero


type EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION"
pattern EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION = 1


type EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME = "VK_EXT_extended_dynamic_state"

-- No documentation found for TopLevel "VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME"
pattern EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME = "VK_EXT_extended_dynamic_state"

