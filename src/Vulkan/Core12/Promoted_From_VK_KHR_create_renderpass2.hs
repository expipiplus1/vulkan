{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_create_renderpass2"
module Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2  ( createRenderPass2
                                                              , cmdBeginRenderPass2
                                                              , cmdUseRenderPass2
                                                              , cmdNextSubpass2
                                                              , cmdEndRenderPass2
                                                              , AttachmentDescription2(..)
                                                              , AttachmentReference2(..)
                                                              , SubpassDescription2(..)
                                                              , SubpassDependency2(..)
                                                              , RenderPassCreateInfo2(..)
                                                              , SubpassBeginInfo(..)
                                                              , SubpassEndInfo(..)
                                                              , StructureType(..)
                                                              ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
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
import qualified Data.Vector (null)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Enums.AttachmentDescriptionFlagBits (AttachmentDescriptionFlags)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (AttachmentDescriptionStencilLayout)
import Vulkan.Core10.Enums.AttachmentLoadOp (AttachmentLoadOp)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (AttachmentReferenceStencilLayout)
import Vulkan.Core10.Enums.AttachmentStoreOp (AttachmentStoreOp)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginRenderPass2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndRenderPass2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdNextSubpass2))
import Vulkan.Dynamic (DeviceCmds(pVkCreateRenderPass2))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (FragmentShadingRateAttachmentInfoKHR)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Handles (RenderPass)
import Vulkan.Core10.Handles (RenderPass(..))
import Vulkan.Core10.CommandBufferBuilding (RenderPassBeginInfo)
import Vulkan.Core10.Enums.RenderPassCreateFlagBits (RenderPassCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (RenderPassFragmentDensityMapCreateInfoEXT)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.SubpassContents (SubpassContents)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve (SubpassDescriptionDepthStencilResolve)
import Vulkan.Core10.Enums.SubpassDescriptionFlagBits (SubpassDescriptionFlags)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_BEGIN_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_END_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRenderPass2
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct RenderPassCreateInfo2) -> Ptr AllocationCallbacks -> Ptr RenderPass -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct RenderPassCreateInfo2) -> Ptr AllocationCallbacks -> Ptr RenderPass -> IO Result

-- No documentation found for TopLevel "vkCreateRenderPass2"
createRenderPass2 :: forall a io
                   . (Extendss RenderPassCreateInfo2 a, PokeChain a, MonadIO io)
                  => -- No documentation found for Nested "vkCreateRenderPass2" "device"
                     Device
                  -> -- No documentation found for Nested "vkCreateRenderPass2" "pCreateInfo"
                     (RenderPassCreateInfo2 a)
                  -> -- No documentation found for Nested "vkCreateRenderPass2" "pAllocator"
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io (RenderPass)
createRenderPass2 device createInfo allocator = liftIO . evalContT $ do
  let vkCreateRenderPass2Ptr = pVkCreateRenderPass2 (deviceCmds (device :: Device))
  lift $ unless (vkCreateRenderPass2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateRenderPass2 is null" Nothing Nothing
  let vkCreateRenderPass2' = mkVkCreateRenderPass2 vkCreateRenderPass2Ptr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPRenderPass <- ContT $ bracket (callocBytes @RenderPass 8) free
  r <- lift $ vkCreateRenderPass2' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPRenderPass)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pRenderPass <- lift $ peek @RenderPass pPRenderPass
  pure $ (pRenderPass)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginRenderPass2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct RenderPassBeginInfo) -> Ptr SubpassBeginInfo -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct RenderPassBeginInfo) -> Ptr SubpassBeginInfo -> IO ()

-- No documentation found for TopLevel "vkCmdBeginRenderPass2"
cmdBeginRenderPass2 :: forall a io
                     . (Extendss RenderPassBeginInfo a, PokeChain a, MonadIO io)
                    => -- No documentation found for Nested "vkCmdBeginRenderPass2" "commandBuffer"
                       CommandBuffer
                    -> -- No documentation found for Nested "vkCmdBeginRenderPass2" "pRenderPassBegin"
                       (RenderPassBeginInfo a)
                    -> -- No documentation found for Nested "vkCmdBeginRenderPass2" "pSubpassBeginInfo"
                       SubpassBeginInfo
                    -> io ()
cmdBeginRenderPass2 commandBuffer renderPassBegin subpassBeginInfo = liftIO . evalContT $ do
  let vkCmdBeginRenderPass2Ptr = pVkCmdBeginRenderPass2 (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBeginRenderPass2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginRenderPass2 is null" Nothing Nothing
  let vkCmdBeginRenderPass2' = mkVkCmdBeginRenderPass2 vkCmdBeginRenderPass2Ptr
  pRenderPassBegin <- ContT $ withCStruct (renderPassBegin)
  pSubpassBeginInfo <- ContT $ withCStruct (subpassBeginInfo)
  lift $ vkCmdBeginRenderPass2' (commandBufferHandle (commandBuffer)) (forgetExtensions pRenderPassBegin) pSubpassBeginInfo
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginRenderPass2' and 'cmdEndRenderPass2'
--
-- Note that 'cmdEndRenderPass2' is *not* called if an exception is thrown
-- by the inner action.
cmdUseRenderPass2 :: forall a io r . (Extendss RenderPassBeginInfo a, PokeChain a, MonadIO io) => CommandBuffer -> RenderPassBeginInfo a -> SubpassBeginInfo -> SubpassEndInfo -> io r -> io r
cmdUseRenderPass2 commandBuffer pRenderPassBegin pSubpassBeginInfo pSubpassEndInfo a =
  (cmdBeginRenderPass2 commandBuffer pRenderPassBegin pSubpassBeginInfo) *> a <* (cmdEndRenderPass2 commandBuffer pSubpassEndInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdNextSubpass2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr SubpassBeginInfo -> Ptr SubpassEndInfo -> IO ()) -> Ptr CommandBuffer_T -> Ptr SubpassBeginInfo -> Ptr SubpassEndInfo -> IO ()

-- No documentation found for TopLevel "vkCmdNextSubpass2"
cmdNextSubpass2 :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkCmdNextSubpass2" "commandBuffer"
                   CommandBuffer
                -> -- No documentation found for Nested "vkCmdNextSubpass2" "pSubpassBeginInfo"
                   SubpassBeginInfo
                -> -- No documentation found for Nested "vkCmdNextSubpass2" "pSubpassEndInfo"
                   SubpassEndInfo
                -> io ()
cmdNextSubpass2 commandBuffer subpassBeginInfo subpassEndInfo = liftIO . evalContT $ do
  let vkCmdNextSubpass2Ptr = pVkCmdNextSubpass2 (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdNextSubpass2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdNextSubpass2 is null" Nothing Nothing
  let vkCmdNextSubpass2' = mkVkCmdNextSubpass2 vkCmdNextSubpass2Ptr
  pSubpassBeginInfo <- ContT $ withCStruct (subpassBeginInfo)
  pSubpassEndInfo <- ContT $ withCStruct (subpassEndInfo)
  lift $ vkCmdNextSubpass2' (commandBufferHandle (commandBuffer)) pSubpassBeginInfo pSubpassEndInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndRenderPass2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr SubpassEndInfo -> IO ()) -> Ptr CommandBuffer_T -> Ptr SubpassEndInfo -> IO ()

-- No documentation found for TopLevel "vkCmdEndRenderPass2"
cmdEndRenderPass2 :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkCmdEndRenderPass2" "commandBuffer"
                     CommandBuffer
                  -> -- No documentation found for Nested "vkCmdEndRenderPass2" "pSubpassEndInfo"
                     SubpassEndInfo
                  -> io ()
cmdEndRenderPass2 commandBuffer subpassEndInfo = liftIO . evalContT $ do
  let vkCmdEndRenderPass2Ptr = pVkCmdEndRenderPass2 (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdEndRenderPass2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndRenderPass2 is null" Nothing Nothing
  let vkCmdEndRenderPass2' = mkVkCmdEndRenderPass2 vkCmdEndRenderPass2Ptr
  pSubpassEndInfo <- ContT $ withCStruct (subpassEndInfo)
  lift $ vkCmdEndRenderPass2' (commandBufferHandle (commandBuffer)) pSubpassEndInfo
  pure $ ()



-- No documentation found for TopLevel "VkAttachmentDescription2"
data AttachmentDescription2 (es :: [Type]) = AttachmentDescription2
  { -- No documentation found for Nested "VkAttachmentDescription2" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkAttachmentDescription2" "flags"
    flags :: AttachmentDescriptionFlags
  , -- No documentation found for Nested "VkAttachmentDescription2" "format"
    format :: Format
  , -- No documentation found for Nested "VkAttachmentDescription2" "samples"
    samples :: SampleCountFlagBits
  , -- No documentation found for Nested "VkAttachmentDescription2" "loadOp"
    loadOp :: AttachmentLoadOp
  , -- No documentation found for Nested "VkAttachmentDescription2" "storeOp"
    storeOp :: AttachmentStoreOp
  , -- No documentation found for Nested "VkAttachmentDescription2" "stencilLoadOp"
    stencilLoadOp :: AttachmentLoadOp
  , -- No documentation found for Nested "VkAttachmentDescription2" "stencilStoreOp"
    stencilStoreOp :: AttachmentStoreOp
  , -- No documentation found for Nested "VkAttachmentDescription2" "initialLayout"
    initialLayout :: ImageLayout
  , -- No documentation found for Nested "VkAttachmentDescription2" "finalLayout"
    finalLayout :: ImageLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentDescription2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (AttachmentDescription2 es)

instance Extensible AttachmentDescription2 where
  extensibleType = STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2
  setNext x next = x{next = next}
  getNext AttachmentDescription2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends AttachmentDescription2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @AttachmentDescriptionStencilLayout = Just f
    | otherwise = Nothing

instance (Extendss AttachmentDescription2 es, PokeChain es) => ToCStruct (AttachmentDescription2 es) where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentDescription2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr AttachmentDescriptionFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Format)) (format)
    lift $ poke ((p `plusPtr` 24 :: Ptr SampleCountFlagBits)) (samples)
    lift $ poke ((p `plusPtr` 28 :: Ptr AttachmentLoadOp)) (loadOp)
    lift $ poke ((p `plusPtr` 32 :: Ptr AttachmentStoreOp)) (storeOp)
    lift $ poke ((p `plusPtr` 36 :: Ptr AttachmentLoadOp)) (stencilLoadOp)
    lift $ poke ((p `plusPtr` 40 :: Ptr AttachmentStoreOp)) (stencilStoreOp)
    lift $ poke ((p `plusPtr` 44 :: Ptr ImageLayout)) (initialLayout)
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageLayout)) (finalLayout)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr SampleCountFlagBits)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr AttachmentLoadOp)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr AttachmentStoreOp)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr AttachmentLoadOp)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr AttachmentStoreOp)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageLayout)) (zero)
    lift $ f

instance (Extendss AttachmentDescription2 es, PeekChain es) => FromCStruct (AttachmentDescription2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @AttachmentDescriptionFlags ((p `plusPtr` 16 :: Ptr AttachmentDescriptionFlags))
    format <- peek @Format ((p `plusPtr` 20 :: Ptr Format))
    samples <- peek @SampleCountFlagBits ((p `plusPtr` 24 :: Ptr SampleCountFlagBits))
    loadOp <- peek @AttachmentLoadOp ((p `plusPtr` 28 :: Ptr AttachmentLoadOp))
    storeOp <- peek @AttachmentStoreOp ((p `plusPtr` 32 :: Ptr AttachmentStoreOp))
    stencilLoadOp <- peek @AttachmentLoadOp ((p `plusPtr` 36 :: Ptr AttachmentLoadOp))
    stencilStoreOp <- peek @AttachmentStoreOp ((p `plusPtr` 40 :: Ptr AttachmentStoreOp))
    initialLayout <- peek @ImageLayout ((p `plusPtr` 44 :: Ptr ImageLayout))
    finalLayout <- peek @ImageLayout ((p `plusPtr` 48 :: Ptr ImageLayout))
    pure $ AttachmentDescription2
             next flags format samples loadOp storeOp stencilLoadOp stencilStoreOp initialLayout finalLayout

instance es ~ '[] => Zero (AttachmentDescription2 es) where
  zero = AttachmentDescription2
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkAttachmentReference2"
data AttachmentReference2 (es :: [Type]) = AttachmentReference2
  { -- No documentation found for Nested "VkAttachmentReference2" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkAttachmentReference2" "attachment"
    attachment :: Word32
  , -- No documentation found for Nested "VkAttachmentReference2" "layout"
    layout :: ImageLayout
  , -- No documentation found for Nested "VkAttachmentReference2" "aspectMask"
    aspectMask :: ImageAspectFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentReference2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (AttachmentReference2 es)

instance Extensible AttachmentReference2 where
  extensibleType = STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2
  setNext x next = x{next = next}
  getNext AttachmentReference2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends AttachmentReference2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @AttachmentReferenceStencilLayout = Just f
    | otherwise = Nothing

instance (Extendss AttachmentReference2 es, PokeChain es) => ToCStruct (AttachmentReference2 es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentReference2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (attachment)
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageLayout)) (layout)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageAspectFlags)) (aspectMask)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageAspectFlags)) (zero)
    lift $ f

instance (Extendss AttachmentReference2 es, PeekChain es) => FromCStruct (AttachmentReference2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    attachment <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    layout <- peek @ImageLayout ((p `plusPtr` 20 :: Ptr ImageLayout))
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 24 :: Ptr ImageAspectFlags))
    pure $ AttachmentReference2
             next attachment layout aspectMask

instance es ~ '[] => Zero (AttachmentReference2 es) where
  zero = AttachmentReference2
           ()
           zero
           zero
           zero



-- No documentation found for TopLevel "VkSubpassDescription2"
data SubpassDescription2 (es :: [Type]) = SubpassDescription2
  { -- No documentation found for Nested "VkSubpassDescription2" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkSubpassDescription2" "flags"
    flags :: SubpassDescriptionFlags
  , -- No documentation found for Nested "VkSubpassDescription2" "pipelineBindPoint"
    pipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "VkSubpassDescription2" "viewMask"
    viewMask :: Word32
  , -- No documentation found for Nested "VkSubpassDescription2" "pInputAttachments"
    inputAttachments :: Vector (SomeStruct AttachmentReference2)
  , -- No documentation found for Nested "VkSubpassDescription2" "pColorAttachments"
    colorAttachments :: Vector (SomeStruct AttachmentReference2)
  , -- No documentation found for Nested "VkSubpassDescription2" "pResolveAttachments"
    resolveAttachments :: Vector (SomeStruct AttachmentReference2)
  , -- No documentation found for Nested "VkSubpassDescription2" "pDepthStencilAttachment"
    depthStencilAttachment :: Maybe (SomeStruct AttachmentReference2)
  , -- No documentation found for Nested "VkSubpassDescription2" "pPreserveAttachments"
    preserveAttachments :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassDescription2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SubpassDescription2 es)

instance Extensible SubpassDescription2 where
  extensibleType = STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2
  setNext x next = x{next = next}
  getNext SubpassDescription2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SubpassDescription2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @FragmentShadingRateAttachmentInfoKHR = Just f
    | Just Refl <- eqT @e @SubpassDescriptionDepthStencilResolve = Just f
    | otherwise = Nothing

instance (Extendss SubpassDescription2 es, PokeChain es) => ToCStruct (SubpassDescription2 es) where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassDescription2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SubpassDescriptionFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (viewMask)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (inputAttachments)) :: Word32))
    pPInputAttachments' <- ContT $ allocaBytesAligned @(AttachmentReference2 _) ((Data.Vector.length (inputAttachments)) * 32) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPInputAttachments' `plusPtr` (32 * (i)) :: Ptr (AttachmentReference2 _))) (e) . ($ ())) (inputAttachments)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr (AttachmentReference2 _)))) (pPInputAttachments')
    let pColorAttachmentsLength = Data.Vector.length $ (colorAttachments)
    let pResolveAttachmentsLength = Data.Vector.length $ (resolveAttachments)
    lift $ unless (fromIntegral pResolveAttachmentsLength == pColorAttachmentsLength || pResolveAttachmentsLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pResolveAttachments and pColorAttachments must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral pColorAttachmentsLength :: Word32))
    pPColorAttachments' <- ContT $ allocaBytesAligned @(AttachmentReference2 _) ((Data.Vector.length (colorAttachments)) * 32) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPColorAttachments' `plusPtr` (32 * (i)) :: Ptr (AttachmentReference2 _))) (e) . ($ ())) (colorAttachments)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (AttachmentReference2 _)))) (pPColorAttachments')
    pResolveAttachments'' <- if Data.Vector.null (resolveAttachments)
      then pure nullPtr
      else do
        pPResolveAttachments <- ContT $ allocaBytesAligned @(AttachmentReference2 _) (((Data.Vector.length (resolveAttachments))) * 32) 8
        Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPResolveAttachments `plusPtr` (32 * (i)) :: Ptr (AttachmentReference2 _))) (e) . ($ ())) ((resolveAttachments))
        pure $ pPResolveAttachments
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (AttachmentReference2 _)))) pResolveAttachments''
    pDepthStencilAttachment'' <- case (depthStencilAttachment) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (AttachmentReference2 '[])) $ \cont -> withSomeCStruct @AttachmentReference2 (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr (AttachmentReference2 _)))) pDepthStencilAttachment''
    lift $ poke ((p `plusPtr` 72 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (preserveAttachments)) :: Word32))
    pPPreserveAttachments' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (preserveAttachments)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPreserveAttachments' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (preserveAttachments)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr Word32))) (pPPreserveAttachments')
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    pPInputAttachments' <- ContT $ allocaBytesAligned @(AttachmentReference2 _) ((Data.Vector.length (mempty)) * 32) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPInputAttachments' `plusPtr` (32 * (i)) :: Ptr (AttachmentReference2 _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr (AttachmentReference2 _)))) (pPInputAttachments')
    pPColorAttachments' <- ContT $ allocaBytesAligned @(AttachmentReference2 _) ((Data.Vector.length (mempty)) * 32) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPColorAttachments' `plusPtr` (32 * (i)) :: Ptr (AttachmentReference2 _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (AttachmentReference2 _)))) (pPColorAttachments')
    pPPreserveAttachments' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPreserveAttachments' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr Word32))) (pPPreserveAttachments')
    lift $ f

instance (Extendss SubpassDescription2 es, PeekChain es) => FromCStruct (SubpassDescription2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @SubpassDescriptionFlags ((p `plusPtr` 16 :: Ptr SubpassDescriptionFlags))
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 20 :: Ptr PipelineBindPoint))
    viewMask <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    inputAttachmentCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pInputAttachments <- peek @(Ptr (AttachmentReference2 _)) ((p `plusPtr` 32 :: Ptr (Ptr (AttachmentReference2 a))))
    pInputAttachments' <- generateM (fromIntegral inputAttachmentCount) (\i -> peekSomeCStruct (forgetExtensions ((pInputAttachments `advancePtrBytes` (32 * (i)) :: Ptr (AttachmentReference2 _)))))
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pColorAttachments <- peek @(Ptr (AttachmentReference2 _)) ((p `plusPtr` 48 :: Ptr (Ptr (AttachmentReference2 a))))
    pColorAttachments' <- generateM (fromIntegral colorAttachmentCount) (\i -> peekSomeCStruct (forgetExtensions ((pColorAttachments `advancePtrBytes` (32 * (i)) :: Ptr (AttachmentReference2 _)))))
    pResolveAttachments <- peek @(Ptr (AttachmentReference2 _)) ((p `plusPtr` 56 :: Ptr (Ptr (AttachmentReference2 a))))
    let pResolveAttachmentsLength = if pResolveAttachments == nullPtr then 0 else (fromIntegral colorAttachmentCount)
    pResolveAttachments' <- generateM pResolveAttachmentsLength (\i -> peekSomeCStruct (forgetExtensions ((pResolveAttachments `advancePtrBytes` (32 * (i)) :: Ptr (AttachmentReference2 _)))))
    pDepthStencilAttachment <- peek @(Ptr (AttachmentReference2 _)) ((p `plusPtr` 64 :: Ptr (Ptr (AttachmentReference2 a))))
    pDepthStencilAttachment' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pDepthStencilAttachment
    preserveAttachmentCount <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    pPreserveAttachments <- peek @(Ptr Word32) ((p `plusPtr` 80 :: Ptr (Ptr Word32)))
    pPreserveAttachments' <- generateM (fromIntegral preserveAttachmentCount) (\i -> peek @Word32 ((pPreserveAttachments `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ SubpassDescription2
             next flags pipelineBindPoint viewMask pInputAttachments' pColorAttachments' pResolveAttachments' pDepthStencilAttachment' pPreserveAttachments'

instance es ~ '[] => Zero (SubpassDescription2 es) where
  zero = SubpassDescription2
           ()
           zero
           zero
           zero
           mempty
           mempty
           mempty
           Nothing
           mempty



-- No documentation found for TopLevel "VkSubpassDependency2"
data SubpassDependency2 = SubpassDependency2
  { -- No documentation found for Nested "VkSubpassDependency2" "srcSubpass"
    srcSubpass :: Word32
  , -- No documentation found for Nested "VkSubpassDependency2" "dstSubpass"
    dstSubpass :: Word32
  , -- No documentation found for Nested "VkSubpassDependency2" "srcStageMask"
    srcStageMask :: PipelineStageFlags
  , -- No documentation found for Nested "VkSubpassDependency2" "dstStageMask"
    dstStageMask :: PipelineStageFlags
  , -- No documentation found for Nested "VkSubpassDependency2" "srcAccessMask"
    srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "VkSubpassDependency2" "dstAccessMask"
    dstAccessMask :: AccessFlags
  , -- No documentation found for Nested "VkSubpassDependency2" "dependencyFlags"
    dependencyFlags :: DependencyFlags
  , -- No documentation found for Nested "VkSubpassDependency2" "viewOffset"
    viewOffset :: Int32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassDependency2)
#endif
deriving instance Show SubpassDependency2

instance ToCStruct SubpassDependency2 where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassDependency2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (srcSubpass)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (dstSubpass)
    poke ((p `plusPtr` 24 :: Ptr PipelineStageFlags)) (srcStageMask)
    poke ((p `plusPtr` 28 :: Ptr PipelineStageFlags)) (dstStageMask)
    poke ((p `plusPtr` 32 :: Ptr AccessFlags)) (srcAccessMask)
    poke ((p `plusPtr` 36 :: Ptr AccessFlags)) (dstAccessMask)
    poke ((p `plusPtr` 40 :: Ptr DependencyFlags)) (dependencyFlags)
    poke ((p `plusPtr` 44 :: Ptr Int32)) (viewOffset)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PipelineStageFlags)) (zero)
    poke ((p `plusPtr` 28 :: Ptr PipelineStageFlags)) (zero)
    f

instance FromCStruct SubpassDependency2 where
  peekCStruct p = do
    srcSubpass <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    dstSubpass <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    srcStageMask <- peek @PipelineStageFlags ((p `plusPtr` 24 :: Ptr PipelineStageFlags))
    dstStageMask <- peek @PipelineStageFlags ((p `plusPtr` 28 :: Ptr PipelineStageFlags))
    srcAccessMask <- peek @AccessFlags ((p `plusPtr` 32 :: Ptr AccessFlags))
    dstAccessMask <- peek @AccessFlags ((p `plusPtr` 36 :: Ptr AccessFlags))
    dependencyFlags <- peek @DependencyFlags ((p `plusPtr` 40 :: Ptr DependencyFlags))
    viewOffset <- peek @Int32 ((p `plusPtr` 44 :: Ptr Int32))
    pure $ SubpassDependency2
             srcSubpass dstSubpass srcStageMask dstStageMask srcAccessMask dstAccessMask dependencyFlags viewOffset


instance Storable SubpassDependency2 where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubpassDependency2 where
  zero = SubpassDependency2
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkRenderPassCreateInfo2"
data RenderPassCreateInfo2 (es :: [Type]) = RenderPassCreateInfo2
  { -- No documentation found for Nested "VkRenderPassCreateInfo2" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkRenderPassCreateInfo2" "flags"
    flags :: RenderPassCreateFlags
  , -- No documentation found for Nested "VkRenderPassCreateInfo2" "pAttachments"
    attachments :: Vector (SomeStruct AttachmentDescription2)
  , -- No documentation found for Nested "VkRenderPassCreateInfo2" "pSubpasses"
    subpasses :: Vector (SomeStruct SubpassDescription2)
  , -- No documentation found for Nested "VkRenderPassCreateInfo2" "pDependencies"
    dependencies :: Vector SubpassDependency2
  , -- No documentation found for Nested "VkRenderPassCreateInfo2" "pCorrelatedViewMasks"
    correlatedViewMasks :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassCreateInfo2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RenderPassCreateInfo2 es)

instance Extensible RenderPassCreateInfo2 where
  extensibleType = STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2
  setNext x next = x{next = next}
  getNext RenderPassCreateInfo2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RenderPassCreateInfo2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @RenderPassFragmentDensityMapCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss RenderPassCreateInfo2 es, PokeChain es) => ToCStruct (RenderPassCreateInfo2 es) where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassCreateInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderPassCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (attachments)) :: Word32))
    pPAttachments' <- ContT $ allocaBytesAligned @(AttachmentDescription2 _) ((Data.Vector.length (attachments)) * 56) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPAttachments' `plusPtr` (56 * (i)) :: Ptr (AttachmentDescription2 _))) (e) . ($ ())) (attachments)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (AttachmentDescription2 _)))) (pPAttachments')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (subpasses)) :: Word32))
    pPSubpasses' <- ContT $ allocaBytesAligned @(SubpassDescription2 _) ((Data.Vector.length (subpasses)) * 88) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPSubpasses' `plusPtr` (88 * (i)) :: Ptr (SubpassDescription2 _))) (e) . ($ ())) (subpasses)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (SubpassDescription2 _)))) (pPSubpasses')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (dependencies)) :: Word32))
    pPDependencies' <- ContT $ allocaBytesAligned @SubpassDependency2 ((Data.Vector.length (dependencies)) * 48) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDependencies' `plusPtr` (48 * (i)) :: Ptr SubpassDependency2) (e)) (dependencies)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr SubpassDependency2))) (pPDependencies')
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (correlatedViewMasks)) :: Word32))
    pPCorrelatedViewMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (correlatedViewMasks)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCorrelatedViewMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (correlatedViewMasks)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr Word32))) (pPCorrelatedViewMasks')
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    pPAttachments' <- ContT $ allocaBytesAligned @(AttachmentDescription2 _) ((Data.Vector.length (mempty)) * 56) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPAttachments' `plusPtr` (56 * (i)) :: Ptr (AttachmentDescription2 _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (AttachmentDescription2 _)))) (pPAttachments')
    pPSubpasses' <- ContT $ allocaBytesAligned @(SubpassDescription2 _) ((Data.Vector.length (mempty)) * 88) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPSubpasses' `plusPtr` (88 * (i)) :: Ptr (SubpassDescription2 _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (SubpassDescription2 _)))) (pPSubpasses')
    pPDependencies' <- ContT $ allocaBytesAligned @SubpassDependency2 ((Data.Vector.length (mempty)) * 48) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDependencies' `plusPtr` (48 * (i)) :: Ptr SubpassDependency2) (e)) (mempty)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr SubpassDependency2))) (pPDependencies')
    pPCorrelatedViewMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCorrelatedViewMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr Word32))) (pPCorrelatedViewMasks')
    lift $ f

instance (Extendss RenderPassCreateInfo2 es, PeekChain es) => FromCStruct (RenderPassCreateInfo2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @RenderPassCreateFlags ((p `plusPtr` 16 :: Ptr RenderPassCreateFlags))
    attachmentCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pAttachments <- peek @(Ptr (AttachmentDescription2 _)) ((p `plusPtr` 24 :: Ptr (Ptr (AttachmentDescription2 a))))
    pAttachments' <- generateM (fromIntegral attachmentCount) (\i -> peekSomeCStruct (forgetExtensions ((pAttachments `advancePtrBytes` (56 * (i)) :: Ptr (AttachmentDescription2 _)))))
    subpassCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pSubpasses <- peek @(Ptr (SubpassDescription2 _)) ((p `plusPtr` 40 :: Ptr (Ptr (SubpassDescription2 a))))
    pSubpasses' <- generateM (fromIntegral subpassCount) (\i -> peekSomeCStruct (forgetExtensions ((pSubpasses `advancePtrBytes` (88 * (i)) :: Ptr (SubpassDescription2 _)))))
    dependencyCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pDependencies <- peek @(Ptr SubpassDependency2) ((p `plusPtr` 56 :: Ptr (Ptr SubpassDependency2)))
    pDependencies' <- generateM (fromIntegral dependencyCount) (\i -> peekCStruct @SubpassDependency2 ((pDependencies `advancePtrBytes` (48 * (i)) :: Ptr SubpassDependency2)))
    correlatedViewMaskCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pCorrelatedViewMasks <- peek @(Ptr Word32) ((p `plusPtr` 72 :: Ptr (Ptr Word32)))
    pCorrelatedViewMasks' <- generateM (fromIntegral correlatedViewMaskCount) (\i -> peek @Word32 ((pCorrelatedViewMasks `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ RenderPassCreateInfo2
             next flags pAttachments' pSubpasses' pDependencies' pCorrelatedViewMasks'

instance es ~ '[] => Zero (RenderPassCreateInfo2 es) where
  zero = RenderPassCreateInfo2
           ()
           zero
           mempty
           mempty
           mempty
           mempty



-- No documentation found for TopLevel "VkSubpassBeginInfo"
data SubpassBeginInfo = SubpassBeginInfo
  { -- No documentation found for Nested "VkSubpassBeginInfo" "contents"
    contents :: SubpassContents }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassBeginInfo)
#endif
deriving instance Show SubpassBeginInfo

instance ToCStruct SubpassBeginInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassBeginInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_BEGIN_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SubpassContents)) (contents)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_BEGIN_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SubpassContents)) (zero)
    f

instance FromCStruct SubpassBeginInfo where
  peekCStruct p = do
    contents <- peek @SubpassContents ((p `plusPtr` 16 :: Ptr SubpassContents))
    pure $ SubpassBeginInfo
             contents


instance Storable SubpassBeginInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubpassBeginInfo where
  zero = SubpassBeginInfo
           zero



-- No documentation found for TopLevel "VkSubpassEndInfo"
data SubpassEndInfo = SubpassEndInfo
  {}
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassEndInfo)
#endif
deriving instance Show SubpassEndInfo

instance ToCStruct SubpassEndInfo where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassEndInfo f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_END_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_END_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SubpassEndInfo where
  peekCStruct _ = pure $ SubpassEndInfo
                           


instance Storable SubpassEndInfo where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubpassEndInfo where
  zero = SubpassEndInfo
           

