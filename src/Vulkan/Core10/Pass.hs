{-# language CPP #-}
-- No documentation found for Chapter "Pass"
module Vulkan.Core10.Pass  ( createFramebuffer
                           , withFramebuffer
                           , destroyFramebuffer
                           , createRenderPass
                           , withRenderPass
                           , destroyRenderPass
                           , getRenderAreaGranularity
                           , AttachmentDescription(..)
                           , AttachmentReference(..)
                           , SubpassDescription(..)
                           , SubpassDependency(..)
                           , RenderPassCreateInfo(..)
                           , FramebufferCreateInfo(..)
                           , Framebuffer(..)
                           , RenderPass(..)
                           , AttachmentLoadOp(..)
                           , AttachmentStoreOp(..)
                           , PipelineBindPoint(..)
                           , RenderPassCreateFlagBits(..)
                           , RenderPassCreateFlags
                           , AccessFlagBits(..)
                           , AccessFlags
                           , AttachmentDescriptionFlagBits(..)
                           , AttachmentDescriptionFlags
                           , DependencyFlagBits(..)
                           , DependencyFlags
                           , SubpassDescriptionFlagBits(..)
                           , SubpassDescriptionFlags
                           , FramebufferCreateFlagBits(..)
                           , FramebufferCreateFlags
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
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Enums.AttachmentDescriptionFlagBits (AttachmentDescriptionFlags)
import Vulkan.Core10.Enums.AttachmentLoadOp (AttachmentLoadOp)
import Vulkan.Core10.Enums.AttachmentStoreOp (AttachmentStoreOp)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateFramebuffer))
import Vulkan.Dynamic (DeviceCmds(pVkCreateRenderPass))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyFramebuffer))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyRenderPass))
import Vulkan.Dynamic (DeviceCmds(pVkGetRenderAreaGranularity))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Handles (Framebuffer)
import Vulkan.Core10.Handles (Framebuffer(..))
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (FramebufferAttachmentsCreateInfo)
import Vulkan.Core10.Enums.FramebufferCreateFlagBits (FramebufferCreateFlags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Handles (ImageView)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Handles (RenderPass)
import Vulkan.Core10.Handles (RenderPass(..))
import Vulkan.Core10.Enums.RenderPassCreateFlagBits (RenderPassCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (RenderPassFragmentDensityMapCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (RenderPassInputAttachmentAspectCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_multiview (RenderPassMultiviewCreateInfo)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.SubpassDescriptionFlagBits (SubpassDescriptionFlags)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(..))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AttachmentDescriptionFlagBits (AttachmentDescriptionFlagBits(..))
import Vulkan.Core10.Enums.AttachmentDescriptionFlagBits (AttachmentDescriptionFlags)
import Vulkan.Core10.Enums.AttachmentLoadOp (AttachmentLoadOp(..))
import Vulkan.Core10.Enums.AttachmentStoreOp (AttachmentStoreOp(..))
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlagBits(..))
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core10.Handles (Framebuffer(..))
import Vulkan.Core10.Enums.FramebufferCreateFlagBits (FramebufferCreateFlagBits(..))
import Vulkan.Core10.Enums.FramebufferCreateFlagBits (FramebufferCreateFlags)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Vulkan.Core10.Handles (RenderPass(..))
import Vulkan.Core10.Enums.RenderPassCreateFlagBits (RenderPassCreateFlagBits(..))
import Vulkan.Core10.Enums.RenderPassCreateFlagBits (RenderPassCreateFlags)
import Vulkan.Core10.Enums.SubpassDescriptionFlagBits (SubpassDescriptionFlagBits(..))
import Vulkan.Core10.Enums.SubpassDescriptionFlagBits (SubpassDescriptionFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateFramebuffer
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct FramebufferCreateInfo) -> Ptr AllocationCallbacks -> Ptr Framebuffer -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct FramebufferCreateInfo) -> Ptr AllocationCallbacks -> Ptr Framebuffer -> IO Result

-- No documentation found for TopLevel "vkCreateFramebuffer"
createFramebuffer :: forall a io
                   . (Extendss FramebufferCreateInfo a, PokeChain a, MonadIO io)
                  => -- No documentation found for Nested "vkCreateFramebuffer" "device"
                     Device
                  -> -- No documentation found for Nested "vkCreateFramebuffer" "pCreateInfo"
                     (FramebufferCreateInfo a)
                  -> -- No documentation found for Nested "vkCreateFramebuffer" "pAllocator"
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io (Framebuffer)
createFramebuffer device createInfo allocator = liftIO . evalContT $ do
  let vkCreateFramebufferPtr = pVkCreateFramebuffer (deviceCmds (device :: Device))
  lift $ unless (vkCreateFramebufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateFramebuffer is null" Nothing Nothing
  let vkCreateFramebuffer' = mkVkCreateFramebuffer vkCreateFramebufferPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPFramebuffer <- ContT $ bracket (callocBytes @Framebuffer 8) free
  r <- lift $ vkCreateFramebuffer' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPFramebuffer)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFramebuffer <- lift $ peek @Framebuffer pPFramebuffer
  pure $ (pFramebuffer)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createFramebuffer' and 'destroyFramebuffer'
--
-- To ensure that 'destroyFramebuffer' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withFramebuffer :: forall a io r . (Extendss FramebufferCreateInfo a, PokeChain a, MonadIO io) => Device -> FramebufferCreateInfo a -> Maybe AllocationCallbacks -> (io Framebuffer -> (Framebuffer -> io ()) -> r) -> r
withFramebuffer device pCreateInfo pAllocator b =
  b (createFramebuffer device pCreateInfo pAllocator)
    (\(o0) -> destroyFramebuffer device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyFramebuffer
  :: FunPtr (Ptr Device_T -> Framebuffer -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Framebuffer -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyFramebuffer"
destroyFramebuffer :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkDestroyFramebuffer" "device"
                      Device
                   -> -- No documentation found for Nested "vkDestroyFramebuffer" "framebuffer"
                      Framebuffer
                   -> -- No documentation found for Nested "vkDestroyFramebuffer" "pAllocator"
                      ("allocator" ::: Maybe AllocationCallbacks)
                   -> io ()
destroyFramebuffer device framebuffer allocator = liftIO . evalContT $ do
  let vkDestroyFramebufferPtr = pVkDestroyFramebuffer (deviceCmds (device :: Device))
  lift $ unless (vkDestroyFramebufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyFramebuffer is null" Nothing Nothing
  let vkDestroyFramebuffer' = mkVkDestroyFramebuffer vkDestroyFramebufferPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyFramebuffer' (deviceHandle (device)) (framebuffer) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRenderPass
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct RenderPassCreateInfo) -> Ptr AllocationCallbacks -> Ptr RenderPass -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct RenderPassCreateInfo) -> Ptr AllocationCallbacks -> Ptr RenderPass -> IO Result

-- No documentation found for TopLevel "vkCreateRenderPass"
createRenderPass :: forall a io
                  . (Extendss RenderPassCreateInfo a, PokeChain a, MonadIO io)
                 => -- No documentation found for Nested "vkCreateRenderPass" "device"
                    Device
                 -> -- No documentation found for Nested "vkCreateRenderPass" "pCreateInfo"
                    (RenderPassCreateInfo a)
                 -> -- No documentation found for Nested "vkCreateRenderPass" "pAllocator"
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io (RenderPass)
createRenderPass device createInfo allocator = liftIO . evalContT $ do
  let vkCreateRenderPassPtr = pVkCreateRenderPass (deviceCmds (device :: Device))
  lift $ unless (vkCreateRenderPassPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateRenderPass is null" Nothing Nothing
  let vkCreateRenderPass' = mkVkCreateRenderPass vkCreateRenderPassPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPRenderPass <- ContT $ bracket (callocBytes @RenderPass 8) free
  r <- lift $ vkCreateRenderPass' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPRenderPass)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pRenderPass <- lift $ peek @RenderPass pPRenderPass
  pure $ (pRenderPass)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createRenderPass' and 'destroyRenderPass'
--
-- To ensure that 'destroyRenderPass' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withRenderPass :: forall a io r . (Extendss RenderPassCreateInfo a, PokeChain a, MonadIO io) => Device -> RenderPassCreateInfo a -> Maybe AllocationCallbacks -> (io RenderPass -> (RenderPass -> io ()) -> r) -> r
withRenderPass device pCreateInfo pAllocator b =
  b (createRenderPass device pCreateInfo pAllocator)
    (\(o0) -> destroyRenderPass device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyRenderPass
  :: FunPtr (Ptr Device_T -> RenderPass -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> RenderPass -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyRenderPass"
destroyRenderPass :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkDestroyRenderPass" "device"
                     Device
                  -> -- No documentation found for Nested "vkDestroyRenderPass" "renderPass"
                     RenderPass
                  -> -- No documentation found for Nested "vkDestroyRenderPass" "pAllocator"
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io ()
destroyRenderPass device renderPass allocator = liftIO . evalContT $ do
  let vkDestroyRenderPassPtr = pVkDestroyRenderPass (deviceCmds (device :: Device))
  lift $ unless (vkDestroyRenderPassPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyRenderPass is null" Nothing Nothing
  let vkDestroyRenderPass' = mkVkDestroyRenderPass vkDestroyRenderPassPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyRenderPass' (deviceHandle (device)) (renderPass) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRenderAreaGranularity
  :: FunPtr (Ptr Device_T -> RenderPass -> Ptr Extent2D -> IO ()) -> Ptr Device_T -> RenderPass -> Ptr Extent2D -> IO ()

-- No documentation found for TopLevel "vkGetRenderAreaGranularity"
getRenderAreaGranularity :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkGetRenderAreaGranularity" "device"
                            Device
                         -> -- No documentation found for Nested "vkGetRenderAreaGranularity" "renderPass"
                            RenderPass
                         -> io (("granularity" ::: Extent2D))
getRenderAreaGranularity device renderPass = liftIO . evalContT $ do
  let vkGetRenderAreaGranularityPtr = pVkGetRenderAreaGranularity (deviceCmds (device :: Device))
  lift $ unless (vkGetRenderAreaGranularityPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRenderAreaGranularity is null" Nothing Nothing
  let vkGetRenderAreaGranularity' = mkVkGetRenderAreaGranularity vkGetRenderAreaGranularityPtr
  pPGranularity <- ContT (withZeroCStruct @Extent2D)
  lift $ vkGetRenderAreaGranularity' (deviceHandle (device)) (renderPass) (pPGranularity)
  pGranularity <- lift $ peekCStruct @Extent2D pPGranularity
  pure $ (pGranularity)



-- No documentation found for TopLevel "VkAttachmentDescription"
data AttachmentDescription = AttachmentDescription
  { -- No documentation found for Nested "VkAttachmentDescription" "flags"
    flags :: AttachmentDescriptionFlags
  , -- No documentation found for Nested "VkAttachmentDescription" "format"
    format :: Format
  , -- No documentation found for Nested "VkAttachmentDescription" "samples"
    samples :: SampleCountFlagBits
  , -- No documentation found for Nested "VkAttachmentDescription" "loadOp"
    loadOp :: AttachmentLoadOp
  , -- No documentation found for Nested "VkAttachmentDescription" "storeOp"
    storeOp :: AttachmentStoreOp
  , -- No documentation found for Nested "VkAttachmentDescription" "stencilLoadOp"
    stencilLoadOp :: AttachmentLoadOp
  , -- No documentation found for Nested "VkAttachmentDescription" "stencilStoreOp"
    stencilStoreOp :: AttachmentStoreOp
  , -- No documentation found for Nested "VkAttachmentDescription" "initialLayout"
    initialLayout :: ImageLayout
  , -- No documentation found for Nested "VkAttachmentDescription" "finalLayout"
    finalLayout :: ImageLayout
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentDescription)
#endif
deriving instance Show AttachmentDescription

instance ToCStruct AttachmentDescription where
  withCStruct x f = allocaBytesAligned 36 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentDescription{..} f = do
    poke ((p `plusPtr` 0 :: Ptr AttachmentDescriptionFlags)) (flags)
    poke ((p `plusPtr` 4 :: Ptr Format)) (format)
    poke ((p `plusPtr` 8 :: Ptr SampleCountFlagBits)) (samples)
    poke ((p `plusPtr` 12 :: Ptr AttachmentLoadOp)) (loadOp)
    poke ((p `plusPtr` 16 :: Ptr AttachmentStoreOp)) (storeOp)
    poke ((p `plusPtr` 20 :: Ptr AttachmentLoadOp)) (stencilLoadOp)
    poke ((p `plusPtr` 24 :: Ptr AttachmentStoreOp)) (stencilStoreOp)
    poke ((p `plusPtr` 28 :: Ptr ImageLayout)) (initialLayout)
    poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (finalLayout)
    f
  cStructSize = 36
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 4 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 8 :: Ptr SampleCountFlagBits)) (zero)
    poke ((p `plusPtr` 12 :: Ptr AttachmentLoadOp)) (zero)
    poke ((p `plusPtr` 16 :: Ptr AttachmentStoreOp)) (zero)
    poke ((p `plusPtr` 20 :: Ptr AttachmentLoadOp)) (zero)
    poke ((p `plusPtr` 24 :: Ptr AttachmentStoreOp)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct AttachmentDescription where
  peekCStruct p = do
    flags <- peek @AttachmentDescriptionFlags ((p `plusPtr` 0 :: Ptr AttachmentDescriptionFlags))
    format <- peek @Format ((p `plusPtr` 4 :: Ptr Format))
    samples <- peek @SampleCountFlagBits ((p `plusPtr` 8 :: Ptr SampleCountFlagBits))
    loadOp <- peek @AttachmentLoadOp ((p `plusPtr` 12 :: Ptr AttachmentLoadOp))
    storeOp <- peek @AttachmentStoreOp ((p `plusPtr` 16 :: Ptr AttachmentStoreOp))
    stencilLoadOp <- peek @AttachmentLoadOp ((p `plusPtr` 20 :: Ptr AttachmentLoadOp))
    stencilStoreOp <- peek @AttachmentStoreOp ((p `plusPtr` 24 :: Ptr AttachmentStoreOp))
    initialLayout <- peek @ImageLayout ((p `plusPtr` 28 :: Ptr ImageLayout))
    finalLayout <- peek @ImageLayout ((p `plusPtr` 32 :: Ptr ImageLayout))
    pure $ AttachmentDescription
             flags format samples loadOp storeOp stencilLoadOp stencilStoreOp initialLayout finalLayout


instance Storable AttachmentDescription where
  sizeOf ~_ = 36
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AttachmentDescription where
  zero = AttachmentDescription
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkAttachmentReference"
data AttachmentReference = AttachmentReference
  { -- No documentation found for Nested "VkAttachmentReference" "attachment"
    attachment :: Word32
  , -- No documentation found for Nested "VkAttachmentReference" "layout"
    layout :: ImageLayout
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentReference)
#endif
deriving instance Show AttachmentReference

instance ToCStruct AttachmentReference where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentReference{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (attachment)
    poke ((p `plusPtr` 4 :: Ptr ImageLayout)) (layout)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct AttachmentReference where
  peekCStruct p = do
    attachment <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    layout <- peek @ImageLayout ((p `plusPtr` 4 :: Ptr ImageLayout))
    pure $ AttachmentReference
             attachment layout


instance Storable AttachmentReference where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AttachmentReference where
  zero = AttachmentReference
           zero
           zero



-- No documentation found for TopLevel "VkSubpassDescription"
data SubpassDescription = SubpassDescription
  { -- No documentation found for Nested "VkSubpassDescription" "flags"
    flags :: SubpassDescriptionFlags
  , -- No documentation found for Nested "VkSubpassDescription" "pipelineBindPoint"
    pipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "VkSubpassDescription" "pInputAttachments"
    inputAttachments :: Vector AttachmentReference
  , -- No documentation found for Nested "VkSubpassDescription" "pColorAttachments"
    colorAttachments :: Vector AttachmentReference
  , -- No documentation found for Nested "VkSubpassDescription" "pResolveAttachments"
    resolveAttachments :: Vector AttachmentReference
  , -- No documentation found for Nested "VkSubpassDescription" "pDepthStencilAttachment"
    depthStencilAttachment :: Maybe AttachmentReference
  , -- No documentation found for Nested "VkSubpassDescription" "pPreserveAttachments"
    preserveAttachments :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassDescription)
#endif
deriving instance Show SubpassDescription

instance ToCStruct SubpassDescription where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassDescription{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr SubpassDescriptionFlags)) (flags)
    lift $ poke ((p `plusPtr` 4 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (inputAttachments)) :: Word32))
    pPInputAttachments' <- ContT $ allocaBytesAligned @AttachmentReference ((Data.Vector.length (inputAttachments)) * 8) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPInputAttachments' `plusPtr` (8 * (i)) :: Ptr AttachmentReference) (e)) (inputAttachments)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr AttachmentReference))) (pPInputAttachments')
    let pColorAttachmentsLength = Data.Vector.length $ (colorAttachments)
    let pResolveAttachmentsLength = Data.Vector.length $ (resolveAttachments)
    lift $ unless (fromIntegral pResolveAttachmentsLength == pColorAttachmentsLength || pResolveAttachmentsLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pResolveAttachments and pColorAttachments must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral pColorAttachmentsLength :: Word32))
    pPColorAttachments' <- ContT $ allocaBytesAligned @AttachmentReference ((Data.Vector.length (colorAttachments)) * 8) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachments' `plusPtr` (8 * (i)) :: Ptr AttachmentReference) (e)) (colorAttachments)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr AttachmentReference))) (pPColorAttachments')
    pResolveAttachments'' <- if Data.Vector.null (resolveAttachments)
      then pure nullPtr
      else do
        pPResolveAttachments <- ContT $ allocaBytesAligned @AttachmentReference (((Data.Vector.length (resolveAttachments))) * 8) 4
        lift $ Data.Vector.imapM_ (\i e -> poke (pPResolveAttachments `plusPtr` (8 * (i)) :: Ptr AttachmentReference) (e)) ((resolveAttachments))
        pure $ pPResolveAttachments
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr AttachmentReference))) pResolveAttachments''
    pDepthStencilAttachment'' <- case (depthStencilAttachment) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr AttachmentReference))) pDepthStencilAttachment''
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (preserveAttachments)) :: Word32))
    pPPreserveAttachments' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (preserveAttachments)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPreserveAttachments' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (preserveAttachments)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr Word32))) (pPPreserveAttachments')
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 4 :: Ptr PipelineBindPoint)) (zero)
    pPInputAttachments' <- ContT $ allocaBytesAligned @AttachmentReference ((Data.Vector.length (mempty)) * 8) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPInputAttachments' `plusPtr` (8 * (i)) :: Ptr AttachmentReference) (e)) (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr AttachmentReference))) (pPInputAttachments')
    pPColorAttachments' <- ContT $ allocaBytesAligned @AttachmentReference ((Data.Vector.length (mempty)) * 8) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachments' `plusPtr` (8 * (i)) :: Ptr AttachmentReference) (e)) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr AttachmentReference))) (pPColorAttachments')
    pPPreserveAttachments' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPreserveAttachments' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr Word32))) (pPPreserveAttachments')
    lift $ f

instance FromCStruct SubpassDescription where
  peekCStruct p = do
    flags <- peek @SubpassDescriptionFlags ((p `plusPtr` 0 :: Ptr SubpassDescriptionFlags))
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 4 :: Ptr PipelineBindPoint))
    inputAttachmentCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pInputAttachments <- peek @(Ptr AttachmentReference) ((p `plusPtr` 16 :: Ptr (Ptr AttachmentReference)))
    pInputAttachments' <- generateM (fromIntegral inputAttachmentCount) (\i -> peekCStruct @AttachmentReference ((pInputAttachments `advancePtrBytes` (8 * (i)) :: Ptr AttachmentReference)))
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pColorAttachments <- peek @(Ptr AttachmentReference) ((p `plusPtr` 32 :: Ptr (Ptr AttachmentReference)))
    pColorAttachments' <- generateM (fromIntegral colorAttachmentCount) (\i -> peekCStruct @AttachmentReference ((pColorAttachments `advancePtrBytes` (8 * (i)) :: Ptr AttachmentReference)))
    pResolveAttachments <- peek @(Ptr AttachmentReference) ((p `plusPtr` 40 :: Ptr (Ptr AttachmentReference)))
    let pResolveAttachmentsLength = if pResolveAttachments == nullPtr then 0 else (fromIntegral colorAttachmentCount)
    pResolveAttachments' <- generateM pResolveAttachmentsLength (\i -> peekCStruct @AttachmentReference ((pResolveAttachments `advancePtrBytes` (8 * (i)) :: Ptr AttachmentReference)))
    pDepthStencilAttachment <- peek @(Ptr AttachmentReference) ((p `plusPtr` 48 :: Ptr (Ptr AttachmentReference)))
    pDepthStencilAttachment' <- maybePeek (\j -> peekCStruct @AttachmentReference (j)) pDepthStencilAttachment
    preserveAttachmentCount <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    pPreserveAttachments <- peek @(Ptr Word32) ((p `plusPtr` 64 :: Ptr (Ptr Word32)))
    pPreserveAttachments' <- generateM (fromIntegral preserveAttachmentCount) (\i -> peek @Word32 ((pPreserveAttachments `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ SubpassDescription
             flags pipelineBindPoint pInputAttachments' pColorAttachments' pResolveAttachments' pDepthStencilAttachment' pPreserveAttachments'

instance Zero SubpassDescription where
  zero = SubpassDescription
           zero
           zero
           mempty
           mempty
           mempty
           Nothing
           mempty



-- No documentation found for TopLevel "VkSubpassDependency"
data SubpassDependency = SubpassDependency
  { -- No documentation found for Nested "VkSubpassDependency" "srcSubpass"
    srcSubpass :: Word32
  , -- No documentation found for Nested "VkSubpassDependency" "dstSubpass"
    dstSubpass :: Word32
  , -- No documentation found for Nested "VkSubpassDependency" "srcStageMask"
    srcStageMask :: PipelineStageFlags
  , -- No documentation found for Nested "VkSubpassDependency" "dstStageMask"
    dstStageMask :: PipelineStageFlags
  , -- No documentation found for Nested "VkSubpassDependency" "srcAccessMask"
    srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "VkSubpassDependency" "dstAccessMask"
    dstAccessMask :: AccessFlags
  , -- No documentation found for Nested "VkSubpassDependency" "dependencyFlags"
    dependencyFlags :: DependencyFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassDependency)
#endif
deriving instance Show SubpassDependency

instance ToCStruct SubpassDependency where
  withCStruct x f = allocaBytesAligned 28 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassDependency{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (srcSubpass)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (dstSubpass)
    poke ((p `plusPtr` 8 :: Ptr PipelineStageFlags)) (srcStageMask)
    poke ((p `plusPtr` 12 :: Ptr PipelineStageFlags)) (dstStageMask)
    poke ((p `plusPtr` 16 :: Ptr AccessFlags)) (srcAccessMask)
    poke ((p `plusPtr` 20 :: Ptr AccessFlags)) (dstAccessMask)
    poke ((p `plusPtr` 24 :: Ptr DependencyFlags)) (dependencyFlags)
    f
  cStructSize = 28
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr PipelineStageFlags)) (zero)
    poke ((p `plusPtr` 12 :: Ptr PipelineStageFlags)) (zero)
    f

instance FromCStruct SubpassDependency where
  peekCStruct p = do
    srcSubpass <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    dstSubpass <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    srcStageMask <- peek @PipelineStageFlags ((p `plusPtr` 8 :: Ptr PipelineStageFlags))
    dstStageMask <- peek @PipelineStageFlags ((p `plusPtr` 12 :: Ptr PipelineStageFlags))
    srcAccessMask <- peek @AccessFlags ((p `plusPtr` 16 :: Ptr AccessFlags))
    dstAccessMask <- peek @AccessFlags ((p `plusPtr` 20 :: Ptr AccessFlags))
    dependencyFlags <- peek @DependencyFlags ((p `plusPtr` 24 :: Ptr DependencyFlags))
    pure $ SubpassDependency
             srcSubpass dstSubpass srcStageMask dstStageMask srcAccessMask dstAccessMask dependencyFlags


instance Storable SubpassDependency where
  sizeOf ~_ = 28
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubpassDependency where
  zero = SubpassDependency
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkRenderPassCreateInfo"
data RenderPassCreateInfo (es :: [Type]) = RenderPassCreateInfo
  { -- No documentation found for Nested "VkRenderPassCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "flags"
    flags :: RenderPassCreateFlags
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "pAttachments"
    attachments :: Vector AttachmentDescription
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "pSubpasses"
    subpasses :: Vector SubpassDescription
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "pDependencies"
    dependencies :: Vector SubpassDependency
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RenderPassCreateInfo es)

instance Extensible RenderPassCreateInfo where
  extensibleType = STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
  setNext x next = x{next = next}
  getNext RenderPassCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RenderPassCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @RenderPassFragmentDensityMapCreateInfoEXT = Just f
    | Just Refl <- eqT @e @RenderPassInputAttachmentAspectCreateInfo = Just f
    | Just Refl <- eqT @e @RenderPassMultiviewCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss RenderPassCreateInfo es, PokeChain es) => ToCStruct (RenderPassCreateInfo es) where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderPassCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (attachments)) :: Word32))
    pPAttachments' <- ContT $ allocaBytesAligned @AttachmentDescription ((Data.Vector.length (attachments)) * 36) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAttachments' `plusPtr` (36 * (i)) :: Ptr AttachmentDescription) (e)) (attachments)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AttachmentDescription))) (pPAttachments')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (subpasses)) :: Word32))
    pPSubpasses' <- ContT $ allocaBytesAligned @SubpassDescription ((Data.Vector.length (subpasses)) * 72) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPSubpasses' `plusPtr` (72 * (i)) :: Ptr SubpassDescription) (e) . ($ ())) (subpasses)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SubpassDescription))) (pPSubpasses')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (dependencies)) :: Word32))
    pPDependencies' <- ContT $ allocaBytesAligned @SubpassDependency ((Data.Vector.length (dependencies)) * 28) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDependencies' `plusPtr` (28 * (i)) :: Ptr SubpassDependency) (e)) (dependencies)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr SubpassDependency))) (pPDependencies')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    pPAttachments' <- ContT $ allocaBytesAligned @AttachmentDescription ((Data.Vector.length (mempty)) * 36) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAttachments' `plusPtr` (36 * (i)) :: Ptr AttachmentDescription) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AttachmentDescription))) (pPAttachments')
    pPSubpasses' <- ContT $ allocaBytesAligned @SubpassDescription ((Data.Vector.length (mempty)) * 72) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPSubpasses' `plusPtr` (72 * (i)) :: Ptr SubpassDescription) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SubpassDescription))) (pPSubpasses')
    pPDependencies' <- ContT $ allocaBytesAligned @SubpassDependency ((Data.Vector.length (mempty)) * 28) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDependencies' `plusPtr` (28 * (i)) :: Ptr SubpassDependency) (e)) (mempty)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr SubpassDependency))) (pPDependencies')
    lift $ f

instance (Extendss RenderPassCreateInfo es, PeekChain es) => FromCStruct (RenderPassCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @RenderPassCreateFlags ((p `plusPtr` 16 :: Ptr RenderPassCreateFlags))
    attachmentCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pAttachments <- peek @(Ptr AttachmentDescription) ((p `plusPtr` 24 :: Ptr (Ptr AttachmentDescription)))
    pAttachments' <- generateM (fromIntegral attachmentCount) (\i -> peekCStruct @AttachmentDescription ((pAttachments `advancePtrBytes` (36 * (i)) :: Ptr AttachmentDescription)))
    subpassCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pSubpasses <- peek @(Ptr SubpassDescription) ((p `plusPtr` 40 :: Ptr (Ptr SubpassDescription)))
    pSubpasses' <- generateM (fromIntegral subpassCount) (\i -> peekCStruct @SubpassDescription ((pSubpasses `advancePtrBytes` (72 * (i)) :: Ptr SubpassDescription)))
    dependencyCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pDependencies <- peek @(Ptr SubpassDependency) ((p `plusPtr` 56 :: Ptr (Ptr SubpassDependency)))
    pDependencies' <- generateM (fromIntegral dependencyCount) (\i -> peekCStruct @SubpassDependency ((pDependencies `advancePtrBytes` (28 * (i)) :: Ptr SubpassDependency)))
    pure $ RenderPassCreateInfo
             next flags pAttachments' pSubpasses' pDependencies'

instance es ~ '[] => Zero (RenderPassCreateInfo es) where
  zero = RenderPassCreateInfo
           ()
           zero
           mempty
           mempty
           mempty



-- No documentation found for TopLevel "VkFramebufferCreateInfo"
data FramebufferCreateInfo (es :: [Type]) = FramebufferCreateInfo
  { -- No documentation found for Nested "VkFramebufferCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "flags"
    flags :: FramebufferCreateFlags
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "renderPass"
    renderPass :: RenderPass
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "pAttachments"
    attachments :: Vector ImageView
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "width"
    width :: Word32
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "height"
    height :: Word32
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "layers"
    layers :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FramebufferCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (FramebufferCreateInfo es)

instance Extensible FramebufferCreateInfo where
  extensibleType = STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
  setNext x next = x{next = next}
  getNext FramebufferCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends FramebufferCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @FramebufferAttachmentsCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss FramebufferCreateInfo es, PokeChain es) => ToCStruct (FramebufferCreateInfo es) where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FramebufferCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr FramebufferCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr RenderPass)) (renderPass)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (attachments)) :: Word32))
    pPAttachments' <- ContT $ allocaBytesAligned @ImageView ((Data.Vector.length (attachments)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAttachments' `plusPtr` (8 * (i)) :: Ptr ImageView) (e)) (attachments)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ImageView))) (pPAttachments')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (width)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (height)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (layers)
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr RenderPass)) (zero)
    pPAttachments' <- ContT $ allocaBytesAligned @ImageView ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAttachments' `plusPtr` (8 * (i)) :: Ptr ImageView) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ImageView))) (pPAttachments')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    lift $ f

instance (Extendss FramebufferCreateInfo es, PeekChain es) => FromCStruct (FramebufferCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @FramebufferCreateFlags ((p `plusPtr` 16 :: Ptr FramebufferCreateFlags))
    renderPass <- peek @RenderPass ((p `plusPtr` 24 :: Ptr RenderPass))
    attachmentCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pAttachments <- peek @(Ptr ImageView) ((p `plusPtr` 40 :: Ptr (Ptr ImageView)))
    pAttachments' <- generateM (fromIntegral attachmentCount) (\i -> peek @ImageView ((pAttachments `advancePtrBytes` (8 * (i)) :: Ptr ImageView)))
    width <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    layers <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    pure $ FramebufferCreateInfo
             next flags renderPass pAttachments' width height layers

instance es ~ '[] => Zero (FramebufferCreateInfo es) where
  zero = FramebufferCreateInfo
           ()
           zero
           zero
           mempty
           zero
           zero
           zero

