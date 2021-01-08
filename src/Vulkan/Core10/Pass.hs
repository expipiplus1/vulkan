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

import Vulkan.Internal.Utils (traceAroundEvent)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Exception (VulkanException(..))
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

-- | vkCreateFramebuffer - Create a new framebuffer object
--
-- == Valid Usage
--
-- -   #VUID-vkCreateFramebuffer-pCreateInfo-02777# If @pCreateInfo->flags@
--     does not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     and @attachmentCount@ is not @0@, each element of
--     @pCreateInfo->pAttachments@ /must/ have been created on @device@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateFramebuffer-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateFramebuffer-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'FramebufferCreateInfo'
--     structure
--
-- -   #VUID-vkCreateFramebuffer-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateFramebuffer-pFramebuffer-parameter# @pFramebuffer@
--     /must/ be a valid pointer to a 'Vulkan.Core10.Handles.Framebuffer'
--     handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Framebuffer',
-- 'FramebufferCreateInfo'
createFramebuffer :: forall a io
                   . (Extendss FramebufferCreateInfo a, PokeChain a, MonadIO io)
                  => -- | @device@ is the logical device that creates the framebuffer.
                     Device
                  -> -- | @pCreateInfo@ is a pointer to a 'FramebufferCreateInfo' structure
                     -- describing additional information about framebuffer creation.
                     (FramebufferCreateInfo a)
                  -> -- | @pAllocator@ controls host memory allocation as described in the
                     -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                     -- chapter.
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
  r <- lift $ traceAroundEvent "vkCreateFramebuffer" (vkCreateFramebuffer' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPFramebuffer))
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

-- | vkDestroyFramebuffer - Destroy a framebuffer object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyFramebuffer-framebuffer-00892# All submitted commands
--     that refer to @framebuffer@ /must/ have completed execution
--
-- -   #VUID-vkDestroyFramebuffer-framebuffer-00893# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @framebuffer@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyFramebuffer-framebuffer-00894# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @framebuffer@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyFramebuffer-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyFramebuffer-framebuffer-parameter# If @framebuffer@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @framebuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Framebuffer' handle
--
-- -   #VUID-vkDestroyFramebuffer-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyFramebuffer-framebuffer-parent# If @framebuffer@ is a
--     valid handle, it /must/ have been created, allocated, or retrieved
--     from @device@
--
-- == Host Synchronization
--
-- -   Host access to @framebuffer@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Framebuffer'
destroyFramebuffer :: forall io
                    . (MonadIO io)
                   => -- | @device@ is the logical device that destroys the framebuffer.
                      Device
                   -> -- | @framebuffer@ is the handle of the framebuffer to destroy.
                      Framebuffer
                   -> -- | @pAllocator@ controls host memory allocation as described in the
                      -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                      -- chapter.
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
  lift $ traceAroundEvent "vkDestroyFramebuffer" (vkDestroyFramebuffer' (deviceHandle (device)) (framebuffer) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRenderPass
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct RenderPassCreateInfo) -> Ptr AllocationCallbacks -> Ptr RenderPass -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct RenderPassCreateInfo) -> Ptr AllocationCallbacks -> Ptr RenderPass -> IO Result

-- | vkCreateRenderPass - Create a new render pass object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateRenderPass-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateRenderPass-pCreateInfo-parameter# @pCreateInfo@ /must/
--     be a valid pointer to a valid 'RenderPassCreateInfo' structure
--
-- -   #VUID-vkCreateRenderPass-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateRenderPass-pRenderPass-parameter# @pRenderPass@ /must/
--     be a valid pointer to a 'Vulkan.Core10.Handles.RenderPass' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.RenderPass',
-- 'RenderPassCreateInfo'
createRenderPass :: forall a io
                  . (Extendss RenderPassCreateInfo a, PokeChain a, MonadIO io)
                 => -- | @device@ is the logical device that creates the render pass.
                    Device
                 -> -- | @pCreateInfo@ is a pointer to a 'RenderPassCreateInfo' structure
                    -- describing the parameters of the render pass.
                    (RenderPassCreateInfo a)
                 -> -- | @pAllocator@ controls host memory allocation as described in the
                    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                    -- chapter.
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
  r <- lift $ traceAroundEvent "vkCreateRenderPass" (vkCreateRenderPass' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPRenderPass))
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

-- | vkDestroyRenderPass - Destroy a render pass object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyRenderPass-renderPass-00873# All submitted commands
--     that refer to @renderPass@ /must/ have completed execution
--
-- -   #VUID-vkDestroyRenderPass-renderPass-00874# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @renderPass@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyRenderPass-renderPass-00875# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @renderPass@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyRenderPass-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyRenderPass-renderPass-parameter# If @renderPass@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @renderPass@ /must/ be
--     a valid 'Vulkan.Core10.Handles.RenderPass' handle
--
-- -   #VUID-vkDestroyRenderPass-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyRenderPass-renderPass-parent# If @renderPass@ is a
--     valid handle, it /must/ have been created, allocated, or retrieved
--     from @device@
--
-- == Host Synchronization
--
-- -   Host access to @renderPass@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.RenderPass'
destroyRenderPass :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the logical device that destroys the render pass.
                     Device
                  -> -- | @renderPass@ is the handle of the render pass to destroy.
                     RenderPass
                  -> -- | @pAllocator@ controls host memory allocation as described in the
                     -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                     -- chapter.
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
  lift $ traceAroundEvent "vkDestroyRenderPass" (vkDestroyRenderPass' (deviceHandle (device)) (renderPass) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRenderAreaGranularity
  :: FunPtr (Ptr Device_T -> RenderPass -> Ptr Extent2D -> IO ()) -> Ptr Device_T -> RenderPass -> Ptr Extent2D -> IO ()

-- | vkGetRenderAreaGranularity - Returns the granularity for optimal render
-- area
--
-- = Description
--
-- The conditions leading to an optimal @renderArea@ are:
--
-- -   the @offset.x@ member in @renderArea@ is a multiple of the @width@
--     member of the returned 'Vulkan.Core10.FundamentalTypes.Extent2D'
--     (the horizontal granularity).
--
-- -   the @offset.y@ member in @renderArea@ is a multiple of the @height@
--     of the returned 'Vulkan.Core10.FundamentalTypes.Extent2D' (the
--     vertical granularity).
--
-- -   either the @offset.width@ member in @renderArea@ is a multiple of
--     the horizontal granularity or @offset.x@+@offset.width@ is equal to
--     the @width@ of the @framebuffer@ in the
--     'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'.
--
-- -   either the @offset.height@ member in @renderArea@ is a multiple of
--     the vertical granularity or @offset.y@+@offset.height@ is equal to
--     the @height@ of the @framebuffer@ in the
--     'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'.
--
-- Subpass dependencies are not affected by the render area, and apply to
-- the entire image subresources attached to the framebuffer as specified
-- in the description of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-layout-transitions automatic layout transitions>.
-- Similarly, pipeline barriers are valid even if their effect extends
-- outside the render area.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetRenderAreaGranularity-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetRenderAreaGranularity-renderPass-parameter# @renderPass@
--     /must/ be a valid 'Vulkan.Core10.Handles.RenderPass' handle
--
-- -   #VUID-vkGetRenderAreaGranularity-pGranularity-parameter#
--     @pGranularity@ /must/ be a valid pointer to a
--     'Vulkan.Core10.FundamentalTypes.Extent2D' structure
--
-- -   #VUID-vkGetRenderAreaGranularity-renderPass-parent# @renderPass@
--     /must/ have been created, allocated, or retrieved from @device@
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Handles.RenderPass'
getRenderAreaGranularity :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device that owns the render pass.
                            Device
                         -> -- | @renderPass@ is a handle to a render pass.
                            RenderPass
                         -> io (("granularity" ::: Extent2D))
getRenderAreaGranularity device renderPass = liftIO . evalContT $ do
  let vkGetRenderAreaGranularityPtr = pVkGetRenderAreaGranularity (deviceCmds (device :: Device))
  lift $ unless (vkGetRenderAreaGranularityPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRenderAreaGranularity is null" Nothing Nothing
  let vkGetRenderAreaGranularity' = mkVkGetRenderAreaGranularity vkGetRenderAreaGranularityPtr
  pPGranularity <- ContT (withZeroCStruct @Extent2D)
  lift $ traceAroundEvent "vkGetRenderAreaGranularity" (vkGetRenderAreaGranularity' (deviceHandle (device)) (renderPass) (pPGranularity))
  pGranularity <- lift $ peekCStruct @Extent2D pPGranularity
  pure $ (pGranularity)


-- | VkAttachmentDescription - Structure specifying an attachment description
--
-- = Description
--
-- If the attachment uses a color format, then @loadOp@ and @storeOp@ are
-- used, and @stencilLoadOp@ and @stencilStoreOp@ are ignored. If the
-- format has depth and\/or stencil components, @loadOp@ and @storeOp@
-- apply only to the depth data, while @stencilLoadOp@ and @stencilStoreOp@
-- define how the stencil data is handled. @loadOp@ and @stencilLoadOp@
-- define the /load operations/ that execute as part of the first subpass
-- that uses the attachment. @storeOp@ and @stencilStoreOp@ define the
-- /store operations/ that execute as part of the last subpass that uses
-- the attachment.
--
-- The load operation for each sample in an attachment happens-before any
-- recorded command which accesses the sample in the first subpass where
-- the attachment is used. Load operations for attachments with a
-- depth\/stencil format execute in the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT'
-- pipeline stage. Load operations for attachments with a color format
-- execute in the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
-- pipeline stage.
--
-- The store operation for each sample in an attachment happens-after any
-- recorded command which accesses the sample in the last subpass where the
-- attachment is used. Store operations for attachments with a
-- depth\/stencil format execute in the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT'
-- pipeline stage. Store operations for attachments with a color format
-- execute in the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
-- pipeline stage.
--
-- If an attachment is not used by any subpass, then @loadOp@, @storeOp@,
-- @stencilStoreOp@, and @stencilLoadOp@ are ignored, and the attachment’s
-- memory contents will not be modified by execution of a render pass
-- instance.
--
-- The load and store operations apply on the first and last use of each
-- view in the render pass, respectively. If a view index of an attachment
-- is not included in the view mask in any subpass that uses it, then the
-- load and store operations are ignored, and the attachment’s memory
-- contents will not be modified by execution of a render pass instance.
--
-- During a render pass instance, input\/color attachments with color
-- formats that have a component size of 8, 16, or 32 bits /must/ be
-- represented in the attachment’s format throughout the instance.
-- Attachments with other floating- or fixed-point color formats, or with
-- depth components /may/ be represented in a format with a precision
-- higher than the attachment format, but /must/ be represented with the
-- same range. When such a component is loaded via the @loadOp@, it will be
-- converted into an implementation-dependent format used by the render
-- pass. Such components /must/ be converted from the render pass format,
-- to the format of the attachment, before they are resolved or stored at
-- the end of a render pass instance via @storeOp@. Conversions occur as
-- described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-numerics Numeric Representation and Computation>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-fixedconv Fixed-Point Data Conversions>.
--
-- If @flags@ includes
-- 'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT',
-- then the attachment is treated as if it shares physical memory with
-- another attachment in the same render pass. This information limits the
-- ability of the implementation to reorder certain operations (like layout
-- transitions and the @loadOp@) such that it is not improperly reordered
-- against other uses of the same physical memory via a different
-- attachment. This is described in more detail below.
--
-- If a render pass uses multiple attachments that alias the same device
-- memory, those attachments /must/ each include the
-- 'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT'
-- bit in their attachment description flags. Attachments aliasing the same
-- memory occurs in multiple ways:
--
-- -   Multiple attachments being assigned the same image view as part of
--     framebuffer creation.
--
-- -   Attachments using distinct image views that correspond to the same
--     image subresource of an image.
--
-- -   Attachments using views of distinct image subresources which are
--     bound to overlapping memory ranges.
--
-- Note
--
-- Render passes /must/ include subpass dependencies (either directly or
-- via a subpass dependency chain) between any two subpasses that operate
-- on the same attachment or aliasing attachments and those subpass
-- dependencies /must/ include execution and memory dependencies separating
-- uses of the aliases, if at least one of those subpasses writes to one of
-- the aliases. These dependencies /must/ not include the
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_BY_REGION_BIT' if the
-- aliases are views of distinct image subresources which overlap in
-- memory.
--
-- Multiple attachments that alias the same memory /must/ not be used in a
-- single subpass. A given attachment index /must/ not be used multiple
-- times in a single subpass, with one exception: two subpass attachments
-- /can/ use the same attachment index if at least one use is as an input
-- attachment and neither use is as a resolve or preserve attachment. In
-- other words, the same view /can/ be used simultaneously as an input and
-- color or depth\/stencil attachment, but /must/ not be used as multiple
-- color or depth\/stencil attachments nor as resolve or preserve
-- attachments. The precise set of valid scenarios is described in more
-- detail
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-feedbackloop below>.
--
-- If a set of attachments alias each other, then all except the first to
-- be used in the render pass /must/ use an @initialLayout@ of
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED', since the
-- earlier uses of the other aliases make their contents undefined. Once an
-- alias has been used and a different alias has been used after it, the
-- first alias /must/ not be used in any later subpasses. However, an
-- application /can/ assign the same image view to multiple aliasing
-- attachment indices, which allows that image view to be used multiple
-- times even if other aliases are used in between.
--
-- Note
--
-- Once an attachment needs the
-- 'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT'
-- bit, there /should/ be no additional cost of introducing additional
-- aliases, and using these additional aliases /may/ allow more efficient
-- clearing of the attachments on multiple uses via
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'.
--
-- == Valid Usage
--
-- -   #VUID-VkAttachmentDescription-finalLayout-00843# @finalLayout@
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   #VUID-VkAttachmentDescription-format-03280# If @format@ is a color
--     format, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-format-03281# If @format@ is a
--     depth\/stencil format, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-format-03282# If @format@ is a color
--     format, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-format-03283# If @format@ is a
--     depth\/stencil format, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-separateDepthStencilLayouts-03284# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-separateDepthStencilLayouts-03285# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-format-03286# If @format@ is a color
--     format, @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-format-03287# If @format@ is a color
--     format, @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-format-03288# If @format@ is a
--     depth\/stencil format which includes both depth and stencil aspects,
--     @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-format-03289# If @format@ is a
--     depth\/stencil format which includes both depth and stencil aspects,
--     @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-format-03290# If @format@ is a
--     depth\/stencil format which includes only the depth aspect,
--     @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-format-03291# If @format@ is a
--     depth\/stencil format which includes only the depth aspect,
--     @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-format-03292# If @format@ is a
--     depth\/stencil format which includes only the stencil aspect,
--     @initialLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkAttachmentDescription-format-03293# If @format@ is a
--     depth\/stencil format which includes only the stencil aspect,
--     @finalLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAttachmentDescription-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.AttachmentDescriptionFlagBits'
--     values
--
-- -   #VUID-VkAttachmentDescription-format-parameter# @format@ /must/ be a
--     valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkAttachmentDescription-samples-parameter# @samples@ /must/ be
--     a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- -   #VUID-VkAttachmentDescription-loadOp-parameter# @loadOp@ /must/ be a
--     valid 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp' value
--
-- -   #VUID-VkAttachmentDescription-storeOp-parameter# @storeOp@ /must/ be
--     a valid 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp'
--     value
--
-- -   #VUID-VkAttachmentDescription-stencilLoadOp-parameter#
--     @stencilLoadOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp' value
--
-- -   #VUID-VkAttachmentDescription-stencilStoreOp-parameter#
--     @stencilStoreOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp' value
--
-- -   #VUID-VkAttachmentDescription-initialLayout-parameter#
--     @initialLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkAttachmentDescription-finalLayout-parameter# @finalLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.AttachmentDescriptionFlags',
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp',
-- 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout', 'RenderPassCreateInfo',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
data AttachmentDescription = AttachmentDescription
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.AttachmentDescriptionFlagBits'
    -- specifying additional properties of the attachment.
    flags :: AttachmentDescriptionFlags
  , -- | @format@ is a 'Vulkan.Core10.Enums.Format.Format' value specifying the
    -- format of the image view that will be used for the attachment.
    format :: Format
  , -- | @samples@ is the number of samples of the image as defined in
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'.
    samples :: SampleCountFlagBits
  , -- | @loadOp@ is a 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp'
    -- value specifying how the contents of color and depth components of the
    -- attachment are treated at the beginning of the subpass where it is first
    -- used.
    loadOp :: AttachmentLoadOp
  , -- | @storeOp@ is a 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp'
    -- value specifying how the contents of color and depth components of the
    -- attachment are treated at the end of the subpass where it is last used.
    storeOp :: AttachmentStoreOp
  , -- | @stencilLoadOp@ is a
    -- 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp' value specifying
    -- how the contents of stencil components of the attachment are treated at
    -- the beginning of the subpass where it is first used.
    stencilLoadOp :: AttachmentLoadOp
  , -- | @stencilStoreOp@ is a
    -- 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp' value
    -- specifying how the contents of stencil components of the attachment are
    -- treated at the end of the last subpass where it is used.
    stencilStoreOp :: AttachmentStoreOp
  , -- | @initialLayout@ is the layout the attachment image subresource will be
    -- in when a render pass instance begins.
    initialLayout :: ImageLayout
  , -- | @finalLayout@ is the layout the attachment image subresource will be
    -- transitioned to when a render pass instance ends.
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


-- | VkAttachmentReference - Structure specifying an attachment reference
--
-- == Valid Usage
--
-- -   #VUID-VkAttachmentReference-layout-00857# If @attachment@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', @layout@ /must/ not
--     be 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR',
--     'Vulkan.Extensions.VK_KHR_separate_depth_stencil_layouts.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR',
--     'Vulkan.Extensions.VK_KHR_separate_depth_stencil_layouts.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR',
--     'Vulkan.Extensions.VK_KHR_separate_depth_stencil_layouts.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL_KHR',
--     or
--     'Vulkan.Extensions.VK_KHR_separate_depth_stencil_layouts.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAttachmentReference-layout-parameter# @layout@ /must/ be a
--     valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map.RenderPassFragmentDensityMapCreateInfoEXT',
-- 'SubpassDescription'
data AttachmentReference = AttachmentReference
  { -- | @attachment@ is either an integer value identifying an attachment at the
    -- corresponding index in 'RenderPassCreateInfo'::@pAttachments@, or
    -- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' to signify that this
    -- attachment is not used.
    attachment :: Word32
  , -- | @layout@ is a 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
    -- specifying the layout the attachment uses during the subpass.
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


-- | VkSubpassDescription - Structure specifying a subpass description
--
-- = Description
--
-- Each element of the @pInputAttachments@ array corresponds to an input
-- attachment index in a fragment shader, i.e. if a shader declares an
-- image variable decorated with a @InputAttachmentIndex@ value of __X__,
-- then it uses the attachment provided in @pInputAttachments@[__X__].
-- Input attachments /must/ also be bound to the pipeline in a descriptor
-- set. If the @attachment@ member of any element of @pInputAttachments@ is
-- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the application /must/
-- not read from the corresponding input attachment index. Fragment shaders
-- /can/ use subpass input variables to access the contents of an input
-- attachment at the fragment’s (x, y, layer) framebuffer coordinates.
-- Input attachments /must/ not be used by any subpasses within a
-- renderpass that enables
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-renderpass-transform render pass transform>.
--
-- Each element of the @pColorAttachments@ array corresponds to an output
-- location in the shader, i.e. if the shader declares an output variable
-- decorated with a @Location@ value of __X__, then it uses the attachment
-- provided in @pColorAttachments@[__X__]. If the @attachment@ member of
-- any element of @pColorAttachments@ is
-- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', writes to the
-- corresponding location by a fragment are discarded.
--
-- If @flags@ does not include
-- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM',
-- and if @pResolveAttachments@ is not @NULL@, each of its elements
-- corresponds to a color attachment (the element in @pColorAttachments@ at
-- the same index), and a multisample resolve operation is defined for each
-- attachment. At the end of each subpass, multisample resolve operations
-- read the subpass’s color attachments, and resolve the samples for each
-- pixel within the render area to the same pixel location in the
-- corresponding resolve attachments, unless the resolve attachment index
-- is 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'.
--
-- Similarly, if @flags@ does not include
-- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM',
-- and
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'::@pDepthStencilResolveAttachment@
-- is not @NULL@ and does not have the value
-- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', it corresponds to the
-- depth\/stencil attachment in @pDepthStencilAttachment@, and multisample
-- resolve operations for depth and stencil are defined by
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'::@depthResolveMode@
-- and
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'::@stencilResolveMode@,
-- respectively. At the end of each subpass, multisample resolve operations
-- read the subpass’s depth\/stencil attachment, and resolve the samples
-- for each pixel to the same pixel location in the corresponding resolve
-- attachment. If
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'::@depthResolveMode@
-- is 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', then the
-- depth component of the resolve attachment is not written to and its
-- contents are preserved. Similarly, if
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'::@stencilResolveMode@
-- is 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_NONE', then the
-- stencil component of the resolve attachment is not written to and its
-- contents are preserved.
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'::@depthResolveMode@
-- is ignored if the 'Vulkan.Core10.Enums.Format.Format' of the
-- @pDepthStencilResolveAttachment@ does not have a depth component.
-- Similarly,
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'::@stencilResolveMode@
-- is ignored if the 'Vulkan.Core10.Enums.Format.Format' of the
-- @pDepthStencilResolveAttachment@ does not have a stencil component.
--
-- If the image subresource range referenced by the depth\/stencil
-- attachment is created with
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT',
-- then the multisample resolve operation uses the sample locations state
-- specified in the @sampleLocationsInfo@ member of the element of the
-- 'Vulkan.Extensions.VK_EXT_sample_locations.RenderPassSampleLocationsBeginInfoEXT'::@pPostSubpassSampleLocations@
-- for the subpass.
--
-- If @pDepthStencilAttachment@ is @NULL@, or if its attachment index is
-- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', it indicates that no
-- depth\/stencil attachment will be used in the subpass.
--
-- The contents of an attachment within the render area become undefined at
-- the start of a subpass __S__ if all of the following conditions are
-- true:
--
-- -   The attachment is used as a color, depth\/stencil, or resolve
--     attachment in any subpass in the render pass.
--
-- -   There is a subpass __S1__ that uses or preserves the attachment, and
--     a subpass dependency from __S1__ to __S__.
--
-- -   The attachment is not used or preserved in subpass __S__.
--
-- In addition, the contents of an attachment within the render area become
-- undefined at the start of a subpass __S__ if all of the following
-- conditions are true:
--
-- -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM'
--     is set.
--
-- -   The attachment is used as a color or depth\/stencil in the subpass.
--
-- Once the contents of an attachment become undefined in subpass __S__,
-- they remain undefined for subpasses in subpass dependency chains
-- starting with subpass __S__ until they are written again. However, they
-- remain valid for subpasses in other subpass dependency chains starting
-- with subpass __S1__ if those subpasses use or preserve the attachment.
--
-- == Valid Usage
--
-- -   #VUID-VkSubpassDescription-pipelineBindPoint-00844#
--     @pipelineBindPoint@ /must/ be
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-VkSubpassDescription-colorAttachmentCount-00845#
--     @colorAttachmentCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxColorAttachments@
--
-- -   #VUID-VkSubpassDescription-loadOp-00846# If the first use of an
--     attachment in this render pass is as an input attachment, and the
--     attachment is not also used as a color or depth\/stencil attachment
--     in the same subpass, then @loadOp@ /must/ not be
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'
--
-- -   #VUID-VkSubpassDescription-pResolveAttachments-00847# If
--     @pResolveAttachments@ is not @NULL@, for each resolve attachment
--     that is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the
--     corresponding color attachment /must/ not be
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription-pResolveAttachments-00848# If
--     @pResolveAttachments@ is not @NULL@, for each resolve attachment
--     that is not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the
--     corresponding color attachment /must/ not have a sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkSubpassDescription-pResolveAttachments-00849# If
--     @pResolveAttachments@ is not @NULL@, each resolve attachment that is
--     not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have a
--     sample count of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkSubpassDescription-pResolveAttachments-00850# If
--     @pResolveAttachments@ is not @NULL@, each resolve attachment that is
--     not 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have the
--     same 'Vulkan.Core10.Enums.Format.Format' as its corresponding color
--     attachment
--
-- -   #VUID-VkSubpassDescription-pColorAttachments-01417# All attachments
--     in @pColorAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have the same
--     sample count
--
-- -   #VUID-VkSubpassDescription-pInputAttachments-02647# All attachments
--     in @pInputAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have image
--     formats whose
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain at least
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescription-pColorAttachments-02648# All attachments
--     in @pColorAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have image
--     formats whose
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescription-pResolveAttachments-02649# All
--     attachments in @pResolveAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have image
--     formats whose
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescription-pDepthStencilAttachment-02650# If
--     @pDepthStencilAttachment@ is not @NULL@ and the attachment is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' then it /must/ have a
--     image format whose
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkSubpassDescription-pColorAttachments-01506# If the
--     @VK_AMD_mixed_attachment_samples@ extension is enabled, and all
--     attachments in @pColorAttachments@ that are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' /must/ have a sample
--     count that is smaller than or equal to the sample count of
--     @pDepthStencilAttachment@ if it is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription-pDepthStencilAttachment-01418# If neither
--     the @VK_AMD_mixed_attachment_samples@ nor the
--     @VK_NV_framebuffer_mixed_samples@ extensions are enabled, and if
--     @pDepthStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' and any attachments
--     in @pColorAttachments@ are not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', they /must/ have the
--     same sample count
--
-- -   #VUID-VkSubpassDescription-attachment-00853# The @attachment@ member
--     of each element of @pPreserveAttachments@ /must/ not be
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription-pPreserveAttachments-00854# Each element
--     of @pPreserveAttachments@ /must/ not also be an element of any other
--     member of the subpass description
--
-- -   #VUID-VkSubpassDescription-layout-02519# If any attachment is used
--     by more than one 'AttachmentReference' member, then each use /must/
--     use the same @layout@
--
-- -   #VUID-VkSubpassDescription-None-04437# Each attachment /must/ follow
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#attachment-type-imagelayout image layout requirements>
--     specified for its attachment type
--
-- -   #VUID-VkSubpassDescription-flags-00856# If @flags@ includes
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX',
--     it /must/ also include
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX'
--
-- -   #VUID-VkSubpassDescription-flags-03341# If @flags@ includes
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM',
--     and if @pResolveAttachments@ is not @NULL@, then each resolve
--     attachment /must/ be 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription-flags-03342# If @flags@ includes
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM',
--     and if @pDepthStencilResolveAttachmentKHR@ is not @NULL@, then the
--     depth\/stencil resolve attachment /must/ be
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription-flags-03343# If @flags@ includes
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM',
--     then the subpass /must/ be the last subpass in a subpass dependency
--     chain
--
-- -   #VUID-VkSubpassDescription-flags-03344# If @flags@ includes
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM',
--     then the sample count of the input attachments /must/ equal
--     @rasterizationSamples@
--
-- -   #VUID-VkSubpassDescription-flags-03345# If @flags@ includes
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM',
--     and if @sampleShadingEnable@ is enabled (explicitly or implicitly)
--     then @minSampleShading@ /must/ equal 0.0
--
-- -   #VUID-VkSubpassDescription-pInputAttachments-02868# If the render
--     pass is created with
--     'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM'
--     each of the elements of @pInputAttachments@ /must/ be
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-VkSubpassDescription-pDepthStencilAttachment-04438#
--     @pDepthStencilAttachment@ and @pColorAttachments@ must not contain
--     references to the same attachment
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubpassDescription-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits'
--     values
--
-- -   #VUID-VkSubpassDescription-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-VkSubpassDescription-pInputAttachments-parameter# If
--     @inputAttachmentCount@ is not @0@, @pInputAttachments@ /must/ be a
--     valid pointer to an array of @inputAttachmentCount@ valid
--     'AttachmentReference' structures
--
-- -   #VUID-VkSubpassDescription-pColorAttachments-parameter# If
--     @colorAttachmentCount@ is not @0@, @pColorAttachments@ /must/ be a
--     valid pointer to an array of @colorAttachmentCount@ valid
--     'AttachmentReference' structures
--
-- -   #VUID-VkSubpassDescription-pResolveAttachments-parameter# If
--     @colorAttachmentCount@ is not @0@, and @pResolveAttachments@ is not
--     @NULL@, @pResolveAttachments@ /must/ be a valid pointer to an array
--     of @colorAttachmentCount@ valid 'AttachmentReference' structures
--
-- -   #VUID-VkSubpassDescription-pDepthStencilAttachment-parameter# If
--     @pDepthStencilAttachment@ is not @NULL@, @pDepthStencilAttachment@
--     /must/ be a valid pointer to a valid 'AttachmentReference' structure
--
-- -   #VUID-VkSubpassDescription-pPreserveAttachments-parameter# If
--     @preserveAttachmentCount@ is not @0@, @pPreserveAttachments@ /must/
--     be a valid pointer to an array of @preserveAttachmentCount@
--     @uint32_t@ values
--
-- = See Also
--
-- 'AttachmentReference',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'RenderPassCreateInfo',
-- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlags'
data SubpassDescription = SubpassDescription
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits'
    -- specifying usage of the subpass.
    flags :: SubpassDescriptionFlags
  , -- | @pipelineBindPoint@ is a
    -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
    -- specifying the pipeline type supported for this subpass.
    pipelineBindPoint :: PipelineBindPoint
  , -- | @pInputAttachments@ is a pointer to an array of 'AttachmentReference'
    -- structures defining the input attachments for this subpass and their
    -- layouts.
    inputAttachments :: Vector AttachmentReference
  , -- | @pColorAttachments@ is a pointer to an array of 'AttachmentReference'
    -- structures defining the color attachments for this subpass and their
    -- layouts.
    colorAttachments :: Vector AttachmentReference
  , -- | @pResolveAttachments@ is an optional array of @colorAttachmentCount@
    -- 'AttachmentReference' structures defining the resolve attachments for
    -- this subpass and their layouts.
    resolveAttachments :: Vector AttachmentReference
  , -- | @pDepthStencilAttachment@ is a pointer to a 'AttachmentReference'
    -- structure specifying the depth\/stencil attachment for this subpass and
    -- its layout.
    depthStencilAttachment :: Maybe AttachmentReference
  , -- | @pPreserveAttachments@ is a pointer to an array of
    -- @preserveAttachmentCount@ render pass attachment indices identifying
    -- attachments that are not used by this subpass, but whose contents /must/
    -- be preserved throughout the subpass.
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
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 4 :: Ptr PipelineBindPoint)) (zero)
    f

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


-- | VkSubpassDependency - Structure specifying a subpass dependency
--
-- = Description
--
-- If @srcSubpass@ is equal to @dstSubpass@ then the 'SubpassDependency'
-- describes a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-barriers-subpass-self-dependencies subpass self-dependency>,
-- and only constrains the pipeline barriers allowed within a subpass
-- instance. Otherwise, when a render pass instance which includes a
-- subpass dependency is submitted to a queue, it defines a memory
-- dependency between the subpasses identified by @srcSubpass@ and
-- @dstSubpass@.
--
-- If @srcSubpass@ is equal to
-- 'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL', the first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes commands that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- than the 'Vulkan.Core10.CommandBufferBuilding.cmdBeginRenderPass' used
-- to begin the render pass instance. Otherwise, the first set of commands
-- includes all commands submitted as part of the subpass instance
-- identified by @srcSubpass@ and any load, store or multisample resolve
-- operations on attachments used in @srcSubpass@. In either case, the
-- first synchronization scope is limited to operations on the pipeline
-- stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@.
--
-- If @dstSubpass@ is equal to
-- 'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL', the second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes commands that occur later in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- than the 'Vulkan.Core10.CommandBufferBuilding.cmdEndRenderPass' used to
-- end the render pass instance. Otherwise, the second set of commands
-- includes all commands submitted as part of the subpass instance
-- identified by @dstSubpass@ and any load, store or multisample resolve
-- operations on attachments used in @dstSubpass@. In either case, the
-- second synchronization scope is limited to operations on the pipeline
-- stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@. It is also limited to access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@. It is also limited to access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks destination access mask>
-- specified by @dstAccessMask@.
--
-- The
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-available-and-visible availability and visibility operations>
-- defined by a subpass dependency affect the execution of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-layout-transitions image layout transitions>
-- within the render pass.
--
-- Note
--
-- For non-attachment resources, the memory dependency expressed by subpass
-- dependency is nearly identical to that of a
-- 'Vulkan.Core10.OtherTypes.MemoryBarrier' (with matching @srcAccessMask@
-- and @dstAccessMask@ parameters) submitted as a part of a
-- 'Vulkan.Core10.CommandBufferBuilding.cmdPipelineBarrier' (with matching
-- @srcStageMask@ and @dstStageMask@ parameters). The only difference being
-- that its scopes are limited to the identified subpasses rather than
-- potentially affecting everything before and after.
--
-- For attachments however, subpass dependencies work more like a
-- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier' defined similarly to the
-- 'Vulkan.Core10.OtherTypes.MemoryBarrier' above, the queue family indices
-- set to 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED', and layouts as
-- follows:
--
-- -   The equivalent to @oldLayout@ is the attachment’s layout according
--     to the subpass description for @srcSubpass@.
--
-- -   The equivalent to @newLayout@ is the attachment’s layout according
--     to the subpass description for @dstSubpass@.
--
-- == Valid Usage
--
-- -   #VUID-VkSubpassDependency-srcStageMask-00860# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkSubpassDependency-dstStageMask-00861# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkSubpassDependency-srcStageMask-00862# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkSubpassDependency-dstStageMask-00863# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkSubpassDependency-srcSubpass-00864# @srcSubpass@ /must/ be
--     less than or equal to @dstSubpass@, unless one of them is
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL', to avoid cyclic
--     dependencies and ensure a valid execution order
--
-- -   #VUID-VkSubpassDependency-srcSubpass-00865# @srcSubpass@ and
--     @dstSubpass@ /must/ not both be equal to
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL'
--
-- -   #VUID-VkSubpassDependency-srcSubpass-00867# If @srcSubpass@ is equal
--     to @dstSubpass@ and not all of the stages in @srcStageMask@ and
--     @dstStageMask@ are
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stages>,
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically latest>
--     pipeline stage in @srcStageMask@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earlier>
--     than or equal to the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earliest>
--     pipeline stage in @dstStageMask@
--
-- -   #VUID-VkSubpassDependency-srcAccessMask-00868# Any access flag
--     included in @srcAccessMask@ /must/ be supported by one of the
--     pipeline stages in @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-VkSubpassDependency-dstAccessMask-00869# Any access flag
--     included in @dstAccessMask@ /must/ be supported by one of the
--     pipeline stages in @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-VkSubpassDependency-srcSubpass-02243# If @srcSubpass@ equals
--     @dstSubpass@, and @srcStageMask@ and @dstStageMask@ both include a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stage>,
--     then @dependencyFlags@ /must/ include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_BY_REGION_BIT'
--
-- -   #VUID-VkSubpassDependency-dependencyFlags-02520# If
--     @dependencyFlags@ includes
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT',
--     @srcSubpass@ /must/ not be equal to
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL'
--
-- -   #VUID-VkSubpassDependency-dependencyFlags-02521# If
--     @dependencyFlags@ includes
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT',
--     @dstSubpass@ /must/ not be equal to
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL'
--
-- -   #VUID-VkSubpassDependency-srcSubpass-00872# If @srcSubpass@ equals
--     @dstSubpass@ and that subpass has more than one bit set in the view
--     mask, then @dependencyFlags@ /must/ include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT'
--
-- -   #VUID-VkSubpassDependency-srcStageMask-02099# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   #VUID-VkSubpassDependency-srcStageMask-02100# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   #VUID-VkSubpassDependency-dstStageMask-02101# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   #VUID-VkSubpassDependency-dstStageMask-02102# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubpassDependency-srcStageMask-parameter# @srcStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-VkSubpassDependency-srcStageMask-requiredbitmask#
--     @srcStageMask@ /must/ not be @0@
--
-- -   #VUID-VkSubpassDependency-dstStageMask-parameter# @dstStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-VkSubpassDependency-dstStageMask-requiredbitmask#
--     @dstStageMask@ /must/ not be @0@
--
-- -   #VUID-VkSubpassDependency-srcAccessMask-parameter# @srcAccessMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' values
--
-- -   #VUID-VkSubpassDependency-dstAccessMask-parameter# @dstAccessMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' values
--
-- -   #VUID-VkSubpassDependency-dependencyFlags-parameter#
--     @dependencyFlags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits' values
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlags',
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags',
-- 'RenderPassCreateInfo'
data SubpassDependency = SubpassDependency
  { -- | @srcSubpass@ is the subpass index of the first subpass in the
    -- dependency, or 'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL'.
    srcSubpass :: Word32
  , -- | @dstSubpass@ is the subpass index of the second subpass in the
    -- dependency, or 'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL'.
    dstSubpass :: Word32
  , -- | @srcStageMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
    -- specifying the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>.
    srcStageMask :: PipelineStageFlags
  , -- | @dstStageMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
    -- specifying the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
    dstStageMask :: PipelineStageFlags
  , -- | @srcAccessMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks source access mask>.
    srcAccessMask :: AccessFlags
  , -- | @dstAccessMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks destination access mask>.
    dstAccessMask :: AccessFlags
  , -- | @dependencyFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits'.
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


-- | VkRenderPassCreateInfo - Structure specifying parameters of a newly
-- created render pass
--
-- = Description
--
-- Note
--
-- Care should be taken to avoid a data race here; if any subpasses access
-- attachments with overlapping memory locations, and one of those accesses
-- is a write, a subpass dependency needs to be included between them.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderPassCreateInfo-attachment-00834# If the @attachment@
--     member of any element of @pInputAttachments@, @pColorAttachments@,
--     @pResolveAttachments@ or @pDepthStencilAttachment@, or any element
--     of @pPreserveAttachments@ in any element of @pSubpasses@ is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', it /must/ be less
--     than @attachmentCount@
--
-- -   #VUID-VkRenderPassCreateInfo-pAttachments-00836# For any member of
--     @pAttachments@ with a @loadOp@ equal to
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR', the
--     first use of that attachment /must/ not specify a @layout@ equal to
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderPassCreateInfo-pAttachments-02511# For any member of
--     @pAttachments@ with a @stencilLoadOp@ equal to
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR', the
--     first use of that attachment /must/ not specify a @layout@ equal to
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderPassCreateInfo-pAttachments-01566# For any member of
--     @pAttachments@ with a @loadOp@ equal to
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR', the
--     first use of that attachment /must/ not specify a @layout@ equal to
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   #VUID-VkRenderPassCreateInfo-pAttachments-01567# For any member of
--     @pAttachments@ with a @stencilLoadOp@ equal to
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR', the
--     first use of that attachment /must/ not specify a @layout@ equal to
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkRenderPassCreateInfo-pNext-01926# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.RenderPassInputAttachmentAspectCreateInfo'
--     structure, the @subpass@ member of each element of its
--     @pAspectReferences@ member /must/ be less than @subpassCount@
--
-- -   #VUID-VkRenderPassCreateInfo-pNext-01927# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.RenderPassInputAttachmentAspectCreateInfo'
--     structure, the @inputAttachmentIndex@ member of each element of its
--     @pAspectReferences@ member /must/ be less than the value of
--     @inputAttachmentCount@ in the member of @pSubpasses@ identified by
--     its @subpass@ member
--
-- -   #VUID-VkRenderPassCreateInfo-pNext-01963# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.RenderPassInputAttachmentAspectCreateInfo'
--     structure, for any element of the @pInputAttachments@ member of any
--     element of @pSubpasses@ where the @attachment@ member is not
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', the @aspectMask@
--     member of the corresponding element of
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.RenderPassInputAttachmentAspectCreateInfo'::@pAspectReferences@
--     /must/ only include aspects that are present in images of the format
--     specified by the element of @pAttachments@ at @attachment@
--
-- -   #VUID-VkRenderPassCreateInfo-pNext-01928# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'
--     structure, and its @subpassCount@ member is not zero, that member
--     /must/ be equal to the value of @subpassCount@
--
-- -   #VUID-VkRenderPassCreateInfo-pNext-01929# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'
--     structure, if its @dependencyCount@ member is not zero, it /must/ be
--     equal to @dependencyCount@
--
-- -   #VUID-VkRenderPassCreateInfo-pNext-01930# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'
--     structure, for each non-zero element of @pViewOffsets@, the
--     @srcSubpass@ and @dstSubpass@ members of @pDependencies@ at the same
--     index /must/ not be equal
--
-- -   #VUID-VkRenderPassCreateInfo-pNext-02512# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'
--     structure, for any element of @pDependencies@ with a
--     @dependencyFlags@ member that does not include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT',
--     the corresponding element of the @pViewOffsets@ member of that
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'
--     instance /must/ be @0@
--
-- -   #VUID-VkRenderPassCreateInfo-pNext-02513# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'
--     structure, elements of its @pViewMasks@ member /must/ either all be
--     @0@, or all not be @0@
--
-- -   #VUID-VkRenderPassCreateInfo-pNext-02514# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'
--     structure, and each element of its @pViewMasks@ member is @0@, the
--     @dependencyFlags@ member of each element of @pDependencies@ /must/
--     not include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT'
--
-- -   #VUID-VkRenderPassCreateInfo-pNext-02515# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'
--     structure, and each element of its @pViewMasks@ member is @0@,
--     @correlatedViewMaskCount@ /must/ be @0@
--
-- -   #VUID-VkRenderPassCreateInfo-pDependencies-00837# For any element of
--     @pDependencies@, if the @srcSubpass@ is not
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL', all stage flags
--     included in the @srcStageMask@ member of that dependency /must/ be a
--     pipeline stage supported by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types pipeline>
--     identified by the @pipelineBindPoint@ member of the source subpass
--
-- -   #VUID-VkRenderPassCreateInfo-pDependencies-00838# For any element of
--     @pDependencies@, if the @dstSubpass@ is not
--     'Vulkan.Core10.APIConstants.SUBPASS_EXTERNAL', all stage flags
--     included in the @dstStageMask@ member of that dependency /must/ be a
--     pipeline stage supported by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types pipeline>
--     identified by the @pipelineBindPoint@ member of the destination
--     subpass
--
-- -   #VUID-VkRenderPassCreateInfo-srcSubpass-02517# The @srcSubpass@
--     member of each element of @pDependencies@ /must/ be less than
--     @subpassCount@
--
-- -   #VUID-VkRenderPassCreateInfo-dstSubpass-02518# The @dstSubpass@
--     member of each element of @pDependencies@ /must/ be less than
--     @subpassCount@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderPassCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO'
--
-- -   #VUID-VkRenderPassCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.RenderPassFragmentDensityMapCreateInfoEXT',
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.RenderPassInputAttachmentAspectCreateInfo',
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'
--
-- -   #VUID-VkRenderPassCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkRenderPassCreateInfo-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RenderPassCreateFlagBits'
--     values
--
-- -   #VUID-VkRenderPassCreateInfo-pAttachments-parameter# If
--     @attachmentCount@ is not @0@, @pAttachments@ /must/ be a valid
--     pointer to an array of @attachmentCount@ valid
--     'AttachmentDescription' structures
--
-- -   #VUID-VkRenderPassCreateInfo-pSubpasses-parameter# @pSubpasses@
--     /must/ be a valid pointer to an array of @subpassCount@ valid
--     'SubpassDescription' structures
--
-- -   #VUID-VkRenderPassCreateInfo-pDependencies-parameter# If
--     @dependencyCount@ is not @0@, @pDependencies@ /must/ be a valid
--     pointer to an array of @dependencyCount@ valid 'SubpassDependency'
--     structures
--
-- -   #VUID-VkRenderPassCreateInfo-subpassCount-arraylength#
--     @subpassCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'AttachmentDescription',
-- 'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RenderPassCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'SubpassDependency',
-- 'SubpassDescription', 'createRenderPass'
data RenderPassCreateInfo (es :: [Type]) = RenderPassCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RenderPassCreateFlagBits'
    flags :: RenderPassCreateFlags
  , -- | @pAttachments@ is a pointer to an array of @attachmentCount@
    -- 'AttachmentDescription' structures describing the attachments used by
    -- the render pass.
    attachments :: Vector AttachmentDescription
  , -- | @pSubpasses@ is a pointer to an array of @subpassCount@
    -- 'SubpassDescription' structures describing each subpass.
    subpasses :: Vector SubpassDescription
  , -- | @pDependencies@ is a pointer to an array of @dependencyCount@
    -- 'SubpassDependency' structures describing dependencies between pairs of
    -- subpasses.
    dependencies :: Vector SubpassDependency
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RenderPassCreateInfo es)

instance Extensible RenderPassCreateInfo where
  extensibleTypeName = "RenderPassCreateInfo"
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


-- | VkFramebufferCreateInfo - Structure specifying parameters of a newly
-- created framebuffer
--
-- = Description
--
-- Other than the exceptions listed below, applications /must/ ensure that
-- all accesses to memory that backs image subresources used as attachments
-- in a given renderpass instance either happen-before the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops load operations>
-- for those attachments, or happen-after the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops store operations>
-- for those attachments.
--
-- The exceptions to the general rule are:
--
-- -   For depth\/stencil attachments, an aspect /can/ be used separately
--     as attachment and non-attachment if both accesses are read-only.
--
-- -   For depth\/stencil attachments, each aspect /can/ be used separately
--     as attachments and non-attachments as long as the non-attachment
--     accesses are also via an image subresource in either the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--     layout or the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     layout, and the attachment resource uses whichever of those two
--     layouts the image accesses do not.
--
-- Use of non-attachment aspects in these cases is only well defined if the
-- attachment is used in the subpass where the non-attachment access is
-- being made, or the layout of the image subresource is constant
-- throughout the entire render pass instance, including the
-- @initialLayout@ and @finalLayout@.
--
-- Note
--
-- These restrictions mean that the render pass has full knowledge of all
-- uses of all of the attachments, so that the implementation is able to
-- make correct decisions about when and how to perform layout transitions,
-- when to overlap execution of subpasses, etc.
--
-- It is legal for a subpass to use no color or depth\/stencil attachments,
-- either because it has no attachment references or because all of them
-- are 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'. This kind of subpass
-- /can/ use shader side effects such as image stores and atomics to
-- produce an output. In this case, the subpass continues to use the
-- @width@, @height@, and @layers@ of the framebuffer to define the
-- dimensions of the rendering area, and the @rasterizationSamples@ from
-- each pipeline’s
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo' to define
-- the number of samples used in rasterization; however, if
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures'::@variableMultisampleRate@
-- is 'Vulkan.Core10.FundamentalTypes.FALSE', then all pipelines to be
-- bound with the subpass /must/ have the same value for
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@.
--
-- == Valid Usage
--
-- -   #VUID-VkFramebufferCreateInfo-attachmentCount-00876#
--     @attachmentCount@ /must/ be equal to the attachment count specified
--     in @renderPass@
--
-- -   #VUID-VkFramebufferCreateInfo-flags-02778# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     and @attachmentCount@ is not @0@, @pAttachments@ /must/ be a valid
--     pointer to an array of @attachmentCount@ valid
--     'Vulkan.Core10.Handles.ImageView' handles
--
-- -   #VUID-VkFramebufferCreateInfo-pAttachments-00877# If @flags@ does
--     not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ that is used as a color attachment or
--     resolve attachment by @renderPass@ /must/ have been created with a
--     @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkFramebufferCreateInfo-pAttachments-02633# If @flags@ does
--     not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ that is used as a depth\/stencil
--     attachment by @renderPass@ /must/ have been created with a @usage@
--     value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkFramebufferCreateInfo-pAttachments-02634# If @flags@ does
--     not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ that is used as a depth\/stencil
--     resolve attachment by @renderPass@ /must/ have been created with a
--     @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkFramebufferCreateInfo-pAttachments-00879# If @flags@ does
--     not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ that is used as an input attachment
--     by @renderPass@ /must/ have been created with a @usage@ value
--     including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkFramebufferCreateInfo-pAttachments-02552# Each element of
--     @pAttachments@ that is used as a fragment density map attachment by
--     @renderPass@ /must/ not have been created with a @flags@ value
--     including
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkFramebufferCreateInfo-renderPass-02553# If @renderPass@ has
--     a fragment density map attachment and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMapNonSubsampledImages non-subsample image feature>
--     is not enabled, each element of @pAttachments@ /must/ have been
--     created with a @flags@ value including
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--     unless that element is the fragment density map attachment
--
-- -   #VUID-VkFramebufferCreateInfo-pAttachments-00880# If @flags@ does
--     not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ /must/ have been created with a
--     'Vulkan.Core10.Enums.Format.Format' value that matches the
--     'Vulkan.Core10.Enums.Format.Format' specified by the corresponding
--     'AttachmentDescription' in @renderPass@
--
-- -   #VUID-VkFramebufferCreateInfo-pAttachments-00881# If @flags@ does
--     not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ /must/ have been created with a
--     @samples@ value that matches the @samples@ value specified by the
--     corresponding 'AttachmentDescription' in @renderPass@
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04533# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ that is used as an input, color,
--     resolve, or depth\/stencil attachment by @renderPass@ /must/ have
--     been created with a VkImageCreateInfo::@width@ greater than or equal
--     to @width@
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04534# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ that is used as an input, color,
--     resolve, or depth\/stencil attachment by @renderPass@ /must/ have
--     been created with a VkImageCreateInfo::@height@ greater than or
--     equal to @height@
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04535# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ that is used as an input, color,
--     resolve, or depth\/stencil attachment by @renderPass@ /must/ have
--     been created with a
--     VkImageViewCreateInfo::@subresourceRange.layerCount@ greater than or
--     equal to @layers@
--
-- -   #VUID-VkFramebufferCreateInfo-renderPass-04536# If @renderPass@ was
--     specified with non-zero view masks, each element of @pAttachments@
--     that is used as an input, color, resolve, or depth\/stencil
--     attachment by @renderPass@ /must/ have a @layerCount@ greater than
--     the index of the most significant bit set in any of those view masks
--
-- -   #VUID-VkFramebufferCreateInfo-renderPass-02746# If @renderPass@ was
--     specified with non-zero view masks, each element of @pAttachments@
--     that is referenced by @fragmentDensityMapAttachment@ /must/ have a
--     @layerCount@ equal to @1@ or greater than the index of the most
--     significant bit set in any of those view masks
--
-- -   #VUID-VkFramebufferCreateInfo-renderPass-02747# If @renderPass@ was
--     not specified with non-zero view masks, each element of
--     @pAttachments@ that is referenced by @fragmentDensityMapAttachment@
--     /must/ have a @layerCount@ equal to @1@
--
-- -   #VUID-VkFramebufferCreateInfo-pAttachments-02555# If @flags@ does
--     not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     an element of @pAttachments@ that is referenced by
--     @fragmentDensityMapAttachment@ /must/ have a width at least as large
--     as
--     \(\left\lceil{\frac{width}{maxFragmentDensityTexelSize_{width}}}\right\rceil\)
--
-- -   #VUID-VkFramebufferCreateInfo-pAttachments-02556# If @flags@ does
--     not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     an element of @pAttachments@ that is referenced by
--     @fragmentDensityMapAttachment@ /must/ have a height at least as
--     large as
--     \(\left\lceil{\frac{height}{maxFragmentDensityTexelSize_{height}}}\right\rceil\)
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04537# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     and @renderPass@ was specified with non-zero view masks, each
--     element of @pAttachments@ that is used as a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
--     by @renderPass@ /must/ have a @layerCount@ that is either @1@, or
--     greater than the index of the most significant bit set in any of
--     those view masks
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04538# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     and @renderPass@ was not specified with non-zero view masks, each
--     element of @pAttachments@ that is used as a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
--     by @renderPass@ /must/ have a @layerCount@ that is either @1@, or
--     greater than @layers@
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04539# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     an element of @pAttachments@ that is used as a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
--     /must/ have a width at least as large as ⌈@width@ \\ @texelWidth@⌉,
--     where @texelWidth@ is the largest value of
--     @shadingRateAttachmentTexelSize.width@ in a
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR'
--     which references that attachment
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04540# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     an element of @pAttachments@ that is used as a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
--     /must/ have a height at least as large as ⌈@height@ \\
--     @texelHeight@⌉, where @texelHeight@ is the largest value of
--     @shadingRateAttachmentTexelSize.height@ in a
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR'
--     which references that attachment
--
-- -   #VUID-VkFramebufferCreateInfo-pAttachments-00883# If @flags@ does
--     not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ /must/ only specify a single mip
--     level
--
-- -   #VUID-VkFramebufferCreateInfo-pAttachments-00884# If @flags@ does
--     not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ /must/ have been created with the
--     identity swizzle
--
-- -   #VUID-VkFramebufferCreateInfo-width-00885# @width@ /must/ be greater
--     than @0@
--
-- -   #VUID-VkFramebufferCreateInfo-width-00886# @width@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxFramebufferWidth@
--
-- -   #VUID-VkFramebufferCreateInfo-height-00887# @height@ /must/ be
--     greater than @0@
--
-- -   #VUID-VkFramebufferCreateInfo-height-00888# @height@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxFramebufferHeight@
--
-- -   #VUID-VkFramebufferCreateInfo-layers-00889# @layers@ /must/ be
--     greater than @0@
--
-- -   #VUID-VkFramebufferCreateInfo-layers-00890# @layers@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxFramebufferLayers@
--
-- -   #VUID-VkFramebufferCreateInfo-renderPass-02531# If @renderPass@ was
--     specified with non-zero view masks, @layers@ /must/ be @1@
--
-- -   #VUID-VkFramebufferCreateInfo-pAttachments-00891# If @flags@ does
--     not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ that is a 2D or 2D array image view
--     taken from a 3D image /must/ not be a depth\/stencil format
--
-- -   #VUID-VkFramebufferCreateInfo-flags-03189# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-imagelessFramebuffer imageless framebuffer>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT'
--
-- -   #VUID-VkFramebufferCreateInfo-flags-03190# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @pNext@ chain /must/ include an instance of
--     'Vulkan.Extensions.VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfoKHR'
--
-- -   #VUID-VkFramebufferCreateInfo-flags-03191# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @attachmentImageInfoCount@ member of an instance of
--     'Vulkan.Extensions.VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfoKHR'
--     in the @pNext@ chain /must/ be equal to either zero or
--     @attachmentCount@
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04541# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @width@ member of any element of the @pAttachmentImageInfos@
--     member of an instance of
--     'Vulkan.Extensions.VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfoKHR'
--     in the @pNext@ chain that is used as an input, color, resolve or
--     depth\/stencil attachment in @pRenderPass@ /must/ be greater than or
--     equal to @width@
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04542# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @height@ member of any element of the @pAttachmentImageInfos@
--     member of an instance of
--     'Vulkan.Extensions.VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfoKHR'
--     in the @pNext@ chain that is used as an input, color, resolve or
--     depth\/stencil attachment in @pRenderPass@ /must/ be greater than or
--     equal to @height@
--
-- -   #VUID-VkFramebufferCreateInfo-flags-03196# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @width@ member of any element of the @pAttachmentImageInfos@
--     member of an instance of
--     'Vulkan.Extensions.VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfoKHR'
--     in the @pNext@ chain that is referenced by
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.RenderPassFragmentDensityMapCreateInfoEXT'::@fragmentDensityMapAttachment@
--     in @renderPass@ /must/ be greater than or equal to
--     \(\left\lceil{\frac{width}{maxFragmentDensityTexelSize_{width}}}\right\rceil\)
--
-- -   #VUID-VkFramebufferCreateInfo-flags-03197# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @height@ member of any element of the @pAttachmentImageInfos@
--     member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--     structure included in the @pNext@ chain that is referenced by
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.RenderPassFragmentDensityMapCreateInfoEXT'::@fragmentDensityMapAttachment@
--     in @renderPass@ /must/ be greater than or equal to
--     \(\left\lceil{\frac{height}{maxFragmentDensityTexelSize_{height}}}\right\rceil\)
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04543# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @width@ member of any element of the @pAttachmentImageInfos@
--     member of an instance of
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--     in the @pNext@ chain that is used as a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
--     /must/ be greater than or equal to ⌈@width@ \\ @texelWidth@⌉, where
--     @texelWidth@ is the largest value of
--     @shadingRateAttachmentTexelSize.width@ in a
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR'
--     which references that attachment
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04544# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @height@ member of any element of the @pAttachmentImageInfos@
--     member of an instance of
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--     in the @pNext@ chain that is used as a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
--     /must/ be greater than or equal to ⌈@height@ \\ @texelHeight@⌉,
--     where @texelHeight@ is the largest value of
--     @shadingRateAttachmentTexelSize.height@ in a
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateAttachmentInfoKHR'
--     which references that attachment
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04545# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @layerCount@ member of any element of the
--     @pAttachmentImageInfos@ member of an instance of
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--     in the @pNext@ chain that is used as a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
--     /must/ be either @1@, or greater than or equal to @layers@
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04587# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     and @renderPass@ was specified with non-zero view masks, each
--     element of @pAttachments@ that is used as a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
--     by @renderPass@ /must/ have a @layerCount@ that is either @1@, or
--     greater than the index of the most significant bit set in any of
--     those view masks
--
-- -   #VUID-VkFramebufferCreateInfo-renderPass-03198# If multiview is
--     enabled for @renderPass@, and @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @layerCount@ member of any element of the
--     @pAttachmentImageInfos@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--     structure included in the @pNext@ chain used as an input, color,
--     resolve, or depth\/stencil attachment in @pRenderPass@ /must/ be
--     greater than the maximum bit index set in the view mask in the
--     subpasses in which it is used in @renderPass@
--
-- -   #VUID-VkFramebufferCreateInfo-renderPass-04546# If multiview is not
--     enabled for @renderPass@, and @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @layerCount@ member of any element of the
--     @pAttachmentImageInfos@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--     structure included in the @pNext@ chain used as an input, color,
--     resolve, or depth\/stencil attachment in @pRenderPass@ /must/ be
--     greater than or equal to @layers@
--
-- -   #VUID-VkFramebufferCreateInfo-flags-03201# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @usage@ member of any element of the @pAttachmentImageInfos@
--     member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--     structure included in the @pNext@ chain that refers to an attachment
--     used as a color attachment or resolve attachment by @renderPass@
--     /must/ include
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkFramebufferCreateInfo-flags-03202# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @usage@ member of any element of the @pAttachmentImageInfos@
--     member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--     structure included in the @pNext@ chain that refers to an attachment
--     used as a depth\/stencil attachment by @renderPass@ /must/ include
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkFramebufferCreateInfo-flags-03203# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @usage@ member of any element of the @pAttachmentImageInfos@
--     member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--     structure included in the @pNext@ chain that refers to an attachment
--     used as a depth\/stencil resolve attachment by @renderPass@ /must/
--     include
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkFramebufferCreateInfo-flags-03204# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @usage@ member of any element of the @pAttachmentImageInfos@
--     member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--     structure included in the @pNext@ chain that refers to an attachment
--     used as an input attachment by @renderPass@ /must/ include
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkFramebufferCreateInfo-flags-03205# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     at least one element of the @pViewFormats@ member of any element of
--     the @pAttachmentImageInfos@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--     structure included in the @pNext@ chain /must/ be equal to the
--     corresponding value of 'AttachmentDescription'::@format@ used to
--     create @renderPass@
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04113# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ /must/ have been created with
--     'Vulkan.Core10.ImageView.ImageViewCreateInfo'::@viewType@ not equal
--     to 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D'
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04548# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of @pAttachments@ that is used as a fragment shading
--     rate attachment by @renderPass@ /must/ have been created with a
--     @usage@ value including
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkFramebufferCreateInfo-flags-04549# If @flags@ includes
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @usage@ member of any element of the @pAttachmentImageInfos@
--     member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--     structure included in the @pNext@ chain that refers to an attachment
--     used as a fragment shading rate attachment by @renderPass@ /must/
--     include
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkFramebufferCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO'
--
-- -   #VUID-VkFramebufferCreateInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'
--
-- -   #VUID-VkFramebufferCreateInfo-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkFramebufferCreateInfo-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FramebufferCreateFlagBits'
--     values
--
-- -   #VUID-VkFramebufferCreateInfo-renderPass-parameter# @renderPass@
--     /must/ be a valid 'Vulkan.Core10.Handles.RenderPass' handle
--
-- -   #VUID-VkFramebufferCreateInfo-commonparent# Both of @renderPass@,
--     and the elements of @pAttachments@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FramebufferCreateFlags',
-- 'Vulkan.Core10.Handles.ImageView', 'Vulkan.Core10.Handles.RenderPass',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createFramebuffer'
data FramebufferCreateInfo (es :: [Type]) = FramebufferCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FramebufferCreateFlagBits'
    flags :: FramebufferCreateFlags
  , -- | @renderPass@ is a render pass defining what render passes the
    -- framebuffer will be compatible with. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility Render Pass Compatibility>
    -- for details.
    renderPass :: RenderPass
  , -- | @pAttachments@ is a pointer to an array of
    -- 'Vulkan.Core10.Handles.ImageView' handles, each of which will be used as
    -- the corresponding attachment in a render pass instance. If @flags@
    -- includes
    -- 'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
    -- this parameter is ignored.
    attachments :: Vector ImageView
  , -- | @width@, @height@ and @layers@ define the dimensions of the framebuffer.
    -- If the render pass uses multiview, then @layers@ /must/ be one and each
    -- attachment requires a number of layers that is greater than the maximum
    -- bit index set in the view mask in the subpasses in which it is used.
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
  extensibleTypeName = "FramebufferCreateInfo"
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

