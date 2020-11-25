{-# language CPP #-}
-- No documentation found for Chapter "CommandBuffer"
module Vulkan.Core10.CommandBuffer  ( allocateCommandBuffers
                                    , withCommandBuffers
                                    , freeCommandBuffers
                                    , beginCommandBuffer
                                    , useCommandBuffer
                                    , endCommandBuffer
                                    , resetCommandBuffer
                                    , CommandBufferAllocateInfo(..)
                                    , CommandBufferInheritanceInfo(..)
                                    , CommandBufferBeginInfo(..)
                                    , CommandBuffer(..)
                                    , CommandBufferLevel(..)
                                    , QueryControlFlagBits(..)
                                    , QueryControlFlags
                                    , CommandBufferUsageFlagBits(..)
                                    , CommandBufferUsageFlags
                                    , CommandBufferResetFlagBits(..)
                                    , CommandBufferResetFlags
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conditional_rendering (CommandBufferInheritanceConditionalRenderingInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_render_pass_transform (CommandBufferInheritanceRenderPassTransformInfoQCOM)
import Vulkan.Core10.Enums.CommandBufferLevel (CommandBufferLevel)
import Vulkan.Core10.Enums.CommandBufferResetFlagBits (CommandBufferResetFlagBits(..))
import Vulkan.Core10.Enums.CommandBufferResetFlagBits (CommandBufferResetFlags)
import Vulkan.Core10.Enums.CommandBufferUsageFlagBits (CommandBufferUsageFlags)
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (CommandPool)
import Vulkan.Core10.Handles (CommandPool(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkAllocateCommandBuffers))
import Vulkan.Dynamic (DeviceCmds(pVkBeginCommandBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkEndCommandBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkFreeCommandBuffers))
import Vulkan.Dynamic (DeviceCmds(pVkResetCommandBuffer))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupCommandBufferBeginInfo)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Handles (Framebuffer)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlags)
import Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits (QueryPipelineStatisticFlags)
import Vulkan.Core10.Handles (RenderPass)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Enums.CommandBufferLevel (CommandBufferLevel(..))
import Vulkan.Core10.Enums.CommandBufferResetFlagBits (CommandBufferResetFlagBits(..))
import Vulkan.Core10.Enums.CommandBufferResetFlagBits (CommandBufferResetFlags)
import Vulkan.Core10.Enums.CommandBufferUsageFlagBits (CommandBufferUsageFlagBits(..))
import Vulkan.Core10.Enums.CommandBufferUsageFlagBits (CommandBufferUsageFlags)
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlagBits(..))
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateCommandBuffers
  :: FunPtr (Ptr Device_T -> Ptr CommandBufferAllocateInfo -> Ptr (Ptr CommandBuffer_T) -> IO Result) -> Ptr Device_T -> Ptr CommandBufferAllocateInfo -> Ptr (Ptr CommandBuffer_T) -> IO Result

-- No documentation found for TopLevel "vkAllocateCommandBuffers"
allocateCommandBuffers :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkAllocateCommandBuffers" "device"
                          Device
                       -> -- No documentation found for Nested "vkAllocateCommandBuffers" "pAllocateInfo"
                          CommandBufferAllocateInfo
                       -> io (("commandBuffers" ::: Vector CommandBuffer))
allocateCommandBuffers device allocateInfo = liftIO . evalContT $ do
  let cmds = deviceCmds (device :: Device)
  let vkAllocateCommandBuffersPtr = pVkAllocateCommandBuffers cmds
  lift $ unless (vkAllocateCommandBuffersPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAllocateCommandBuffers is null" Nothing Nothing
  let vkAllocateCommandBuffers' = mkVkAllocateCommandBuffers vkAllocateCommandBuffersPtr
  pAllocateInfo <- ContT $ withCStruct (allocateInfo)
  pPCommandBuffers <- ContT $ bracket (callocBytes @(Ptr CommandBuffer_T) ((fromIntegral $ commandBufferCount ((allocateInfo) :: CommandBufferAllocateInfo)) * 8)) free
  r <- lift $ vkAllocateCommandBuffers' (deviceHandle (device)) pAllocateInfo (pPCommandBuffers)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCommandBuffers <- lift $ generateM (fromIntegral $ commandBufferCount ((allocateInfo) :: CommandBufferAllocateInfo)) (\i -> do
    pCommandBuffersElem <- peek @(Ptr CommandBuffer_T) ((pPCommandBuffers `advancePtrBytes` (8 * (i)) :: Ptr (Ptr CommandBuffer_T)))
    pure $ (\h -> CommandBuffer h cmds ) pCommandBuffersElem)
  pure $ (pCommandBuffers)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'allocateCommandBuffers' and 'freeCommandBuffers'
--
-- To ensure that 'freeCommandBuffers' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withCommandBuffers :: forall io r . MonadIO io => Device -> CommandBufferAllocateInfo -> (io (Vector CommandBuffer) -> (Vector CommandBuffer -> io ()) -> r) -> r
withCommandBuffers device pAllocateInfo b =
  b (allocateCommandBuffers device pAllocateInfo)
    (\(o0) -> freeCommandBuffers device (commandPool (pAllocateInfo :: CommandBufferAllocateInfo)) o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeCommandBuffers
  :: FunPtr (Ptr Device_T -> CommandPool -> Word32 -> Ptr (Ptr CommandBuffer_T) -> IO ()) -> Ptr Device_T -> CommandPool -> Word32 -> Ptr (Ptr CommandBuffer_T) -> IO ()

-- No documentation found for TopLevel "vkFreeCommandBuffers"
freeCommandBuffers :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkFreeCommandBuffers" "device"
                      Device
                   -> -- No documentation found for Nested "vkFreeCommandBuffers" "commandPool"
                      CommandPool
                   -> -- No documentation found for Nested "vkFreeCommandBuffers" "pCommandBuffers"
                      ("commandBuffers" ::: Vector CommandBuffer)
                   -> io ()
freeCommandBuffers device commandPool commandBuffers = liftIO . evalContT $ do
  let vkFreeCommandBuffersPtr = pVkFreeCommandBuffers (deviceCmds (device :: Device))
  lift $ unless (vkFreeCommandBuffersPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkFreeCommandBuffers is null" Nothing Nothing
  let vkFreeCommandBuffers' = mkVkFreeCommandBuffers vkFreeCommandBuffersPtr
  pPCommandBuffers <- ContT $ allocaBytesAligned @(Ptr CommandBuffer_T) ((Data.Vector.length (commandBuffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBuffers `plusPtr` (8 * (i)) :: Ptr (Ptr CommandBuffer_T)) (commandBufferHandle (e))) (commandBuffers)
  lift $ vkFreeCommandBuffers' (deviceHandle (device)) (commandPool) ((fromIntegral (Data.Vector.length $ (commandBuffers)) :: Word32)) (pPCommandBuffers)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBeginCommandBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct CommandBufferBeginInfo) -> IO Result) -> Ptr CommandBuffer_T -> Ptr (SomeStruct CommandBufferBeginInfo) -> IO Result

-- No documentation found for TopLevel "vkBeginCommandBuffer"
beginCommandBuffer :: forall a io
                    . (Extendss CommandBufferBeginInfo a, PokeChain a, MonadIO io)
                   => -- No documentation found for Nested "vkBeginCommandBuffer" "commandBuffer"
                      CommandBuffer
                   -> -- No documentation found for Nested "vkBeginCommandBuffer" "pBeginInfo"
                      (CommandBufferBeginInfo a)
                   -> io ()
beginCommandBuffer commandBuffer beginInfo = liftIO . evalContT $ do
  let vkBeginCommandBufferPtr = pVkBeginCommandBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkBeginCommandBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBeginCommandBuffer is null" Nothing Nothing
  let vkBeginCommandBuffer' = mkVkBeginCommandBuffer vkBeginCommandBufferPtr
  pBeginInfo <- ContT $ withCStruct (beginInfo)
  r <- lift $ vkBeginCommandBuffer' (commandBufferHandle (commandBuffer)) (forgetExtensions pBeginInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))

-- | This function will call the supplied action between calls to
-- 'beginCommandBuffer' and 'endCommandBuffer'
--
-- Note that 'endCommandBuffer' is *not* called if an exception is thrown
-- by the inner action.
useCommandBuffer :: forall a io r . (Extendss CommandBufferBeginInfo a, PokeChain a, MonadIO io) => CommandBuffer -> CommandBufferBeginInfo a -> io r -> io r
useCommandBuffer commandBuffer pBeginInfo a =
  (beginCommandBuffer commandBuffer pBeginInfo) *> a <* (endCommandBuffer commandBuffer)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEndCommandBuffer
  :: FunPtr (Ptr CommandBuffer_T -> IO Result) -> Ptr CommandBuffer_T -> IO Result

-- No documentation found for TopLevel "vkEndCommandBuffer"
endCommandBuffer :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkEndCommandBuffer" "commandBuffer"
                    CommandBuffer
                 -> io ()
endCommandBuffer commandBuffer = liftIO $ do
  let vkEndCommandBufferPtr = pVkEndCommandBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkEndCommandBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEndCommandBuffer is null" Nothing Nothing
  let vkEndCommandBuffer' = mkVkEndCommandBuffer vkEndCommandBufferPtr
  r <- vkEndCommandBuffer' (commandBufferHandle (commandBuffer))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetCommandBuffer
  :: FunPtr (Ptr CommandBuffer_T -> CommandBufferResetFlags -> IO Result) -> Ptr CommandBuffer_T -> CommandBufferResetFlags -> IO Result

-- No documentation found for TopLevel "vkResetCommandBuffer"
resetCommandBuffer :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkResetCommandBuffer" "commandBuffer"
                      CommandBuffer
                   -> -- No documentation found for Nested "vkResetCommandBuffer" "flags"
                      CommandBufferResetFlags
                   -> io ()
resetCommandBuffer commandBuffer flags = liftIO $ do
  let vkResetCommandBufferPtr = pVkResetCommandBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkResetCommandBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetCommandBuffer is null" Nothing Nothing
  let vkResetCommandBuffer' = mkVkResetCommandBuffer vkResetCommandBufferPtr
  r <- vkResetCommandBuffer' (commandBufferHandle (commandBuffer)) (flags)
  when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkCommandBufferAllocateInfo"
data CommandBufferAllocateInfo = CommandBufferAllocateInfo
  { -- No documentation found for Nested "VkCommandBufferAllocateInfo" "commandPool"
    commandPool :: CommandPool
  , -- No documentation found for Nested "VkCommandBufferAllocateInfo" "level"
    level :: CommandBufferLevel
  , -- No documentation found for Nested "VkCommandBufferAllocateInfo" "commandBufferCount"
    commandBufferCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandBufferAllocateInfo)
#endif
deriving instance Show CommandBufferAllocateInfo

instance ToCStruct CommandBufferAllocateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferAllocateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CommandPool)) (commandPool)
    poke ((p `plusPtr` 24 :: Ptr CommandBufferLevel)) (level)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (commandBufferCount)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CommandPool)) (zero)
    poke ((p `plusPtr` 24 :: Ptr CommandBufferLevel)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct CommandBufferAllocateInfo where
  peekCStruct p = do
    commandPool <- peek @CommandPool ((p `plusPtr` 16 :: Ptr CommandPool))
    level <- peek @CommandBufferLevel ((p `plusPtr` 24 :: Ptr CommandBufferLevel))
    commandBufferCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ CommandBufferAllocateInfo
             commandPool level commandBufferCount


instance Storable CommandBufferAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CommandBufferAllocateInfo where
  zero = CommandBufferAllocateInfo
           zero
           zero
           zero



-- No documentation found for TopLevel "VkCommandBufferInheritanceInfo"
data CommandBufferInheritanceInfo (es :: [Type]) = CommandBufferInheritanceInfo
  { -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "renderPass"
    renderPass :: RenderPass
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "subpass"
    subpass :: Word32
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "framebuffer"
    framebuffer :: Framebuffer
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "occlusionQueryEnable"
    occlusionQueryEnable :: Bool
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "queryFlags"
    queryFlags :: QueryControlFlags
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "pipelineStatistics"
    pipelineStatistics :: QueryPipelineStatisticFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandBufferInheritanceInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (CommandBufferInheritanceInfo es)

instance Extensible CommandBufferInheritanceInfo where
  extensibleType = STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
  setNext x next = x{next = next}
  getNext CommandBufferInheritanceInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends CommandBufferInheritanceInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @CommandBufferInheritanceRenderPassTransformInfoQCOM = Just f
    | Just Refl <- eqT @e @CommandBufferInheritanceConditionalRenderingInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss CommandBufferInheritanceInfo es, PokeChain es) => ToCStruct (CommandBufferInheritanceInfo es) where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferInheritanceInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderPass)) (renderPass)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (subpass)
    lift $ poke ((p `plusPtr` 32 :: Ptr Framebuffer)) (framebuffer)
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (occlusionQueryEnable))
    lift $ poke ((p `plusPtr` 44 :: Ptr QueryControlFlags)) (queryFlags)
    lift $ poke ((p `plusPtr` 48 :: Ptr QueryPipelineStatisticFlags)) (pipelineStatistics)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance (Extendss CommandBufferInheritanceInfo es, PeekChain es) => FromCStruct (CommandBufferInheritanceInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    renderPass <- peek @RenderPass ((p `plusPtr` 16 :: Ptr RenderPass))
    subpass <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    framebuffer <- peek @Framebuffer ((p `plusPtr` 32 :: Ptr Framebuffer))
    occlusionQueryEnable <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    queryFlags <- peek @QueryControlFlags ((p `plusPtr` 44 :: Ptr QueryControlFlags))
    pipelineStatistics <- peek @QueryPipelineStatisticFlags ((p `plusPtr` 48 :: Ptr QueryPipelineStatisticFlags))
    pure $ CommandBufferInheritanceInfo
             next renderPass subpass framebuffer (bool32ToBool occlusionQueryEnable) queryFlags pipelineStatistics

instance es ~ '[] => Zero (CommandBufferInheritanceInfo es) where
  zero = CommandBufferInheritanceInfo
           ()
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkCommandBufferBeginInfo"
data CommandBufferBeginInfo (es :: [Type]) = CommandBufferBeginInfo
  { -- No documentation found for Nested "VkCommandBufferBeginInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkCommandBufferBeginInfo" "flags"
    flags :: CommandBufferUsageFlags
  , -- No documentation found for Nested "VkCommandBufferBeginInfo" "pInheritanceInfo"
    inheritanceInfo :: Maybe (SomeStruct CommandBufferInheritanceInfo)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandBufferBeginInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (CommandBufferBeginInfo es)

instance Extensible CommandBufferBeginInfo where
  extensibleType = STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
  setNext x next = x{next = next}
  getNext CommandBufferBeginInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends CommandBufferBeginInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DeviceGroupCommandBufferBeginInfo = Just f
    | otherwise = Nothing

instance (Extendss CommandBufferBeginInfo es, PokeChain es) => ToCStruct (CommandBufferBeginInfo es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferBeginInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr CommandBufferUsageFlags)) (flags)
    pInheritanceInfo'' <- case (inheritanceInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (CommandBufferInheritanceInfo '[])) $ \cont -> withSomeCStruct @CommandBufferInheritanceInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (CommandBufferInheritanceInfo _)))) pInheritanceInfo''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss CommandBufferBeginInfo es, PeekChain es) => FromCStruct (CommandBufferBeginInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @CommandBufferUsageFlags ((p `plusPtr` 16 :: Ptr CommandBufferUsageFlags))
    pInheritanceInfo <- peek @(Ptr (CommandBufferInheritanceInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (CommandBufferInheritanceInfo a))))
    pInheritanceInfo' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pInheritanceInfo
    pure $ CommandBufferBeginInfo
             next flags pInheritanceInfo'

instance es ~ '[] => Zero (CommandBufferBeginInfo es) where
  zero = CommandBufferBeginInfo
           ()
           zero
           Nothing

