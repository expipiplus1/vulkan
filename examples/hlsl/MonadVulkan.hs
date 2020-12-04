{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module MonadVulkan where

import           AutoApply
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Monad                  ( replicateM
                                                , void
                                                )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.ByteString                ( ByteString )
import           Data.List                      ( isSuffixOf )
import           HasVulkan
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     ( addTopDecls )
import           OpenTelemetry.Eventlog         ( beginSpan
                                                , endSpan
                                                )
import           UnliftIO                       ( Async
                                                , MonadUnliftIO(withRunInIO)
                                                , asyncWithUnmask
                                                , mask
                                                , toIO
                                                , uninterruptibleCancel
                                                )
import           UnliftIO.Exception             ( bracket )
import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
                                               as Timeline
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.Utils.CommandCheck
import           Vulkan.Utils.QueueAssignment
import           VulkanMemoryAllocator         as VMA
                                         hiding ( getPhysicalDeviceProperties )

----------------------------------------------------------------
-- Define the monad in which most of the program will run
----------------------------------------------------------------

-- | @V@ keeps track of a bunch of "global" handles and performs resource
-- management.
newtype V a = V { unV :: ReaderT GlobalHandles (ResourceT IO) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadResource
                   )

instance MonadUnliftIO V where
  withRunInIO a = V $ withRunInIO (\r -> a (r . unV))

newtype CmdT m a = CmdT { unCmdT :: ReaderT CommandBuffer m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadResource
                   , HasVulkan
                   )

instance MonadUnliftIO m => MonadUnliftIO (CmdT m) where
  withRunInIO a = CmdT $ withRunInIO (\r -> a (r . unCmdT))

instance HasVulkan V where
  getInstance       = V (asks ghInstance)
  getGraphicsQueue  = V (asks (snd . graphicsQueue . ghQueues))
  getPhysicalDevice = V (asks ghPhysicalDevice)
  getDevice         = V (asks ghDevice)
  getAllocator      = V (asks ghAllocator)

getGraphicsQueueFamilyIndex :: V QueueFamilyIndex
getGraphicsQueueFamilyIndex = V (asks (fst . graphicsQueue . ghQueues))

getCommandBuffer :: Monad m => CmdT m CommandBuffer
getCommandBuffer = CmdT ask

useCommandBuffer'
  :: forall a m r
   . ( Extendss CommandBufferBeginInfo a
     , PokeChain a
     , MonadIO m
     , MonadUnliftIO m
     )
  => CommandBuffer
  -> CommandBufferBeginInfo a
  -> CmdT m r
  -> m r
useCommandBuffer' commandBuffer beginInfo (CmdT a) =
  useCommandBuffer commandBuffer beginInfo (runReaderT a commandBuffer)

runV
  :: Instance
  -> PhysicalDevice
  -> Device
  -> Queues (QueueFamilyIndex, Queue)
  -> Allocator
  -> V a
  -> ResourceT IO a
runV ghInstance ghPhysicalDevice ghDevice ghQueues ghAllocator v = do
  (bin, nib) <- liftIO newChan
  let ghRecycleBin = writeChan bin
      ghRecycleNib = do
        (try, block) <- tryReadChan nib
        maybe (Left block) Right <$> tryRead try

  flip runReaderT GlobalHandles { .. } . unV $ v

-- | A bunch of global, unchanging state we cart around
data GlobalHandles = GlobalHandles
  { ghInstance       :: Instance
  , ghPhysicalDevice :: PhysicalDevice
  , ghDevice         :: Device
  , ghAllocator      :: Allocator
  , ghQueues         :: Queues (QueueFamilyIndex, Queue)
  , ghRecycleBin     :: RecycledResources -> IO ()
    -- ^ Filled with resources which aren't destroyed after finishing a frame,
    -- but instead are used by another frame which executes after that one is
    -- retired, (taken from ghRecycleNib)
    --
    -- Make sure not to pass any resources which were created with a frame-only
    -- scope however!
  , ghRecycleNib     :: IO (Either (IO RecycledResources) RecycledResources)
    -- ^ The resources of prior frames waiting to be taken
  }

-- | These are resources which are reused by a later frame when the current
-- frame is retired
data RecycledResources = RecycledResources
  { fImageAvailableSemaphore :: Semaphore
    -- ^ A binary semaphore passed to 'acquireNextImageKHR'
  , fRenderFinishedSemaphore :: Semaphore
    -- ^ A binary semaphore to synchronize rendering and presenting
  , fCommandPool             :: CommandPool
    -- ^ Pool for this frame's commands (might want more than one of these for
    -- multithreaded recording)
  }

-- | The shape of all the queues we use for our program, parameterized over the
-- queue type so we can use it with 'Vulkan.Utils.QueueAssignment.assignQueues'
newtype Queues q = Queues { graphicsQueue :: q }
  deriving (Functor, Foldable, Traversable)

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- Start an async thread which will be cancelled at the end of the ResourceT
-- block
spawn :: V a -> V (Async a)
spawn a = do
  aIO <- toIO a
  -- If we don't remove the release key when the thread is done it'll leak,
  -- remove it at the end of the async action when the thread is going to die
  -- anyway.
  --
  -- Mask this so there's no chance we're inturrupted before writing the mvar.
  kv  <- liftIO newEmptyMVar
  UnliftIO.mask $ \_ -> do
    (k, r) <- allocate
      (asyncWithUnmask
        (\unmask -> unmask $ aIO <* (unprotect =<< liftIO (readMVar kv)))
      )
      uninterruptibleCancel
    liftIO $ putMVar kv k
    pure r

spawn_ :: V () -> V ()
spawn_ = void . spawn

-- Profiling span
withSpan_ :: MonadUnliftIO m => ByteString -> m c -> m c
withSpan_ n x = bracket (beginSpan n) endSpan (const x)

----------------------------------------------------------------
-- Commands
----------------------------------------------------------------

--
-- Wrap a bunch of Vulkan commands so that they automatically pull global
-- handles from any `HasVulkan` instance.
--
-- Wrapped functions are suffixed with "'"
--
do
  let vmaCommands =
        [ 'withBuffer
        , 'invalidateAllocation
        ]
      commands =
        [ 'acquireNextImageKHRSafe
        , 'allocateDescriptorSets
        , 'cmdBindDescriptorSets
        , 'cmdBindPipeline
        , 'cmdDispatch
        , 'cmdDraw
        , 'cmdPushConstants
        , 'cmdSetScissor
        , 'cmdSetViewport
        , 'cmdUseRenderPass
        , 'deviceWaitIdle
        , 'deviceWaitIdleSafe
        , 'getDeviceQueue
        , 'getPhysicalDeviceSurfaceCapabilitiesKHR
        , 'getPhysicalDeviceSurfaceFormatsKHR
        , 'getPhysicalDeviceSurfacePresentModesKHR
        , 'getSwapchainImagesKHR
        , 'resetCommandPool
        , 'updateDescriptorSets
        , 'waitForFences
        , 'waitForFencesSafe
        , 'Timeline.waitSemaphores
        , 'Timeline.waitSemaphoresSafe
        , 'withCommandBuffers
        , 'withCommandPool
        , 'withComputePipelines
        , 'withDescriptorPool
        , 'withDescriptorSetLayout
        , 'withFence
        , 'withFramebuffer
        , 'withGraphicsPipelines
        , 'withImageView
        , 'withInstance
        , 'withPipelineLayout
        , 'withRenderPass
        , 'withSemaphore
        , 'withShaderModule
        , 'withSwapchainKHR
        ]
  addTopDecls =<< [d|checkCommands = $(checkCommandsExp commands)|]
  ds <- autoapplyDecs
    (<> "''")
    [ 'getDevice
    , 'getPhysicalDevice
    , 'getInstance
    , 'getAllocator
    , 'noAllocationCallbacks
    , 'getCommandBuffer
    ]
    -- Allocate doesn't subsume the continuation type on the "with" commands, so
    -- put it in the unifying group.
    ['allocate]
    (vmaCommands <> commands)
  -- TODO: neaten this!
  ds' <- concat <$> sequenceA [ case d of
                FunD n [Clause ps (NormalB o) _ ]
                  | b <- nameBase n
                  , "''" `isSuffixOf` b
                  -> do
                    let n' = mkName (init b)
                        vkName = init (init b)
                        eArity = \case
                          LamE ls e -> length ls + eArity e
                          _ -> 0
                        arity = length ps + eArity o
                    vs <- replicateM arity (newName "x")
                    e <- [|withSpan_ $(litE (StringL vkName)) $(foldl appE (varE n) (varE <$> vs))|]
                    pure [FunD n' [Clause (VarP <$> vs) (NormalB e) []]]
                _ -> pure [d]
            | d <- ds
            ]
  pure (ds <> ds')
