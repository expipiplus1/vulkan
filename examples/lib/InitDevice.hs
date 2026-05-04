-- | Helpers shared by the windowed examples for picking a physical device
-- and creating a logical device with a uniform G/C/T queue kit (graphics+
-- present, compute, transfer).
module InitDevice
  ( withDevice
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.Vector                   as V
import           Data.Word                      ( Word64 )
import           Say                            ( sayErr )
import           Utils                          ( noSuchThing )
import           VkResources                    ( Queues(..) )
import           Vulkan.CStruct.Extends         ( SomeStruct(..) )
import           Vulkan.Core10           hiding ( withDevice )
import qualified Vulkan.Core10.DeviceInitialization
                                               as DI
import           Vulkan.Extensions.VK_KHR_surface
                                                ( SurfaceKHR )
import           Vulkan.Requirement             ( DeviceRequirement )
import           Vulkan.Utils.Initialization    ( createDeviceFromRequirements
                                                , pickPhysicalDevice
                                                )
import           Vulkan.Utils.QueueAssignment   ( QueueFamilyIndex(..)
                                                , QueueSpec(..)
                                                , assignQueues
                                                , isComputeQueueFamily
                                                , isGraphicsQueueFamily
                                                , isPresentQueueFamily
                                                , isTransferOnlyQueueFamily
                                                )
import           Vulkan.Zero                    ( zero )

-- | Pick a physical device that has a graphics+present queue family AND a
-- compute queue family, then create a logical device exposing one queue per
-- G/C/T slot. Devices are scored by total memory.
--
-- Each capability prefers its own dedicated family (async compute, DMA-only
-- transfer); falls back to aliasing graphics+present when the hardware
-- doesn't expose one. When two slots target the same family, two distinct
-- 'Queue' handles are still allocated within that family with the requested
-- priorities (1.0 / 0.5 / 0.2).
--
-- Pass any extra device requirements (extensions, features, API version) in
-- @extraReqs@; they are forwarded to 'createDeviceFromRequirements'.
withDevice
  :: (MonadResource m, MonadFail m)
  => Instance
  -> SurfaceKHR
  -> [DeviceRequirement]
  -> m (PhysicalDevice, Device, Queues (QueueFamilyIndex, Queue))
withDevice inst surface extraReqs = do
  mPd <- pickPhysicalDevice inst (discoverFamilies surface)
                                 (snd :: (Queues QueueFamilyIndex, Word64) -> Word64)
  ((qFams, _score), phys) <- case mPd of
    Just x  -> pure x
    Nothing -> sayErr "No suitable physical device found"
            >> noSuchThing "No physical device with graphics+present and compute"

  let mkSpec target prio = QueueSpec prio (\i _ -> pure (i == target))
      specs              = mkSpec <$> qFams <*> Queues 1.0 0.5 0.2

  Just (qInfos, getQs) <- assignQueues phys specs

  dev <- createDeviceFromRequirements
    extraReqs
    []
    phys
    zero { queueCreateInfos = SomeStruct <$> qInfos }
  qs <- liftIO (getQs dev)
  pure (phys, dev, qs)

-- | Suitability probe used by 'pickPhysicalDevice'. Returns the discovered
-- @(graphics+present, compute, transfer)@ family triple plus a memory score.
discoverFamilies
  :: MonadIO m
  => SurfaceKHR
  -> PhysicalDevice
  -> m (Maybe (Queues QueueFamilyIndex, Word64))
discoverFamilies surf phys = do
  qProps <- getPhysicalDeviceQueueFamilyProperties phys
  let withIndex = V.toList (V.indexed qProps)
      asQfi i   = QueueFamilyIndex (fromIntegral i)

      graphicsFamilies =
        [ asQfi i | (i, q) <- withIndex, isGraphicsQueueFamily q ]
      asyncCompute =
        [ asQfi i
        | (i, q) <- withIndex
        , isComputeQueueFamily q && not (isGraphicsQueueFamily q)
        ]
      anyCompute =
        [ asQfi i | (i, q) <- withIndex, isComputeQueueFamily q ]
      dedicatedTransfer =
        [ asQfi i | (i, q) <- withIndex, isTransferOnlyQueueFamily q ]

  presentResults <- mapM
    (\qfi -> (qfi, ) <$> isPresentQueueFamily phys surf qfi)
    graphicsFamilies
  let mGp = case [ qfi | (qfi, True) <- presentResults ] of
        qfi : _ -> Just qfi
        []      -> Nothing
      mCp = case asyncCompute of
        qfi : _ -> Just qfi
        []      -> case anyCompute of
          qfi : _ -> Just qfi
          []      -> Nothing

  case (mGp, mCp) of
    (Just gp, Just cp) -> do
      let tf = case dedicatedTransfer of
            qfi : _ -> qfi
            []      -> cp
      heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
      let score = sum (DI.size <$> heaps) :: Word64
      pure (Just (Queues gp cp tf, score))
    _ -> pure Nothing
