{-| Helpers shared by the windowed examples for picking a physical device
and creating a logical device with a uniform G/C/T queue kit (graphics+
present, compute, transfer).
-}
module InitDevice
  ( withDevice
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.Vector as V
import Data.Word (Word64)
import Say (sayErr)
import Utils (noSuchThing)
import VkResources (Queues (..))
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.DeviceInitialization as DI
import Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR
  )
import Vulkan.Requirement (DeviceRequirement)
import Vulkan.Utils.Initialization
  ( createDeviceFromRequirements
  , pickPhysicalDevice
  )
import Vulkan.Utils.QueueAssignment
  ( QueueFamilyIndex (..)
  , QueueSpec (..)
  , assignQueues
  , isComputeQueueFamily
  , isGraphicsQueueFamily
  , isPresentQueueFamily
  , isTransferOnlyQueueFamily
  )
import Vulkan.Zero (zero)

{- | Pick a physical device that has the queue families needed for the
caller, then create a logical device exposing one queue per G/C/T slot.
Devices are scored by total memory.

Pass 'Just surface' for windowed callers — the graphics family must also
support presentation. Pass 'Nothing' for headless callers — any graphics
family will do.

Each capability prefers its own dedicated family (async compute, DMA-only
transfer); falls back to aliasing graphics when the hardware doesn't
expose one. When two slots target the same family, two distinct 'Queue'
handles are still allocated within that family with the requested
priorities (1.0 / 0.5 / 0.2).

Pass any extra device requirements (extensions, features, API version) in
@extraReqs@; they are forwarded to 'createDeviceFromRequirements'.
-}
withDevice
  :: (MonadResource m, MonadFail m)
  => Vk.Instance
  -> Maybe SurfaceKHR
  -> [DeviceRequirement]
  -> m (Vk.PhysicalDevice, Vk.Device, Queues (QueueFamilyIndex, Vk.Queue))
withDevice inst mSurface extraReqs = do
  mPd <-
    pickPhysicalDevice
      inst
      (discoverFamilies mSurface)
      (snd :: (Queues QueueFamilyIndex, Word64) -> Word64)
  ((qFams, _score), phys) <- case mPd of
    Just x -> pure x
    Nothing ->
      sayErr "No suitable physical device found"
        >> noSuchThing "No physical device with the required queue families"

  let
    mkSpec target prio = QueueSpec prio (\i _ -> pure (i == target))
    specs = mkSpec <$> qFams <*> Queues 1.0 0.5 0.2

  Just (qInfos, getQs) <- assignQueues phys specs

  dev <-
    createDeviceFromRequirements
      extraReqs
      []
      phys
      zero{Vk.queueCreateInfos = SomeStruct <$> qInfos}
  qs <- liftIO (getQs dev)
  pure (phys, dev, qs)

{- | Suitability probe used by 'pickPhysicalDevice'. Returns the discovered
@(graphics, compute, transfer)@ family triple plus a memory score. When a
surface is supplied the chosen graphics family must also support
presentation; otherwise any graphics family will do.
-}
discoverFamilies
  :: (MonadIO m)
  => Maybe SurfaceKHR
  -> Vk.PhysicalDevice
  -> m (Maybe (Queues QueueFamilyIndex, Word64))
discoverFamilies mSurf phys = do
  qProps <- Vk.getPhysicalDeviceQueueFamilyProperties phys
  let
    withIndex = V.toList (V.indexed qProps)
    asQfi i = QueueFamilyIndex (fromIntegral i)

    graphicsFamilies =
      [asQfi i | (i, q) <- withIndex, isGraphicsQueueFamily q]
    asyncCompute =
      [ asQfi i
      | (i, q) <- withIndex
      , isComputeQueueFamily q && not (isGraphicsQueueFamily q)
      ]
    anyCompute =
      [asQfi i | (i, q) <- withIndex, isComputeQueueFamily q]
    dedicatedTransfer =
      [asQfi i | (i, q) <- withIndex, isTransferOnlyQueueFamily q]

  mGp <- case mSurf of
    Just surf -> do
      presentResults <-
        mapM
          (\qfi -> (qfi,) <$> isPresentQueueFamily phys surf qfi)
          graphicsFamilies
      pure $ case [qfi | (qfi, True) <- presentResults] of
        qfi : _ -> Just qfi
        [] -> Nothing
    Nothing ->
      pure $ case graphicsFamilies of
        qfi : _ -> Just qfi
        [] -> Nothing

  let mCp = case asyncCompute of
        qfi : _ -> Just qfi
        [] -> case anyCompute of
          qfi : _ -> Just qfi
          [] -> Nothing

  case (mGp, mCp) of
    (Just gp, Just cp) -> do
      let tf = case dedicatedTransfer of
            qfi : _ -> qfi
            [] -> cp
      heaps <- Vk.memoryHeaps <$> Vk.getPhysicalDeviceMemoryProperties phys
      let score = sum (DI.size <$> heaps) :: Word64
      pure (Just (Queues gp cp tf, score))
    _ -> pure Nothing
