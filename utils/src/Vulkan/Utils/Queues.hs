{-| A common physical-device + logical-device boot recipe: pick a device
that exposes a graphics/compute/transfer queue triple (with the graphics
family also presenting, when a surface is supplied), then create a logical
device with one 'Queue' allocated per slot.

This is the wheel every "draw a thing in Vulkan" application reinvents.
The recipe here is opinionated:

- The graphics slot doubles as the present queue.
- The compute slot prefers a compute-only queue family (async compute);
  falls back to aliasing the graphics family.
- The transfer slot prefers a transfer-only queue family (DMA-only
  hardware queue); falls back to aliasing the compute family.
- Priorities are 1.0 / 0.5 / 0.2 for graphics / compute / transfer.

When two slots target the same family, two distinct 'Queue' handles are
still allocated within that shared family with the requested priorities.

If you need a different shape (compute-only, multiple graphics queues,
custom priorities, …) reach for the lower-level
'Vulkan.Utils.QueueAssignment.assignQueues' directly.
-}
module Vulkan.Utils.Queues
  ( Queues (..)
  , withDevice
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.Vector as V
import Data.Word (Word64)
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.DeviceInitialization as DI
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR)
import Vulkan.Requirement (DeviceRequirement)
import Vulkan.Utils.Initialization (createDeviceFromRequirements, pickPhysicalDevice)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..), QueueSpec (..), assignQueues, isComputeQueueFamily, isGraphicsQueueFamily, isPresentQueueFamily, isTransferOnlyQueueFamily)
import Vulkan.Zero (zero)

{- | The G/C/T queue kit. Parametric in the slot contents so the same
shape can carry priorities ('Float'), family indices ('QueueFamilyIndex'),
queue specs ('QueueSpec'), or fully-resolved @(QueueFamilyIndex, Queue)@
pairs.
-}
data Queues a = Queues
  { qGraphics :: a
  -- ^ graphics + present, priority 1.0
  , qCompute :: a
  -- ^ compute (prefers compute-only family), priority 0.5
  , qTransfer :: a
  -- ^ transfer (prefers transfer-only family), priority 0.2
  }
  deriving (Functor, Foldable, Traversable)

-- | Elementwise zip — handy for combining priorities with family predicates.
instance Applicative Queues where
  pure x = Queues x x x
  Queues f g h <*> Queues x y z = Queues (f x) (g y) (h z)

{- | Pick a physical device that has the queue families needed for the
caller, then create a logical device exposing one queue per G/C/T slot.
Devices are scored by total memory.

Pass @'Just' surface@ for windowed callers — the graphics family must also
support presentation. Pass 'Nothing' for headless callers — any graphics
family will do.

Pass any extra device requirements (extensions, features, API version) in
the third argument; they are forwarded to 'createDeviceFromRequirements'.

Fails (via 'MonadFail') when no physical device satisfies the family
requirements.
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
    Nothing -> fail "No physical device with the required G/C/T queue families"

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
