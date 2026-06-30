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
  , allocateDevice
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Foldable (foldl', toList)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Traversable (mapAccumL)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.DeviceInitialization as DI
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR)
import Vulkan.Requirement (DeviceRequirement)
import Vulkan.Utils.Initialization (allocateDeviceFromRequirements, pickPhysicalDevice)
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
  deriving (Show, Functor, Foldable, Traversable)

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
the third argument; they are forwarded to 'allocateDeviceFromRequirements'.

Fails (via 'MonadFail') when no physical device satisfies the family
requirements.
-}
allocateDevice
  :: (MonadResource m, MonadFail m)
  => Vk.Instance
  -> Maybe SurfaceKHR
  -> [DeviceRequirement]
  -> m (Vk.PhysicalDevice, Vk.Device, Queues (QueueFamilyIndex, Vk.Queue))
allocateDevice inst mSurface extraReqs = do
  mPd <-
    pickPhysicalDevice
      inst
      (discoverFamilies mSurface)
      (snd :: (Queues QueueFamilyIndex, Word64) -> Word64)
  ((qFams, _score), phys) <- case mPd of
    Just x -> pure x
    Nothing -> fail "No physical device with the required G/C/T queue families"

  let
    prios = Queues 1.0 0.5 0.2
    mkSpec target prio = QueueSpec prio (\i _ -> pure (i == target))
    specs = mkSpec <$> qFams <*> prios

  -- Prefer 'assignQueues', which hands each slot its own queue for maximum
  -- parallelism. When the hardware can't supply that many distinct queues
  -- (e.g. a lone graphics+compute family exposing a single queue, as some
  -- mobile and translation-layer drivers do) fall back to sharing rather than
  -- failing: the triple still works, just with serialized submission.
  (qInfos, getQs) <-
    assignQueues phys specs >>= \case
      Just qs -> pure qs
      Nothing -> shareQueues phys ((,) <$> qFams <*> prios)

  dev <-
    allocateDeviceFromRequirements
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

{- | Robust fallback for 'allocateDevice' when 'assignQueues' can't give every
slot its own queue. Allocates as many distinct queues per family as the
hardware exposes, then aliases the surplus slots onto them round-robin, so it
always succeeds.

A shared queue keeps every capability it was selected for — a graphics+compute
family also handles transfer — so the result is correct, just less concurrent.
Two slots that resolve to the same 'Vk.Queue' compare equal, so a caller who
cares can detect aliasing. Callers submitting from multiple threads must
externally synchronize a shared queue themselves; see
'Vulkan.Utils.QueueAssignment'.
-}
shareQueues
  :: (MonadIO m)
  => Vk.PhysicalDevice
  -> Queues (QueueFamilyIndex, Float)
  -- ^ The resolved family and queue priority for each slot.
  -> m
       ( V.Vector (Vk.DeviceQueueCreateInfo '[])
       , Vk.Device -> IO (Queues (QueueFamilyIndex, Vk.Queue))
       )
shareQueues phys famPrios = do
  capacities <- familyCapacities phys
  let
    capOf fam = Map.findWithDefault 0 fam capacities

    -- Hand each slot a queue index within its family, wrapping at the family's
    -- capacity so surplus slots reuse (alias) earlier queues.
    step counts (fam, prio) =
      let
        used = Map.findWithDefault 0 fam counts
        idx = used `mod` max 1 (capOf fam)
      in
        (Map.insert fam (used + 1) counts, (fam, prio, idx))

    slots :: Queues (QueueFamilyIndex, Float, Word32)
    slots = snd (mapAccumL step Map.empty famPrios)

    -- The highest requested priority wins for a queue shared by several slots.
    priorityAt :: Map (QueueFamilyIndex, Word32) Float
    priorityAt =
      foldl'
        (\acc (fam, prio, idx) -> Map.insertWith max (fam, idx) prio acc)
        Map.empty
        (toList slots)

    -- One create-info per family, priorities ordered by queue index.
    perFamily :: Map QueueFamilyIndex [(Word32, Float)]
    perFamily =
      Map.fromListWith
        (<>)
        [(fam, [(idx, prio)]) | ((fam, idx), prio) <- Map.toList priorityAt]

    createInfos =
      V.fromList
        [ zero
            { Vk.queueFamilyIndex = unQueueFamilyIndex fam
            , Vk.queuePriorities = V.fromList (snd <$> sortOn fst idxPrios)
            }
        | (fam, idxPrios) <- Map.toList perFamily
        ]

    getQueues dev =
      traverse
        ( \(fam, _, idx) ->
            (fam,) <$> Vk.getDeviceQueue dev (unQueueFamilyIndex fam) idx
        )
        slots

  pure (createInfos, getQueues)

-- | The number of queues each queue family of a 'Vk.PhysicalDevice' exposes.
familyCapacities
  :: (MonadIO m) => Vk.PhysicalDevice -> m (Map QueueFamilyIndex Word32)
familyCapacities phys = do
  props <- Vk.getPhysicalDeviceQueueFamilyProperties phys
  pure $
    Map.fromList
      [ (QueueFamilyIndex (fromIntegral i), Vk.queueCount qfp)
      | (i, qfp) <- zip [0 :: Int ..] (V.toList props)
      ]
