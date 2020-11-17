module Vulkan.Utils.QueueAssignment
  ( assignQueues
  , QueueSpec(..)
  , QueueFamilyIndex(..)
  , QueueIndex(..)
  -- * Queue Family Predicates
  , isComputeQueueFamily
  , isGraphicsQueueFamily
  , isTransferQueueFamily
  , isTransferOnlyQueueFamily
  , isPresentQueueFamily
  ) where

import           Control.Applicative
import           Control.Category               ( (>>>) )
import           Control.Monad                  ( filterM )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Strict
                                                ( evalState
                                                , evalStateT
                                                , get
                                                , put
                                                )
import           Data.Bits
import           Data.Foldable
import           Data.Functor                   ( (<&>) )
import           Data.Traversable
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Data.Word
import           GHC.Stack                      ( HasCallStack )
import           Vulkan.Core10
import           Vulkan.Extensions.VK_KHR_surface
                                                ( SurfaceKHR
                                                , getPhysicalDeviceSurfaceSupportKHR
                                                )
import           Vulkan.Utils.Misc
import           Vulkan.Zero

----------------------------------------------------------------
-- Device Queue creation
----------------------------------------------------------------

-- | Requirements for a 'Queue' to be assigned a family by 'assignQueues'.
--
-- To assign to a specific queue family index @f@:
--
-- @
-- queueSpecFamilyPredicate = \i _ -> i == f
-- @
--
-- To assign to any queue family which supports compute operations:
--
-- @
-- let isComputeQueue q = QUEUE_COMPUTE_BIT .&&. queueFlags q
-- in QueueSpec priority (\_index q -> pure (isComputeQueue q))
-- @
data QueueSpec m = QueueSpec
  { queueSpecQueuePriority :: Float
  , queueSpecFamilyPredicate
      :: QueueFamilyIndex -> QueueFamilyProperties -> m Bool
  }

newtype QueueFamilyIndex = QueueFamilyIndex { unQueueFamilyIndex :: Word32 }
  deriving (Eq, Ord, Enum, Show)

newtype QueueIndex = QueueIndex { unQueueIndex :: Word32 }
  deriving (Eq, Ord, Enum, Show)

-- | Given a 'PhysicalDevice' and a set of requirements for queues, calculate an
-- assignment of queues to queue families and return information with which to
-- create a 'Device' and also a function to extract the requested 'Queue's from
-- the device.
--
-- You may want to create a custom type with a 'Traversable' instance to store
-- your queues like:
--
-- @
-- data MyQueues q = MyQueues
--   { computeQueue            :: q
--   , graphicsAndPresentQueue :: q
--   , transferQueue           :: q
--   }
--
-- myQueueSpecs :: MyQueues QueueSpec
-- myQueueSpecs = MyQueues
--   { computeQueue            = QueueSpec 0.5 isComputeQueueFamily
--   , graphicsAndPresentQueue = QueueSpec 1   isPresentQueueFamily
--   , transferQueue           = QueueSpec 1   isTransferOnlyQueueFamily
--   }
-- @
--
-- Note, this doesn't permit differentiating queue family assignment based on
-- whether or not the queue is protected.
assignQueues
  :: forall f m n
   . (Traversable f, MonadIO m, MonadIO n)
  => PhysicalDevice
  -> f (QueueSpec m)
  -- ^ A set of requirements for 'Queue's to be created
  -> m
       ( Maybe
           (Vector (DeviceQueueCreateInfo '[]), Device -> n (f Queue))
       )
  -- ^
  -- - A set of 'DeviceQueueCreateInfo's to pass to 'createDevice'
  -- - A function to extract the requested 'Queue's from the 'Device' created
  --   with the 'DeviceQueueCreateInfo's
  --
  -- 'Nothing' if it wasn't possible to satisfy all the 'QueueSpec's
assignQueues phys specs = runMaybeT $ do
  queueFamilyProperties <-
    zip [QueueFamilyIndex 0 ..]
    .   V.toList
    <$> getPhysicalDeviceQueueFamilyProperties phys

  -- For each QueueSpec find the list of applicable families
  specsWithFamilies <- for specs $ \spec -> do
    families <- filterM (lift . uncurry (queueSpecFamilyPredicate spec))
                        queueFamilyProperties
    pure (spec, fst <$> families)

  let -- Get the number of available queues for each family
      familiesWithCapacities :: [(QueueFamilyIndex, Word32)]
      familiesWithCapacities =
        [ (i, queueCount)
        | (i, QueueFamilyProperties {..}) <- queueFamilyProperties
        ]

  -- Assign each QueueSpec to a queue family
  specsWithFamily :: f (QueueSpec m, QueueFamilyIndex) <- headMay
    (assign
      familiesWithCapacities
      (specsWithFamilies <&> \(spec, indices) index ->
        if index `elem` indices then Just (spec, index) else Nothing
      )
    )

  let maxFamilyIndex :: Maybe QueueFamilyIndex
      maxFamilyIndex = maximumMay (snd <$> toList specsWithFamily)

      -- Assign each QueueSpec an index within its queue family
      specsWithQueueIndex :: f (QueueSpec m, QueueFamilyIndex, QueueIndex)
      specsWithQueueIndex =
        flip evalState (repeat (QueueIndex 0))
          $ for specsWithFamily
          $ \(spec, familyIndex) -> do
              indices <- get
              let (index, indices') =
                    incrementAt (unQueueFamilyIndex familyIndex) indices
              put indices'
              pure (spec, familyIndex, index)

      -- Gather the priorities for each queue in each queue family
      queuePriorities :: [[Float]]
      queuePriorities = foldr
        (\(QueueSpec {..}, QueueFamilyIndex i) ps ->
          prependAt i queueSpecQueuePriority ps
        )
        (replicate
          (maybe 0 (fromIntegral . unQueueFamilyIndex . succ) maxFamilyIndex)
          []
        )
        specsWithFamily

      -- Make 'DeviceQueueCreateInfo's for the required queue families and
      -- priorities.
      queueCreateInfos :: Vector (DeviceQueueCreateInfo '[])
      queueCreateInfos = V.fromList
        [ zero { queueFamilyIndex = familyIndex
               , queuePriorities  = V.fromList ps
               }
        | (familyIndex, ps) <- zip [0 ..] queuePriorities
        , not (null ps)
        ]

      -- Get
      extractQueues :: Device -> n (f Queue)
      extractQueues dev =
        for specsWithQueueIndex
          $ \(_, QueueFamilyIndex familyIndex, QueueIndex index) ->
              getDeviceQueue dev familyIndex index

  pure (queueCreateInfos, extractQueues)

----------------------------------------------------------------
-- Queue Predicates
----------------------------------------------------------------

isComputeQueueFamily :: QueueFamilyProperties -> Bool
isComputeQueueFamily q = QUEUE_COMPUTE_BIT .&&. queueFlags q

isGraphicsQueueFamily :: QueueFamilyProperties -> Bool
isGraphicsQueueFamily q = QUEUE_GRAPHICS_BIT .&&. queueFlags q

isTransferQueueFamily :: QueueFamilyProperties -> Bool
isTransferQueueFamily q = QUEUE_TRANSFER_BIT .&&. queueFlags q

-- | Does this queue have 'QUEUE_TRANSFER_BIT' set and not 'QUEUE_COMPUTE_BIT'
-- or 'QUEUE_GRAPHICS_BIT'
isTransferOnlyQueueFamily :: QueueFamilyProperties -> Bool
isTransferOnlyQueueFamily q =
  (   queueFlags q
    .&. (QUEUE_TRANSFER_BIT .|. QUEUE_GRAPHICS_BIT .|. QUEUE_COMPUTE_BIT)
    )
    == QUEUE_TRANSFER_BIT

-- | Can this queue family present to this surface on this device
isPresentQueueFamily
  :: MonadIO m => PhysicalDevice -> SurfaceKHR -> QueueFamilyIndex -> m Bool
isPresentQueueFamily phys surf (QueueFamilyIndex i) =
  getPhysicalDeviceSurfaceSupportKHR phys i surf

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Find all possible valid assignments for elements of a 'Traversable' with
-- some limited resources.
--
-- >>> assign @[] @_ @() [("a", 1)] []
-- [[]]
--
-- >>> assign @[] [("hi", 1), ("foo", 3)] [Just, Just . reverse, Just . take 1 ]
-- [["hi","oof","f"],["foo","ih","f"],["foo","oof","h"],["foo","oof","f"]]
--
-- >>> assign @[] [("a", 1), ("b", 2)] [\case {"a" -> Just 1; "b" -> Just 2; _ -> Nothing}, \case {"b" -> Just 3; _ -> Nothing}, \case {"a" -> Just 4; _ -> Nothing}]
-- [[2,3,4]]
assign
  :: forall f a b
   . Traversable f
  => [(a, Word32)]
  -- ^ How many of each 'a' are available
  -> f (a -> Maybe b)
  -- ^ Which 'a's can each element use
  -> [f b]
  -- ^ A list of assignments, each element in this list has the length of the
  -- requirements list
assign capacities = flip evalStateT capacities . traverse
  (\p -> do
    cs            <- get
    (choice, cs') <- lift (select p cs)
    put cs'
    pure choice
  )

-- | Select an element from the list according to some predicate, and return
-- that element along with the decremented list.
select :: (a -> Maybe b) -> [(a, Word32)] -> [(b, [(a, Word32)])]
select p = \case
  [] -> []
  x : xs ->
    let hit b = (b, if snd x == 1 then xs else (pred <$> x) : xs)
        miss = do
          (selected, xs') <- select p xs
          pure (selected, x : xs')
    in  if snd x == 0
          then miss
          else case p (fst x) of
            Nothing -> miss
            Just b  -> hit b : miss

headMay :: Alternative f => [a] -> f a
headMay = \case
  []    -> empty
  x : _ -> pure x

maximumMay :: (Foldable f, Ord a) => f a -> Maybe a
maximumMay f = if null f then Nothing else Just (maximum f)

incrementAt :: (HasCallStack, Enum a) => Word32 -> [a] -> (a, [a])
incrementAt index = modAt index succ

prependAt :: HasCallStack => Word32 -> a -> [[a]] -> [[a]]
prependAt index p = snd . modAt index (p :)

modAt :: HasCallStack => Word32 -> (a -> a) -> [a] -> (a, [a])
modAt index f = splitAt (fromIntegral index) >>> \case
  (_ , []    ) -> error "modAt, out of bounds"
  (xs, y : ys) -> (y, xs <> (f y : ys))
