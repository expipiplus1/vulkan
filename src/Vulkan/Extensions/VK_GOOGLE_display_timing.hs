{-# language CPP #-}
module Vulkan.Extensions.VK_GOOGLE_display_timing  ( getRefreshCycleDurationGOOGLE
                                                   , getPastPresentationTimingGOOGLE
                                                   , RefreshCycleDurationGOOGLE(..)
                                                   , PastPresentationTimingGOOGLE(..)
                                                   , PresentTimesInfoGOOGLE(..)
                                                   , PresentTimeGOOGLE(..)
                                                   , GOOGLE_DISPLAY_TIMING_SPEC_VERSION
                                                   , pattern GOOGLE_DISPLAY_TIMING_SPEC_VERSION
                                                   , GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
                                                   , pattern GOOGLE_DISPLAY_TIMING_EXTENSION_NAME
                                                   , SwapchainKHR(..)
                                                   ) where

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetPastPresentationTimingGOOGLE))
import Vulkan.Dynamic (DeviceCmds(pVkGetRefreshCycleDurationGOOGLE))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRefreshCycleDurationGOOGLE
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr RefreshCycleDurationGOOGLE -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Ptr RefreshCycleDurationGOOGLE -> IO Result

-- | vkGetRefreshCycleDurationGOOGLE - Obtain the RC duration of the PE’s
-- display
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to obtain the refresh duration for.
--
-- -   @pDisplayTimingProperties@ is a pointer to a
--     'RefreshCycleDurationGOOGLE' structure.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   @pDisplayTimingProperties@ /must/ be a valid pointer to a
--     'RefreshCycleDurationGOOGLE' structure
--
-- -   Both of @device@, and @swapchain@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'RefreshCycleDurationGOOGLE',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
getRefreshCycleDurationGOOGLE :: forall io . MonadIO io => Device -> SwapchainKHR -> io (("displayTimingProperties" ::: RefreshCycleDurationGOOGLE))
getRefreshCycleDurationGOOGLE device swapchain = liftIO . evalContT $ do
  let vkGetRefreshCycleDurationGOOGLE' = mkVkGetRefreshCycleDurationGOOGLE (pVkGetRefreshCycleDurationGOOGLE (deviceCmds (device :: Device)))
  pPDisplayTimingProperties <- ContT (withZeroCStruct @RefreshCycleDurationGOOGLE)
  r <- lift $ vkGetRefreshCycleDurationGOOGLE' (deviceHandle (device)) (swapchain) (pPDisplayTimingProperties)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDisplayTimingProperties <- lift $ peekCStruct @RefreshCycleDurationGOOGLE pPDisplayTimingProperties
  pure $ (pDisplayTimingProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPastPresentationTimingGOOGLE
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr Word32 -> Ptr PastPresentationTimingGOOGLE -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Ptr Word32 -> Ptr PastPresentationTimingGOOGLE -> IO Result

-- | vkGetPastPresentationTimingGOOGLE - Obtain timing of a
-- previously-presented image
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to obtain presentation timing
--     information duration for.
--
-- -   @pPresentationTimingCount@ is a pointer to an integer related to the
--     number of 'PastPresentationTimingGOOGLE' structures to query, as
--     described below.
--
-- -   @pPresentationTimings@ is either @NULL@ or a pointer to an array of
--     'PastPresentationTimingGOOGLE' structures.
--
-- = Description
--
-- If @pPresentationTimings@ is @NULL@, then the number of newly-available
-- timing records for the given @swapchain@ is returned in
-- @pPresentationTimingCount@. Otherwise, @pPresentationTimingCount@ /must/
-- point to a variable set by the user to the number of elements in the
-- @pPresentationTimings@ array, and on return the variable is overwritten
-- with the number of structures actually written to
-- @pPresentationTimings@. If the value of @pPresentationTimingCount@ is
-- less than the number of newly-available timing records, at most
-- @pPresentationTimingCount@ structures will be written. If
-- @pPresentationTimingCount@ is smaller than the number of newly-available
-- timing records for the given @swapchain@,
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS' to indicate that not all the
-- available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   @pPresentationTimingCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pPresentationTimingCount@ is not @0@,
--     and @pPresentationTimings@ is not @NULL@, @pPresentationTimings@
--     /must/ be a valid pointer to an array of @pPresentationTimingCount@
--     'PastPresentationTimingGOOGLE' structures
--
-- -   Both of @device@, and @swapchain@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'PastPresentationTimingGOOGLE',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
getPastPresentationTimingGOOGLE :: forall io . MonadIO io => Device -> SwapchainKHR -> io (Result, ("presentationTimings" ::: Vector PastPresentationTimingGOOGLE))
getPastPresentationTimingGOOGLE device swapchain = liftIO . evalContT $ do
  let vkGetPastPresentationTimingGOOGLE' = mkVkGetPastPresentationTimingGOOGLE (pVkGetPastPresentationTimingGOOGLE (deviceCmds (device :: Device)))
  let device' = deviceHandle (device)
  pPPresentationTimingCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPastPresentationTimingGOOGLE' device' (swapchain) (pPPresentationTimingCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPresentationTimingCount <- lift $ peek @Word32 pPPresentationTimingCount
  pPPresentationTimings <- ContT $ bracket (callocBytes @PastPresentationTimingGOOGLE ((fromIntegral (pPresentationTimingCount)) * 40)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPPresentationTimings `advancePtrBytes` (i * 40) :: Ptr PastPresentationTimingGOOGLE) . ($ ())) [0..(fromIntegral (pPresentationTimingCount)) - 1]
  r' <- lift $ vkGetPastPresentationTimingGOOGLE' device' (swapchain) (pPPresentationTimingCount) ((pPPresentationTimings))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPresentationTimingCount' <- lift $ peek @Word32 pPPresentationTimingCount
  pPresentationTimings' <- lift $ generateM (fromIntegral (pPresentationTimingCount')) (\i -> peekCStruct @PastPresentationTimingGOOGLE (((pPPresentationTimings) `advancePtrBytes` (40 * (i)) :: Ptr PastPresentationTimingGOOGLE)))
  pure $ ((r'), pPresentationTimings')


-- | VkRefreshCycleDurationGOOGLE - Structure containing the RC duration of a
-- display
--
-- = See Also
--
-- 'getRefreshCycleDurationGOOGLE'
data RefreshCycleDurationGOOGLE = RefreshCycleDurationGOOGLE
  { -- | @refreshDuration@ is the number of nanoseconds from the start of one
    -- refresh cycle to the next.
    refreshDuration :: Word64 }
  deriving (Typeable)
deriving instance Show RefreshCycleDurationGOOGLE

instance ToCStruct RefreshCycleDurationGOOGLE where
  withCStruct x f = allocaBytesAligned 8 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RefreshCycleDurationGOOGLE{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word64)) (refreshDuration)
    f
  cStructSize = 8
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word64)) (zero)
    f

instance FromCStruct RefreshCycleDurationGOOGLE where
  peekCStruct p = do
    refreshDuration <- peek @Word64 ((p `plusPtr` 0 :: Ptr Word64))
    pure $ RefreshCycleDurationGOOGLE
             refreshDuration

instance Storable RefreshCycleDurationGOOGLE where
  sizeOf ~_ = 8
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RefreshCycleDurationGOOGLE where
  zero = RefreshCycleDurationGOOGLE
           zero


-- | VkPastPresentationTimingGOOGLE - Structure containing timing information
-- about a previously-presented image
--
-- = Description
--
-- The results for a given @swapchain@ and @presentID@ are only returned
-- once from 'getPastPresentationTimingGOOGLE'.
--
-- The application /can/ use the 'PastPresentationTimingGOOGLE' values to
-- occasionally adjust its timing. For example, if @actualPresentTime@ is
-- later than expected (e.g. one @refreshDuration@ late), the application
-- may increase its target IPD to a higher multiple of @refreshDuration@
-- (e.g. decrease its frame rate from 60Hz to 30Hz). If @actualPresentTime@
-- and @earliestPresentTime@ are consistently different, and if
-- @presentMargin@ is consistently large enough, the application may
-- decrease its target IPD to a smaller multiple of @refreshDuration@ (e.g.
-- increase its frame rate from 30Hz to 60Hz). If @actualPresentTime@ and
-- @earliestPresentTime@ are same, and if @presentMargin@ is consistently
-- high, the application may delay the start of its input-render-present
-- loop in order to decrease the latency between user input and the
-- corresponding present (always leaving some margin in case a new image
-- takes longer to render than the previous image). An application that
-- desires its target IPD to always be the same as @refreshDuration@, can
-- also adjust features until @actualPresentTime@ is never late and
-- @presentMargin@ is satisfactory.
--
-- = See Also
--
-- 'getPastPresentationTimingGOOGLE'
data PastPresentationTimingGOOGLE = PastPresentationTimingGOOGLE
  { -- | @presentID@ is an application-provided value that was given to a
    -- previous 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' command
    -- via 'PresentTimeGOOGLE'::@presentID@ (see below). It /can/ be used to
    -- uniquely identify a previous present with the
    -- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' command.
    presentID :: Word32
  , -- | @desiredPresentTime@ is an application-provided value that was given to
    -- a previous 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' command
    -- via 'PresentTimeGOOGLE'::@desiredPresentTime@. If non-zero, it was used
    -- by the application to indicate that an image not be presented any sooner
    -- than @desiredPresentTime@.
    desiredPresentTime :: Word64
  , -- | @actualPresentTime@ is the time when the image of the @swapchain@ was
    -- actually displayed.
    actualPresentTime :: Word64
  , -- | @earliestPresentTime@ is the time when the image of the @swapchain@
    -- could have been displayed. This /may/ differ from @actualPresentTime@ if
    -- the application requested that the image be presented no sooner than
    -- 'PresentTimeGOOGLE'::@desiredPresentTime@.
    earliestPresentTime :: Word64
  , -- | @presentMargin@ is an indication of how early the
    -- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' command was
    -- processed compared to how soon it needed to be processed, and still be
    -- presented at @earliestPresentTime@.
    presentMargin :: Word64
  }
  deriving (Typeable)
deriving instance Show PastPresentationTimingGOOGLE

instance ToCStruct PastPresentationTimingGOOGLE where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PastPresentationTimingGOOGLE{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (presentID)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (desiredPresentTime)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (actualPresentTime)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (earliestPresentTime)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (presentMargin)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    f

instance FromCStruct PastPresentationTimingGOOGLE where
  peekCStruct p = do
    presentID <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    desiredPresentTime <- peek @Word64 ((p `plusPtr` 8 :: Ptr Word64))
    actualPresentTime <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    earliestPresentTime <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    presentMargin <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    pure $ PastPresentationTimingGOOGLE
             presentID desiredPresentTime actualPresentTime earliestPresentTime presentMargin

instance Storable PastPresentationTimingGOOGLE where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PastPresentationTimingGOOGLE where
  zero = PastPresentationTimingGOOGLE
           zero
           zero
           zero
           zero
           zero


-- | VkPresentTimesInfoGOOGLE - The earliest time each image should be
-- presented
--
-- == Valid Usage
--
-- -   @swapchainCount@ /must/ be the same value as
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@swapchainCount@,
--     where 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' is
--     included in the @pNext@ chain of this 'PresentTimesInfoGOOGLE'
--     structure
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE'
--
-- -   If @pTimes@ is not @NULL@, @pTimes@ /must/ be a valid pointer to an
--     array of @swapchainCount@ 'PresentTimeGOOGLE' structures
--
-- -   @swapchainCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'PresentTimeGOOGLE', 'Vulkan.Core10.Enums.StructureType.StructureType'
data PresentTimesInfoGOOGLE = PresentTimesInfoGOOGLE
  { -- | @swapchainCount@ is the number of swapchains being presented to by this
    -- command.
    swapchainCount :: Word32
  , -- | @pTimes@ is @NULL@ or a pointer to an array of 'PresentTimeGOOGLE'
    -- elements with @swapchainCount@ entries. If not @NULL@, each element of
    -- @pTimes@ contains the earliest time to present the image corresponding
    -- to the entry in the
    -- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@pImageIndices@
    -- array.
    times :: Vector PresentTimeGOOGLE
  }
  deriving (Typeable)
deriving instance Show PresentTimesInfoGOOGLE

instance ToCStruct PresentTimesInfoGOOGLE where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentTimesInfoGOOGLE{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (swapchainCount)
    pTimes'' <- if Data.Vector.null (times)
      then pure nullPtr
      else do
        pPTimes <- ContT $ allocaBytesAligned @PresentTimeGOOGLE (((Data.Vector.length (times))) * 16) 8
        Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPTimes `plusPtr` (16 * (i)) :: Ptr PresentTimeGOOGLE) (e) . ($ ())) ((times))
        pure $ pPTimes
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr PresentTimeGOOGLE))) pTimes''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PresentTimesInfoGOOGLE where
  peekCStruct p = do
    swapchainCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pTimes <- peek @(Ptr PresentTimeGOOGLE) ((p `plusPtr` 24 :: Ptr (Ptr PresentTimeGOOGLE)))
    let pTimesLength = if pTimes == nullPtr then 0 else (fromIntegral swapchainCount)
    pTimes' <- generateM pTimesLength (\i -> peekCStruct @PresentTimeGOOGLE ((pTimes `advancePtrBytes` (16 * (i)) :: Ptr PresentTimeGOOGLE)))
    pure $ PresentTimesInfoGOOGLE
             swapchainCount pTimes'

instance Zero PresentTimesInfoGOOGLE where
  zero = PresentTimesInfoGOOGLE
           zero
           mempty


-- | VkPresentTimeGOOGLE - The earliest time image should be presented
--
-- = See Also
--
-- 'PresentTimesInfoGOOGLE'
data PresentTimeGOOGLE = PresentTimeGOOGLE
  { -- | @presentID@ is an application-provided identification value, that /can/
    -- be used with the results of 'getPastPresentationTimingGOOGLE', in order
    -- to uniquely identify this present. In order to be useful to the
    -- application, it /should/ be unique within some period of time that is
    -- meaningful to the application.
    presentID :: Word32
  , -- | @desiredPresentTime@ specifies that the image given /should/ not be
    -- displayed to the user any earlier than this time. @desiredPresentTime@
    -- is a time in nanoseconds, relative to a monotonically-increasing clock
    -- (e.g. @CLOCK_MONOTONIC@ (see clock_gettime(2)) on Android and Linux). A
    -- value of zero specifies that the presentation engine /may/ display the
    -- image at any time. This is useful when the application desires to
    -- provide @presentID@, but does not need a specific @desiredPresentTime@.
    desiredPresentTime :: Word64
  }
  deriving (Typeable)
deriving instance Show PresentTimeGOOGLE

instance ToCStruct PresentTimeGOOGLE where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentTimeGOOGLE{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (presentID)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (desiredPresentTime)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (zero)
    f

instance FromCStruct PresentTimeGOOGLE where
  peekCStruct p = do
    presentID <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    desiredPresentTime <- peek @Word64 ((p `plusPtr` 8 :: Ptr Word64))
    pure $ PresentTimeGOOGLE
             presentID desiredPresentTime

instance Storable PresentTimeGOOGLE where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PresentTimeGOOGLE where
  zero = PresentTimeGOOGLE
           zero
           zero


type GOOGLE_DISPLAY_TIMING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION"
pattern GOOGLE_DISPLAY_TIMING_SPEC_VERSION :: forall a . Integral a => a
pattern GOOGLE_DISPLAY_TIMING_SPEC_VERSION = 1


type GOOGLE_DISPLAY_TIMING_EXTENSION_NAME = "VK_GOOGLE_display_timing"

-- No documentation found for TopLevel "VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME"
pattern GOOGLE_DISPLAY_TIMING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern GOOGLE_DISPLAY_TIMING_EXTENSION_NAME = "VK_GOOGLE_display_timing"

