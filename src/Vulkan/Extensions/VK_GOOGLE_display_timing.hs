{-# language CPP #-}
-- No documentation found for Chapter "VK_GOOGLE_display_timing"
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
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
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
import Data.String (IsString)
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

-- No documentation found for TopLevel "vkGetRefreshCycleDurationGOOGLE"
getRefreshCycleDurationGOOGLE :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkGetRefreshCycleDurationGOOGLE" "device"
                                 Device
                              -> -- No documentation found for Nested "vkGetRefreshCycleDurationGOOGLE" "swapchain"
                                 SwapchainKHR
                              -> io (("displayTimingProperties" ::: RefreshCycleDurationGOOGLE))
getRefreshCycleDurationGOOGLE device swapchain = liftIO . evalContT $ do
  let vkGetRefreshCycleDurationGOOGLEPtr = pVkGetRefreshCycleDurationGOOGLE (deviceCmds (device :: Device))
  lift $ unless (vkGetRefreshCycleDurationGOOGLEPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRefreshCycleDurationGOOGLE is null" Nothing Nothing
  let vkGetRefreshCycleDurationGOOGLE' = mkVkGetRefreshCycleDurationGOOGLE vkGetRefreshCycleDurationGOOGLEPtr
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

-- No documentation found for TopLevel "vkGetPastPresentationTimingGOOGLE"
getPastPresentationTimingGOOGLE :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkGetPastPresentationTimingGOOGLE" "device"
                                   Device
                                -> -- No documentation found for Nested "vkGetPastPresentationTimingGOOGLE" "swapchain"
                                   SwapchainKHR
                                -> io (Result, ("presentationTimings" ::: Vector PastPresentationTimingGOOGLE))
getPastPresentationTimingGOOGLE device swapchain = liftIO . evalContT $ do
  let vkGetPastPresentationTimingGOOGLEPtr = pVkGetPastPresentationTimingGOOGLE (deviceCmds (device :: Device))
  lift $ unless (vkGetPastPresentationTimingGOOGLEPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPastPresentationTimingGOOGLE is null" Nothing Nothing
  let vkGetPastPresentationTimingGOOGLE' = mkVkGetPastPresentationTimingGOOGLE vkGetPastPresentationTimingGOOGLEPtr
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



-- No documentation found for TopLevel "VkRefreshCycleDurationGOOGLE"
data RefreshCycleDurationGOOGLE = RefreshCycleDurationGOOGLE
  { -- No documentation found for Nested "VkRefreshCycleDurationGOOGLE" "refreshDuration"
    refreshDuration :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RefreshCycleDurationGOOGLE)
#endif
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



-- No documentation found for TopLevel "VkPastPresentationTimingGOOGLE"
data PastPresentationTimingGOOGLE = PastPresentationTimingGOOGLE
  { -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "presentID"
    presentID :: Word32
  , -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "desiredPresentTime"
    desiredPresentTime :: Word64
  , -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "actualPresentTime"
    actualPresentTime :: Word64
  , -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "earliestPresentTime"
    earliestPresentTime :: Word64
  , -- No documentation found for Nested "VkPastPresentationTimingGOOGLE" "presentMargin"
    presentMargin :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PastPresentationTimingGOOGLE)
#endif
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



-- No documentation found for TopLevel "VkPresentTimesInfoGOOGLE"
data PresentTimesInfoGOOGLE = PresentTimesInfoGOOGLE
  { -- No documentation found for Nested "VkPresentTimesInfoGOOGLE" "swapchainCount"
    swapchainCount :: Word32
  , -- No documentation found for Nested "VkPresentTimesInfoGOOGLE" "pTimes"
    times :: Vector PresentTimeGOOGLE
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentTimesInfoGOOGLE)
#endif
deriving instance Show PresentTimesInfoGOOGLE

instance ToCStruct PresentTimesInfoGOOGLE where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentTimesInfoGOOGLE{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let pTimesLength = Data.Vector.length $ (times)
    swapchainCount'' <- lift $ if (swapchainCount) == 0
      then pure $ fromIntegral pTimesLength
      else do
        unless (fromIntegral pTimesLength == (swapchainCount) || pTimesLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pTimes must be empty or have 'swapchainCount' elements" Nothing Nothing
        pure (swapchainCount)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (swapchainCount'')
    pTimes'' <- if Data.Vector.null (times)
      then pure nullPtr
      else do
        pPTimes <- ContT $ allocaBytesAligned @PresentTimeGOOGLE (((Data.Vector.length (times))) * 16) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPTimes `plusPtr` (16 * (i)) :: Ptr PresentTimeGOOGLE) (e)) ((times))
        pure $ pPTimes
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr PresentTimeGOOGLE))) pTimes''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
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



-- No documentation found for TopLevel "VkPresentTimeGOOGLE"
data PresentTimeGOOGLE = PresentTimeGOOGLE
  { -- No documentation found for Nested "VkPresentTimeGOOGLE" "presentID"
    presentID :: Word32
  , -- No documentation found for Nested "VkPresentTimeGOOGLE" "desiredPresentTime"
    desiredPresentTime :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentTimeGOOGLE)
#endif
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

