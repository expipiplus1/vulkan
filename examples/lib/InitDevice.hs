-- | Helpers shared by the windowed examples for picking a physical device
-- and creating a logical device with one unified graphics+present queue.
module InitDevice
  ( withGraphicsPresentDevice
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe      ( runMaybeT )
import           Control.Monad.Trans.Resource
import           Data.Functor.Identity          ( Identity(..) )
import qualified Data.Vector                   as V
import           Data.Word                      ( Word64 )
import           Say                            ( sayErr )
import           Utils                          ( noSuchThing )
import           Vulkan.CStruct.Extends         ( SomeStruct(..) )
import           Vulkan.Core10
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
                                                , isGraphicsQueueFamily
                                                , isPresentQueueFamily
                                                )
import           Vulkan.Zero                    ( zero )

-- | Pick a physical device that has a graphics queue family that can also
-- present to the given surface, then create a logical device exposing one
-- queue from that family. Devices are scored by total memory.
--
-- Pass any extra device requirements (extensions, features, API version) in
-- @extraReqs@; they are forwarded to 'createDeviceFromRequirements', which
-- will fail loudly if the chosen device cannot satisfy them.
withGraphicsPresentDevice
  :: (MonadResource m, MonadFail m)
  => Instance
  -> SurfaceKHR
  -> [DeviceRequirement]
  -> m (PhysicalDevice, Device, QueueFamilyIndex, Queue)
withGraphicsPresentDevice inst surface extraReqs = do
  mPd <- pickPhysicalDevice inst (suitable surface) id
  (_, phys) <- case mPd of
    Just x  -> pure x
    Nothing -> sayErr "No suitable physical device found"
            >> noSuchThing "No physical device with graphics+present queue"

  let queueSpec = QueueSpec 1 $ \i q ->
        if isGraphicsQueueFamily q
          then isPresentQueueFamily phys surface i
          else pure False
  Just (qInfos, getQs) <- assignQueues phys (Identity queueSpec)

  dev <- createDeviceFromRequirements
    extraReqs
    []
    phys
    zero { queueCreateInfos = SomeStruct <$> qInfos }
  Identity (qfi, queue) <- liftIO (getQs dev)
  pure (phys, dev, qfi, queue)
 where
  suitable surf phys = runMaybeT $ do
    qProps <- getPhysicalDeviceQueueFamilyProperties phys
    True   <- pure $ V.any isGraphicsQueueFamily qProps
    let presentSupport i =
          isPresentQueueFamily phys surf (QueueFamilyIndex (fromIntegral i))
    hasPresent <- V.or <$> V.imapM (\i _ -> presentSupport i) qProps
    True       <- pure hasPresent
    heaps      <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure (sum $ DI.size <$> heaps :: Word64)
