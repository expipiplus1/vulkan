{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_global_priority"
module Vulkan.Extensions.VK_EXT_global_priority  ( DeviceQueueGlobalPriorityCreateInfoEXT(..)
                                                 , QueueGlobalPriorityEXT( QUEUE_GLOBAL_PRIORITY_LOW_EXT
                                                                         , QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT
                                                                         , QUEUE_GLOBAL_PRIORITY_HIGH_EXT
                                                                         , QUEUE_GLOBAL_PRIORITY_REALTIME_EXT
                                                                         , ..
                                                                         )
                                                 , EXT_GLOBAL_PRIORITY_SPEC_VERSION
                                                 , pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION
                                                 , EXT_GLOBAL_PRIORITY_EXTENSION_NAME
                                                 , pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME
                                                 ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT))

-- No documentation found for TopLevel "VkDeviceQueueGlobalPriorityCreateInfoEXT"
data DeviceQueueGlobalPriorityCreateInfoEXT = DeviceQueueGlobalPriorityCreateInfoEXT
  { -- No documentation found for Nested "VkDeviceQueueGlobalPriorityCreateInfoEXT" "globalPriority"
    globalPriority :: QueueGlobalPriorityEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceQueueGlobalPriorityCreateInfoEXT)
#endif
deriving instance Show DeviceQueueGlobalPriorityCreateInfoEXT

instance ToCStruct DeviceQueueGlobalPriorityCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceQueueGlobalPriorityCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueGlobalPriorityEXT)) (globalPriority)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueGlobalPriorityEXT)) (zero)
    f

instance FromCStruct DeviceQueueGlobalPriorityCreateInfoEXT where
  peekCStruct p = do
    globalPriority <- peek @QueueGlobalPriorityEXT ((p `plusPtr` 16 :: Ptr QueueGlobalPriorityEXT))
    pure $ DeviceQueueGlobalPriorityCreateInfoEXT
             globalPriority


instance Storable DeviceQueueGlobalPriorityCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceQueueGlobalPriorityCreateInfoEXT where
  zero = DeviceQueueGlobalPriorityCreateInfoEXT
           zero


-- No documentation found for TopLevel "VkQueueGlobalPriorityEXT"
newtype QueueGlobalPriorityEXT = QueueGlobalPriorityEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkQueueGlobalPriorityEXT" "VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT"
pattern QUEUE_GLOBAL_PRIORITY_LOW_EXT      = QueueGlobalPriorityEXT 128
-- No documentation found for Nested "VkQueueGlobalPriorityEXT" "VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT"
pattern QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT   = QueueGlobalPriorityEXT 256
-- No documentation found for Nested "VkQueueGlobalPriorityEXT" "VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT"
pattern QUEUE_GLOBAL_PRIORITY_HIGH_EXT     = QueueGlobalPriorityEXT 512
-- No documentation found for Nested "VkQueueGlobalPriorityEXT" "VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT"
pattern QUEUE_GLOBAL_PRIORITY_REALTIME_EXT = QueueGlobalPriorityEXT 1024
{-# complete QUEUE_GLOBAL_PRIORITY_LOW_EXT,
             QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT,
             QUEUE_GLOBAL_PRIORITY_HIGH_EXT,
             QUEUE_GLOBAL_PRIORITY_REALTIME_EXT :: QueueGlobalPriorityEXT #-}

conNameQueueGlobalPriorityEXT :: String
conNameQueueGlobalPriorityEXT = "QueueGlobalPriorityEXT"

enumPrefixQueueGlobalPriorityEXT :: String
enumPrefixQueueGlobalPriorityEXT = "QUEUE_GLOBAL_PRIORITY_"

showTableQueueGlobalPriorityEXT :: [(QueueGlobalPriorityEXT, String)]
showTableQueueGlobalPriorityEXT =
  [ (QUEUE_GLOBAL_PRIORITY_LOW_EXT     , "LOW_EXT")
  , (QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT  , "MEDIUM_EXT")
  , (QUEUE_GLOBAL_PRIORITY_HIGH_EXT    , "HIGH_EXT")
  , (QUEUE_GLOBAL_PRIORITY_REALTIME_EXT, "REALTIME_EXT")
  ]


instance Show QueueGlobalPriorityEXT where
showsPrec = enumShowsPrec enumPrefixQueueGlobalPriorityEXT
                          showTableQueueGlobalPriorityEXT
                          conNameQueueGlobalPriorityEXT
                          (\(QueueGlobalPriorityEXT x) -> x)
                          (showsPrec 11)


instance Read QueueGlobalPriorityEXT where
  readPrec = enumReadPrec enumPrefixQueueGlobalPriorityEXT
                          showTableQueueGlobalPriorityEXT
                          conNameQueueGlobalPriorityEXT
                          QueueGlobalPriorityEXT


type EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION"
pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2


type EXT_GLOBAL_PRIORITY_EXTENSION_NAME = "VK_EXT_global_priority"

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME"
pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME = "VK_EXT_global_priority"

