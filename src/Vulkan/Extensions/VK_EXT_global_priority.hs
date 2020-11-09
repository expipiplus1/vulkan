{-# language CPP #-}
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

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
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
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT))
-- | VkDeviceQueueGlobalPriorityCreateInfoEXT - Specify a system wide
-- priority
--
-- = Description
--
-- A queue created without specifying
-- 'DeviceQueueGlobalPriorityCreateInfoEXT' will default to
-- 'QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceQueueGlobalPriorityCreateInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT'
--
-- -   #VUID-VkDeviceQueueGlobalPriorityCreateInfoEXT-globalPriority-parameter#
--     @globalPriority@ /must/ be a valid 'QueueGlobalPriorityEXT' value
--
-- = See Also
--
-- 'QueueGlobalPriorityEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceQueueGlobalPriorityCreateInfoEXT = DeviceQueueGlobalPriorityCreateInfoEXT
  { -- | @globalPriority@ is the system-wide priority associated to this queue as
    -- specified by 'QueueGlobalPriorityEXT'
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


-- | VkQueueGlobalPriorityEXT - Values specifying a system-wide queue
-- priority
--
-- = Description
--
-- Priority values are sorted in ascending order. A comparison operation on
-- the enum values can be used to determine the priority order.
--
-- = See Also
--
-- 'DeviceQueueGlobalPriorityCreateInfoEXT'
newtype QueueGlobalPriorityEXT = QueueGlobalPriorityEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'QUEUE_GLOBAL_PRIORITY_LOW_EXT' is below the system default. Useful for
-- non-interactive tasks.
pattern QUEUE_GLOBAL_PRIORITY_LOW_EXT = QueueGlobalPriorityEXT 128
-- | 'QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT' is the system default priority.
pattern QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT = QueueGlobalPriorityEXT 256
-- | 'QUEUE_GLOBAL_PRIORITY_HIGH_EXT' is above the system default.
pattern QUEUE_GLOBAL_PRIORITY_HIGH_EXT = QueueGlobalPriorityEXT 512
-- | 'QUEUE_GLOBAL_PRIORITY_REALTIME_EXT' is the highest priority. Useful for
-- critical tasks.
pattern QUEUE_GLOBAL_PRIORITY_REALTIME_EXT = QueueGlobalPriorityEXT 1024
{-# complete QUEUE_GLOBAL_PRIORITY_LOW_EXT,
             QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT,
             QUEUE_GLOBAL_PRIORITY_HIGH_EXT,
             QUEUE_GLOBAL_PRIORITY_REALTIME_EXT :: QueueGlobalPriorityEXT #-}

instance Show QueueGlobalPriorityEXT where
  showsPrec p = \case
    QUEUE_GLOBAL_PRIORITY_LOW_EXT -> showString "QUEUE_GLOBAL_PRIORITY_LOW_EXT"
    QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT -> showString "QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT"
    QUEUE_GLOBAL_PRIORITY_HIGH_EXT -> showString "QUEUE_GLOBAL_PRIORITY_HIGH_EXT"
    QUEUE_GLOBAL_PRIORITY_REALTIME_EXT -> showString "QUEUE_GLOBAL_PRIORITY_REALTIME_EXT"
    QueueGlobalPriorityEXT x -> showParen (p >= 11) (showString "QueueGlobalPriorityEXT " . showsPrec 11 x)

instance Read QueueGlobalPriorityEXT where
  readPrec = parens (choose [("QUEUE_GLOBAL_PRIORITY_LOW_EXT", pure QUEUE_GLOBAL_PRIORITY_LOW_EXT)
                            , ("QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT", pure QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT)
                            , ("QUEUE_GLOBAL_PRIORITY_HIGH_EXT", pure QUEUE_GLOBAL_PRIORITY_HIGH_EXT)
                            , ("QUEUE_GLOBAL_PRIORITY_REALTIME_EXT", pure QUEUE_GLOBAL_PRIORITY_REALTIME_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "QueueGlobalPriorityEXT")
                       v <- step readPrec
                       pure (QueueGlobalPriorityEXT v)))


type EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION"
pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2


type EXT_GLOBAL_PRIORITY_EXTENSION_NAME = "VK_EXT_global_priority"

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME"
pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME = "VK_EXT_global_priority"

