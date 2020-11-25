{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_host_query_reset"
module Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset  ( resetQueryPool
                                                            , PhysicalDeviceHostQueryResetFeatures(..)
                                                            , StructureType(..)
                                                            ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.IO.Class (MonadIO)
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
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkResetQueryPool))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetQueryPool
  :: FunPtr (Ptr Device_T -> QueryPool -> Word32 -> Word32 -> IO ()) -> Ptr Device_T -> QueryPool -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkResetQueryPool"
resetQueryPool :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vkResetQueryPool" "device"
                  Device
               -> -- No documentation found for Nested "vkResetQueryPool" "queryPool"
                  QueryPool
               -> -- No documentation found for Nested "vkResetQueryPool" "firstQuery"
                  ("firstQuery" ::: Word32)
               -> -- No documentation found for Nested "vkResetQueryPool" "queryCount"
                  ("queryCount" ::: Word32)
               -> io ()
resetQueryPool device queryPool firstQuery queryCount = liftIO $ do
  let vkResetQueryPoolPtr = pVkResetQueryPool (deviceCmds (device :: Device))
  unless (vkResetQueryPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetQueryPool is null" Nothing Nothing
  let vkResetQueryPool' = mkVkResetQueryPool vkResetQueryPoolPtr
  vkResetQueryPool' (deviceHandle (device)) (queryPool) (firstQuery) (queryCount)
  pure $ ()



-- No documentation found for TopLevel "VkPhysicalDeviceHostQueryResetFeatures"
data PhysicalDeviceHostQueryResetFeatures = PhysicalDeviceHostQueryResetFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceHostQueryResetFeatures" "hostQueryReset"
    hostQueryReset :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceHostQueryResetFeatures)
#endif
deriving instance Show PhysicalDeviceHostQueryResetFeatures

instance ToCStruct PhysicalDeviceHostQueryResetFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceHostQueryResetFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (hostQueryReset))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceHostQueryResetFeatures where
  peekCStruct p = do
    hostQueryReset <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceHostQueryResetFeatures
             (bool32ToBool hostQueryReset)


instance Storable PhysicalDeviceHostQueryResetFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceHostQueryResetFeatures where
  zero = PhysicalDeviceHostQueryResetFeatures
           zero

