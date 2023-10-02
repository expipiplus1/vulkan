{-# language CPP #-}
-- No documentation found for Chapter "FaultHandlingFunctionality"
module Vulkan.Core10.FaultHandlingFunctionality  ( getFaultData
                                                 , FaultData(..)
                                                 , FaultCallbackInfo(..)
                                                 , StructureType(..)
                                                 , FaultLevel(..)
                                                 , FaultType(..)
                                                 , FaultQueryBehavior(..)
                                                 , FN_vkFaultCallbackFunction
                                                 , PFN_vkFaultCallbackFunction
                                                 ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetFaultData))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.FaultLevel (FaultLevel)
import Vulkan.Core10.Enums.FaultQueryBehavior (FaultQueryBehavior)
import Vulkan.Core10.Enums.FaultQueryBehavior (FaultQueryBehavior(..))
import Vulkan.Core10.Enums.FaultType (FaultType)
import Vulkan.Core10.FuncPointers (PFN_vkFaultCallbackFunction)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FAULT_CALLBACK_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FAULT_DATA))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.FuncPointers (FN_vkFaultCallbackFunction)
import Vulkan.Core10.Enums.FaultLevel (FaultLevel(..))
import Vulkan.Core10.Enums.FaultQueryBehavior (FaultQueryBehavior(..))
import Vulkan.Core10.Enums.FaultType (FaultType(..))
import Vulkan.Core10.FuncPointers (PFN_vkFaultCallbackFunction)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFaultData
  :: FunPtr (Ptr Device_T -> FaultQueryBehavior -> Ptr Bool32 -> Ptr Word32 -> Ptr FaultData -> IO Result) -> Ptr Device_T -> FaultQueryBehavior -> Ptr Bool32 -> Ptr Word32 -> Ptr FaultData -> IO Result

-- No documentation found for TopLevel "vkGetFaultData"
getFaultData :: forall io
              . (MonadIO io)
             => -- No documentation found for Nested "vkGetFaultData" "device"
                Device
             -> -- No documentation found for Nested "vkGetFaultData" "faultQueryBehavior"
                FaultQueryBehavior
             -> io (Result, ("unrecordedFaults" ::: Bool), ("faultCount" ::: Word32), ("faults" ::: Vector FaultData))
getFaultData device faultQueryBehavior = liftIO . evalContT $ do
  let vkGetFaultDataPtr = pVkGetFaultData (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetFaultDataPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetFaultData is null" Nothing Nothing
  let vkGetFaultData' = mkVkGetFaultData vkGetFaultDataPtr
  pPUnrecordedFaults <- ContT $ bracket (callocBytes @Bool32 4) free
  pPFaultCount <- ContT $ bracket (callocBytes @Word32 4) free
  pPFaults <- ContT $ bracket (callocBytes @FaultData ((fromIntegral (faultCount)) * 24)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPFaults `advancePtrBytes` (i * 24) :: Ptr FaultData) . ($ ())) [0..(fromIntegral (faultCount)) - 1]
  r <- lift $ traceAroundEvent "vkGetFaultData" (vkGetFaultData'
                                                   (deviceHandle (device))
                                                   (faultQueryBehavior)
                                                   (pPUnrecordedFaults)
                                                   (pPFaultCount)
                                                   ((pPFaults)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pUnrecordedFaults <- lift $ peek @Bool32 pPUnrecordedFaults
  pFaultCount <- lift $ peek @Word32 pPFaultCount
  pFaults <- lift $ generateM (fromIntegral (faultCount)) (\i -> peekCStruct @FaultData (((pPFaults) `advancePtrBytes` (24 * (i)) :: Ptr FaultData)))
  pure $ (r, (bool32ToBool pUnrecordedFaults), pFaultCount, pFaults)


-- No documentation found for TopLevel "VkFaultData"
data FaultData = FaultData
  { -- No documentation found for Nested "VkFaultData" "faultLevel"
    faultLevel :: FaultLevel
  , -- No documentation found for Nested "VkFaultData" "faultType"
    faultType :: FaultType
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FaultData)
#endif
deriving instance Show FaultData

instance ToCStruct FaultData where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FaultData{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FAULT_DATA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FaultLevel)) (faultLevel)
    poke ((p `plusPtr` 20 :: Ptr FaultType)) (faultType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FAULT_DATA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FaultLevel)) (zero)
    poke ((p `plusPtr` 20 :: Ptr FaultType)) (zero)
    f

instance FromCStruct FaultData where
  peekCStruct p = do
    faultLevel <- peek @FaultLevel ((p `plusPtr` 16 :: Ptr FaultLevel))
    faultType <- peek @FaultType ((p `plusPtr` 20 :: Ptr FaultType))
    pure $ FaultData
             faultLevel faultType

instance Storable FaultData where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FaultData where
  zero = FaultData
           zero
           zero


-- No documentation found for TopLevel "VkFaultCallbackInfo"
data FaultCallbackInfo = FaultCallbackInfo
  { -- No documentation found for Nested "VkFaultCallbackInfo" "faultCount"
    faultCount :: Word32
  , -- No documentation found for Nested "VkFaultCallbackInfo" "pFaults"
    faults :: Ptr FaultData
  , -- No documentation found for Nested "VkFaultCallbackInfo" "pfnFaultCallback"
    pfnFaultCallback :: PFN_vkFaultCallbackFunction
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FaultCallbackInfo)
#endif
deriving instance Show FaultCallbackInfo

instance ToCStruct FaultCallbackInfo where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FaultCallbackInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FAULT_CALLBACK_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (faultCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr FaultData))) (faults)
    poke ((p `plusPtr` 32 :: Ptr PFN_vkFaultCallbackFunction)) (pfnFaultCallback)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FAULT_CALLBACK_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 32 :: Ptr PFN_vkFaultCallbackFunction)) (zero)
    f

instance FromCStruct FaultCallbackInfo where
  peekCStruct p = do
    faultCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pFaults <- peek @(Ptr FaultData) ((p `plusPtr` 24 :: Ptr (Ptr FaultData)))
    pfnFaultCallback <- peek @PFN_vkFaultCallbackFunction ((p `plusPtr` 32 :: Ptr PFN_vkFaultCallbackFunction))
    pure $ FaultCallbackInfo
             faultCount pFaults pfnFaultCallback

instance Storable FaultCallbackInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FaultCallbackInfo where
  zero = FaultCallbackInfo
           zero
           zero
           zero

