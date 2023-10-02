{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_object_refresh"
module Vulkan.Extensions.VK_KHR_object_refresh  ( cmdRefreshObjectsKHR
                                                , getPhysicalDeviceRefreshableObjectTypesKHR
                                                , RefreshObjectKHR(..)
                                                , RefreshObjectListKHR(..)
                                                , RefreshObjectFlagsKHR
                                                , RefreshObjectFlagBitsKHR(..)
                                                , KHR_OBJECT_REFRESH_SPEC_VERSION
                                                , pattern KHR_OBJECT_REFRESH_SPEC_VERSION
                                                , KHR_OBJECT_REFRESH_EXTENSION_NAME
                                                , pattern KHR_OBJECT_REFRESH_EXTENSION_NAME
                                                ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdRefreshObjectsKHR))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceRefreshableObjectTypesKHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType)
import Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_REFRESH_OBJECT_LIST_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdRefreshObjectsKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr RefreshObjectListKHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr RefreshObjectListKHR -> IO ()

-- No documentation found for TopLevel "vkCmdRefreshObjectsKHR"
cmdRefreshObjectsKHR :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkCmdRefreshObjectsKHR" "commandBuffer"
                        CommandBuffer
                     -> -- No documentation found for Nested "vkCmdRefreshObjectsKHR" "pRefreshObjects"
                        ("refreshObjects" ::: RefreshObjectListKHR)
                     -> io ()
cmdRefreshObjectsKHR commandBuffer refreshObjects = liftIO . evalContT $ do
  let vkCmdRefreshObjectsKHRPtr = pVkCmdRefreshObjectsKHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdRefreshObjectsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdRefreshObjectsKHR is null" Nothing Nothing
  let vkCmdRefreshObjectsKHR' = mkVkCmdRefreshObjectsKHR vkCmdRefreshObjectsKHRPtr
  pRefreshObjects <- ContT $ withCStruct (refreshObjects)
  lift $ traceAroundEvent "vkCmdRefreshObjectsKHR" (vkCmdRefreshObjectsKHR'
                                                      (commandBufferHandle (commandBuffer))
                                                      pRefreshObjects)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceRefreshableObjectTypesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr ObjectType -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr ObjectType -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceRefreshableObjectTypesKHR"
getPhysicalDeviceRefreshableObjectTypesKHR :: forall io
                                            . (MonadIO io)
                                           => -- No documentation found for Nested "vkGetPhysicalDeviceRefreshableObjectTypesKHR" "physicalDevice"
                                              PhysicalDevice
                                           -> io (Result, ("refreshableObjectTypes" ::: Vector ObjectType))
getPhysicalDeviceRefreshableObjectTypesKHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceRefreshableObjectTypesKHRPtr = pVkGetPhysicalDeviceRefreshableObjectTypesKHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceRefreshableObjectTypesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceRefreshableObjectTypesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceRefreshableObjectTypesKHR' = mkVkGetPhysicalDeviceRefreshableObjectTypesKHR vkGetPhysicalDeviceRefreshableObjectTypesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPRefreshableObjectTypeCount <- ContT $ bracket (callocBytes @Word32 4) free
  _ <- lift $ traceAroundEvent "vkGetPhysicalDeviceRefreshableObjectTypesKHR" (vkGetPhysicalDeviceRefreshableObjectTypesKHR'
                                                                                 physicalDevice'
                                                                                 (pPRefreshableObjectTypeCount)
                                                                                 (nullPtr))
  pRefreshableObjectTypeCount <- lift $ peek @Word32 pPRefreshableObjectTypeCount
  pPRefreshableObjectTypes <- ContT $ bracket (callocBytes @ObjectType ((fromIntegral (pRefreshableObjectTypeCount)) * 4)) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceRefreshableObjectTypesKHR" (vkGetPhysicalDeviceRefreshableObjectTypesKHR'
                                                                                 physicalDevice'
                                                                                 (pPRefreshableObjectTypeCount)
                                                                                 (pPRefreshableObjectTypes))
  pRefreshableObjectTypeCount' <- lift $ peek @Word32 pPRefreshableObjectTypeCount
  pRefreshableObjectTypes' <- lift $ generateM (fromIntegral (pRefreshableObjectTypeCount')) (\i -> peek @ObjectType ((pPRefreshableObjectTypes `advancePtrBytes` (4 * (i)) :: Ptr ObjectType)))
  pure $ (r, pRefreshableObjectTypes')


-- No documentation found for TopLevel "VkRefreshObjectKHR"
data RefreshObjectKHR = RefreshObjectKHR
  { -- No documentation found for Nested "VkRefreshObjectKHR" "objectType"
    objectType :: ObjectType
  , -- No documentation found for Nested "VkRefreshObjectKHR" "objectHandle"
    objectHandle :: Word64
  , -- No documentation found for Nested "VkRefreshObjectKHR" "flags"
    flags :: RefreshObjectFlagsKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RefreshObjectKHR)
#endif
deriving instance Show RefreshObjectKHR

instance ToCStruct RefreshObjectKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RefreshObjectKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectType)) (objectType)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (objectHandle)
    poke ((p `plusPtr` 16 :: Ptr RefreshObjectFlagsKHR)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ObjectType)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (zero)
    f

instance FromCStruct RefreshObjectKHR where
  peekCStruct p = do
    objectType <- peek @ObjectType ((p `plusPtr` 0 :: Ptr ObjectType))
    objectHandle <- peek @Word64 ((p `plusPtr` 8 :: Ptr Word64))
    flags <- peek @RefreshObjectFlagsKHR ((p `plusPtr` 16 :: Ptr RefreshObjectFlagsKHR))
    pure $ RefreshObjectKHR
             objectType objectHandle flags

instance Storable RefreshObjectKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RefreshObjectKHR where
  zero = RefreshObjectKHR
           zero
           zero
           zero


-- No documentation found for TopLevel "VkRefreshObjectListKHR"
data RefreshObjectListKHR = RefreshObjectListKHR
  { -- No documentation found for Nested "VkRefreshObjectListKHR" "pObjects"
    objects :: Vector RefreshObjectKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RefreshObjectListKHR)
#endif
deriving instance Show RefreshObjectListKHR

instance ToCStruct RefreshObjectListKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RefreshObjectListKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_REFRESH_OBJECT_LIST_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (objects)) :: Word32))
    pPObjects' <- ContT $ allocaBytes @RefreshObjectKHR ((Data.Vector.length (objects)) * 24)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPObjects' `plusPtr` (24 * (i)) :: Ptr RefreshObjectKHR) (e)) (objects)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr RefreshObjectKHR))) (pPObjects')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_REFRESH_OBJECT_LIST_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct RefreshObjectListKHR where
  peekCStruct p = do
    objectCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pObjects <- peek @(Ptr RefreshObjectKHR) ((p `plusPtr` 24 :: Ptr (Ptr RefreshObjectKHR)))
    pObjects' <- generateM (fromIntegral objectCount) (\i -> peekCStruct @RefreshObjectKHR ((pObjects `advancePtrBytes` (24 * (i)) :: Ptr RefreshObjectKHR)))
    pure $ RefreshObjectListKHR
             pObjects'

instance Zero RefreshObjectListKHR where
  zero = RefreshObjectListKHR
           mempty


type RefreshObjectFlagsKHR = RefreshObjectFlagBitsKHR

-- No documentation found for TopLevel "VkRefreshObjectFlagBitsKHR"
newtype RefreshObjectFlagBitsKHR = RefreshObjectFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameRefreshObjectFlagBitsKHR :: String
conNameRefreshObjectFlagBitsKHR = "RefreshObjectFlagBitsKHR"

enumPrefixRefreshObjectFlagBitsKHR :: String
enumPrefixRefreshObjectFlagBitsKHR = ""

showTableRefreshObjectFlagBitsKHR :: [(RefreshObjectFlagBitsKHR, String)]
showTableRefreshObjectFlagBitsKHR = []

instance Show RefreshObjectFlagBitsKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixRefreshObjectFlagBitsKHR
      showTableRefreshObjectFlagBitsKHR
      conNameRefreshObjectFlagBitsKHR
      (\(RefreshObjectFlagBitsKHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read RefreshObjectFlagBitsKHR where
  readPrec =
    enumReadPrec
      enumPrefixRefreshObjectFlagBitsKHR
      showTableRefreshObjectFlagBitsKHR
      conNameRefreshObjectFlagBitsKHR
      RefreshObjectFlagBitsKHR

type KHR_OBJECT_REFRESH_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_OBJECT_REFRESH_SPEC_VERSION"
pattern KHR_OBJECT_REFRESH_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_OBJECT_REFRESH_SPEC_VERSION = 1


type KHR_OBJECT_REFRESH_EXTENSION_NAME = "VK_KHR_object_refresh"

-- No documentation found for TopLevel "VK_KHR_OBJECT_REFRESH_EXTENSION_NAME"
pattern KHR_OBJECT_REFRESH_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_OBJECT_REFRESH_EXTENSION_NAME = "VK_KHR_object_refresh"

