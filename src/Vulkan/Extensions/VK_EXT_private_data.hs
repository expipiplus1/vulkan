{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_private_data"
module Vulkan.Extensions.VK_EXT_private_data  ( createPrivateDataSlotEXT
                                              , withPrivateDataSlotEXT
                                              , destroyPrivateDataSlotEXT
                                              , setPrivateDataEXT
                                              , getPrivateDataEXT
                                              , DevicePrivateDataCreateInfoEXT(..)
                                              , PrivateDataSlotCreateInfoEXT(..)
                                              , PhysicalDevicePrivateDataFeaturesEXT(..)
                                              , PrivateDataSlotCreateFlagsEXT
                                              , PrivateDataSlotCreateFlagBitsEXT(..)
                                              , EXT_PRIVATE_DATA_SPEC_VERSION
                                              , pattern EXT_PRIVATE_DATA_SPEC_VERSION
                                              , EXT_PRIVATE_DATA_EXTENSION_NAME
                                              , pattern EXT_PRIVATE_DATA_EXTENSION_NAME
                                              , PrivateDataSlotEXT(..)
                                              ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreatePrivateDataSlotEXT))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyPrivateDataSlotEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetPrivateDataEXT))
import Vulkan.Dynamic (DeviceCmds(pVkSetPrivateDataEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ObjectType (ObjectType)
import Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Vulkan.Extensions.Handles (PrivateDataSlotEXT)
import Vulkan.Extensions.Handles (PrivateDataSlotEXT(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (PrivateDataSlotEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePrivateDataSlotEXT
  :: FunPtr (Ptr Device_T -> Ptr PrivateDataSlotCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr PrivateDataSlotEXT -> IO Result) -> Ptr Device_T -> Ptr PrivateDataSlotCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr PrivateDataSlotEXT -> IO Result

-- No documentation found for TopLevel "vkCreatePrivateDataSlotEXT"
createPrivateDataSlotEXT :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkCreatePrivateDataSlotEXT" "device"
                            Device
                         -> -- No documentation found for Nested "vkCreatePrivateDataSlotEXT" "pCreateInfo"
                            PrivateDataSlotCreateInfoEXT
                         -> -- No documentation found for Nested "vkCreatePrivateDataSlotEXT" "pAllocator"
                            ("allocator" ::: Maybe AllocationCallbacks)
                         -> io (PrivateDataSlotEXT)
createPrivateDataSlotEXT device createInfo allocator = liftIO . evalContT $ do
  let vkCreatePrivateDataSlotEXTPtr = pVkCreatePrivateDataSlotEXT (deviceCmds (device :: Device))
  lift $ unless (vkCreatePrivateDataSlotEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreatePrivateDataSlotEXT is null" Nothing Nothing
  let vkCreatePrivateDataSlotEXT' = mkVkCreatePrivateDataSlotEXT vkCreatePrivateDataSlotEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPrivateDataSlot <- ContT $ bracket (callocBytes @PrivateDataSlotEXT 8) free
  r <- lift $ vkCreatePrivateDataSlotEXT' (deviceHandle (device)) pCreateInfo pAllocator (pPPrivateDataSlot)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPrivateDataSlot <- lift $ peek @PrivateDataSlotEXT pPPrivateDataSlot
  pure $ (pPrivateDataSlot)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createPrivateDataSlotEXT' and 'destroyPrivateDataSlotEXT'
--
-- To ensure that 'destroyPrivateDataSlotEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withPrivateDataSlotEXT :: forall io r . MonadIO io => Device -> PrivateDataSlotCreateInfoEXT -> Maybe AllocationCallbacks -> (io PrivateDataSlotEXT -> (PrivateDataSlotEXT -> io ()) -> r) -> r
withPrivateDataSlotEXT device pCreateInfo pAllocator b =
  b (createPrivateDataSlotEXT device pCreateInfo pAllocator)
    (\(o0) -> destroyPrivateDataSlotEXT device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPrivateDataSlotEXT
  :: FunPtr (Ptr Device_T -> PrivateDataSlotEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> PrivateDataSlotEXT -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyPrivateDataSlotEXT"
destroyPrivateDataSlotEXT :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkDestroyPrivateDataSlotEXT" "device"
                             Device
                          -> -- No documentation found for Nested "vkDestroyPrivateDataSlotEXT" "privateDataSlot"
                             PrivateDataSlotEXT
                          -> -- No documentation found for Nested "vkDestroyPrivateDataSlotEXT" "pAllocator"
                             ("allocator" ::: Maybe AllocationCallbacks)
                          -> io ()
destroyPrivateDataSlotEXT device privateDataSlot allocator = liftIO . evalContT $ do
  let vkDestroyPrivateDataSlotEXTPtr = pVkDestroyPrivateDataSlotEXT (deviceCmds (device :: Device))
  lift $ unless (vkDestroyPrivateDataSlotEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyPrivateDataSlotEXT is null" Nothing Nothing
  let vkDestroyPrivateDataSlotEXT' = mkVkDestroyPrivateDataSlotEXT vkDestroyPrivateDataSlotEXTPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyPrivateDataSlotEXT' (deviceHandle (device)) (privateDataSlot) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetPrivateDataEXT
  :: FunPtr (Ptr Device_T -> ObjectType -> Word64 -> PrivateDataSlotEXT -> Word64 -> IO Result) -> Ptr Device_T -> ObjectType -> Word64 -> PrivateDataSlotEXT -> Word64 -> IO Result

-- No documentation found for TopLevel "vkSetPrivateDataEXT"
setPrivateDataEXT :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkSetPrivateDataEXT" "device"
                     Device
                  -> -- No documentation found for Nested "vkSetPrivateDataEXT" "objectType"
                     ObjectType
                  -> -- No documentation found for Nested "vkSetPrivateDataEXT" "objectHandle"
                     ("objectHandle" ::: Word64)
                  -> -- No documentation found for Nested "vkSetPrivateDataEXT" "privateDataSlot"
                     PrivateDataSlotEXT
                  -> -- No documentation found for Nested "vkSetPrivateDataEXT" "data"
                     ("data" ::: Word64)
                  -> io ()
setPrivateDataEXT device objectType objectHandle privateDataSlot data' = liftIO $ do
  let vkSetPrivateDataEXTPtr = pVkSetPrivateDataEXT (deviceCmds (device :: Device))
  unless (vkSetPrivateDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetPrivateDataEXT is null" Nothing Nothing
  let vkSetPrivateDataEXT' = mkVkSetPrivateDataEXT vkSetPrivateDataEXTPtr
  r <- vkSetPrivateDataEXT' (deviceHandle (device)) (objectType) (objectHandle) (privateDataSlot) (data')
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPrivateDataEXT
  :: FunPtr (Ptr Device_T -> ObjectType -> Word64 -> PrivateDataSlotEXT -> Ptr Word64 -> IO ()) -> Ptr Device_T -> ObjectType -> Word64 -> PrivateDataSlotEXT -> Ptr Word64 -> IO ()

-- No documentation found for TopLevel "vkGetPrivateDataEXT"
getPrivateDataEXT :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkGetPrivateDataEXT" "device"
                     Device
                  -> -- No documentation found for Nested "vkGetPrivateDataEXT" "objectType"
                     ObjectType
                  -> -- No documentation found for Nested "vkGetPrivateDataEXT" "objectHandle"
                     ("objectHandle" ::: Word64)
                  -> -- No documentation found for Nested "vkGetPrivateDataEXT" "privateDataSlot"
                     PrivateDataSlotEXT
                  -> io (("data" ::: Word64))
getPrivateDataEXT device objectType objectHandle privateDataSlot = liftIO . evalContT $ do
  let vkGetPrivateDataEXTPtr = pVkGetPrivateDataEXT (deviceCmds (device :: Device))
  lift $ unless (vkGetPrivateDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPrivateDataEXT is null" Nothing Nothing
  let vkGetPrivateDataEXT' = mkVkGetPrivateDataEXT vkGetPrivateDataEXTPtr
  pPData <- ContT $ bracket (callocBytes @Word64 8) free
  lift $ vkGetPrivateDataEXT' (deviceHandle (device)) (objectType) (objectHandle) (privateDataSlot) (pPData)
  pData <- lift $ peek @Word64 pPData
  pure $ (pData)



-- No documentation found for TopLevel "VkDevicePrivateDataCreateInfoEXT"
data DevicePrivateDataCreateInfoEXT = DevicePrivateDataCreateInfoEXT
  { -- No documentation found for Nested "VkDevicePrivateDataCreateInfoEXT" "privateDataSlotRequestCount"
    privateDataSlotRequestCount :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DevicePrivateDataCreateInfoEXT)
#endif
deriving instance Show DevicePrivateDataCreateInfoEXT

instance ToCStruct DevicePrivateDataCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DevicePrivateDataCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (privateDataSlotRequestCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DevicePrivateDataCreateInfoEXT where
  peekCStruct p = do
    privateDataSlotRequestCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DevicePrivateDataCreateInfoEXT
             privateDataSlotRequestCount


instance Storable DevicePrivateDataCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DevicePrivateDataCreateInfoEXT where
  zero = DevicePrivateDataCreateInfoEXT
           zero



-- No documentation found for TopLevel "VkPrivateDataSlotCreateInfoEXT"
data PrivateDataSlotCreateInfoEXT = PrivateDataSlotCreateInfoEXT
  { -- No documentation found for Nested "VkPrivateDataSlotCreateInfoEXT" "flags"
    flags :: PrivateDataSlotCreateFlagsEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PrivateDataSlotCreateInfoEXT)
#endif
deriving instance Show PrivateDataSlotCreateInfoEXT

instance ToCStruct PrivateDataSlotCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PrivateDataSlotCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PrivateDataSlotCreateFlagsEXT)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PrivateDataSlotCreateFlagsEXT)) (zero)
    f

instance FromCStruct PrivateDataSlotCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @PrivateDataSlotCreateFlagsEXT ((p `plusPtr` 16 :: Ptr PrivateDataSlotCreateFlagsEXT))
    pure $ PrivateDataSlotCreateInfoEXT
             flags


instance Storable PrivateDataSlotCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PrivateDataSlotCreateInfoEXT where
  zero = PrivateDataSlotCreateInfoEXT
           zero



-- No documentation found for TopLevel "VkPhysicalDevicePrivateDataFeaturesEXT"
data PhysicalDevicePrivateDataFeaturesEXT = PhysicalDevicePrivateDataFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDevicePrivateDataFeaturesEXT" "privateData"
    privateData :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePrivateDataFeaturesEXT)
#endif
deriving instance Show PhysicalDevicePrivateDataFeaturesEXT

instance ToCStruct PhysicalDevicePrivateDataFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePrivateDataFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (privateData))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePrivateDataFeaturesEXT where
  peekCStruct p = do
    privateData <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePrivateDataFeaturesEXT
             (bool32ToBool privateData)


instance Storable PhysicalDevicePrivateDataFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePrivateDataFeaturesEXT where
  zero = PhysicalDevicePrivateDataFeaturesEXT
           zero


type PrivateDataSlotCreateFlagsEXT = PrivateDataSlotCreateFlagBitsEXT

-- No documentation found for TopLevel "VkPrivateDataSlotCreateFlagBitsEXT"
newtype PrivateDataSlotCreateFlagBitsEXT = PrivateDataSlotCreateFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePrivateDataSlotCreateFlagBitsEXT :: String
conNamePrivateDataSlotCreateFlagBitsEXT = "PrivateDataSlotCreateFlagBitsEXT"

enumPrefixPrivateDataSlotCreateFlagBitsEXT :: String
enumPrefixPrivateDataSlotCreateFlagBitsEXT = ""

showTablePrivateDataSlotCreateFlagBitsEXT :: [(PrivateDataSlotCreateFlagBitsEXT, String)]
showTablePrivateDataSlotCreateFlagBitsEXT = []


instance Show PrivateDataSlotCreateFlagBitsEXT where
showsPrec = enumShowsPrec enumPrefixPrivateDataSlotCreateFlagBitsEXT
                          showTablePrivateDataSlotCreateFlagBitsEXT
                          conNamePrivateDataSlotCreateFlagBitsEXT
                          (\(PrivateDataSlotCreateFlagBitsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PrivateDataSlotCreateFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixPrivateDataSlotCreateFlagBitsEXT
                          showTablePrivateDataSlotCreateFlagBitsEXT
                          conNamePrivateDataSlotCreateFlagBitsEXT
                          PrivateDataSlotCreateFlagBitsEXT


type EXT_PRIVATE_DATA_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PRIVATE_DATA_SPEC_VERSION"
pattern EXT_PRIVATE_DATA_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PRIVATE_DATA_SPEC_VERSION = 1


type EXT_PRIVATE_DATA_EXTENSION_NAME = "VK_EXT_private_data"

-- No documentation found for TopLevel "VK_EXT_PRIVATE_DATA_EXTENSION_NAME"
pattern EXT_PRIVATE_DATA_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PRIVATE_DATA_EXTENSION_NAME = "VK_EXT_private_data"

