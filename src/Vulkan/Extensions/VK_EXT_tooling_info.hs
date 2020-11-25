{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_tooling_info"
module Vulkan.Extensions.VK_EXT_tooling_info  ( getPhysicalDeviceToolPropertiesEXT
                                              , PhysicalDeviceToolPropertiesEXT(..)
                                              , ToolPurposeFlagsEXT
                                              , ToolPurposeFlagBitsEXT( TOOL_PURPOSE_VALIDATION_BIT_EXT
                                                                      , TOOL_PURPOSE_PROFILING_BIT_EXT
                                                                      , TOOL_PURPOSE_TRACING_BIT_EXT
                                                                      , TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT
                                                                      , TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT
                                                                      , TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT
                                                                      , TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT
                                                                      , ..
                                                                      )
                                              , EXT_TOOLING_INFO_SPEC_VERSION
                                              , pattern EXT_TOOLING_INFO_SPEC_VERSION
                                              , EXT_TOOLING_INFO_EXTENSION_NAME
                                              , pattern EXT_TOOLING_INFO_EXTENSION_NAME
                                              ) where

import Vulkan.CStruct.Utils (FixedArray)
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
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
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
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceToolPropertiesEXT))
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Vulkan.Core10.APIConstants (MAX_EXTENSION_NAME_SIZE)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceToolPropertiesEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr PhysicalDeviceToolPropertiesEXT -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr PhysicalDeviceToolPropertiesEXT -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceToolPropertiesEXT"
getPhysicalDeviceToolPropertiesEXT :: forall io
                                    . (MonadIO io)
                                   => -- No documentation found for Nested "vkGetPhysicalDeviceToolPropertiesEXT" "physicalDevice"
                                      PhysicalDevice
                                   -> io (Result, ("toolProperties" ::: Vector PhysicalDeviceToolPropertiesEXT))
getPhysicalDeviceToolPropertiesEXT physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceToolPropertiesEXTPtr = pVkGetPhysicalDeviceToolPropertiesEXT (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceToolPropertiesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceToolPropertiesEXT is null" Nothing Nothing
  let vkGetPhysicalDeviceToolPropertiesEXT' = mkVkGetPhysicalDeviceToolPropertiesEXT vkGetPhysicalDeviceToolPropertiesEXTPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPToolCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceToolPropertiesEXT' physicalDevice' (pPToolCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pToolCount <- lift $ peek @Word32 pPToolCount
  pPToolProperties <- ContT $ bracket (callocBytes @PhysicalDeviceToolPropertiesEXT ((fromIntegral (pToolCount)) * 1048)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPToolProperties `advancePtrBytes` (i * 1048) :: Ptr PhysicalDeviceToolPropertiesEXT) . ($ ())) [0..(fromIntegral (pToolCount)) - 1]
  r' <- lift $ vkGetPhysicalDeviceToolPropertiesEXT' physicalDevice' (pPToolCount) ((pPToolProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pToolCount' <- lift $ peek @Word32 pPToolCount
  pToolProperties' <- lift $ generateM (fromIntegral (pToolCount')) (\i -> peekCStruct @PhysicalDeviceToolPropertiesEXT (((pPToolProperties) `advancePtrBytes` (1048 * (i)) :: Ptr PhysicalDeviceToolPropertiesEXT)))
  pure $ ((r'), pToolProperties')



-- No documentation found for TopLevel "VkPhysicalDeviceToolPropertiesEXT"
data PhysicalDeviceToolPropertiesEXT = PhysicalDeviceToolPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceToolPropertiesEXT" "name"
    name :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceToolPropertiesEXT" "version"
    version :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceToolPropertiesEXT" "purposes"
    purposes :: ToolPurposeFlagsEXT
  , -- No documentation found for Nested "VkPhysicalDeviceToolPropertiesEXT" "description"
    description :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceToolPropertiesEXT" "layer"
    layer :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceToolPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceToolPropertiesEXT

instance ToCStruct PhysicalDeviceToolPropertiesEXT where
  withCStruct x f = allocaBytesAligned 1048 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceToolPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (name)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (version)
    poke ((p `plusPtr` 528 :: Ptr ToolPurposeFlagsEXT)) (purposes)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 532 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 788 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (layer)
    f
  cStructSize = 1048
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 528 :: Ptr ToolPurposeFlagsEXT)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 532 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 788 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (mempty)
    f

instance FromCStruct PhysicalDeviceToolPropertiesEXT where
  peekCStruct p = do
    name <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))))
    version <- packCString (lowerArrayPtr ((p `plusPtr` 272 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))))
    purposes <- peek @ToolPurposeFlagsEXT ((p `plusPtr` 528 :: Ptr ToolPurposeFlagsEXT))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 532 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    layer <- packCString (lowerArrayPtr ((p `plusPtr` 788 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))))
    pure $ PhysicalDeviceToolPropertiesEXT
             name version purposes description layer


instance Storable PhysicalDeviceToolPropertiesEXT where
  sizeOf ~_ = 1048
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceToolPropertiesEXT where
  zero = PhysicalDeviceToolPropertiesEXT
           mempty
           mempty
           zero
           mempty
           mempty


type ToolPurposeFlagsEXT = ToolPurposeFlagBitsEXT

-- No documentation found for TopLevel "VkToolPurposeFlagBitsEXT"
newtype ToolPurposeFlagBitsEXT = ToolPurposeFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkToolPurposeFlagBitsEXT" "VK_TOOL_PURPOSE_VALIDATION_BIT_EXT"
pattern TOOL_PURPOSE_VALIDATION_BIT_EXT          = ToolPurposeFlagBitsEXT 0x00000001
-- No documentation found for Nested "VkToolPurposeFlagBitsEXT" "VK_TOOL_PURPOSE_PROFILING_BIT_EXT"
pattern TOOL_PURPOSE_PROFILING_BIT_EXT           = ToolPurposeFlagBitsEXT 0x00000002
-- No documentation found for Nested "VkToolPurposeFlagBitsEXT" "VK_TOOL_PURPOSE_TRACING_BIT_EXT"
pattern TOOL_PURPOSE_TRACING_BIT_EXT             = ToolPurposeFlagBitsEXT 0x00000004
-- No documentation found for Nested "VkToolPurposeFlagBitsEXT" "VK_TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT"
pattern TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT = ToolPurposeFlagBitsEXT 0x00000008
-- No documentation found for Nested "VkToolPurposeFlagBitsEXT" "VK_TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT"
pattern TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT  = ToolPurposeFlagBitsEXT 0x00000010
-- No documentation found for Nested "VkToolPurposeFlagBitsEXT" "VK_TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT"
pattern TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT       = ToolPurposeFlagBitsEXT 0x00000040
-- No documentation found for Nested "VkToolPurposeFlagBitsEXT" "VK_TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT"
pattern TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT     = ToolPurposeFlagBitsEXT 0x00000020

conNameToolPurposeFlagBitsEXT :: String
conNameToolPurposeFlagBitsEXT = "ToolPurposeFlagBitsEXT"

enumPrefixToolPurposeFlagBitsEXT :: String
enumPrefixToolPurposeFlagBitsEXT = "TOOL_PURPOSE_"

showTableToolPurposeFlagBitsEXT :: [(ToolPurposeFlagBitsEXT, String)]
showTableToolPurposeFlagBitsEXT =
  [ (TOOL_PURPOSE_VALIDATION_BIT_EXT         , "VALIDATION_BIT_EXT")
  , (TOOL_PURPOSE_PROFILING_BIT_EXT          , "PROFILING_BIT_EXT")
  , (TOOL_PURPOSE_TRACING_BIT_EXT            , "TRACING_BIT_EXT")
  , (TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT, "ADDITIONAL_FEATURES_BIT_EXT")
  , (TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT , "MODIFYING_FEATURES_BIT_EXT")
  , (TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT      , "DEBUG_MARKERS_BIT_EXT")
  , (TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT    , "DEBUG_REPORTING_BIT_EXT")
  ]


instance Show ToolPurposeFlagBitsEXT where
showsPrec = enumShowsPrec enumPrefixToolPurposeFlagBitsEXT
                          showTableToolPurposeFlagBitsEXT
                          conNameToolPurposeFlagBitsEXT
                          (\(ToolPurposeFlagBitsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ToolPurposeFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixToolPurposeFlagBitsEXT
                          showTableToolPurposeFlagBitsEXT
                          conNameToolPurposeFlagBitsEXT
                          ToolPurposeFlagBitsEXT


type EXT_TOOLING_INFO_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TOOLING_INFO_SPEC_VERSION"
pattern EXT_TOOLING_INFO_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TOOLING_INFO_SPEC_VERSION = 1


type EXT_TOOLING_INFO_EXTENSION_NAME = "VK_EXT_tooling_info"

-- No documentation found for TopLevel "VK_EXT_TOOLING_INFO_EXTENSION_NAME"
pattern EXT_TOOLING_INFO_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TOOLING_INFO_EXTENSION_NAME = "VK_EXT_tooling_info"

