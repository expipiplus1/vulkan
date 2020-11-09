{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_tooling_info  ( getPhysicalDeviceToolPropertiesEXT
                                              , PhysicalDeviceToolPropertiesEXT(..)
                                              , ToolPurposeFlagBitsEXT( TOOL_PURPOSE_VALIDATION_BIT_EXT
                                                                      , TOOL_PURPOSE_PROFILING_BIT_EXT
                                                                      , TOOL_PURPOSE_TRACING_BIT_EXT
                                                                      , TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT
                                                                      , TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT
                                                                      , TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT
                                                                      , TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT
                                                                      , ..
                                                                      )
                                              , ToolPurposeFlagsEXT
                                              , EXT_TOOLING_INFO_SPEC_VERSION
                                              , pattern EXT_TOOLING_INFO_SPEC_VERSION
                                              , EXT_TOOLING_INFO_EXTENSION_NAME
                                              , pattern EXT_TOOLING_INFO_EXTENSION_NAME
                                              ) where

import Vulkan.CStruct.Utils (FixedArray)
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
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
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
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
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

-- | vkGetPhysicalDeviceToolPropertiesEXT - Reports properties of tools
-- active on the specified physical device
--
-- = Description
--
-- If @pToolProperties@ is @NULL@, then the number of tools currently
-- active on @physicalDevice@ is returned in @pToolCount@. Otherwise,
-- @pToolCount@ /must/ point to a variable set by the user to the number of
-- elements in the @pToolProperties@ array, and on return the variable is
-- overwritten with the number of structures actually written to
-- @pToolProperties@. If @pToolCount@ is less than the number of currently
-- active tools, at most @pToolCount@ structures will be written.
--
-- The count and properties of active tools /may/ change in response to
-- events outside the scope of the specification. An application /should/
-- assume these properties might change at any given time.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceToolPropertiesEXT-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceToolPropertiesEXT-pToolCount-parameter#
--     @pToolCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceToolPropertiesEXT-pToolProperties-parameter#
--     If the value referenced by @pToolCount@ is not @0@, and
--     @pToolProperties@ is not @NULL@, @pToolProperties@ /must/ be a valid
--     pointer to an array of @pToolCount@
--     'PhysicalDeviceToolPropertiesEXT' structures
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'PhysicalDeviceToolPropertiesEXT'
getPhysicalDeviceToolPropertiesEXT :: forall io
                                    . (MonadIO io)
                                   => -- | @physicalDevice@ is the handle to the physical device to query for
                                      -- active tools.
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


-- | VkPhysicalDeviceToolPropertiesEXT - Structure providing information
-- about an active tool
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'ToolPurposeFlagsEXT', 'getPhysicalDeviceToolPropertiesEXT'
data PhysicalDeviceToolPropertiesEXT = PhysicalDeviceToolPropertiesEXT
  { -- | @name@ is a null-terminated UTF-8 string containing the name of the
    -- tool.
    name :: ByteString
  , -- | @version@ is a null-terminated UTF-8 string containing the version of
    -- the tool.
    version :: ByteString
  , -- | @purposes@ is a bitmask of 'ToolPurposeFlagBitsEXT' which is populated
    -- with purposes supported by the tool.
    purposes :: ToolPurposeFlagsEXT
  , -- | @description@ is a null-terminated UTF-8 string containing a description
    -- of the tool.
    description :: ByteString
  , -- | @layer@ is a null-terminated UTF-8 string that contains the name of the
    -- layer implementing the tool, if the tool is implemented in a layer -
    -- otherwise it /may/ be an empty string.
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


-- | VkToolPurposeFlagBitsEXT - Bitmask specifying the purposes of an active
-- tool
--
-- = See Also
--
-- 'ToolPurposeFlagsEXT'
newtype ToolPurposeFlagBitsEXT = ToolPurposeFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'TOOL_PURPOSE_VALIDATION_BIT_EXT' specifies that the tool provides
-- validation of API usage.
pattern TOOL_PURPOSE_VALIDATION_BIT_EXT = ToolPurposeFlagBitsEXT 0x00000001
-- | 'TOOL_PURPOSE_PROFILING_BIT_EXT' specifies that the tool provides
-- profiling of API usage.
pattern TOOL_PURPOSE_PROFILING_BIT_EXT = ToolPurposeFlagBitsEXT 0x00000002
-- | 'TOOL_PURPOSE_TRACING_BIT_EXT' specifies that the tool is capturing data
-- about the application’s API usage, including anything from simple
-- logging to capturing data for later replay.
pattern TOOL_PURPOSE_TRACING_BIT_EXT = ToolPurposeFlagBitsEXT 0x00000004
-- | 'TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT' specifies that the tool
-- provides additional API features\/extensions on top of the underlying
-- implementation.
pattern TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT = ToolPurposeFlagBitsEXT 0x00000008
-- | 'TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT' specifies that the tool
-- modifies the API features\/limits\/extensions presented to the
-- application.
pattern TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT = ToolPurposeFlagBitsEXT 0x00000010
-- | 'TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT' specifies that the tool consumes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-debug-markers debug markers>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-object-debug-annotation object debug annotation>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-queue-labels queue labels>,
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-command-buffer-labels command buffer labels>
pattern TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT = ToolPurposeFlagBitsEXT 0x00000040
-- | 'TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT' specifies that the tool reports
-- additional information to the application via callbacks specified by
-- 'Vulkan.Extensions.VK_EXT_debug_report.createDebugReportCallbackEXT' or
-- 'Vulkan.Extensions.VK_EXT_debug_utils.createDebugUtilsMessengerEXT'
pattern TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT = ToolPurposeFlagBitsEXT 0x00000020

type ToolPurposeFlagsEXT = ToolPurposeFlagBitsEXT

instance Show ToolPurposeFlagBitsEXT where
  showsPrec p = \case
    TOOL_PURPOSE_VALIDATION_BIT_EXT -> showString "TOOL_PURPOSE_VALIDATION_BIT_EXT"
    TOOL_PURPOSE_PROFILING_BIT_EXT -> showString "TOOL_PURPOSE_PROFILING_BIT_EXT"
    TOOL_PURPOSE_TRACING_BIT_EXT -> showString "TOOL_PURPOSE_TRACING_BIT_EXT"
    TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT -> showString "TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT"
    TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT -> showString "TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT"
    TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT -> showString "TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT"
    TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT -> showString "TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT"
    ToolPurposeFlagBitsEXT x -> showParen (p >= 11) (showString "ToolPurposeFlagBitsEXT 0x" . showHex x)

instance Read ToolPurposeFlagBitsEXT where
  readPrec = parens (choose [("TOOL_PURPOSE_VALIDATION_BIT_EXT", pure TOOL_PURPOSE_VALIDATION_BIT_EXT)
                            , ("TOOL_PURPOSE_PROFILING_BIT_EXT", pure TOOL_PURPOSE_PROFILING_BIT_EXT)
                            , ("TOOL_PURPOSE_TRACING_BIT_EXT", pure TOOL_PURPOSE_TRACING_BIT_EXT)
                            , ("TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT", pure TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT)
                            , ("TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT", pure TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT)
                            , ("TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT", pure TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT)
                            , ("TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT", pure TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ToolPurposeFlagBitsEXT")
                       v <- step readPrec
                       pure (ToolPurposeFlagBitsEXT v)))


type EXT_TOOLING_INFO_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TOOLING_INFO_SPEC_VERSION"
pattern EXT_TOOLING_INFO_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TOOLING_INFO_SPEC_VERSION = 1


type EXT_TOOLING_INFO_EXTENSION_NAME = "VK_EXT_tooling_info"

-- No documentation found for TopLevel "VK_EXT_TOOLING_INFO_EXTENSION_NAME"
pattern EXT_TOOLING_INFO_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TOOLING_INFO_EXTENSION_NAME = "VK_EXT_tooling_info"

