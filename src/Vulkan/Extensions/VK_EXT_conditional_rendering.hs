{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_conditional_rendering"
module Vulkan.Extensions.VK_EXT_conditional_rendering  ( cmdBeginConditionalRenderingEXT
                                                       , cmdUseConditionalRenderingEXT
                                                       , cmdEndConditionalRenderingEXT
                                                       , ConditionalRenderingBeginInfoEXT(..)
                                                       , CommandBufferInheritanceConditionalRenderingInfoEXT(..)
                                                       , PhysicalDeviceConditionalRenderingFeaturesEXT(..)
                                                       , ConditionalRenderingFlagsEXT
                                                       , ConditionalRenderingFlagBitsEXT( CONDITIONAL_RENDERING_INVERTED_BIT_EXT
                                                                                        , ..
                                                                                        )
                                                       , EXT_CONDITIONAL_RENDERING_SPEC_VERSION
                                                       , pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION
                                                       , EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
                                                       , pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
                                                       ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginConditionalRenderingEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndConditionalRenderingEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginConditionalRenderingEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr ConditionalRenderingBeginInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr ConditionalRenderingBeginInfoEXT -> IO ()

-- No documentation found for TopLevel "vkCmdBeginConditionalRenderingEXT"
cmdBeginConditionalRenderingEXT :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkCmdBeginConditionalRenderingEXT" "commandBuffer"
                                   CommandBuffer
                                -> -- No documentation found for Nested "vkCmdBeginConditionalRenderingEXT" "pConditionalRenderingBegin"
                                   ConditionalRenderingBeginInfoEXT
                                -> io ()
cmdBeginConditionalRenderingEXT commandBuffer conditionalRenderingBegin = liftIO . evalContT $ do
  let vkCmdBeginConditionalRenderingEXTPtr = pVkCmdBeginConditionalRenderingEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBeginConditionalRenderingEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginConditionalRenderingEXT is null" Nothing Nothing
  let vkCmdBeginConditionalRenderingEXT' = mkVkCmdBeginConditionalRenderingEXT vkCmdBeginConditionalRenderingEXTPtr
  pConditionalRenderingBegin <- ContT $ withCStruct (conditionalRenderingBegin)
  lift $ vkCmdBeginConditionalRenderingEXT' (commandBufferHandle (commandBuffer)) pConditionalRenderingBegin
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginConditionalRenderingEXT' and 'cmdEndConditionalRenderingEXT'
--
-- Note that 'cmdEndConditionalRenderingEXT' is *not* called if an
-- exception is thrown by the inner action.
cmdUseConditionalRenderingEXT :: forall io r . MonadIO io => CommandBuffer -> ConditionalRenderingBeginInfoEXT -> io r -> io r
cmdUseConditionalRenderingEXT commandBuffer pConditionalRenderingBegin a =
  (cmdBeginConditionalRenderingEXT commandBuffer pConditionalRenderingBegin) *> a <* (cmdEndConditionalRenderingEXT commandBuffer)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndConditionalRenderingEXT
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- No documentation found for TopLevel "vkCmdEndConditionalRenderingEXT"
cmdEndConditionalRenderingEXT :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkCmdEndConditionalRenderingEXT" "commandBuffer"
                                 CommandBuffer
                              -> io ()
cmdEndConditionalRenderingEXT commandBuffer = liftIO $ do
  let vkCmdEndConditionalRenderingEXTPtr = pVkCmdEndConditionalRenderingEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdEndConditionalRenderingEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndConditionalRenderingEXT is null" Nothing Nothing
  let vkCmdEndConditionalRenderingEXT' = mkVkCmdEndConditionalRenderingEXT vkCmdEndConditionalRenderingEXTPtr
  vkCmdEndConditionalRenderingEXT' (commandBufferHandle (commandBuffer))
  pure $ ()



-- No documentation found for TopLevel "VkConditionalRenderingBeginInfoEXT"
data ConditionalRenderingBeginInfoEXT = ConditionalRenderingBeginInfoEXT
  { -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "buffer"
    buffer :: Buffer
  , -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "offset"
    offset :: DeviceSize
  , -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "flags"
    flags :: ConditionalRenderingFlagsEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ConditionalRenderingBeginInfoEXT)
#endif
deriving instance Show ConditionalRenderingBeginInfoEXT

instance ToCStruct ConditionalRenderingBeginInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ConditionalRenderingBeginInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 32 :: Ptr ConditionalRenderingFlagsEXT)) (flags)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct ConditionalRenderingBeginInfoEXT where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    flags <- peek @ConditionalRenderingFlagsEXT ((p `plusPtr` 32 :: Ptr ConditionalRenderingFlagsEXT))
    pure $ ConditionalRenderingBeginInfoEXT
             buffer offset flags


instance Storable ConditionalRenderingBeginInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ConditionalRenderingBeginInfoEXT where
  zero = ConditionalRenderingBeginInfoEXT
           zero
           zero
           zero



-- No documentation found for TopLevel "VkCommandBufferInheritanceConditionalRenderingInfoEXT"
data CommandBufferInheritanceConditionalRenderingInfoEXT = CommandBufferInheritanceConditionalRenderingInfoEXT
  { -- No documentation found for Nested "VkCommandBufferInheritanceConditionalRenderingInfoEXT" "conditionalRenderingEnable"
    conditionalRenderingEnable :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandBufferInheritanceConditionalRenderingInfoEXT)
#endif
deriving instance Show CommandBufferInheritanceConditionalRenderingInfoEXT

instance ToCStruct CommandBufferInheritanceConditionalRenderingInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferInheritanceConditionalRenderingInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (conditionalRenderingEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct CommandBufferInheritanceConditionalRenderingInfoEXT where
  peekCStruct p = do
    conditionalRenderingEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ CommandBufferInheritanceConditionalRenderingInfoEXT
             (bool32ToBool conditionalRenderingEnable)


instance Storable CommandBufferInheritanceConditionalRenderingInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CommandBufferInheritanceConditionalRenderingInfoEXT where
  zero = CommandBufferInheritanceConditionalRenderingInfoEXT
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceConditionalRenderingFeaturesEXT"
data PhysicalDeviceConditionalRenderingFeaturesEXT = PhysicalDeviceConditionalRenderingFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceConditionalRenderingFeaturesEXT" "conditionalRendering"
    conditionalRendering :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceConditionalRenderingFeaturesEXT" "inheritedConditionalRendering"
    inheritedConditionalRendering :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceConditionalRenderingFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceConditionalRenderingFeaturesEXT

instance ToCStruct PhysicalDeviceConditionalRenderingFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceConditionalRenderingFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (conditionalRendering))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (inheritedConditionalRendering))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceConditionalRenderingFeaturesEXT where
  peekCStruct p = do
    conditionalRendering <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    inheritedConditionalRendering <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceConditionalRenderingFeaturesEXT
             (bool32ToBool conditionalRendering) (bool32ToBool inheritedConditionalRendering)


instance Storable PhysicalDeviceConditionalRenderingFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceConditionalRenderingFeaturesEXT where
  zero = PhysicalDeviceConditionalRenderingFeaturesEXT
           zero
           zero


type ConditionalRenderingFlagsEXT = ConditionalRenderingFlagBitsEXT

-- No documentation found for TopLevel "VkConditionalRenderingFlagBitsEXT"
newtype ConditionalRenderingFlagBitsEXT = ConditionalRenderingFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkConditionalRenderingFlagBitsEXT" "VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT"
pattern CONDITIONAL_RENDERING_INVERTED_BIT_EXT = ConditionalRenderingFlagBitsEXT 0x00000001

conNameConditionalRenderingFlagBitsEXT :: String
conNameConditionalRenderingFlagBitsEXT = "ConditionalRenderingFlagBitsEXT"

enumPrefixConditionalRenderingFlagBitsEXT :: String
enumPrefixConditionalRenderingFlagBitsEXT = "CONDITIONAL_RENDERING_INVERTED_BIT_EXT"

showTableConditionalRenderingFlagBitsEXT :: [(ConditionalRenderingFlagBitsEXT, String)]
showTableConditionalRenderingFlagBitsEXT = [(CONDITIONAL_RENDERING_INVERTED_BIT_EXT, "")]


instance Show ConditionalRenderingFlagBitsEXT where
showsPrec = enumShowsPrec enumPrefixConditionalRenderingFlagBitsEXT
                          showTableConditionalRenderingFlagBitsEXT
                          conNameConditionalRenderingFlagBitsEXT
                          (\(ConditionalRenderingFlagBitsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ConditionalRenderingFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixConditionalRenderingFlagBitsEXT
                          showTableConditionalRenderingFlagBitsEXT
                          conNameConditionalRenderingFlagBitsEXT
                          ConditionalRenderingFlagBitsEXT


type EXT_CONDITIONAL_RENDERING_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION"
pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION = 2


type EXT_CONDITIONAL_RENDERING_EXTENSION_NAME = "VK_EXT_conditional_rendering"

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME"
pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME = "VK_EXT_conditional_rendering"

