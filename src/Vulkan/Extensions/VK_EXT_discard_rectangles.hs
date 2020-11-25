{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_discard_rectangles"
module Vulkan.Extensions.VK_EXT_discard_rectangles  ( cmdSetDiscardRectangleEXT
                                                    , PhysicalDeviceDiscardRectanglePropertiesEXT(..)
                                                    , PipelineDiscardRectangleStateCreateInfoEXT(..)
                                                    , PipelineDiscardRectangleStateCreateFlagsEXT(..)
                                                    , DiscardRectangleModeEXT( DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT
                                                                             , DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT
                                                                             , ..
                                                                             )
                                                    , EXT_DISCARD_RECTANGLES_SPEC_VERSION
                                                    , pattern EXT_DISCARD_RECTANGLES_SPEC_VERSION
                                                    , EXT_DISCARD_RECTANGLES_EXTENSION_NAME
                                                    , pattern EXT_DISCARD_RECTANGLES_EXTENSION_NAME
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
import GHC.Show (showsPrec)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDiscardRectangleEXT))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDiscardRectangleEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()

-- No documentation found for TopLevel "vkCmdSetDiscardRectangleEXT"
cmdSetDiscardRectangleEXT :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkCmdSetDiscardRectangleEXT" "commandBuffer"
                             CommandBuffer
                          -> -- No documentation found for Nested "vkCmdSetDiscardRectangleEXT" "firstDiscardRectangle"
                             ("firstDiscardRectangle" ::: Word32)
                          -> -- No documentation found for Nested "vkCmdSetDiscardRectangleEXT" "pDiscardRectangles"
                             ("discardRectangles" ::: Vector Rect2D)
                          -> io ()
cmdSetDiscardRectangleEXT commandBuffer firstDiscardRectangle discardRectangles = liftIO . evalContT $ do
  let vkCmdSetDiscardRectangleEXTPtr = pVkCmdSetDiscardRectangleEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetDiscardRectangleEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDiscardRectangleEXT is null" Nothing Nothing
  let vkCmdSetDiscardRectangleEXT' = mkVkCmdSetDiscardRectangleEXT vkCmdSetDiscardRectangleEXTPtr
  pPDiscardRectangles <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (discardRectangles)) * 16) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDiscardRectangles `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (discardRectangles)
  lift $ vkCmdSetDiscardRectangleEXT' (commandBufferHandle (commandBuffer)) (firstDiscardRectangle) ((fromIntegral (Data.Vector.length $ (discardRectangles)) :: Word32)) (pPDiscardRectangles)
  pure $ ()



-- No documentation found for TopLevel "VkPhysicalDeviceDiscardRectanglePropertiesEXT"
data PhysicalDeviceDiscardRectanglePropertiesEXT = PhysicalDeviceDiscardRectanglePropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceDiscardRectanglePropertiesEXT" "maxDiscardRectangles"
    maxDiscardRectangles :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDiscardRectanglePropertiesEXT)
#endif
deriving instance Show PhysicalDeviceDiscardRectanglePropertiesEXT

instance ToCStruct PhysicalDeviceDiscardRectanglePropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDiscardRectanglePropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxDiscardRectangles)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceDiscardRectanglePropertiesEXT where
  peekCStruct p = do
    maxDiscardRectangles <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceDiscardRectanglePropertiesEXT
             maxDiscardRectangles


instance Storable PhysicalDeviceDiscardRectanglePropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDiscardRectanglePropertiesEXT where
  zero = PhysicalDeviceDiscardRectanglePropertiesEXT
           zero



-- No documentation found for TopLevel "VkPipelineDiscardRectangleStateCreateInfoEXT"
data PipelineDiscardRectangleStateCreateInfoEXT = PipelineDiscardRectangleStateCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineDiscardRectangleStateCreateInfoEXT" "flags"
    flags :: PipelineDiscardRectangleStateCreateFlagsEXT
  , -- No documentation found for Nested "VkPipelineDiscardRectangleStateCreateInfoEXT" "discardRectangleMode"
    discardRectangleMode :: DiscardRectangleModeEXT
  , -- No documentation found for Nested "VkPipelineDiscardRectangleStateCreateInfoEXT" "pDiscardRectangles"
    discardRectangles :: Vector Rect2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineDiscardRectangleStateCreateInfoEXT)
#endif
deriving instance Show PipelineDiscardRectangleStateCreateInfoEXT

instance ToCStruct PipelineDiscardRectangleStateCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineDiscardRectangleStateCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineDiscardRectangleStateCreateFlagsEXT)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr DiscardRectangleModeEXT)) (discardRectangleMode)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (discardRectangles)) :: Word32))
    pPDiscardRectangles' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (discardRectangles)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDiscardRectangles' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (discardRectangles)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Rect2D))) (pPDiscardRectangles')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 20 :: Ptr DiscardRectangleModeEXT)) (zero)
    pPDiscardRectangles' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (mempty)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDiscardRectangles' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Rect2D))) (pPDiscardRectangles')
    lift $ f

instance FromCStruct PipelineDiscardRectangleStateCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @PipelineDiscardRectangleStateCreateFlagsEXT ((p `plusPtr` 16 :: Ptr PipelineDiscardRectangleStateCreateFlagsEXT))
    discardRectangleMode <- peek @DiscardRectangleModeEXT ((p `plusPtr` 20 :: Ptr DiscardRectangleModeEXT))
    discardRectangleCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pDiscardRectangles <- peek @(Ptr Rect2D) ((p `plusPtr` 32 :: Ptr (Ptr Rect2D)))
    pDiscardRectangles' <- generateM (fromIntegral discardRectangleCount) (\i -> peekCStruct @Rect2D ((pDiscardRectangles `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
    pure $ PipelineDiscardRectangleStateCreateInfoEXT
             flags discardRectangleMode pDiscardRectangles'

instance Zero PipelineDiscardRectangleStateCreateInfoEXT where
  zero = PipelineDiscardRectangleStateCreateInfoEXT
           zero
           zero
           mempty


-- No documentation found for TopLevel "VkPipelineDiscardRectangleStateCreateFlagsEXT"
newtype PipelineDiscardRectangleStateCreateFlagsEXT = PipelineDiscardRectangleStateCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineDiscardRectangleStateCreateFlagsEXT :: String
conNamePipelineDiscardRectangleStateCreateFlagsEXT = "PipelineDiscardRectangleStateCreateFlagsEXT"

enumPrefixPipelineDiscardRectangleStateCreateFlagsEXT :: String
enumPrefixPipelineDiscardRectangleStateCreateFlagsEXT = ""

showTablePipelineDiscardRectangleStateCreateFlagsEXT :: [(PipelineDiscardRectangleStateCreateFlagsEXT, String)]
showTablePipelineDiscardRectangleStateCreateFlagsEXT = []


instance Show PipelineDiscardRectangleStateCreateFlagsEXT where
showsPrec = enumShowsPrec enumPrefixPipelineDiscardRectangleStateCreateFlagsEXT
                          showTablePipelineDiscardRectangleStateCreateFlagsEXT
                          conNamePipelineDiscardRectangleStateCreateFlagsEXT
                          (\(PipelineDiscardRectangleStateCreateFlagsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineDiscardRectangleStateCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixPipelineDiscardRectangleStateCreateFlagsEXT
                          showTablePipelineDiscardRectangleStateCreateFlagsEXT
                          conNamePipelineDiscardRectangleStateCreateFlagsEXT
                          PipelineDiscardRectangleStateCreateFlagsEXT


-- No documentation found for TopLevel "VkDiscardRectangleModeEXT"
newtype DiscardRectangleModeEXT = DiscardRectangleModeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDiscardRectangleModeEXT" "VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT"
pattern DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT = DiscardRectangleModeEXT 0
-- No documentation found for Nested "VkDiscardRectangleModeEXT" "VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT"
pattern DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT = DiscardRectangleModeEXT 1
{-# complete DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT,
             DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT :: DiscardRectangleModeEXT #-}

conNameDiscardRectangleModeEXT :: String
conNameDiscardRectangleModeEXT = "DiscardRectangleModeEXT"

enumPrefixDiscardRectangleModeEXT :: String
enumPrefixDiscardRectangleModeEXT = "DISCARD_RECTANGLE_MODE_"

showTableDiscardRectangleModeEXT :: [(DiscardRectangleModeEXT, String)]
showTableDiscardRectangleModeEXT =
  [(DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT, "INCLUSIVE_EXT"), (DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT, "EXCLUSIVE_EXT")]


instance Show DiscardRectangleModeEXT where
showsPrec = enumShowsPrec enumPrefixDiscardRectangleModeEXT
                          showTableDiscardRectangleModeEXT
                          conNameDiscardRectangleModeEXT
                          (\(DiscardRectangleModeEXT x) -> x)
                          (showsPrec 11)


instance Read DiscardRectangleModeEXT where
  readPrec = enumReadPrec enumPrefixDiscardRectangleModeEXT
                          showTableDiscardRectangleModeEXT
                          conNameDiscardRectangleModeEXT
                          DiscardRectangleModeEXT


type EXT_DISCARD_RECTANGLES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION"
pattern EXT_DISCARD_RECTANGLES_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DISCARD_RECTANGLES_SPEC_VERSION = 1


type EXT_DISCARD_RECTANGLES_EXTENSION_NAME = "VK_EXT_discard_rectangles"

-- No documentation found for TopLevel "VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME"
pattern EXT_DISCARD_RECTANGLES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DISCARD_RECTANGLES_EXTENSION_NAME = "VK_EXT_discard_rectangles"

