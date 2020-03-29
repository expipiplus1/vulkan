{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata  ( setHdrMetadataEXT
                                                       , XYColorEXT(..)
                                                       , HdrMetadataEXT(..)
                                                       , EXT_HDR_METADATA_SPEC_VERSION
                                                       , pattern EXT_HDR_METADATA_SPEC_VERSION
                                                       , EXT_HDR_METADATA_EXTENSION_NAME
                                                       , pattern EXT_HDR_METADATA_EXTENSION_NAME
                                                       , SwapchainKHR(..)
                                                       ) where

import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkSetHdrMetadataEXT))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.Extensions.Handles (SwapchainKHR)
import Graphics.Vulkan.Extensions.Handles (SwapchainKHR(..))
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_HDR_METADATA_EXT))
import Graphics.Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetHdrMetadataEXT
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr SwapchainKHR -> Ptr HdrMetadataEXT -> IO ()) -> Ptr Device_T -> Word32 -> Ptr SwapchainKHR -> Ptr HdrMetadataEXT -> IO ()

-- | vkSetHdrMetadataEXT - function to set Hdr metadata
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device where
--     the swapchain(s) were created.
--
-- -   @swapchainCount@ is the number of swapchains included in
--     @pSwapchains@.
--
-- -   @pSwapchains@ is a pointer to an array of @swapchainCount@
--     'Graphics.Vulkan.Extensions.Handles.SwapchainKHR' handles.
--
-- -   @pMetadata@ is a pointer to an array of @swapchainCount@
--     'HdrMetadataEXT' structures.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   @pSwapchains@ /must/ be a valid pointer to an array of
--     @swapchainCount@ valid
--     'Graphics.Vulkan.Extensions.Handles.SwapchainKHR' handles
--
-- -   @pMetadata@ /must/ be a valid pointer to an array of
--     @swapchainCount@ valid 'HdrMetadataEXT' structures
--
-- -   @swapchainCount@ /must/ be greater than @0@
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.Device', and the elements of
--     @pSwapchains@ /must/ have been created, allocated, or retrieved from
--     the same 'Graphics.Vulkan.Core10.Handles.Instance'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device', 'HdrMetadataEXT',
-- 'Graphics.Vulkan.Extensions.Handles.SwapchainKHR'
setHdrMetadataEXT :: Device -> ("swapchains" ::: Vector SwapchainKHR) -> ("metadata" ::: Vector HdrMetadataEXT) -> IO ()
setHdrMetadataEXT device swapchains metadata = evalContT $ do
  let vkSetHdrMetadataEXT' = mkVkSetHdrMetadataEXT (pVkSetHdrMetadataEXT (deviceCmds (device :: Device)))
  let pSwapchainsLength = Data.Vector.length $ (swapchains)
  let pMetadataLength = Data.Vector.length $ (metadata)
  lift $ unless (pMetadataLength == pSwapchainsLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pMetadata and pSwapchains must have the same length" Nothing Nothing
  pPSwapchains <- ContT $ allocaBytesAligned @SwapchainKHR ((Data.Vector.length (swapchains)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPSwapchains `plusPtr` (8 * (i)) :: Ptr SwapchainKHR) (e)) (swapchains)
  pPMetadata <- ContT $ allocaBytesAligned @HdrMetadataEXT ((Data.Vector.length (metadata)) * 64) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPMetadata `plusPtr` (64 * (i)) :: Ptr HdrMetadataEXT) (e) . ($ ())) (metadata)
  lift $ vkSetHdrMetadataEXT' (deviceHandle (device)) ((fromIntegral pSwapchainsLength :: Word32)) (pPSwapchains) (pPMetadata)
  pure $ ()


-- | VkXYColorEXT - structure to specify X,Y chromaticity coordinates
--
-- = See Also
--
-- 'HdrMetadataEXT'
data XYColorEXT = XYColorEXT
  { -- No documentation found for Nested "VkXYColorEXT" "x"
    x :: Float
  , -- No documentation found for Nested "VkXYColorEXT" "y"
    y :: Float
  }
  deriving (Typeable)
deriving instance Show XYColorEXT

instance ToCStruct XYColorEXT where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p XYColorEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (x))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (y))
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct XYColorEXT where
  peekCStruct p = do
    x <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    y <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    pure $ XYColorEXT
             ((\(CFloat a) -> a) x) ((\(CFloat a) -> a) y)

instance Storable XYColorEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero XYColorEXT where
  zero = XYColorEXT
           zero
           zero


-- | VkHdrMetadataEXT - structure to specify Hdr metadata
--
-- == Valid Usage (Implicit)
--
-- Note
--
-- The validity and use of this data is outside the scope of Vulkan.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'XYColorEXT', 'setHdrMetadataEXT'
data HdrMetadataEXT = HdrMetadataEXT
  { -- | @displayPrimaryRed@ is the mastering display’s red primary in
    -- chromaticity coordinates
    displayPrimaryRed :: XYColorEXT
  , -- | @displayPrimaryGreen@ is the mastering display’s green primary in
    -- chromaticity coordinates
    displayPrimaryGreen :: XYColorEXT
  , -- | @displayPrimaryBlue@ is the mastering display’s blue primary in
    -- chromaticity coordinates
    displayPrimaryBlue :: XYColorEXT
  , -- | @whitePoint@ is the mastering display’s white-point in chromaticity
    -- coordinates
    whitePoint :: XYColorEXT
  , -- | @maxLuminance@ is the maximum luminance of the mastering display in nits
    maxLuminance :: Float
  , -- | @minLuminance@ is the minimum luminance of the mastering display in nits
    minLuminance :: Float
  , -- | @maxContentLightLevel@ is content’s maximum luminance in nits
    maxContentLightLevel :: Float
  , -- | @maxFrameAverageLightLevel@ is the maximum frame average light level in
    -- nits
    maxFrameAverageLightLevel :: Float
  }
  deriving (Typeable)
deriving instance Show HdrMetadataEXT

instance ToCStruct HdrMetadataEXT where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HdrMetadataEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HDR_METADATA_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr XYColorEXT)) (displayPrimaryRed) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr XYColorEXT)) (displayPrimaryGreen) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 32 :: Ptr XYColorEXT)) (displayPrimaryBlue) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr XYColorEXT)) (whitePoint) . ($ ())
    lift $ poke ((p `plusPtr` 48 :: Ptr CFloat)) (CFloat (maxLuminance))
    lift $ poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (minLuminance))
    lift $ poke ((p `plusPtr` 56 :: Ptr CFloat)) (CFloat (maxContentLightLevel))
    lift $ poke ((p `plusPtr` 60 :: Ptr CFloat)) (CFloat (maxFrameAverageLightLevel))
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HDR_METADATA_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr XYColorEXT)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr XYColorEXT)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 32 :: Ptr XYColorEXT)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr XYColorEXT)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 48 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 56 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 60 :: Ptr CFloat)) (CFloat (zero))
    lift $ f

instance FromCStruct HdrMetadataEXT where
  peekCStruct p = do
    displayPrimaryRed <- peekCStruct @XYColorEXT ((p `plusPtr` 16 :: Ptr XYColorEXT))
    displayPrimaryGreen <- peekCStruct @XYColorEXT ((p `plusPtr` 24 :: Ptr XYColorEXT))
    displayPrimaryBlue <- peekCStruct @XYColorEXT ((p `plusPtr` 32 :: Ptr XYColorEXT))
    whitePoint <- peekCStruct @XYColorEXT ((p `plusPtr` 40 :: Ptr XYColorEXT))
    maxLuminance <- peek @CFloat ((p `plusPtr` 48 :: Ptr CFloat))
    minLuminance <- peek @CFloat ((p `plusPtr` 52 :: Ptr CFloat))
    maxContentLightLevel <- peek @CFloat ((p `plusPtr` 56 :: Ptr CFloat))
    maxFrameAverageLightLevel <- peek @CFloat ((p `plusPtr` 60 :: Ptr CFloat))
    pure $ HdrMetadataEXT
             displayPrimaryRed displayPrimaryGreen displayPrimaryBlue whitePoint ((\(CFloat a) -> a) maxLuminance) ((\(CFloat a) -> a) minLuminance) ((\(CFloat a) -> a) maxContentLightLevel) ((\(CFloat a) -> a) maxFrameAverageLightLevel)

instance Zero HdrMetadataEXT where
  zero = HdrMetadataEXT
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


type EXT_HDR_METADATA_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_HDR_METADATA_SPEC_VERSION"
pattern EXT_HDR_METADATA_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_HDR_METADATA_SPEC_VERSION = 2


type EXT_HDR_METADATA_EXTENSION_NAME = "VK_EXT_hdr_metadata"

-- No documentation found for TopLevel "VK_EXT_HDR_METADATA_EXTENSION_NAME"
pattern EXT_HDR_METADATA_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_HDR_METADATA_EXTENSION_NAME = "VK_EXT_hdr_metadata"

