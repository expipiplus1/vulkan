{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_hdr_metadata"
module Vulkan.Extensions.VK_EXT_hdr_metadata  ( setHdrMetadataEXT
                                              , XYColorEXT(..)
                                              , HdrMetadataEXT(..)
                                              , EXT_HDR_METADATA_SPEC_VERSION
                                              , pattern EXT_HDR_METADATA_SPEC_VERSION
                                              , EXT_HDR_METADATA_EXTENSION_NAME
                                              , pattern EXT_HDR_METADATA_EXTENSION_NAME
                                              , SwapchainKHR(..)
                                              ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkSetHdrMetadataEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_HDR_METADATA_EXT))
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetHdrMetadataEXT
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr SwapchainKHR -> Ptr HdrMetadataEXT -> IO ()) -> Ptr Device_T -> Word32 -> Ptr SwapchainKHR -> Ptr HdrMetadataEXT -> IO ()

-- No documentation found for TopLevel "vkSetHdrMetadataEXT"
setHdrMetadataEXT :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkSetHdrMetadataEXT" "device"
                     Device
                  -> -- No documentation found for Nested "vkSetHdrMetadataEXT" "pSwapchains"
                     ("swapchains" ::: Vector SwapchainKHR)
                  -> -- No documentation found for Nested "vkSetHdrMetadataEXT" "pMetadata"
                     ("metadata" ::: Vector HdrMetadataEXT)
                  -> io ()
setHdrMetadataEXT device swapchains metadata = liftIO . evalContT $ do
  let vkSetHdrMetadataEXTPtr = pVkSetHdrMetadataEXT (deviceCmds (device :: Device))
  lift $ unless (vkSetHdrMetadataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetHdrMetadataEXT is null" Nothing Nothing
  let vkSetHdrMetadataEXT' = mkVkSetHdrMetadataEXT vkSetHdrMetadataEXTPtr
  let pSwapchainsLength = Data.Vector.length $ (swapchains)
  lift $ unless ((Data.Vector.length $ (metadata)) == pSwapchainsLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pMetadata and pSwapchains must have the same length" Nothing Nothing
  pPSwapchains <- ContT $ allocaBytesAligned @SwapchainKHR ((Data.Vector.length (swapchains)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPSwapchains `plusPtr` (8 * (i)) :: Ptr SwapchainKHR) (e)) (swapchains)
  pPMetadata <- ContT $ allocaBytesAligned @HdrMetadataEXT ((Data.Vector.length (metadata)) * 64) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMetadata `plusPtr` (64 * (i)) :: Ptr HdrMetadataEXT) (e)) (metadata)
  lift $ vkSetHdrMetadataEXT' (deviceHandle (device)) ((fromIntegral pSwapchainsLength :: Word32)) (pPSwapchains) (pPMetadata)
  pure $ ()



-- No documentation found for TopLevel "VkXYColorEXT"
data XYColorEXT = XYColorEXT
  { -- No documentation found for Nested "VkXYColorEXT" "x"
    x :: Float
  , -- No documentation found for Nested "VkXYColorEXT" "y"
    y :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (XYColorEXT)
#endif
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



-- No documentation found for TopLevel "VkHdrMetadataEXT"
data HdrMetadataEXT = HdrMetadataEXT
  { -- No documentation found for Nested "VkHdrMetadataEXT" "displayPrimaryRed"
    displayPrimaryRed :: XYColorEXT
  , -- No documentation found for Nested "VkHdrMetadataEXT" "displayPrimaryGreen"
    displayPrimaryGreen :: XYColorEXT
  , -- No documentation found for Nested "VkHdrMetadataEXT" "displayPrimaryBlue"
    displayPrimaryBlue :: XYColorEXT
  , -- No documentation found for Nested "VkHdrMetadataEXT" "whitePoint"
    whitePoint :: XYColorEXT
  , -- No documentation found for Nested "VkHdrMetadataEXT" "maxLuminance"
    maxLuminance :: Float
  , -- No documentation found for Nested "VkHdrMetadataEXT" "minLuminance"
    minLuminance :: Float
  , -- No documentation found for Nested "VkHdrMetadataEXT" "maxContentLightLevel"
    maxContentLightLevel :: Float
  , -- No documentation found for Nested "VkHdrMetadataEXT" "maxFrameAverageLightLevel"
    maxFrameAverageLightLevel :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HdrMetadataEXT)
#endif
deriving instance Show HdrMetadataEXT

instance ToCStruct HdrMetadataEXT where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HdrMetadataEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HDR_METADATA_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr XYColorEXT)) (displayPrimaryRed)
    poke ((p `plusPtr` 24 :: Ptr XYColorEXT)) (displayPrimaryGreen)
    poke ((p `plusPtr` 32 :: Ptr XYColorEXT)) (displayPrimaryBlue)
    poke ((p `plusPtr` 40 :: Ptr XYColorEXT)) (whitePoint)
    poke ((p `plusPtr` 48 :: Ptr CFloat)) (CFloat (maxLuminance))
    poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (minLuminance))
    poke ((p `plusPtr` 56 :: Ptr CFloat)) (CFloat (maxContentLightLevel))
    poke ((p `plusPtr` 60 :: Ptr CFloat)) (CFloat (maxFrameAverageLightLevel))
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HDR_METADATA_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr XYColorEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr XYColorEXT)) (zero)
    poke ((p `plusPtr` 32 :: Ptr XYColorEXT)) (zero)
    poke ((p `plusPtr` 40 :: Ptr XYColorEXT)) (zero)
    poke ((p `plusPtr` 48 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 56 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 60 :: Ptr CFloat)) (CFloat (zero))
    f

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


instance Storable HdrMetadataEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

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

