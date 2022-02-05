{-# language CPP #-}
-- | = Name
--
-- VK_EXT_hdr_metadata - device extension
--
-- == VK_EXT_hdr_metadata
--
-- [__Name String__]
--     @VK_EXT_hdr_metadata@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     106
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_swapchain@
--
-- [__Contact__]
--
--     -   Courtney Goeltzenleuchter
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_hdr_metadata] @courtney-g%0A<<Here describe the issue or question you have about the VK_EXT_hdr_metadata extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-12-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Courtney Goeltzenleuchter, Google
--
-- == Description
--
-- This extension defines two new structures and a function to assign SMPTE
-- (the Society of Motion Picture and Television Engineers) 2086 metadata
-- and CTA (Consumer Technology Association) 861.3 metadata to a swapchain.
-- The metadata includes the color primaries, white point, and luminance
-- range of the reference monitor, which all together define the color
-- volume containing all the possible colors the reference monitor can
-- produce. The reference monitor is the display where creative work is
-- done and creative intent is established. To preserve such creative
-- intent as much as possible and achieve consistent color reproduction on
-- different viewing displays, it is useful for the display pipeline to
-- know the color volume of the original reference monitor where content
-- was created or tuned. This avoids performing unnecessary mapping of
-- colors that are not displayable on the original reference monitor. The
-- metadata also includes the @maxContentLightLevel@ and
-- @maxFrameAverageLightLevel@ as defined by CTA 861.3.
--
-- While the general purpose of the metadata is to assist in the
-- transformation between different color volumes of different displays and
-- help achieve better color reproduction, it is not in the scope of this
-- extension to define how exactly the metadata should be used in such a
-- process. It is up to the implementation to determine how to make use of
-- the metadata.
--
-- == New Commands
--
-- -   'setHdrMetadataEXT'
--
-- == New Structures
--
-- -   'HdrMetadataEXT'
--
-- -   'XYColorEXT'
--
-- == New Enum Constants
--
-- -   'EXT_HDR_METADATA_EXTENSION_NAME'
--
-- -   'EXT_HDR_METADATA_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_HDR_METADATA_EXT'
--
-- == Issues
--
-- 1) Do we need a query function?
--
-- __PROPOSED__: No, Vulkan does not provide queries for state that the
-- application can track on its own.
--
-- 2) Should we specify default if not specified by the application?
--
-- __PROPOSED__: No, that leaves the default up to the display.
--
-- == Version History
--
-- -   Revision 1, 2016-12-27 (Courtney Goeltzenleuchter)
--
--     -   Initial version
--
-- -   Revision 2, 2018-12-19 (Courtney Goeltzenleuchter)
--
--     -   Correct implicit validity for VkHdrMetadataEXT structure
--
-- == See Also
--
-- 'HdrMetadataEXT', 'XYColorEXT', 'setHdrMetadataEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_hdr_metadata Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_hdr_metadata  ( setHdrMetadataEXT
                                              , XYColorEXT(..)
                                              , HdrMetadataEXT(..)
                                              , EXT_HDR_METADATA_SPEC_VERSION
                                              , pattern EXT_HDR_METADATA_SPEC_VERSION
                                              , EXT_HDR_METADATA_EXTENSION_NAME
                                              , pattern EXT_HDR_METADATA_EXTENSION_NAME
                                              , SwapchainKHR(..)
                                              ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
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
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkSetHdrMetadataEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_HDR_METADATA_EXT))
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetHdrMetadataEXT
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr SwapchainKHR -> Ptr HdrMetadataEXT -> IO ()) -> Ptr Device_T -> Word32 -> Ptr SwapchainKHR -> Ptr HdrMetadataEXT -> IO ()

-- | vkSetHdrMetadataEXT - Set Hdr metadata
--
-- = Description
--
-- The metadata will be applied to the specified
-- 'Vulkan.Extensions.Handles.SwapchainKHR' objects at the next
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' call using that
-- 'Vulkan.Extensions.Handles.SwapchainKHR' object. The metadata will
-- persist until a subsequent 'setHdrMetadataEXT' changes it.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkSetHdrMetadataEXT-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkSetHdrMetadataEXT-pSwapchains-parameter# @pSwapchains@
--     /must/ be a valid pointer to an array of @swapchainCount@ valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handles
--
-- -   #VUID-vkSetHdrMetadataEXT-pMetadata-parameter# @pMetadata@ /must/ be
--     a valid pointer to an array of @swapchainCount@ valid
--     'HdrMetadataEXT' structures
--
-- -   #VUID-vkSetHdrMetadataEXT-swapchainCount-arraylength#
--     @swapchainCount@ /must/ be greater than @0@
--
-- -   #VUID-vkSetHdrMetadataEXT-commonparent# Both of @device@, and the
--     elements of @pSwapchains@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Instance'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_hdr_metadata VK_EXT_hdr_metadata>,
-- 'Vulkan.Core10.Handles.Device', 'HdrMetadataEXT',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
setHdrMetadataEXT :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the logical device where the swapchain(s) were created.
                     Device
                  -> -- | @pSwapchains@ is a pointer to an array of @swapchainCount@
                     -- 'Vulkan.Extensions.Handles.SwapchainKHR' handles.
                     ("swapchains" ::: Vector SwapchainKHR)
                  -> -- | @pMetadata@ is a pointer to an array of @swapchainCount@
                     -- 'HdrMetadataEXT' structures.
                     ("metadata" ::: Vector HdrMetadataEXT)
                  -> io ()
setHdrMetadataEXT device swapchains metadata = liftIO . evalContT $ do
  let vkSetHdrMetadataEXTPtr = pVkSetHdrMetadataEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkSetHdrMetadataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetHdrMetadataEXT is null" Nothing Nothing
  let vkSetHdrMetadataEXT' = mkVkSetHdrMetadataEXT vkSetHdrMetadataEXTPtr
  let pSwapchainsLength = Data.Vector.length $ (swapchains)
  lift $ unless ((Data.Vector.length $ (metadata)) == pSwapchainsLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pMetadata and pSwapchains must have the same length" Nothing Nothing
  pPSwapchains <- ContT $ allocaBytes @SwapchainKHR ((Data.Vector.length (swapchains)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPSwapchains `plusPtr` (8 * (i)) :: Ptr SwapchainKHR) (e)) (swapchains)
  pPMetadata <- ContT $ allocaBytes @HdrMetadataEXT ((Data.Vector.length (metadata)) * 64)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMetadata `plusPtr` (64 * (i)) :: Ptr HdrMetadataEXT) (e)) (metadata)
  lift $ traceAroundEvent "vkSetHdrMetadataEXT" (vkSetHdrMetadataEXT' (deviceHandle (device)) ((fromIntegral pSwapchainsLength :: Word32)) (pPSwapchains) (pPMetadata))
  pure $ ()


-- | VkXYColorEXT - Specify X,Y chromaticity coordinates
--
-- = Description
--
-- Chromaticity coordinates are as specified in CIE 15:2004 “Calculation of
-- chromaticity coordinates” (Section 7.3) and are limited to between 0 and
-- 1 for real colors for the reference monitor.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_hdr_metadata VK_EXT_hdr_metadata>,
-- 'HdrMetadataEXT'
data XYColorEXT = XYColorEXT
  { -- | @x@ is the x chromaticity coordinate.
    x :: Float
  , -- | @y@ is the y chromaticity coordinate.
    y :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (XYColorEXT)
#endif
deriving instance Show XYColorEXT

instance ToCStruct XYColorEXT where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
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
             (coerce @CFloat @Float x) (coerce @CFloat @Float y)

instance Storable XYColorEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero XYColorEXT where
  zero = XYColorEXT
           zero
           zero


-- | VkHdrMetadataEXT - Specify Hdr metadata
--
-- == Valid Usage (Implicit)
--
-- Note
--
-- The validity and use of this data is outside the scope of Vulkan.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_hdr_metadata VK_EXT_hdr_metadata>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'XYColorEXT',
-- 'setHdrMetadataEXT'
data HdrMetadataEXT = HdrMetadataEXT
  { -- | @displayPrimaryRed@ is a 'XYColorEXT' structure specifying the reference
    -- monitor’s red primary in chromaticity coordinates
    displayPrimaryRed :: XYColorEXT
  , -- | @displayPrimaryGreen@ is a 'XYColorEXT' structure specifying the
    -- reference monitor’s green primary in chromaticity coordinates
    displayPrimaryGreen :: XYColorEXT
  , -- | @displayPrimaryBlue@ is a 'XYColorEXT' structure specifying the
    -- reference monitor’s blue primary in chromaticity coordinates
    displayPrimaryBlue :: XYColorEXT
  , -- | @whitePoint@ is a 'XYColorEXT' structure specifying the reference
    -- monitor’s white-point in chromaticity coordinates
    whitePoint :: XYColorEXT
  , -- | @maxLuminance@ is the maximum luminance of the reference monitor in nits
    maxLuminance :: Float
  , -- | @minLuminance@ is the minimum luminance of the reference monitor in nits
    minLuminance :: Float
  , -- | @maxContentLightLevel@ is content’s maximum luminance in nits
    maxContentLightLevel :: Float
  , -- | @maxFrameAverageLightLevel@ is the maximum frame average light level in
    -- nits
    maxFrameAverageLightLevel :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HdrMetadataEXT)
#endif
deriving instance Show HdrMetadataEXT

instance ToCStruct HdrMetadataEXT where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
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
             displayPrimaryRed displayPrimaryGreen displayPrimaryBlue whitePoint (coerce @CFloat @Float maxLuminance) (coerce @CFloat @Float minLuminance) (coerce @CFloat @Float maxContentLightLevel) (coerce @CFloat @Float maxFrameAverageLightLevel)

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

