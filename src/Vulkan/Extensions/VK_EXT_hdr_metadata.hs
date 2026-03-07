{-# language CPP #-}
-- | = Name
--
-- VK_EXT_hdr_metadata - device extension
--
-- = VK_EXT_hdr_metadata
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
--     3
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--
-- [__Contact__]
--
--     -   Courtney Goeltzenleuchter
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_hdr_metadata] @courtney-g%0A*Here describe the issue or question you have about the VK_EXT_hdr_metadata extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-03-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Courtney Goeltzenleuchter, Google
--
--     -   Sebastian Wick, Red Hat Inc.
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension defines two new structures and a function to assign SMPTE
-- (the Society of Motion Picture and Television Engineers) 2086 metadata
-- and CTA (Consumer Technology Association) 861.3 metadata to a swapchain.
--
-- SMPTE 2086 metadata defines the color volume of the display on which the
-- content was optimized for viewing and includes the color primaries,
-- white point, and luminance range. When such content is reproduced on
-- another display, this metadata can be used by the presentation engine to
-- improve processing of images. For instance, values in the image can
-- first be clamped to the color volume described in the metadata, and then
-- what remains can be remapped to the color volume of the presentation
-- engine.
--
-- CTA 861.3 metadata additionally includes the maximum intended luminance
-- for the content and the maximum average light level across frames.
--
-- This extension does not define exactly how this metadata is used,
-- however, it simply provides a mechanism to provide it to the
-- presentation engine. Presentation engines may process the image based on
-- the metadata before displaying it, resulting in the image being modified
-- outside of Vulkan. For example, the clamping of colors in the image to
-- the color volume may change those values in the image itself.
--
-- The metadata does not override or otherwise influence the color space
-- and color encoding.
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
-- 1) Do we need a query function for the currently specified metadata?
--
-- No, Vulkan does not provide queries for state that the application can
-- track on its own.
--
-- 2) Should we specify default metadata if not specified by the
-- application?
--
-- No, the metadata is optional and the absence of the metadata is
-- well-defined.
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
-- -   Revision 3, 2024-03-26 (Tobias Hector & Sebastian Wick)
--
--     -   Clarifications and removal of erroneous “reference monitor” term
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_hdr_metadata Vulkan Specification>.
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
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
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
import Data.Type.Equality ((:~:)(Refl))
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
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkSetHdrMetadataEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_hdr_vivid (HdrVividDynamicMetadataHUAWEI)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.CStruct.Extends (SomeStruct)
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
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr SwapchainKHR -> Ptr (SomeStruct HdrMetadataEXT) -> IO ()) -> Ptr Device_T -> Word32 -> Ptr SwapchainKHR -> Ptr (SomeStruct HdrMetadataEXT) -> IO ()

-- | vkSetHdrMetadataEXT - Set HDR metadata
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
-- -   #VUID-vkSetHdrMetadataEXT-pSwapchains-parent# Each element of
--     @pSwapchains@ /must/ have been created, allocated, or retrieved from
--     @device@
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
                     ("metadata" ::: Vector (SomeStruct HdrMetadataEXT))
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
  pPMetadata <- ContT $ allocaBytes @(HdrMetadataEXT _) ((Data.Vector.length (metadata)) * 64)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPMetadata `plusPtr` (64 * (i)) :: Ptr (HdrMetadataEXT _))) (e) . ($ ())) (metadata)
  lift $ traceAroundEvent "vkSetHdrMetadataEXT" (vkSetHdrMetadataEXT'
                                                   (deviceHandle (device))
                                                   ((fromIntegral pSwapchainsLength :: Word32))
                                                   (pPSwapchains)
                                                   (forgetExtensions (pPMetadata)))
  pure $ ()


-- | VkXYColorEXT - Specify X,Y chromaticity coordinates
--
-- = Description
--
-- Chromaticity coordinates are as specified in CIE 15:2004 “Calculation of
-- chromaticity coordinates” (Section 7.3) and are limited to between 0 and
-- 1 for real colors.
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


-- | VkHdrMetadataEXT - Specify HDR metadata
--
-- = Description
--
-- If any of the above values are unknown, they /can/ be set to 0.
--
-- The meta-data provided here is intended to be used as defined in the
-- SMPTE 2086, CTA 861.3 and CIE 15:2004 specifications. The validity and
-- use of this data is outside the scope of Vulkan.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkHdrMetadataEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_HDR_METADATA_EXT'
--
-- -   #VUID-VkHdrMetadataEXT-pNext-pNext# @pNext@ /must/ be @NULL@ or a
--     pointer to a valid instance of
--     'Vulkan.Extensions.VK_HUAWEI_hdr_vivid.HdrVividDynamicMetadataHUAWEI'
--
-- -   #VUID-VkHdrMetadataEXT-sType-unique# The @sType@ value of each
--     structure in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_hdr_metadata VK_EXT_hdr_metadata>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'XYColorEXT',
-- 'setHdrMetadataEXT'
data HdrMetadataEXT (es :: [Type]) = HdrMetadataEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @displayPrimaryRed@ is a 'XYColorEXT' structure specifying the red
    -- primary of the display used to optimize the content
    displayPrimaryRed :: XYColorEXT
  , -- | @displayPrimaryGreen@ is a 'XYColorEXT' structure specifying the green
    -- primary of the display used to optimize the content
    displayPrimaryGreen :: XYColorEXT
  , -- | @displayPrimaryBlue@ is a 'XYColorEXT' structure specifying the blue
    -- primary of the display used to optimize the content
    displayPrimaryBlue :: XYColorEXT
  , -- | @whitePoint@ is a 'XYColorEXT' structure specifying the white-point of
    -- the display used to optimize the content
    whitePoint :: XYColorEXT
  , -- | @maxLuminance@ is the maximum luminance of the display used to optimize
    -- the content in nits
    maxLuminance :: Float
  , -- | @minLuminance@ is the minimum luminance of the display used to optimize
    -- the content in nits
    minLuminance :: Float
  , -- | @maxContentLightLevel@ is the value in nits of the desired luminance for
    -- the brightest pixels in the displayed image.
    maxContentLightLevel :: Float
  , -- | @maxFrameAverageLightLevel@ is the value in nits of the average
    -- luminance of the frame which has the brightest average luminance
    -- anywhere in the content.
    maxFrameAverageLightLevel :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HdrMetadataEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (HdrMetadataEXT es)

instance Extensible HdrMetadataEXT where
  extensibleTypeName = "HdrMetadataEXT"
  setNext HdrMetadataEXT{..} next' = HdrMetadataEXT{next = next', ..}
  getNext HdrMetadataEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends HdrMetadataEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @HdrVividDynamicMetadataHUAWEI = Just f
    | otherwise = Nothing

instance ( Extendss HdrMetadataEXT es
         , PokeChain es ) => ToCStruct (HdrMetadataEXT es) where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HdrMetadataEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HDR_METADATA_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr XYColorEXT)) (displayPrimaryRed)
    lift $ poke ((p `plusPtr` 24 :: Ptr XYColorEXT)) (displayPrimaryGreen)
    lift $ poke ((p `plusPtr` 32 :: Ptr XYColorEXT)) (displayPrimaryBlue)
    lift $ poke ((p `plusPtr` 40 :: Ptr XYColorEXT)) (whitePoint)
    lift $ poke ((p `plusPtr` 48 :: Ptr CFloat)) (CFloat (maxLuminance))
    lift $ poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (minLuminance))
    lift $ poke ((p `plusPtr` 56 :: Ptr CFloat)) (CFloat (maxContentLightLevel))
    lift $ poke ((p `plusPtr` 60 :: Ptr CFloat)) (CFloat (maxFrameAverageLightLevel))
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HDR_METADATA_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr XYColorEXT)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr XYColorEXT)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr XYColorEXT)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr XYColorEXT)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 56 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 60 :: Ptr CFloat)) (CFloat (zero))
    lift $ f

instance ( Extendss HdrMetadataEXT es
         , PeekChain es ) => FromCStruct (HdrMetadataEXT es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    displayPrimaryRed <- peekCStruct @XYColorEXT ((p `plusPtr` 16 :: Ptr XYColorEXT))
    displayPrimaryGreen <- peekCStruct @XYColorEXT ((p `plusPtr` 24 :: Ptr XYColorEXT))
    displayPrimaryBlue <- peekCStruct @XYColorEXT ((p `plusPtr` 32 :: Ptr XYColorEXT))
    whitePoint <- peekCStruct @XYColorEXT ((p `plusPtr` 40 :: Ptr XYColorEXT))
    maxLuminance <- peek @CFloat ((p `plusPtr` 48 :: Ptr CFloat))
    minLuminance <- peek @CFloat ((p `plusPtr` 52 :: Ptr CFloat))
    maxContentLightLevel <- peek @CFloat ((p `plusPtr` 56 :: Ptr CFloat))
    maxFrameAverageLightLevel <- peek @CFloat ((p `plusPtr` 60 :: Ptr CFloat))
    pure $ HdrMetadataEXT
             next
             displayPrimaryRed
             displayPrimaryGreen
             displayPrimaryBlue
             whitePoint
             (coerce @CFloat @Float maxLuminance)
             (coerce @CFloat @Float minLuminance)
             (coerce @CFloat @Float maxContentLightLevel)
             (coerce @CFloat @Float maxFrameAverageLightLevel)

instance es ~ '[] => Zero (HdrMetadataEXT es) where
  zero = HdrMetadataEXT
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


type EXT_HDR_METADATA_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_EXT_HDR_METADATA_SPEC_VERSION"
pattern EXT_HDR_METADATA_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_HDR_METADATA_SPEC_VERSION = 3


type EXT_HDR_METADATA_EXTENSION_NAME = "VK_EXT_hdr_metadata"

-- No documentation found for TopLevel "VK_EXT_HDR_METADATA_EXTENSION_NAME"
pattern EXT_HDR_METADATA_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_HDR_METADATA_EXTENSION_NAME = "VK_EXT_hdr_metadata"

