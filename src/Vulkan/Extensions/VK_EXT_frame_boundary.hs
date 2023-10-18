{-# language CPP #-}
-- | = Name
--
-- VK_EXT_frame_boundary - device extension
--
-- == VK_EXT_frame_boundary
--
-- [__Name String__]
--     @VK_EXT_frame_boundary@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     376
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   James Fitzpatrick
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_frame_boundary] @jamesfitzpatrick%0A*Here describe the issue or question you have about the VK_EXT_frame_boundary extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_frame_boundary.adoc VK_EXT_frame_boundary>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-06-14
--
-- [__Contributors__]
--
--     -   James Fitzpatrick, Imagination Technologies
--
--     -   Hugues Evrard, Google
--
--     -   Melih Yasin Yalcin, Google
--
--     -   Andrew Garrard, Imagination Technologies
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Vassili Nikolaev, NVIDIA
--
--     -   Ting Wei, Huawei
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_frame_boundary VK_EXT_frame_boundary>
-- is a device extension that helps __tools__ (such as debuggers) to group
-- queue submissions per frames in non-trivial scenarios, typically when
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' is not a relevant
-- frame boundary delimiter.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFrameBoundaryFeaturesEXT'
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.SubmitInfo2',
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR',
--     'Vulkan.Core10.SparseResourceMemoryManagement.BindSparseInfo':
--
--     -   'FrameBoundaryEXT'
--
-- == New Enums
--
-- -   'FrameBoundaryFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'FrameBoundaryFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_FRAME_BOUNDARY_EXTENSION_NAME'
--
-- -   'EXT_FRAME_BOUNDARY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAME_BOUNDARY_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAME_BOUNDARY_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 0, 2022-01-14 (Hugues Evard)
--
--     -   Initial proposal
--
-- -   Revision 1, 2023-06-14 (James Fitzpatrick)
--
--     -   Initial draft
--
-- == See Also
--
-- 'FrameBoundaryEXT', 'FrameBoundaryFlagBitsEXT', 'FrameBoundaryFlagsEXT',
-- 'PhysicalDeviceFrameBoundaryFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_frame_boundary Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_frame_boundary  ( FrameBoundaryEXT(..)
                                                , PhysicalDeviceFrameBoundaryFeaturesEXT(..)
                                                , FrameBoundaryFlagsEXT
                                                , FrameBoundaryFlagBitsEXT( FRAME_BOUNDARY_FRAME_END_BIT_EXT
                                                                          , ..
                                                                          )
                                                , EXT_FRAME_BOUNDARY_SPEC_VERSION
                                                , pattern EXT_FRAME_BOUNDARY_SPEC_VERSION
                                                , EXT_FRAME_BOUNDARY_EXTENSION_NAME
                                                , pattern EXT_FRAME_BOUNDARY_EXTENSION_NAME
                                                ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FRAME_BOUNDARY_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAME_BOUNDARY_FEATURES_EXT))
-- | VkFrameBoundaryEXT - Add frame boundary information to queue submissions
--
-- = Description
--
-- The application /can/ associate frame boundary information to a queue
-- submission call by adding a 'FrameBoundaryEXT' structure to the @pNext@
-- chain of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-submission queue submission>,
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR', or
-- 'Vulkan.Core10.SparseResourceMemoryManagement.BindSparseInfo'.
--
-- The frame identifier is used to associate one or more queue submission
-- to a frame, it is thus meant to be unique within a frame lifetime, i.e.
-- it is possible (but not recommended) to reuse frame identifiers, as long
-- as any two frames with any chance of having overlapping queue
-- submissions (as in the example above) use two different frame
-- identifiers.
--
-- Note
--
-- Since the concept of frame is application-dependent, there is no way to
-- validate the use of frame identifier. It is good practice to use a
-- monotonically increasing counter as the frame identifier and not reuse
-- identifiers between frames.
--
-- The @pImages@ and @pBuffers@ arrays contain a list of images and buffers
-- which store the \"end result\" of the frame. As the concept of frame is
-- application-dependent, not all frames /may/ produce their results in
-- images or buffers, yet this is a sufficiently common case to be handled
-- by 'FrameBoundaryEXT'. Note that no extra information, such as image
-- layout is being provided, since the images are meant to be used by tools
-- which would already be tracking this required information. Having the
-- possibility of passing a list of end-result images makes
-- 'FrameBoundaryEXT' as expressive as
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR', which is often the
-- default frame boundary delimiter.
--
-- The application /can/ also associate arbitrary extra information via tag
-- data using @tagName@, @tagSize@ and @pTag@. This extra information is
-- typically tool-specific.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkFrameBoundaryEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAME_BOUNDARY_EXT'
--
-- -   #VUID-VkFrameBoundaryEXT-flags-parameter# @flags@ /must/ be a valid
--     combination of 'FrameBoundaryFlagBitsEXT' values
--
-- -   #VUID-VkFrameBoundaryEXT-pImages-parameter# If @imageCount@ is not
--     @0@, and @pImages@ is not @NULL@, @pImages@ /must/ be a valid
--     pointer to an array of @imageCount@ valid
--     'Vulkan.Core10.Handles.Image' handles
--
-- -   #VUID-VkFrameBoundaryEXT-pBuffers-parameter# If @bufferCount@ is not
--     @0@, and @pBuffers@ is not @NULL@, @pBuffers@ /must/ be a valid
--     pointer to an array of @bufferCount@ valid
--     'Vulkan.Core10.Handles.Buffer' handles
--
-- -   #VUID-VkFrameBoundaryEXT-pTag-parameter# If @tagSize@ is not @0@,
--     and @pTag@ is not @NULL@, @pTag@ /must/ be a valid pointer to an
--     array of @tagSize@ bytes
--
-- -   #VUID-VkFrameBoundaryEXT-commonparent# Both of the elements of
--     @pBuffers@, and the elements of @pImages@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_frame_boundary VK_EXT_frame_boundary>,
-- 'Vulkan.Core10.Handles.Buffer', 'FrameBoundaryFlagsEXT',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data FrameBoundaryEXT = FrameBoundaryEXT
  { -- | @flags@ is a bitmask of 'FrameBoundaryFlagBitsEXT' that can flag the
    -- last submission of a frame identifier.
    flags :: FrameBoundaryFlagsEXT
  , -- | @frameID@ is the frame identifier.
    frameID :: Word64
  , -- | @imageCount@ is the number of images that store frame results.
    imageCount :: Word32
  , -- | @pImages@ is a pointer to an array of VkImage objects with imageCount
    -- entries.
    images :: Vector Image
  , -- | @bufferCount@ is the number of buffers the store the frame results.
    bufferCount :: Word32
  , -- | @pBuffers@ is a pointer to an array of VkBuffer objects with bufferCount
    -- entries.
    buffers :: Vector Buffer
  , -- | @tagName@ is a numerical identifier for tag data.
    tagName :: Word64
  , -- | @tagSize@ is the number of bytes of tag data.
    tagSize :: Word64
  , -- | @pTag@ is a pointer to an array of @tagSize@ bytes containing tag data.
    tag :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FrameBoundaryEXT)
#endif
deriving instance Show FrameBoundaryEXT

instance ToCStruct FrameBoundaryEXT where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FrameBoundaryEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAME_BOUNDARY_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr FrameBoundaryFlagsEXT)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word64)) (frameID)
    let pImagesLength = Data.Vector.length $ (images)
    imageCount'' <- lift $ if (imageCount) == 0
      then pure $ fromIntegral pImagesLength
      else do
        unless (fromIntegral pImagesLength == (imageCount) || pImagesLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pImages must be empty or have 'imageCount' elements" Nothing Nothing
        pure (imageCount)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (imageCount'')
    pImages'' <- if Data.Vector.null (images)
      then pure nullPtr
      else do
        pPImages <- ContT $ allocaBytes @Image (((Data.Vector.length (images))) * 8)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPImages `plusPtr` (8 * (i)) :: Ptr Image) (e)) ((images))
        pure $ pPImages
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Image))) pImages''
    let pBuffersLength = Data.Vector.length $ (buffers)
    bufferCount'' <- lift $ if (bufferCount) == 0
      then pure $ fromIntegral pBuffersLength
      else do
        unless (fromIntegral pBuffersLength == (bufferCount) || pBuffersLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pBuffers must be empty or have 'bufferCount' elements" Nothing Nothing
        pure (bufferCount)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (bufferCount'')
    pBuffers'' <- if Data.Vector.null (buffers)
      then pure nullPtr
      else do
        pPBuffers <- ContT $ allocaBytes @Buffer (((Data.Vector.length (buffers))) * 8)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) ((buffers))
        pure $ pPBuffers
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr Buffer))) pBuffers''
    lift $ poke ((p `plusPtr` 64 :: Ptr Word64)) (tagName)
    lift $ poke ((p `plusPtr` 72 :: Ptr CSize)) (CSize (tagSize))
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr ()))) (tag)
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAME_BOUNDARY_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct FrameBoundaryEXT where
  peekCStruct p = do
    flags <- peek @FrameBoundaryFlagsEXT ((p `plusPtr` 16 :: Ptr FrameBoundaryFlagsEXT))
    frameID <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    imageCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pImages <- peek @(Ptr Image) ((p `plusPtr` 40 :: Ptr (Ptr Image)))
    let pImagesLength = if pImages == nullPtr then 0 else (fromIntegral imageCount)
    pImages' <- generateM pImagesLength (\i -> peek @Image ((pImages `advancePtrBytes` (8 * (i)) :: Ptr Image)))
    bufferCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pBuffers <- peek @(Ptr Buffer) ((p `plusPtr` 56 :: Ptr (Ptr Buffer)))
    let pBuffersLength = if pBuffers == nullPtr then 0 else (fromIntegral bufferCount)
    pBuffers' <- generateM pBuffersLength (\i -> peek @Buffer ((pBuffers `advancePtrBytes` (8 * (i)) :: Ptr Buffer)))
    tagName <- peek @Word64 ((p `plusPtr` 64 :: Ptr Word64))
    tagSize <- peek @CSize ((p `plusPtr` 72 :: Ptr CSize))
    pTag <- peek @(Ptr ()) ((p `plusPtr` 80 :: Ptr (Ptr ())))
    pure $ FrameBoundaryEXT
             flags
             frameID
             imageCount
             pImages'
             bufferCount
             pBuffers'
             tagName
             (coerce @CSize @Word64 tagSize)
             pTag

instance Zero FrameBoundaryEXT where
  zero = FrameBoundaryEXT
           zero
           zero
           zero
           mempty
           zero
           mempty
           zero
           zero
           zero


-- | VkPhysicalDeviceFrameBoundaryFeaturesEXT - Structure describing the
-- frame boundary features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceFrameBoundaryFeaturesEXT' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceFrameBoundaryFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_frame_boundary VK_EXT_frame_boundary>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFrameBoundaryFeaturesEXT = PhysicalDeviceFrameBoundaryFeaturesEXT
  { -- | #features-frameBoundary# @frameBoundary@ indicates whether the
    -- implementation supports frame boundary information.
    frameBoundary :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFrameBoundaryFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceFrameBoundaryFeaturesEXT

instance ToCStruct PhysicalDeviceFrameBoundaryFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFrameBoundaryFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAME_BOUNDARY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (frameBoundary))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAME_BOUNDARY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFrameBoundaryFeaturesEXT where
  peekCStruct p = do
    frameBoundary <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceFrameBoundaryFeaturesEXT
             (bool32ToBool frameBoundary)

instance Storable PhysicalDeviceFrameBoundaryFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFrameBoundaryFeaturesEXT where
  zero = PhysicalDeviceFrameBoundaryFeaturesEXT
           zero


type FrameBoundaryFlagsEXT = FrameBoundaryFlagBitsEXT

-- | VkFrameBoundaryFlagBitsEXT - Bitmask specifying whether a queue
-- submission is the last one for a given frame
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_frame_boundary VK_EXT_frame_boundary>,
-- 'FrameBoundaryFlagsEXT'
newtype FrameBoundaryFlagBitsEXT = FrameBoundaryFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'FRAME_BOUNDARY_FRAME_END_BIT_EXT' specifies that this queue submission
-- is the last one for this frame, i.e. once this queue submission has
-- terminated, then the work for this frame is completed.
pattern FRAME_BOUNDARY_FRAME_END_BIT_EXT = FrameBoundaryFlagBitsEXT 0x00000001

conNameFrameBoundaryFlagBitsEXT :: String
conNameFrameBoundaryFlagBitsEXT = "FrameBoundaryFlagBitsEXT"

enumPrefixFrameBoundaryFlagBitsEXT :: String
enumPrefixFrameBoundaryFlagBitsEXT = "FRAME_BOUNDARY_FRAME_END_BIT_EXT"

showTableFrameBoundaryFlagBitsEXT :: [(FrameBoundaryFlagBitsEXT, String)]
showTableFrameBoundaryFlagBitsEXT = [(FRAME_BOUNDARY_FRAME_END_BIT_EXT, "")]

instance Show FrameBoundaryFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixFrameBoundaryFlagBitsEXT
      showTableFrameBoundaryFlagBitsEXT
      conNameFrameBoundaryFlagBitsEXT
      (\(FrameBoundaryFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read FrameBoundaryFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixFrameBoundaryFlagBitsEXT
      showTableFrameBoundaryFlagBitsEXT
      conNameFrameBoundaryFlagBitsEXT
      FrameBoundaryFlagBitsEXT

type EXT_FRAME_BOUNDARY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_FRAME_BOUNDARY_SPEC_VERSION"
pattern EXT_FRAME_BOUNDARY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_FRAME_BOUNDARY_SPEC_VERSION = 1


type EXT_FRAME_BOUNDARY_EXTENSION_NAME = "VK_EXT_frame_boundary"

-- No documentation found for TopLevel "VK_EXT_FRAME_BOUNDARY_EXTENSION_NAME"
pattern EXT_FRAME_BOUNDARY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_FRAME_BOUNDARY_EXTENSION_NAME = "VK_EXT_frame_boundary"

