{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_holographic_window_attachment - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_holographic_window_attachment  XR_MSFT_holographic_window_attachment>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 64
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'HolographicWindowAttachmentMSFT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_holographic_window_attachment OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_holographic_window_attachment  ( HolographicWindowAttachmentMSFT(..)
                                                                , MSFT_holographic_window_attachment_SPEC_VERSION
                                                                , pattern MSFT_holographic_window_attachment_SPEC_VERSION
                                                                , MSFT_HOLOGRAPHIC_WINDOW_ATTACHMENT_EXTENSION_NAME
                                                                , pattern MSFT_HOLOGRAPHIC_WINDOW_ATTACHMENT_EXTENSION_NAME
                                                                , IUnknown
                                                                ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import OpenXR.Extensions.XR_MSFT_perception_anchor_interop (IUnknown)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT))
import OpenXR.Extensions.XR_MSFT_perception_anchor_interop (IUnknown)
-- | XrHolographicWindowAttachmentMSFT - The holographic window binding
-- structure which can be passed at session creation
--
-- == Member Descriptions
--
-- = Description
--
-- When creating a holographic window-backed
-- 'OpenXR.Core10.Handles.Session', the application provides a pointer to
-- an 'HolographicWindowAttachmentMSFT' in the @next@ chain of the
-- 'OpenXR.Core10.Device.SessionCreateInfo'.
--
-- The session state of a holographic window-backed
-- 'OpenXR.Core10.Handles.Session' will only reach
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_VISIBLE' when the
-- provided CoreWindow is made visible. If the CoreWindow is for a
-- secondary app view, the application must programmatically request to
-- make the CoreWindow visible (e.g. with
-- @ApplicationViewSwitcher.TryShowAsStandaloneAsync@ or
-- @ApplicationViewSwitcher.SwitchAsync@).
--
-- The app /must/ not call 'OpenXR.Core10.Device.createSession' while the
-- specified CoreWindow thread is blocked, otherwise the call /may/
-- deadlock.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHolographicWindowAttachmentMSFT-extension-notenabled# The
--     @XR_MSFT_holographic_window_attachment@ extension /must/ be enabled
--     prior to using 'HolographicWindowAttachmentMSFT'
--
-- -   #VUID-XrHolographicWindowAttachmentMSFT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT'
--
-- -   #VUID-XrHolographicWindowAttachmentMSFT-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrHolographicWindowAttachmentMSFT-holographicSpace-parameter#
--     @holographicSpace@ /must/ be a pointer to an
--     'OpenXR.Extensions.XR_MSFT_perception_anchor_interop.IUnknown' value
--
-- -   #VUID-XrHolographicWindowAttachmentMSFT-coreWindow-parameter#
--     @coreWindow@ /must/ be a pointer to an
--     'OpenXR.Extensions.XR_MSFT_perception_anchor_interop.IUnknown' value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Device.createSession'
data HolographicWindowAttachmentMSFT = HolographicWindowAttachmentMSFT
  { -- | @holographicSpace@ is a pointer to a valid
    -- @Windows@.Graphics.Holographic.HolographicSpace.
    holographicSpace :: Ptr IUnknown
  , -- | @coreWindow@ is a pointer to a valid @Windows@.UI.Core.CoreWindow.
    coreWindow :: Ptr IUnknown
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HolographicWindowAttachmentMSFT)
#endif
deriving instance Show HolographicWindowAttachmentMSFT

instance ToCStruct HolographicWindowAttachmentMSFT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HolographicWindowAttachmentMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr IUnknown))) (holographicSpace)
    poke ((p `plusPtr` 24 :: Ptr (Ptr IUnknown))) (coreWindow)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr IUnknown))) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr IUnknown))) (zero)
    f

instance FromCStruct HolographicWindowAttachmentMSFT where
  peekCStruct p = do
    holographicSpace <- peek @(Ptr IUnknown) ((p `plusPtr` 16 :: Ptr (Ptr IUnknown)))
    coreWindow <- peek @(Ptr IUnknown) ((p `plusPtr` 24 :: Ptr (Ptr IUnknown)))
    pure $ HolographicWindowAttachmentMSFT
             holographicSpace coreWindow

instance Storable HolographicWindowAttachmentMSFT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HolographicWindowAttachmentMSFT where
  zero = HolographicWindowAttachmentMSFT
           zero
           zero


type MSFT_holographic_window_attachment_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_MSFT_holographic_window_attachment_SPEC_VERSION"
pattern MSFT_holographic_window_attachment_SPEC_VERSION :: forall a . Integral a => a
pattern MSFT_holographic_window_attachment_SPEC_VERSION = 1


type MSFT_HOLOGRAPHIC_WINDOW_ATTACHMENT_EXTENSION_NAME = "XR_MSFT_holographic_window_attachment"

-- No documentation found for TopLevel "XR_MSFT_HOLOGRAPHIC_WINDOW_ATTACHMENT_EXTENSION_NAME"
pattern MSFT_HOLOGRAPHIC_WINDOW_ATTACHMENT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MSFT_HOLOGRAPHIC_WINDOW_ATTACHMENT_EXTENSION_NAME = "XR_MSFT_holographic_window_attachment"

