{-# language CPP #-}
-- | = Name
--
-- XR_EXTX_overlay - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXTX_overlay  XR_EXTX_overlay>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 34
--
-- = Revision
--
-- 4
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'EventDataMainSessionVisibilityChangedEXTX',
-- 'SessionCreateInfoOverlayEXTX'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXTX_overlay OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXTX_overlay  ( SessionCreateInfoOverlayEXTX(..)
                                          , EventDataMainSessionVisibilityChangedEXTX(..)
                                          , OverlayMainSessionFlagsEXTX
                                          , OverlayMainSessionFlagBitsEXTX( OVERLAY_MAIN_SESSION_ENABLED_COMPOSITION_LAYER_INFO_DEPTH_BIT_EXTX
                                                                          , ..
                                                                          )
                                          , OverlaySessionCreateFlagsEXTX
                                          , OverlaySessionCreateFlagBitsEXTX( OVERLAY_SESSION_CREATE_RELAXED_DISPLAY_TIME_BIT_EXTX
                                                                            , ..
                                                                            )
                                          , EXTX_overlay_SPEC_VERSION
                                          , pattern EXTX_overlay_SPEC_VERSION
                                          , EXTX_OVERLAY_EXTENSION_NAME
                                          , pattern EXTX_OVERLAY_EXTENSION_NAME
                                          ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero)
import OpenXR.Zero (Zero(..))
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import OpenXR.Core10.FundamentalTypes (bool32ToBool)
import OpenXR.Core10.FundamentalTypes (boolToBool32)
import OpenXR.Core10.FundamentalTypes (Bool32)
import OpenXR.Core10.OtherTypes (EventDataBaseHeader(..))
import OpenXR.Core10.FundamentalTypes (Flags64)
import OpenXR.Core10.OtherTypes (IsEventData(..))
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SESSION_CREATE_INFO_OVERLAY_EXTX))
-- | XrSessionCreateInfoOverlayEXTX - Session creation extension struct
-- providing overlay session parameters
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSessionCreateInfoOverlayEXTX-extension-notenabled# The
--     @XR_EXTX_overlay@ extension /must/ be enabled prior to using
--     'SessionCreateInfoOverlayEXTX'
--
-- -   #VUID-XrSessionCreateInfoOverlayEXTX-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SESSION_CREATE_INFO_OVERLAY_EXTX'
--
-- -   #VUID-XrSessionCreateInfoOverlayEXTX-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSessionCreateInfoOverlayEXTX-createFlags-parameter#
--     @createFlags@ /must/ be a valid combination of
--     'OverlaySessionCreateFlagBitsEXTX' values
--
-- -   #VUID-XrSessionCreateInfoOverlayEXTX-createFlags-requiredbitmask#
--     @createFlags@ /must/ not be @0@
--
-- = See Also
--
-- 'OverlaySessionCreateFlagsEXTX',
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data SessionCreateInfoOverlayEXTX = SessionCreateInfoOverlayEXTX
  { -- | @createFlags@ is @0@ or one or more 'OverlaySessionCreateFlagBitsEXTX'
    -- which indicate various characteristics desired for the overlay session.
    createFlags :: OverlaySessionCreateFlagsEXTX
  , -- | @sessionLayersPlacement@ is a value indicating the desired placement of
    -- the session’s composition layers in terms of other sessions.
    sessionLayersPlacement :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SessionCreateInfoOverlayEXTX)
#endif
deriving instance Show SessionCreateInfoOverlayEXTX

instance ToCStruct SessionCreateInfoOverlayEXTX where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SessionCreateInfoOverlayEXTX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SESSION_CREATE_INFO_OVERLAY_EXTX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr OverlaySessionCreateFlagsEXTX)) (createFlags)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (sessionLayersPlacement)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SESSION_CREATE_INFO_OVERLAY_EXTX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr OverlaySessionCreateFlagsEXTX)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct SessionCreateInfoOverlayEXTX where
  peekCStruct p = do
    createFlags <- peek @OverlaySessionCreateFlagsEXTX ((p `plusPtr` 16 :: Ptr OverlaySessionCreateFlagsEXTX))
    sessionLayersPlacement <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ SessionCreateInfoOverlayEXTX
             createFlags sessionLayersPlacement

instance Storable SessionCreateInfoOverlayEXTX where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SessionCreateInfoOverlayEXTX where
  zero = SessionCreateInfoOverlayEXTX
           zero
           zero


-- | XrEventDataMainSessionVisibilityChangedEXTX - Event representing main
-- session visibility change
--
-- = Members
--
-- Receiving the 'EventDataMainSessionVisibilityChangedEXTX' event
-- structure indicates that the main session has gained or lost visibility.
-- This can occur in many cases, one typical example is when a user
-- switches from one OpenXR application to another. See
-- 'EventDataMainSessionVisibilityChangedEXTX' for more information on the
-- standard behavior. This structure contains additional information on the
-- main session including @flags@ which indicate additional state
-- information of the main session. Currently, the only flag value supplied
-- is 'OVERLAY_MAIN_SESSION_ENABLED_COMPOSITION_LAYER_INFO_DEPTH_BIT_EXTX'
-- which indicates if the main session has enabled the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_depth>
-- extension.
--
-- == Member Descriptions
--
-- -   @type@ is the 'OpenXR.Core10.Enums.StructureType.StructureType' of
--     this structure.
--
-- -   @next@ is @NULL@ or a pointer to the next structure in a structure
--     chain. No such structures are defined in core OpenXR or this
--     extension.
--
-- -   @visible@ is an
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >
--     which indicates if @session@ is now visible or is not.
--
-- -   @flags@ is 0 or one or more 'OverlayMainSessionFlagBitsEXTX' which
--     indicates various state information for the main session.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrEventDataMainSessionVisibilityChangedEXTX-extension-notenabled#
--     The @XR_EXTX_overlay@ extension /must/ be enabled prior to using
--     'EventDataMainSessionVisibilityChangedEXTX'
--
-- -   #VUID-XrEventDataMainSessionVisibilityChangedEXTX-type-type# @type@
--     /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX'
--
-- -   #VUID-XrEventDataMainSessionVisibilityChangedEXTX-next-next# @next@
--     /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrEventDataMainSessionVisibilityChangedEXTX-flags-parameter#
--     @flags@ /must/ be a valid combination of
--     'OverlayMainSessionFlagBitsEXTX' values
--
-- -   #VUID-XrEventDataMainSessionVisibilityChangedEXTX-flags-requiredbitmask#
--     @flags@ /must/ not be @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'OverlayMainSessionFlagsEXTX',
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data EventDataMainSessionVisibilityChangedEXTX = EventDataMainSessionVisibilityChangedEXTX
  { -- No documentation found for Nested "XrEventDataMainSessionVisibilityChangedEXTX" "visible"
    visible :: Bool
  , -- No documentation found for Nested "XrEventDataMainSessionVisibilityChangedEXTX" "flags"
    flags :: OverlayMainSessionFlagsEXTX
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventDataMainSessionVisibilityChangedEXTX)
#endif
deriving instance Show EventDataMainSessionVisibilityChangedEXTX

instance IsEventData EventDataMainSessionVisibilityChangedEXTX where
  toEventDataBaseHeader EventDataMainSessionVisibilityChangedEXTX{} = EventDataBaseHeader{type' = TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX}

instance ToCStruct EventDataMainSessionVisibilityChangedEXTX where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventDataMainSessionVisibilityChangedEXTX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (visible))
    poke ((p `plusPtr` 24 :: Ptr OverlayMainSessionFlagsEXTX)) (flags)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr OverlayMainSessionFlagsEXTX)) (zero)
    f

instance FromCStruct EventDataMainSessionVisibilityChangedEXTX where
  peekCStruct p = do
    visible <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    flags <- peek @OverlayMainSessionFlagsEXTX ((p `plusPtr` 24 :: Ptr OverlayMainSessionFlagsEXTX))
    pure $ EventDataMainSessionVisibilityChangedEXTX
             (bool32ToBool visible) flags

instance Storable EventDataMainSessionVisibilityChangedEXTX where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EventDataMainSessionVisibilityChangedEXTX where
  zero = EventDataMainSessionVisibilityChangedEXTX
           zero
           zero


type OverlayMainSessionFlagsEXTX = OverlayMainSessionFlagBitsEXTX

-- | XrOverlayMainSessionFlagBitsEXTX - XrOverlayMainSessionFlagBitsEXTX
--
-- = See Also
--
-- No cross-references are available
newtype OverlayMainSessionFlagBitsEXTX = OverlayMainSessionFlagBitsEXTX Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "XrOverlayMainSessionFlagBitsEXTX" "XR_OVERLAY_MAIN_SESSION_ENABLED_COMPOSITION_LAYER_INFO_DEPTH_BIT_EXTX"
pattern OVERLAY_MAIN_SESSION_ENABLED_COMPOSITION_LAYER_INFO_DEPTH_BIT_EXTX =
  OverlayMainSessionFlagBitsEXTX 0x0000000000000001

conNameOverlayMainSessionFlagBitsEXTX :: String
conNameOverlayMainSessionFlagBitsEXTX = "OverlayMainSessionFlagBitsEXTX"

enumPrefixOverlayMainSessionFlagBitsEXTX :: String
enumPrefixOverlayMainSessionFlagBitsEXTX = "OVERLAY_MAIN_SESSION_ENABLED_COMPOSITION_LAYER_INFO_DEPTH_BIT_EXTX"

showTableOverlayMainSessionFlagBitsEXTX :: [(OverlayMainSessionFlagBitsEXTX, String)]
showTableOverlayMainSessionFlagBitsEXTX = [(OVERLAY_MAIN_SESSION_ENABLED_COMPOSITION_LAYER_INFO_DEPTH_BIT_EXTX, "")]

instance Show OverlayMainSessionFlagBitsEXTX where
  showsPrec = enumShowsPrec enumPrefixOverlayMainSessionFlagBitsEXTX
                            showTableOverlayMainSessionFlagBitsEXTX
                            conNameOverlayMainSessionFlagBitsEXTX
                            (\(OverlayMainSessionFlagBitsEXTX x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read OverlayMainSessionFlagBitsEXTX where
  readPrec = enumReadPrec enumPrefixOverlayMainSessionFlagBitsEXTX
                          showTableOverlayMainSessionFlagBitsEXTX
                          conNameOverlayMainSessionFlagBitsEXTX
                          OverlayMainSessionFlagBitsEXTX


type OverlaySessionCreateFlagsEXTX = OverlaySessionCreateFlagBitsEXTX

-- | XrOverlaySessionCreateFlagBitsEXTX - XrOverlaySessionCreateFlagBitsEXTX
--
-- = See Also
--
-- No cross-references are available
newtype OverlaySessionCreateFlagBitsEXTX = OverlaySessionCreateFlagBitsEXTX Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "XrOverlaySessionCreateFlagBitsEXTX" "XR_OVERLAY_SESSION_CREATE_RELAXED_DISPLAY_TIME_BIT_EXTX"
pattern OVERLAY_SESSION_CREATE_RELAXED_DISPLAY_TIME_BIT_EXTX = OverlaySessionCreateFlagBitsEXTX 0x0000000000000001

conNameOverlaySessionCreateFlagBitsEXTX :: String
conNameOverlaySessionCreateFlagBitsEXTX = "OverlaySessionCreateFlagBitsEXTX"

enumPrefixOverlaySessionCreateFlagBitsEXTX :: String
enumPrefixOverlaySessionCreateFlagBitsEXTX = "OVERLAY_SESSION_CREATE_RELAXED_DISPLAY_TIME_BIT_EXTX"

showTableOverlaySessionCreateFlagBitsEXTX :: [(OverlaySessionCreateFlagBitsEXTX, String)]
showTableOverlaySessionCreateFlagBitsEXTX = [(OVERLAY_SESSION_CREATE_RELAXED_DISPLAY_TIME_BIT_EXTX, "")]

instance Show OverlaySessionCreateFlagBitsEXTX where
  showsPrec = enumShowsPrec enumPrefixOverlaySessionCreateFlagBitsEXTX
                            showTableOverlaySessionCreateFlagBitsEXTX
                            conNameOverlaySessionCreateFlagBitsEXTX
                            (\(OverlaySessionCreateFlagBitsEXTX x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read OverlaySessionCreateFlagBitsEXTX where
  readPrec = enumReadPrec enumPrefixOverlaySessionCreateFlagBitsEXTX
                          showTableOverlaySessionCreateFlagBitsEXTX
                          conNameOverlaySessionCreateFlagBitsEXTX
                          OverlaySessionCreateFlagBitsEXTX


type EXTX_overlay_SPEC_VERSION = 4

-- No documentation found for TopLevel "XR_EXTX_overlay_SPEC_VERSION"
pattern EXTX_overlay_SPEC_VERSION :: forall a . Integral a => a
pattern EXTX_overlay_SPEC_VERSION = 4


type EXTX_OVERLAY_EXTENSION_NAME = "XR_EXTX_overlay"

-- No documentation found for TopLevel "XR_EXTX_OVERLAY_EXTENSION_NAME"
pattern EXTX_OVERLAY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXTX_OVERLAY_EXTENSION_NAME = "XR_EXTX_overlay"

