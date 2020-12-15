{-# language CPP #-}
-- | = Name
--
-- XR_KHR_visibility_mask - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_visibility_mask  XR_KHR_visibility_mask>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 32
--
-- = Revision
--
-- 2
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'EventDataVisibilityMaskChangedKHR', 'VisibilityMaskKHR',
-- 'VisibilityMaskTypeKHR', 'getVisibilityMaskKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_visibility_mask OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_visibility_mask  ( getVisibilityMaskKHR
                                                 , EventDataVisibilityMaskChangedKHR(..)
                                                 , VisibilityMaskKHR(..)
                                                 , VisibilityMaskTypeKHR( VISIBILITY_MASK_TYPE_HIDDEN_TRIANGLE_MESH_KHR
                                                                        , VISIBILITY_MASK_TYPE_VISIBLE_TRIANGLE_MESH_KHR
                                                                        , VISIBILITY_MASK_TYPE_LINE_LOOP_KHR
                                                                        , ..
                                                                        )
                                                 , KHR_visibility_mask_SPEC_VERSION
                                                 , pattern KHR_visibility_mask_SPEC_VERSION
                                                 , KHR_VISIBILITY_MASK_EXTENSION_NAME
                                                 , pattern KHR_VISIBILITY_MASK_EXTENSION_NAME
                                                 ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero)
import OpenXR.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
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
import OpenXR.NamedType ((:::))
import OpenXR.Core10.OtherTypes (EventDataBaseHeader(..))
import OpenXR.Dynamic (InstanceCmds(pXrGetVisibilityMaskKHR))
import OpenXR.Core10.OtherTypes (IsEventData(..))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Input (Vector2f)
import OpenXR.Core10.Enums.ViewConfigurationType (ViewConfigurationType)
import OpenXR.Core10.Enums.ViewConfigurationType (ViewConfigurationType(..))
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_VISIBILITY_MASK_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetVisibilityMaskKHR
  :: FunPtr (Ptr Session_T -> ViewConfigurationType -> Word32 -> VisibilityMaskTypeKHR -> Ptr VisibilityMaskKHR -> IO Result) -> Ptr Session_T -> ViewConfigurationType -> Word32 -> VisibilityMaskTypeKHR -> Ptr VisibilityMaskKHR -> IO Result

-- | xrGetVisibilityMaskKHR - Gets visibility mask
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'getVisibilityMaskKHR' retrieves the view mask for a given view. This
-- function follows the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters two-call idiom>
-- for filling multiple buffers in a struct. Specifically, if either
-- @vertexCapacityInput@ or @indexCapacityInput@ is @0@, the runtime /must/
-- respond as if both fields were set to @0@, returning the vertex count
-- and index count through @vertexCountOutput@ or @indexCountOutput@
-- respectively. If a view mask for the specified view isn’t available, the
-- returned vertex and index counts /must/ be @0@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetVisibilityMaskKHR-extension-notenabled# The @@ extension
--     /must/ be enabled prior to calling 'getVisibilityMaskKHR'
--
-- -   #VUID-xrGetVisibilityMaskKHR-session-parameter# @session@ /must/ be
--     a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrGetVisibilityMaskKHR-viewConfigurationType-parameter#
--     @viewConfigurationType@ /must/ be a valid
--     'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
--     value
--
-- -   #VUID-xrGetVisibilityMaskKHR-visibilityMaskType-parameter#
--     @visibilityMaskType@ /must/ be a valid 'VisibilityMaskTypeKHR' value
--
-- -   #VUID-xrGetVisibilityMaskKHR-visibilityMask-parameter#
--     @visibilityMask@ /must/ be a pointer to an 'VisibilityMaskKHR'
--     structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session',
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType',
-- 'VisibilityMaskKHR', 'VisibilityMaskTypeKHR'
getVisibilityMaskKHR :: forall io
                      . (MonadIO io)
                     => -- | @session@ is an 'OpenXR.Core10.Handles.Session' handle previously
                        -- created with 'OpenXR.Core10.Device.createSession'.
                        Session
                     -> -- | @viewConfigurationType@ is the view configuration from which to retrieve
                        -- mask information.
                        ViewConfigurationType
                     -> -- | @viewIndex@ is the individual view within the view configuration from
                        -- which to retrieve mask information.
                        ("viewIndex" ::: Word32)
                     -> -- | @visibilityMaskType@ is the type of visibility mask requested.
                        VisibilityMaskTypeKHR
                     -> io (Result, VisibilityMaskKHR)
getVisibilityMaskKHR session viewConfigurationType viewIndex visibilityMaskType = liftIO . evalContT $ do
  let xrGetVisibilityMaskKHRPtr = pXrGetVisibilityMaskKHR (instanceCmds (session :: Session))
  lift $ unless (xrGetVisibilityMaskKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetVisibilityMaskKHR is null" Nothing Nothing
  let xrGetVisibilityMaskKHR' = mkXrGetVisibilityMaskKHR xrGetVisibilityMaskKHRPtr
  pVisibilityMask <- ContT (withZeroCStruct @VisibilityMaskKHR)
  r <- lift $ traceAroundEvent "xrGetVisibilityMaskKHR" (xrGetVisibilityMaskKHR' (sessionHandle (session)) (viewConfigurationType) (viewIndex) (visibilityMaskType) (pVisibilityMask))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  visibilityMask <- lift $ peekCStruct @VisibilityMaskKHR pVisibilityMask
  pure $ (r, visibilityMask)


-- | XrEventDataVisibilityMaskChangedKHR - Visibility Mask
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrEventDataVisibilityMaskChangedKHR-extension-notenabled# The
--     @@ extension /must/ be enabled prior to using
--     'EventDataVisibilityMaskChangedKHR'
--
-- -   #VUID-XrEventDataVisibilityMaskChangedKHR-type-type# @type@ /must/
--     be
--     'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR'
--
-- -   #VUID-XrEventDataVisibilityMaskChangedKHR-next-next# @next@ /must/
--     be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrEventDataVisibilityMaskChangedKHR-session-parameter#
--     @session@ /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-XrEventDataVisibilityMaskChangedKHR-viewConfigurationType-parameter#
--     @viewConfigurationType@ /must/ be a valid
--     'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
--     value
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
data EventDataVisibilityMaskChangedKHR = EventDataVisibilityMaskChangedKHR
  { -- | @session@ is the 'OpenXR.Core10.Handles.Session' for which the view mask
    -- has changed.
    session :: Ptr Session_T
  , -- | @viewConfigurationType@ is the view configuration whose mask has
    -- changed.
    viewConfigurationType :: ViewConfigurationType
  , -- | @viewIndex@ is the individual view within the view configuration to
    -- which the change refers.
    viewIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventDataVisibilityMaskChangedKHR)
#endif
deriving instance Show EventDataVisibilityMaskChangedKHR

instance IsEventData EventDataVisibilityMaskChangedKHR where
  toEventDataBaseHeader EventDataVisibilityMaskChangedKHR{} = EventDataBaseHeader{type' = TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR}

instance ToCStruct EventDataVisibilityMaskChangedKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventDataVisibilityMaskChangedKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Session_T))) (session)
    poke ((p `plusPtr` 24 :: Ptr ViewConfigurationType)) (viewConfigurationType)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (viewIndex)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Session_T))) (zero)
    poke ((p `plusPtr` 24 :: Ptr ViewConfigurationType)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct EventDataVisibilityMaskChangedKHR where
  peekCStruct p = do
    session <- peek @(Ptr Session_T) ((p `plusPtr` 16 :: Ptr (Ptr Session_T)))
    viewConfigurationType <- peek @ViewConfigurationType ((p `plusPtr` 24 :: Ptr ViewConfigurationType))
    viewIndex <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ EventDataVisibilityMaskChangedKHR
             session viewConfigurationType viewIndex

instance Storable EventDataVisibilityMaskChangedKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EventDataVisibilityMaskChangedKHR where
  zero = EventDataVisibilityMaskChangedKHR
           zero
           zero
           zero


-- | XrVisibilityMaskKHR - Visibility Mask
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
-- -   @vertexCapacityInput@ is the capacity of the @vertices@ array, or
--     @0@ to indicate a request to retrieve the required capacity.
--
-- -   @vertexCountOutput@ is filled in by the runtime with the count of
--     vertices written or the required capacity in the case that
--     @vertexCapacityInput@ or @indexCapacityInput@ is @0@.
--
-- -   @vertices@ is an array of vertices filled in by the runtime that
--     specifies mask coordinates in the z=-1 plane of the rendered
--     view—​i.e., one meter in front of the view. When rendering the mask
--     for use in a projection layer, these vertices must be transformed by
--     the application’s projection matrix used for the respective
--     'OpenXR.Core10.OtherTypes.CompositionLayerProjectionView'.
--
-- == Description
--
-- -   @indexCapacityInput@ is the capacity of the @indices@ array, or @0@
--     to indicate a request to retrieve the required capacity.
--
-- -   @indexCountOutput@ is filled in by the runtime with the count of
--     indices written or the required capacity in the case that
--     @vertexCapacityInput@ or @indexCapacityInput@ is @0@.
--
-- -   @indices@ is an array of indices filled in by the runtime,
--     specifying the indices of the mask geometry in the @vertices@ array.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrVisibilityMaskKHR-extension-notenabled# The @@ extension
--     /must/ be enabled prior to using 'VisibilityMaskKHR'
--
-- -   #VUID-XrVisibilityMaskKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_VISIBILITY_MASK_KHR'
--
-- -   #VUID-XrVisibilityMaskKHR-next-next# @next@ /must/ be @NULL@ or a
--     valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrVisibilityMaskKHR-vertices-parameter# If
--     @vertexCapacityInput@ is not @0@, @vertices@ /must/ be a pointer to
--     an array of @vertexCapacityInput@ 'OpenXR.Core10.Input.Vector2f'
--     structures
--
-- -   #VUID-XrVisibilityMaskKHR-indices-parameter# If @indexCapacityInput@
--     is not @0@, @indices@ /must/ be a pointer to an array of
--     @indexCapacityInput@ @uint32_t@ values
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Input.Vector2f', 'getVisibilityMaskKHR'
data VisibilityMaskKHR = VisibilityMaskKHR
  { -- No documentation found for Nested "XrVisibilityMaskKHR" "vertexCapacityInput"
    vertexCapacityInput :: Word32
  , -- No documentation found for Nested "XrVisibilityMaskKHR" "vertexCountOutput"
    vertexCountOutput :: Word32
  , -- No documentation found for Nested "XrVisibilityMaskKHR" "vertices"
    vertices :: Ptr Vector2f
  , -- No documentation found for Nested "XrVisibilityMaskKHR" "indexCapacityInput"
    indexCapacityInput :: Word32
  , -- No documentation found for Nested "XrVisibilityMaskKHR" "indexCountOutput"
    indexCountOutput :: Word32
  , -- No documentation found for Nested "XrVisibilityMaskKHR" "indices"
    indices :: Ptr Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VisibilityMaskKHR)
#endif
deriving instance Show VisibilityMaskKHR

instance ToCStruct VisibilityMaskKHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VisibilityMaskKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VISIBILITY_MASK_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (vertexCapacityInput)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (vertexCountOutput)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Vector2f))) (vertices)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (indexCapacityInput)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (indexCountOutput)
    poke ((p `plusPtr` 40 :: Ptr (Ptr Word32))) (indices)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VISIBILITY_MASK_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct VisibilityMaskKHR where
  peekCStruct p = do
    vertexCapacityInput <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    vertexCountOutput <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    vertices <- peek @(Ptr Vector2f) ((p `plusPtr` 24 :: Ptr (Ptr Vector2f)))
    indexCapacityInput <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    indexCountOutput <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    indices <- peek @(Ptr Word32) ((p `plusPtr` 40 :: Ptr (Ptr Word32)))
    pure $ VisibilityMaskKHR
             vertexCapacityInput vertexCountOutput vertices indexCapacityInput indexCountOutput indices

instance Storable VisibilityMaskKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VisibilityMaskKHR where
  zero = VisibilityMaskKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | XrVisibilityMaskTypeKHR - Visibility Mask Type
--
-- == Enumerant Descriptions
--
-- = See Also
--
-- 'getVisibilityMaskKHR'
newtype VisibilityMaskTypeKHR = VisibilityMaskTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'VISIBILITY_MASK_TYPE_HIDDEN_TRIANGLE_MESH_KHR' refers to a two
-- dimensional triangle mesh on the view surface which /should/ not be
-- drawn to by the application. 'VisibilityMaskKHR' refers to a set of
-- triangles identified by vertices and vertex indices. The index count
-- will thus be a multiple of three. The triangle vertices will be returned
-- in counter-clockwise order as viewed from the user perspective.
pattern VISIBILITY_MASK_TYPE_HIDDEN_TRIANGLE_MESH_KHR  = VisibilityMaskTypeKHR 1
-- | 'VISIBILITY_MASK_TYPE_VISIBLE_TRIANGLE_MESH_KHR' refers to a two
-- dimensional triangle mesh on the view surface which /should/ be drawn to
-- by the application. 'VisibilityMaskKHR' refers to a set of triangles
-- identified by vertices and vertex indices. The index count will thus be
-- a multiple of three. The triangle vertices will be returned in
-- counter-clockwise order as viewed from the user perspective.
pattern VISIBILITY_MASK_TYPE_VISIBLE_TRIANGLE_MESH_KHR = VisibilityMaskTypeKHR 2
-- | 'VISIBILITY_MASK_TYPE_LINE_LOOP_KHR' refers to a single multi-segmented
-- line loop on the view surface which encompasses the view area which
-- /should/ be drawn by the application. It is the border that exists
-- between the visible and hidden meshes identified by
-- 'VISIBILITY_MASK_TYPE_HIDDEN_TRIANGLE_MESH_KHR' and
-- 'VISIBILITY_MASK_TYPE_VISIBLE_TRIANGLE_MESH_KHR'. The line is
-- counter-clockwise, contiguous, and non-self crossing, with the last
-- point implicitly connecting to the first point. There is one vertex per
-- point, the index count will equal the vertex count, and the indices will
-- refer to the vertices.
pattern VISIBILITY_MASK_TYPE_LINE_LOOP_KHR             = VisibilityMaskTypeKHR 3
{-# complete VISIBILITY_MASK_TYPE_HIDDEN_TRIANGLE_MESH_KHR,
             VISIBILITY_MASK_TYPE_VISIBLE_TRIANGLE_MESH_KHR,
             VISIBILITY_MASK_TYPE_LINE_LOOP_KHR :: VisibilityMaskTypeKHR #-}

conNameVisibilityMaskTypeKHR :: String
conNameVisibilityMaskTypeKHR = "VisibilityMaskTypeKHR"

enumPrefixVisibilityMaskTypeKHR :: String
enumPrefixVisibilityMaskTypeKHR = "VISIBILITY_MASK_TYPE_"

showTableVisibilityMaskTypeKHR :: [(VisibilityMaskTypeKHR, String)]
showTableVisibilityMaskTypeKHR =
  [ (VISIBILITY_MASK_TYPE_HIDDEN_TRIANGLE_MESH_KHR , "HIDDEN_TRIANGLE_MESH_KHR")
  , (VISIBILITY_MASK_TYPE_VISIBLE_TRIANGLE_MESH_KHR, "VISIBLE_TRIANGLE_MESH_KHR")
  , (VISIBILITY_MASK_TYPE_LINE_LOOP_KHR            , "LINE_LOOP_KHR")
  ]

instance Show VisibilityMaskTypeKHR where
  showsPrec = enumShowsPrec enumPrefixVisibilityMaskTypeKHR
                            showTableVisibilityMaskTypeKHR
                            conNameVisibilityMaskTypeKHR
                            (\(VisibilityMaskTypeKHR x) -> x)
                            (showsPrec 11)

instance Read VisibilityMaskTypeKHR where
  readPrec = enumReadPrec enumPrefixVisibilityMaskTypeKHR
                          showTableVisibilityMaskTypeKHR
                          conNameVisibilityMaskTypeKHR
                          VisibilityMaskTypeKHR


type KHR_visibility_mask_SPEC_VERSION = 2

-- No documentation found for TopLevel "XR_KHR_visibility_mask_SPEC_VERSION"
pattern KHR_visibility_mask_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_visibility_mask_SPEC_VERSION = 2


type KHR_VISIBILITY_MASK_EXTENSION_NAME = "XR_KHR_visibility_mask"

-- No documentation found for TopLevel "XR_KHR_VISIBILITY_MASK_EXTENSION_NAME"
pattern KHR_VISIBILITY_MASK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_VISIBILITY_MASK_EXTENSION_NAME = "XR_KHR_visibility_mask"

