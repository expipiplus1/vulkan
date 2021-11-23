{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_spatial_graph_bridge - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_spatial_graph_bridge  XR_MSFT_spatial_graph_bridge>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 50
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
-- 'SpatialGraphNodeSpaceCreateInfoMSFT', 'SpatialGraphNodeTypeMSFT',
-- 'createSpatialGraphNodeSpaceMSFT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_spatial_graph_bridge OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_spatial_graph_bridge  ( createSpatialGraphNodeSpaceMSFT
                                                       , withSpatialGraphNodeSpaceMSFT
                                                       , SpatialGraphNodeSpaceCreateInfoMSFT(..)
                                                       , SpatialGraphNodeTypeMSFT( SPATIAL_GRAPH_NODE_TYPE_STATIC_MSFT
                                                                                 , SPATIAL_GRAPH_NODE_TYPE_DYNAMIC_MSFT
                                                                                 , ..
                                                                                 )
                                                       , MSFT_spatial_graph_bridge_SPEC_VERSION
                                                       , pattern MSFT_spatial_graph_bridge_SPEC_VERSION
                                                       , MSFT_SPATIAL_GRAPH_BRIDGE_EXTENSION_NAME
                                                       , pattern MSFT_SPATIAL_GRAPH_BRIDGE_EXTENSION_NAME
                                                       ) where

import OpenXR.CStruct.Utils (FixedArray)
import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
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
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.Core10.Space (destroySpace)
import OpenXR.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import OpenXR.CStruct.Utils (pokeFixedLengthByteString)
import OpenXR.Dynamic (InstanceCmds(pXrCreateSpatialGraphNodeSpaceMSFT))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Space (Posef)
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session(Session))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.Core10.Handles (Space)
import OpenXR.Core10.Handles (Space(Space))
import OpenXR.Core10.Handles (Space_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SPATIAL_GRAPH_NODE_SPACE_CREATE_INFO_MSFT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateSpatialGraphNodeSpaceMSFT
  :: FunPtr (Ptr Session_T -> Ptr SpatialGraphNodeSpaceCreateInfoMSFT -> Ptr (Ptr Space_T) -> IO Result) -> Ptr Session_T -> Ptr SpatialGraphNodeSpaceCreateInfoMSFT -> Ptr (Ptr Space_T) -> IO Result

-- | xrCreateSpatialGraphNodeSpaceMSFT - Create an
-- 'OpenXR.Core10.Handles.Space' from a spatial graph node.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrCreateSpatialGraphNodeSpaceMSFT-extension-notenabled# The
--     @XR_MSFT_spatial_graph_bridge@ extension /must/ be enabled prior to
--     calling 'createSpatialGraphNodeSpaceMSFT'
--
-- -   #VUID-xrCreateSpatialGraphNodeSpaceMSFT-session-parameter# @session@
--     /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrCreateSpatialGraphNodeSpaceMSFT-createInfo-parameter#
--     @createInfo@ /must/ be a pointer to a valid
--     'SpatialGraphNodeSpaceCreateInfoMSFT' structure
--
-- -   #VUID-xrCreateSpatialGraphNodeSpaceMSFT-space-parameter# @space@
--     /must/ be a pointer to an 'OpenXR.Core10.Handles.Space' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_POSE_INVALID'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'OpenXR.Core10.Handles.Space',
-- 'SpatialGraphNodeSpaceCreateInfoMSFT'
createSpatialGraphNodeSpaceMSFT :: forall io
                                 . (MonadIO io)
                                => -- | @session@ is the 'OpenXR.Core10.Handles.Session' which will use the
                                   -- created space.
                                   Session
                                -> -- | @createInfo@ is an 'SpatialGraphNodeSpaceCreateInfoMSFT' specifying the
                                   -- space to be created.
                                   SpatialGraphNodeSpaceCreateInfoMSFT
                                -> io (Space)
createSpatialGraphNodeSpaceMSFT session createInfo = liftIO . evalContT $ do
  let cmds = case session of Session{instanceCmds} -> instanceCmds
  let xrCreateSpatialGraphNodeSpaceMSFTPtr = pXrCreateSpatialGraphNodeSpaceMSFT cmds
  lift $ unless (xrCreateSpatialGraphNodeSpaceMSFTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateSpatialGraphNodeSpaceMSFT is null" Nothing Nothing
  let xrCreateSpatialGraphNodeSpaceMSFT' = mkXrCreateSpatialGraphNodeSpaceMSFT xrCreateSpatialGraphNodeSpaceMSFTPtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pSpace <- ContT $ bracket (callocBytes @(Ptr Space_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateSpatialGraphNodeSpaceMSFT" (xrCreateSpatialGraphNodeSpaceMSFT' (sessionHandle (session)) createInfo' (pSpace))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  space <- lift $ peek @(Ptr Space_T) pSpace
  pure $ (((\h -> Space h cmds ) space))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSpatialGraphNodeSpaceMSFT' and 'destroySpace'
--
-- To ensure that 'destroySpace' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSpatialGraphNodeSpaceMSFT :: forall io r . MonadIO io => Session -> SpatialGraphNodeSpaceCreateInfoMSFT -> (io Space -> (Space -> io ()) -> r) -> r
withSpatialGraphNodeSpaceMSFT session createInfo b =
  b (createSpatialGraphNodeSpaceMSFT session createInfo)
    (\(o0) -> destroySpace o0)


-- | XrSpatialGraphNodeSpaceCreateInfoMSFT - The information to create space
-- from a spatial graph node.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSpatialGraphNodeSpaceCreateInfoMSFT-extension-notenabled#
--     The @XR_MSFT_spatial_graph_bridge@ extension /must/ be enabled prior
--     to using 'SpatialGraphNodeSpaceCreateInfoMSFT'
--
-- -   #VUID-XrSpatialGraphNodeSpaceCreateInfoMSFT-type-type# @type@ /must/
--     be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SPATIAL_GRAPH_NODE_SPACE_CREATE_INFO_MSFT'
--
-- -   #VUID-XrSpatialGraphNodeSpaceCreateInfoMSFT-next-next# @next@ /must/
--     be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSpatialGraphNodeSpaceCreateInfoMSFT-nodeType-parameter#
--     @nodeType@ /must/ be a valid 'SpatialGraphNodeTypeMSFT' value
--
-- = See Also
--
-- 'OpenXR.Core10.Space.Posef', 'SpatialGraphNodeTypeMSFT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'createSpatialGraphNodeSpaceMSFT'
data SpatialGraphNodeSpaceCreateInfoMSFT = SpatialGraphNodeSpaceCreateInfoMSFT
  { -- | @nodeType@ is an 'SpatialGraphNodeTypeMSFT' specifying the spatial node
    -- type.
    nodeType :: SpatialGraphNodeTypeMSFT
  , -- | @nodeId@ is a global unique identifier (a.k.a. GUID or 16 byte array),
    -- representing the spatial node that is being tracked.
    nodeId :: ByteString
  , -- | @pose@ is an 'OpenXR.Core10.Space.Posef' defining the position and
    -- orientation of the new space’s origin within the natural reference frame
    -- of the spatial graph node.
    pose :: Posef
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SpatialGraphNodeSpaceCreateInfoMSFT)
#endif
deriving instance Show SpatialGraphNodeSpaceCreateInfoMSFT

instance ToCStruct SpatialGraphNodeSpaceCreateInfoMSFT where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SpatialGraphNodeSpaceCreateInfoMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SPATIAL_GRAPH_NODE_SPACE_CREATE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SpatialGraphNodeTypeMSFT)) (nodeType)
    pokeFixedLengthByteString ((p `plusPtr` 20 :: Ptr (FixedArray 16 Word8))) (nodeId)
    poke ((p `plusPtr` 36 :: Ptr Posef)) (pose)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SPATIAL_GRAPH_NODE_SPACE_CREATE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SpatialGraphNodeTypeMSFT)) (zero)
    pokeFixedLengthByteString ((p `plusPtr` 20 :: Ptr (FixedArray 16 Word8))) (mempty)
    poke ((p `plusPtr` 36 :: Ptr Posef)) (zero)
    f

instance FromCStruct SpatialGraphNodeSpaceCreateInfoMSFT where
  peekCStruct p = do
    nodeType <- peek @SpatialGraphNodeTypeMSFT ((p `plusPtr` 16 :: Ptr SpatialGraphNodeTypeMSFT))
    nodeId <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 20 :: Ptr (FixedArray 16 Word8)))
    pose <- peekCStruct @Posef ((p `plusPtr` 36 :: Ptr Posef))
    pure $ SpatialGraphNodeSpaceCreateInfoMSFT
             nodeType nodeId pose

instance Storable SpatialGraphNodeSpaceCreateInfoMSFT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SpatialGraphNodeSpaceCreateInfoMSFT where
  zero = SpatialGraphNodeSpaceCreateInfoMSFT
           zero
           mempty
           zero


-- | XrSpatialGraphNodeTypeMSFT - The type of spatial graph node.
--
-- = Description
--
-- There are two types of spatial graph nodes: static and dynamic.
--
-- Static spatial nodes track the pose of a fixed location in the world
-- relative to reference spaces. The tracking of static nodes /may/ slowly
-- adjust the pose over time for better accuracy but the pose is relatively
-- stable in the short term, such as between rendering frames. For example,
-- a QR code tracking library can use a static node to represent the
-- location of the tracked QR code. Static spatial nodes are represented by
-- 'SPATIAL_GRAPH_NODE_TYPE_STATIC_MSFT'.
--
-- Dynamic spatial nodes track the pose of a physical object that moves
-- continuously relative to reference spaces. The pose of dynamic spatial
-- nodes /can/ be very different within the duration of a rendering frame.
-- It is important for the application to use the correct timestamp to
-- query the space location using 'OpenXR.Core10.Space.locateSpace'. For
-- example, a color camera mounted in front of a HMD is also tracked by the
-- HMD so a web camera library can use a dynamic node to represent the
-- camera location. Dynamic spatial nodes are represented by
-- 'SPATIAL_GRAPH_NODE_TYPE_DYNAMIC_MSFT'.
--
-- = See Also
--
-- 'SpatialGraphNodeSpaceCreateInfoMSFT'
newtype SpatialGraphNodeTypeMSFT = SpatialGraphNodeTypeMSFT Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "XrSpatialGraphNodeTypeMSFT" "XR_SPATIAL_GRAPH_NODE_TYPE_STATIC_MSFT"
pattern SPATIAL_GRAPH_NODE_TYPE_STATIC_MSFT  = SpatialGraphNodeTypeMSFT 1
-- No documentation found for Nested "XrSpatialGraphNodeTypeMSFT" "XR_SPATIAL_GRAPH_NODE_TYPE_DYNAMIC_MSFT"
pattern SPATIAL_GRAPH_NODE_TYPE_DYNAMIC_MSFT = SpatialGraphNodeTypeMSFT 2
{-# complete SPATIAL_GRAPH_NODE_TYPE_STATIC_MSFT,
             SPATIAL_GRAPH_NODE_TYPE_DYNAMIC_MSFT :: SpatialGraphNodeTypeMSFT #-}

conNameSpatialGraphNodeTypeMSFT :: String
conNameSpatialGraphNodeTypeMSFT = "SpatialGraphNodeTypeMSFT"

enumPrefixSpatialGraphNodeTypeMSFT :: String
enumPrefixSpatialGraphNodeTypeMSFT = "SPATIAL_GRAPH_NODE_TYPE_"

showTableSpatialGraphNodeTypeMSFT :: [(SpatialGraphNodeTypeMSFT, String)]
showTableSpatialGraphNodeTypeMSFT =
  [(SPATIAL_GRAPH_NODE_TYPE_STATIC_MSFT, "STATIC_MSFT"), (SPATIAL_GRAPH_NODE_TYPE_DYNAMIC_MSFT, "DYNAMIC_MSFT")]

instance Show SpatialGraphNodeTypeMSFT where
  showsPrec = enumShowsPrec enumPrefixSpatialGraphNodeTypeMSFT
                            showTableSpatialGraphNodeTypeMSFT
                            conNameSpatialGraphNodeTypeMSFT
                            (\(SpatialGraphNodeTypeMSFT x) -> x)
                            (showsPrec 11)

instance Read SpatialGraphNodeTypeMSFT where
  readPrec = enumReadPrec enumPrefixSpatialGraphNodeTypeMSFT
                          showTableSpatialGraphNodeTypeMSFT
                          conNameSpatialGraphNodeTypeMSFT
                          SpatialGraphNodeTypeMSFT


type MSFT_spatial_graph_bridge_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_MSFT_spatial_graph_bridge_SPEC_VERSION"
pattern MSFT_spatial_graph_bridge_SPEC_VERSION :: forall a . Integral a => a
pattern MSFT_spatial_graph_bridge_SPEC_VERSION = 1


type MSFT_SPATIAL_GRAPH_BRIDGE_EXTENSION_NAME = "XR_MSFT_spatial_graph_bridge"

-- No documentation found for TopLevel "XR_MSFT_SPATIAL_GRAPH_BRIDGE_EXTENSION_NAME"
pattern MSFT_SPATIAL_GRAPH_BRIDGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MSFT_SPATIAL_GRAPH_BRIDGE_EXTENSION_NAME = "XR_MSFT_spatial_graph_bridge"

