{-# language CPP #-}
-- | = Name
--
-- XR_FB_color_space - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_FB_color_space  XR_FB_color_space>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 109
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
-- 'ColorSpaceFB', 'SystemColorSpacePropertiesFB',
-- 'enumerateColorSpacesFB', 'setColorSpaceFB'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_FB_color_space OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_FB_color_space  ( enumerateColorSpacesFB
                                            , setColorSpaceFB
                                            , SystemColorSpacePropertiesFB(..)
                                            , ColorSpaceFB( COLOR_SPACE_UNMANAGED_FB
                                                          , COLOR_SPACE_REC2020_FB
                                                          , COLOR_SPACE_REC709_FB
                                                          , COLOR_SPACE_RIFT_CV1_FB
                                                          , COLOR_SPACE_RIFT_S_FB
                                                          , COLOR_SPACE_QUEST_FB
                                                          , COLOR_SPACE_P3_FB
                                                          , COLOR_SPACE_ADOBE_RGB_FB
                                                          , ..
                                                          )
                                            , FB_color_space_SPEC_VERSION
                                            , pattern FB_color_space_SPEC_VERSION
                                            , FB_COLOR_SPACE_EXTENSION_NAME
                                            , pattern FB_COLOR_SPACE_EXTENSION_NAME
                                            ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Data.Vector (generateM)
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
import Data.Vector (Vector)
import OpenXR.CStruct.Utils (advancePtrBytes)
import OpenXR.NamedType ((:::))
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.Dynamic (InstanceCmds(pXrEnumerateColorSpacesFB))
import OpenXR.Dynamic (InstanceCmds(pXrSetColorSpaceFB))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero)
import OpenXR.Zero (Zero(..))
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SYSTEM_COLOR_SPACE_PROPERTIES_FB))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEnumerateColorSpacesFB
  :: FunPtr (Ptr Session_T -> Word32 -> Ptr Word32 -> Ptr ColorSpaceFB -> IO Result) -> Ptr Session_T -> Word32 -> Ptr Word32 -> Ptr ColorSpaceFB -> IO Result

-- | xrEnumerateColorSpacesFB - Enumerates color spaces
--
-- == Parameter Descriptions
--
-- -   @session@ is the session that enumerates the supported color spaces.
--
-- -   @colorSpaceCapacityInput@ is the capacity of the @colorSpaces@
--     array, or 0 to retrieve the required capacity.
--
-- -   @colorSpaceCountOutput@ is a pointer to the count of 'ColorSpaceFB'
--     @colorSpaces@ written, or a pointer to the required capacity in the
--     case that @colorSpaceCapacityInput@ is @0@.
--
-- -   @colorSpaces@ is a pointer to an array of 'ColorSpaceFB' color
--     spaces, but /can/ be @NULL@ if @colorSpaceCapacityInput@ is @0@.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @colorSpaces@ size.
--
-- = Description
--
-- 'enumerateColorSpacesFB' enumerates the color spaces supported by the
-- current session. Runtimes /must/ always return identical buffer contents
-- from this enumeration for the lifetime of the session.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrEnumerateColorSpacesFB-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'enumerateColorSpacesFB'
--
-- -   #VUID-xrEnumerateColorSpacesFB-session-parameter# @session@ /must/
--     be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrEnumerateColorSpacesFB-colorSpaceCountOutput-parameter#
--     @colorSpaceCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrEnumerateColorSpacesFB-colorSpaces-parameter# If
--     @colorSpaceCapacityInput@ is not @0@, @colorSpaces@ /must/ be a
--     pointer to an array of @colorSpaceCapacityInput@ 'ColorSpaceFB'
--     values
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'ColorSpaceFB', 'OpenXR.Core10.Handles.Session', 'setColorSpaceFB'
enumerateColorSpacesFB :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "xrEnumerateColorSpacesFB" "session"
                          Session
                       -> io (Result, ("colorSpaces" ::: Vector ColorSpaceFB))
enumerateColorSpacesFB session = liftIO . evalContT $ do
  let xrEnumerateColorSpacesFBPtr = pXrEnumerateColorSpacesFB (instanceCmds (session :: Session))
  lift $ unless (xrEnumerateColorSpacesFBPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEnumerateColorSpacesFB is null" Nothing Nothing
  let xrEnumerateColorSpacesFB' = mkXrEnumerateColorSpacesFB xrEnumerateColorSpacesFBPtr
  let session' = sessionHandle (session)
  pColorSpaceCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrEnumerateColorSpacesFB" (xrEnumerateColorSpacesFB' session' (0) (pColorSpaceCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  colorSpaceCountOutput <- lift $ peek @Word32 pColorSpaceCountOutput
  pColorSpaces <- ContT $ bracket (callocBytes @ColorSpaceFB ((fromIntegral (colorSpaceCountOutput)) * 4)) free
  r' <- lift $ traceAroundEvent "xrEnumerateColorSpacesFB" (xrEnumerateColorSpacesFB' session' ((colorSpaceCountOutput)) (pColorSpaceCountOutput) (pColorSpaces))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  colorSpaceCountOutput' <- lift $ peek @Word32 pColorSpaceCountOutput
  colorSpaces' <- lift $ generateM (fromIntegral (colorSpaceCountOutput')) (\i -> peek @ColorSpaceFB ((pColorSpaces `advancePtrBytes` (4 * (i)) :: Ptr ColorSpaceFB)))
  pure $ ((r'), colorSpaces')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrSetColorSpaceFB
  :: FunPtr (Ptr Session_T -> ColorSpaceFB -> IO Result) -> Ptr Session_T -> ColorSpaceFB -> IO Result

-- | xrSetColorSpaceFB - Set a color space
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'setColorSpaceFB' provides a mechanism for an application to specify the
-- color space used in the final rendered frame. If this function is not
-- called, the session will use the color space deemed appropriate by the
-- runtime. Facebook HMDs for both PC and Mobile product lines default to
-- 'COLOR_SPACE_RIFT_CV1_FB'. The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_COLOR_SPACE_UNSUPPORTED_FB' if
-- @colorSpace@ is not one of the values enumerated by
-- 'enumerateColorSpacesFB'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrSetColorSpaceFB-extension-notenabled# The @@ extension
--     /must/ be enabled prior to calling 'setColorSpaceFB'
--
-- -   #VUID-xrSetColorSpaceFB-session-parameter# @session@ /must/ be a
--     valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrSetColorSpaceFB-colorspace-parameter# @colorspace@ /must/ be
--     a valid 'ColorSpaceFB' value
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_COLOR_SPACE_UNSUPPORTED_FB'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FEATURE_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'ColorSpaceFB', 'OpenXR.Core10.Handles.Session',
-- 'enumerateColorSpacesFB'
setColorSpaceFB :: forall io
                 . (MonadIO io)
                => -- | @session@ is a valid 'OpenXR.Core10.Handles.Session' handle.
                   Session
                -> -- No documentation found for Nested "xrSetColorSpaceFB" "colorspace"
                   ColorSpaceFB
                -> io (Result)
setColorSpaceFB session colorspace = liftIO $ do
  let xrSetColorSpaceFBPtr = pXrSetColorSpaceFB (instanceCmds (session :: Session))
  unless (xrSetColorSpaceFBPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrSetColorSpaceFB is null" Nothing Nothing
  let xrSetColorSpaceFB' = mkXrSetColorSpaceFB xrSetColorSpaceFBPtr
  r <- traceAroundEvent "xrSetColorSpaceFB" (xrSetColorSpaceFB' (sessionHandle (session)) (colorspace))
  when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


-- | XrSystemColorSpacePropertiesFB - System property for color space
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSystemColorSpacePropertiesFB-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'SystemColorSpacePropertiesFB'
--
-- -   #VUID-XrSystemColorSpacePropertiesFB-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SYSTEM_COLOR_SPACE_PROPERTIES_FB'
--
-- -   #VUID-XrSystemColorSpacePropertiesFB-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSystemColorSpacePropertiesFB-colorSpace-parameter#
--     @colorSpace@ /must/ be a valid 'ColorSpaceFB' value
--
-- = See Also
--
-- 'ColorSpaceFB', 'OpenXR.Core10.Enums.StructureType.StructureType'
data SystemColorSpacePropertiesFB = SystemColorSpacePropertiesFB
  { -- | @colorSpace@ is the native color space of the XR device.
    colorSpace :: ColorSpaceFB }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SystemColorSpacePropertiesFB)
#endif
deriving instance Show SystemColorSpacePropertiesFB

instance ToCStruct SystemColorSpacePropertiesFB where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SystemColorSpacePropertiesFB{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SYSTEM_COLOR_SPACE_PROPERTIES_FB)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ColorSpaceFB)) (colorSpace)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SYSTEM_COLOR_SPACE_PROPERTIES_FB)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ColorSpaceFB)) (zero)
    f

instance FromCStruct SystemColorSpacePropertiesFB where
  peekCStruct p = do
    colorSpace <- peek @ColorSpaceFB ((p `plusPtr` 16 :: Ptr ColorSpaceFB))
    pure $ SystemColorSpacePropertiesFB
             colorSpace

instance Storable SystemColorSpacePropertiesFB where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SystemColorSpacePropertiesFB where
  zero = SystemColorSpacePropertiesFB
           zero


-- | XrColorSpaceFB - Color Space Type
--
-- == Enumerant Descriptions
--
-- = See Also
--
-- 'SystemColorSpacePropertiesFB', 'enumerateColorSpacesFB',
-- 'setColorSpaceFB'
newtype ColorSpaceFB = ColorSpaceFB Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COLOR_SPACE_UNMANAGED_FB'. No color correction, not recommended for
-- production use.
pattern COLOR_SPACE_UNMANAGED_FB = ColorSpaceFB 0
-- | 'COLOR_SPACE_REC2020_FB'. Standard Rec. 2020 chromacities. This is the
-- preferred color space for standardized color across all Oculus HMDs with
-- D65 white point.
pattern COLOR_SPACE_REC2020_FB   = ColorSpaceFB 1
-- | 'COLOR_SPACE_REC709_FB'. Standard Rec. 709 chromaticities, similar to
-- sRGB.
pattern COLOR_SPACE_REC709_FB    = ColorSpaceFB 2
-- | 'COLOR_SPACE_RIFT_CV1_FB'. Unique color space, between P3 and Adobe RGB
-- using D75 white point.
--
-- Color Space Details with Chromacity Primaries in CIE 1931 xy:
--
-- -   Red: (0.666, 0.334)
--
-- -   Green: (0.238, 0.714)
--
-- -   Blue: (0.139, 0.053)
--
-- -   White: (0.298, 0.318)
pattern COLOR_SPACE_RIFT_CV1_FB  = ColorSpaceFB 3
-- | 'COLOR_SPACE_RIFT_S_FB'. Unique color space. Similar to Rec 709 using
-- D75.
--
-- Color Space Details with Chromacity Primaries in CIE 1931 xy:
--
-- -   Red: (0.640, 0.330)
--
-- -   Green: (0.292, 0.586)
--
-- -   Blue: (0.156, 0.058)
--
-- -   White: (0.298, 0.318)
pattern COLOR_SPACE_RIFT_S_FB    = ColorSpaceFB 4
-- | 'COLOR_SPACE_QUEST_FB'. Unique color space. Similar to Rift CV1 using
-- D75 white point
--
-- Color Space Details with Chromacity Primaries in CIE 1931 xy:
--
-- -   Red: (0.661, 0.338)
--
-- -   Green: (0.228, 0.718)
--
-- -   Blue: (0.142, 0.042)
--
-- -   White: (0.298, 0.318)
pattern COLOR_SPACE_QUEST_FB     = ColorSpaceFB 5
-- | 'COLOR_SPACE_P3_FB'. Similar to DCI-P3, but uses D65 white point
-- instead.
--
-- Color Space Details with Chromacity Primaries in CIE 1931 xy:
--
-- -   Red: (0.680, 0.320)
--
-- -   Green: (0.265, 0.690)
--
-- -   Blue: (0.150, 0.060)
--
-- -   White: (0.313, 0.329)
pattern COLOR_SPACE_P3_FB        = ColorSpaceFB 6
-- | 'COLOR_SPACE_ADOBE_RGB_FB'. Standard Adobe chromacities.
pattern COLOR_SPACE_ADOBE_RGB_FB = ColorSpaceFB 7
{-# complete COLOR_SPACE_UNMANAGED_FB,
             COLOR_SPACE_REC2020_FB,
             COLOR_SPACE_REC709_FB,
             COLOR_SPACE_RIFT_CV1_FB,
             COLOR_SPACE_RIFT_S_FB,
             COLOR_SPACE_QUEST_FB,
             COLOR_SPACE_P3_FB,
             COLOR_SPACE_ADOBE_RGB_FB :: ColorSpaceFB #-}

conNameColorSpaceFB :: String
conNameColorSpaceFB = "ColorSpaceFB"

enumPrefixColorSpaceFB :: String
enumPrefixColorSpaceFB = "COLOR_SPACE_"

showTableColorSpaceFB :: [(ColorSpaceFB, String)]
showTableColorSpaceFB =
  [ (COLOR_SPACE_UNMANAGED_FB, "UNMANAGED_FB")
  , (COLOR_SPACE_REC2020_FB  , "REC2020_FB")
  , (COLOR_SPACE_REC709_FB   , "REC709_FB")
  , (COLOR_SPACE_RIFT_CV1_FB , "RIFT_CV1_FB")
  , (COLOR_SPACE_RIFT_S_FB   , "RIFT_S_FB")
  , (COLOR_SPACE_QUEST_FB    , "QUEST_FB")
  , (COLOR_SPACE_P3_FB       , "P3_FB")
  , (COLOR_SPACE_ADOBE_RGB_FB, "ADOBE_RGB_FB")
  ]

instance Show ColorSpaceFB where
  showsPrec = enumShowsPrec enumPrefixColorSpaceFB
                            showTableColorSpaceFB
                            conNameColorSpaceFB
                            (\(ColorSpaceFB x) -> x)
                            (showsPrec 11)

instance Read ColorSpaceFB where
  readPrec = enumReadPrec enumPrefixColorSpaceFB showTableColorSpaceFB conNameColorSpaceFB ColorSpaceFB


type FB_color_space_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_FB_color_space_SPEC_VERSION"
pattern FB_color_space_SPEC_VERSION :: forall a . Integral a => a
pattern FB_color_space_SPEC_VERSION = 1


type FB_COLOR_SPACE_EXTENSION_NAME = "XR_FB_color_space"

-- No documentation found for TopLevel "XR_FB_COLOR_SPACE_EXTENSION_NAME"
pattern FB_COLOR_SPACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern FB_COLOR_SPACE_EXTENSION_NAME = "XR_FB_color_space"

