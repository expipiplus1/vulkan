{-# language CPP #-}
-- | = Name
--
-- XR_KHR_loader_init - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_loader_init  XR_KHR_loader_init>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 89
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
-- 'LoaderInitInfoBaseHeaderKHR', 'initializeLoaderKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_loader_init OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_loader_init  ( initializeLoaderKHR
                                             , LoaderInitInfoBaseHeaderKHR(..)
                                             , IsLoaderInitInfoKHR(..)
                                             , KHR_loader_init_SPEC_VERSION
                                             , pattern KHR_loader_init_SPEC_VERSION
                                             , KHR_LOADER_INIT_EXTENSION_NAME
                                             , pattern KHR_LOADER_INIT_EXTENSION_NAME
                                             ) where

import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (castFunPtr)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
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
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Ptr (Ptr(Ptr))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.Dynamic (getInstanceProcAddr')
import OpenXR.NamedType ((:::))
import OpenXR.CStruct.Extends (Inheritable(..))
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_loader_init_android (LoaderInitInfoAndroidKHR)
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.CStruct.Extends (SomeChild)
import OpenXR.CStruct.Extends (SomeChild(..))
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_LOADER_INIT_INFO_ANDROID_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrInitializeLoaderKHR
  :: FunPtr (Ptr (SomeChild LoaderInitInfoBaseHeaderKHR) -> IO Result) -> Ptr (SomeChild LoaderInitInfoBaseHeaderKHR) -> IO Result

-- | xrInitializeLoaderKHR - Initializes loader
--
-- == Parameter Descriptions
--
-- == Description
--
-- = See Also
--
-- 'LoaderInitInfoBaseHeaderKHR'
initializeLoaderKHR :: forall a io
                     . (ToCStruct a, MonadIO io)
                    => -- | @loaderInitInfo@ is a pointer to an 'LoaderInitInfoBaseHeaderKHR'
                       -- structure, which is a polymorphic type defined by other platform- or
                       -- implementation-specific extensions.
                       ("loaderInitInfo" ::: a)
                    -> io ()
initializeLoaderKHR loaderInitInfo = liftIO . evalContT $ do
  xrInitializeLoaderKHRPtr <- lift $ castFunPtr @_ @(("loaderInitInfo" ::: Ptr (SomeChild LoaderInitInfoBaseHeaderKHR)) -> IO Result) <$> getInstanceProcAddr' nullPtr (Ptr "xrInitializeLoaderKHR"#)
  lift $ unless (xrInitializeLoaderKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrInitializeLoaderKHR is null" Nothing Nothing
  let xrInitializeLoaderKHR' = mkXrInitializeLoaderKHR xrInitializeLoaderKHRPtr
  loaderInitInfo' <- fmap castPtr $ ContT $ withCStruct (loaderInitInfo)
  r <- lift $ traceAroundEvent "xrInitializeLoaderKHR" (xrInitializeLoaderKHR' loaderInitInfo')
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))


-- | XrLoaderInitInfoBaseHeaderKHR - Initializes OpenXR loader
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrLoaderInitInfoBaseHeaderKHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'LoaderInitInfoBaseHeaderKHR'
--
-- -   #VUID-XrLoaderInitInfoBaseHeaderKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_LOADER_INIT_INFO_ANDROID_KHR'
--
-- -   #VUID-XrLoaderInitInfoBaseHeaderKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'initializeLoaderKHR'
data LoaderInitInfoBaseHeaderKHR = LoaderInitInfoBaseHeaderKHR
  { -- | @type@ is the 'OpenXR.Core10.Enums.StructureType.StructureType' of this
    -- structure. This base structure itself has no associated
    -- 'OpenXR.Core10.Enums.StructureType.StructureType' value.
    type' :: StructureType }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (LoaderInitInfoBaseHeaderKHR)
#endif
deriving instance Show LoaderInitInfoBaseHeaderKHR

class ToCStruct a => IsLoaderInitInfoKHR a where
  toLoaderInitInfoBaseHeaderKHR :: a -> LoaderInitInfoBaseHeaderKHR

instance Inheritable LoaderInitInfoBaseHeaderKHR where
  peekSomeCChild :: Ptr (SomeChild LoaderInitInfoBaseHeaderKHR) -> IO (SomeChild LoaderInitInfoBaseHeaderKHR)
  peekSomeCChild p = do
    ty <- peek @StructureType (castPtr @(SomeChild LoaderInitInfoBaseHeaderKHR) @StructureType p)
    case ty of
      TYPE_LOADER_INIT_INFO_ANDROID_KHR -> SomeChild <$> peekCStruct (castPtr @(SomeChild LoaderInitInfoBaseHeaderKHR) @LoaderInitInfoAndroidKHR p)
      c -> throwIO $
        IOError
          Nothing
          InvalidArgument
          "peekSomeCChild"
          ("Illegal struct inheritance of LoaderInitInfoBaseHeaderKHR with " <> show c)
          Nothing
          Nothing

instance ToCStruct LoaderInitInfoBaseHeaderKHR where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p LoaderInitInfoBaseHeaderKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (type')
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct LoaderInitInfoBaseHeaderKHR where
  peekCStruct p = do
    type' <- peek @StructureType ((p `plusPtr` 0 :: Ptr StructureType))
    pure $ LoaderInitInfoBaseHeaderKHR
             type'

instance Storable LoaderInitInfoBaseHeaderKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero LoaderInitInfoBaseHeaderKHR where
  zero = LoaderInitInfoBaseHeaderKHR
           zero


type KHR_loader_init_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_KHR_loader_init_SPEC_VERSION"
pattern KHR_loader_init_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_loader_init_SPEC_VERSION = 1


type KHR_LOADER_INIT_EXTENSION_NAME = "XR_KHR_loader_init"

-- No documentation found for TopLevel "XR_KHR_LOADER_INIT_EXTENSION_NAME"
pattern KHR_LOADER_INIT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_LOADER_INIT_EXTENSION_NAME = "XR_KHR_loader_init"

