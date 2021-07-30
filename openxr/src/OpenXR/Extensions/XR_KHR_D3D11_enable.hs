{-# language CPP #-}
-- | = Name
--
-- XR_KHR_D3D11_enable - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_D3D11_enable  XR_KHR_D3D11_enable>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 28
--
-- = Revision
--
-- 5
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'GraphicsBindingD3D11KHR', 'GraphicsRequirementsD3D11KHR',
-- 'SwapchainImageD3D11KHR', 'getD3D11GraphicsRequirementsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_D3D11_enable OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_D3D11_enable  ( getD3D11GraphicsRequirementsKHR
                                              , GraphicsBindingD3D11KHR(..)
                                              , SwapchainImageD3D11KHR(..)
                                              , GraphicsRequirementsD3D11KHR(..)
                                              , KHR_D3D11_enable_SPEC_VERSION
                                              , pattern KHR_D3D11_enable_SPEC_VERSION
                                              , KHR_D3D11_ENABLE_EXTENSION_NAME
                                              , pattern KHR_D3D11_ENABLE_EXTENSION_NAME
                                              , LUID
                                              , D3D_FEATURE_LEVEL
                                              , ID3D11Device
                                              , ID3D11Texture2D
                                              ) where

import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
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
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.Core10.Handles (Instance)
import OpenXR.Core10.Handles (Instance(..))
import OpenXR.Dynamic (InstanceCmds(pXrGetD3D11GraphicsRequirementsKHR))
import OpenXR.Core10.Handles (Instance_T)
import OpenXR.Core10.Image (IsSwapchainImage(..))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Image (SwapchainImageBaseHeader(..))
import OpenXR.Core10.Device (SystemId)
import OpenXR.Core10.Device (SystemId(..))
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_BINDING_D3D11_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_REQUIREMENTS_D3D11_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_D3D11_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetD3D11GraphicsRequirementsKHR
  :: FunPtr (Ptr Instance_T -> SystemId -> Ptr GraphicsRequirementsD3D11KHR -> IO Result) -> Ptr Instance_T -> SystemId -> Ptr GraphicsRequirementsD3D11KHR -> IO Result

-- | xrGetD3D11GraphicsRequirementsKHR - Retrieve the D3D11 feature level and
-- graphics device requirements for an instance and system
--
-- == Parameter Descriptions
--
-- = Description
--
-- The 'getD3D11GraphicsRequirementsKHR' function identifies to the
-- application what graphics device (Windows LUID) needs to be used and the
-- minimum feature level to use. The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING'
-- ('OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE' /may/ be returned
-- due to legacy behavior) on calls to 'OpenXR.Core10.Device.createSession'
-- if 'getD3D11GraphicsRequirementsKHR' has not been called for the same
-- @instance@ and @systemId@. The LUID and feature level that
-- 'getD3D11GraphicsRequirementsKHR' returns should be used to create the
-- 'ID3D11Device' that the application passes to
-- 'OpenXR.Core10.Device.createSession' in the 'GraphicsBindingD3D11KHR'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetD3D11GraphicsRequirementsKHR-extension-notenabled# The
--     @XR_KHR_D3D11_enable@ extension /must/ be enabled prior to calling
--     'getD3D11GraphicsRequirementsKHR'
--
-- -   #VUID-xrGetD3D11GraphicsRequirementsKHR-instance-parameter#
--     @instance@ /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrGetD3D11GraphicsRequirementsKHR-graphicsRequirements-parameter#
--     @graphicsRequirements@ /must/ be a pointer to an
--     'GraphicsRequirementsD3D11KHR' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SYSTEM_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- = See Also
--
-- 'GraphicsRequirementsD3D11KHR', 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'OpenXR.Core10.Device.createSession'
getD3D11GraphicsRequirementsKHR :: forall io
                                 . (MonadIO io)
                                => -- | @instance@ is an 'OpenXR.Core10.Handles.Instance' handle previously
                                   -- created with 'OpenXR.Core10.Instance.createInstance'.
                                   Instance
                                -> -- | @systemId@ is an
                                   -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
                                   -- handle for the system which will be used to create a session.
                                   SystemId
                                -> io (GraphicsRequirementsD3D11KHR)
getD3D11GraphicsRequirementsKHR instance' systemId = liftIO . evalContT $ do
  let xrGetD3D11GraphicsRequirementsKHRPtr = pXrGetD3D11GraphicsRequirementsKHR (instanceCmds (instance' :: Instance))
  lift $ unless (xrGetD3D11GraphicsRequirementsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetD3D11GraphicsRequirementsKHR is null" Nothing Nothing
  let xrGetD3D11GraphicsRequirementsKHR' = mkXrGetD3D11GraphicsRequirementsKHR xrGetD3D11GraphicsRequirementsKHRPtr
  pGraphicsRequirements <- ContT (withZeroCStruct @GraphicsRequirementsD3D11KHR)
  r <- lift $ traceAroundEvent "xrGetD3D11GraphicsRequirementsKHR" (xrGetD3D11GraphicsRequirementsKHR' (instanceHandle (instance')) (systemId) (pGraphicsRequirements))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  graphicsRequirements <- lift $ peekCStruct @GraphicsRequirementsD3D11KHR pGraphicsRequirements
  pure $ (graphicsRequirements)


-- | XrGraphicsBindingD3D11KHR - The graphics binding structure to be passed
-- at session creation to use D3D11
--
-- == Member Descriptions
--
-- = Description
--
-- When creating a D3D11-backed 'OpenXR.Core10.Handles.Session', the
-- application will provide a pointer to an 'GraphicsBindingD3D11KHR' in
-- the @next@ chain of the 'OpenXR.Core10.Device.SessionCreateInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsBindingD3D11KHR-extension-notenabled# The
--     @XR_KHR_D3D11_enable@ extension /must/ be enabled prior to using
--     'GraphicsBindingD3D11KHR'
--
-- -   #VUID-XrGraphicsBindingD3D11KHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_BINDING_D3D11_KHR'
--
-- -   #VUID-XrGraphicsBindingD3D11KHR-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrGraphicsBindingD3D11KHR-device-parameter# @device@ /must/ be
--     a pointer to an 'ID3D11Device' value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Device.createSession'
data GraphicsBindingD3D11KHR = GraphicsBindingD3D11KHR
  { -- | @device@ is a pointer to a valid 'ID3D11Device' to use.
    device :: Ptr ID3D11Device }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsBindingD3D11KHR)
#endif
deriving instance Show GraphicsBindingD3D11KHR

instance ToCStruct GraphicsBindingD3D11KHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsBindingD3D11KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_D3D11_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ID3D11Device))) (device)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_D3D11_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ID3D11Device))) (zero)
    f

instance FromCStruct GraphicsBindingD3D11KHR where
  peekCStruct p = do
    device <- peek @(Ptr ID3D11Device) ((p `plusPtr` 16 :: Ptr (Ptr ID3D11Device)))
    pure $ GraphicsBindingD3D11KHR
             device

instance Storable GraphicsBindingD3D11KHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsBindingD3D11KHR where
  zero = GraphicsBindingD3D11KHR
           zero


-- | XrSwapchainImageD3D11KHR - D3D11-specific swapchain image structure
--
-- == Member Descriptions
--
-- = Description
--
-- If a given session was created with 'GraphicsBindingD3D11KHR', the
-- following conditions /must/ apply.
--
-- -   Calls to 'OpenXR.Core10.Image.enumerateSwapchainImages' on an
--     'OpenXR.Core10.Handles.Swapchain' in that session /must/ return an
--     array of 'SwapchainImageD3D11KHR' structures.
--
-- -   Whenever an OpenXR function accepts an
--     'OpenXR.Core10.Image.SwapchainImageBaseHeader' pointer as a
--     parameter in that session, the runtime /must/ also accept a pointer
--     to an 'SwapchainImageD3D11KHR'.
--
-- The OpenXR runtime /must/ interpret the top-left corner of the swapchain
-- image as the coordinate origin unless specified otherwise by extension
-- functionality.
--
-- The OpenXR runtime /must/ interpret the swapchain images in a clip space
-- of positive Y pointing up, near Z plane at 0, and far Z plane at 1.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSwapchainImageD3D11KHR-extension-notenabled# The
--     @XR_KHR_D3D11_enable@ extension /must/ be enabled prior to using
--     'SwapchainImageD3D11KHR'
--
-- -   #VUID-XrSwapchainImageD3D11KHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SWAPCHAIN_IMAGE_D3D11_KHR'
--
-- -   #VUID-XrSwapchainImageD3D11KHR-next-next# @next@ /must/ be @NULL@ or
--     a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSwapchainImageD3D11KHR-texture-parameter# @texture@ /must/
--     be a pointer to an 'ID3D11Texture2D' value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Image.SwapchainImageBaseHeader'
data SwapchainImageD3D11KHR = SwapchainImageD3D11KHR
  { -- | @texture@ is a pointer to a valid 'ID3D11Texture2D' to use.
    texture :: Ptr ID3D11Texture2D }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainImageD3D11KHR)
#endif
deriving instance Show SwapchainImageD3D11KHR

instance IsSwapchainImage SwapchainImageD3D11KHR where
  toSwapchainImageBaseHeader SwapchainImageD3D11KHR{} = SwapchainImageBaseHeader{type' = TYPE_SWAPCHAIN_IMAGE_D3D11_KHR}

instance ToCStruct SwapchainImageD3D11KHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainImageD3D11KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_D3D11_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ID3D11Texture2D))) (texture)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_D3D11_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ID3D11Texture2D))) (zero)
    f

instance FromCStruct SwapchainImageD3D11KHR where
  peekCStruct p = do
    texture <- peek @(Ptr ID3D11Texture2D) ((p `plusPtr` 16 :: Ptr (Ptr ID3D11Texture2D)))
    pure $ SwapchainImageD3D11KHR
             texture

instance Storable SwapchainImageD3D11KHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainImageD3D11KHR where
  zero = SwapchainImageD3D11KHR
           zero


-- | XrGraphicsRequirementsD3D11KHR - D3D11 feature level and LUID
-- requirements
--
-- == Member Descriptions
--
-- = Description
--
-- 'GraphicsRequirementsD3D11KHR' is populated by
-- 'getD3D11GraphicsRequirementsKHR'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsRequirementsD3D11KHR-extension-notenabled# The
--     @XR_KHR_D3D11_enable@ extension /must/ be enabled prior to using
--     'GraphicsRequirementsD3D11KHR'
--
-- -   #VUID-XrGraphicsRequirementsD3D11KHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_REQUIREMENTS_D3D11_KHR'
--
-- -   #VUID-XrGraphicsRequirementsD3D11KHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrGraphicsRequirementsD3D11KHR-adapterLuid-parameter#
--     @adapterLuid@ /must/ be a valid 'LUID' value
--
-- -   #VUID-XrGraphicsRequirementsD3D11KHR-minFeatureLevel-parameter#
--     @minFeatureLevel@ /must/ be a valid 'D3D_FEATURE_LEVEL' value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'getD3D11GraphicsRequirementsKHR'
data GraphicsRequirementsD3D11KHR = GraphicsRequirementsD3D11KHR
  { -- | @adapterLuid@ identifies what graphics device needs to be used.
    adapterLuid :: LUID
  , -- | @minFeatureLevel@ is the minimum feature level that the D3D11 device
    -- must be initialized with.
    minFeatureLevel :: D3D_FEATURE_LEVEL
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsRequirementsD3D11KHR)
#endif
deriving instance Show GraphicsRequirementsD3D11KHR

instance ToCStruct GraphicsRequirementsD3D11KHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsRequirementsD3D11KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_REQUIREMENTS_D3D11_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr LUID)) (adapterLuid)
    poke ((p `plusPtr` 24 :: Ptr D3D_FEATURE_LEVEL)) (minFeatureLevel)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_REQUIREMENTS_D3D11_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr LUID)) (zero)
    poke ((p `plusPtr` 24 :: Ptr D3D_FEATURE_LEVEL)) (zero)
    f

instance FromCStruct GraphicsRequirementsD3D11KHR where
  peekCStruct p = do
    adapterLuid <- peek @LUID ((p `plusPtr` 16 :: Ptr LUID))
    minFeatureLevel <- peek @D3D_FEATURE_LEVEL ((p `plusPtr` 24 :: Ptr D3D_FEATURE_LEVEL))
    pure $ GraphicsRequirementsD3D11KHR
             adapterLuid minFeatureLevel

instance Storable GraphicsRequirementsD3D11KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsRequirementsD3D11KHR where
  zero = GraphicsRequirementsD3D11KHR
           zero
           zero


type KHR_D3D11_enable_SPEC_VERSION = 5

-- No documentation found for TopLevel "XR_KHR_D3D11_enable_SPEC_VERSION"
pattern KHR_D3D11_enable_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_D3D11_enable_SPEC_VERSION = 5


type KHR_D3D11_ENABLE_EXTENSION_NAME = "XR_KHR_D3D11_enable"

-- No documentation found for TopLevel "XR_KHR_D3D11_ENABLE_EXTENSION_NAME"
pattern KHR_D3D11_ENABLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_D3D11_ENABLE_EXTENSION_NAME = "XR_KHR_D3D11_enable"


type LUID = Word64


type D3D_FEATURE_LEVEL = Word32


data ID3D11Device


data ID3D11Texture2D

