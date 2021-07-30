{-# language CPP #-}
-- | = Name
--
-- XR_KHR_D3D12_enable - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_D3D12_enable  XR_KHR_D3D12_enable>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 29
--
-- = Revision
--
-- 7
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'GraphicsBindingD3D12KHR', 'GraphicsRequirementsD3D12KHR',
-- 'SwapchainImageD3D12KHR', 'getD3D12GraphicsRequirementsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_D3D12_enable OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_D3D12_enable  ( getD3D12GraphicsRequirementsKHR
                                              , GraphicsBindingD3D12KHR(..)
                                              , SwapchainImageD3D12KHR(..)
                                              , GraphicsRequirementsD3D12KHR(..)
                                              , KHR_D3D12_enable_SPEC_VERSION
                                              , pattern KHR_D3D12_enable_SPEC_VERSION
                                              , KHR_D3D12_ENABLE_EXTENSION_NAME
                                              , pattern KHR_D3D12_ENABLE_EXTENSION_NAME
                                              , ID3D12CommandQueue
                                              , ID3D12Device
                                              , ID3D12Resource
                                              , LUID
                                              , D3D_FEATURE_LEVEL
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.Extensions.XR_KHR_D3D11_enable (D3D_FEATURE_LEVEL)
import OpenXR.Core10.Handles (Instance)
import OpenXR.Core10.Handles (Instance(..))
import OpenXR.Dynamic (InstanceCmds(pXrGetD3D12GraphicsRequirementsKHR))
import OpenXR.Core10.Handles (Instance_T)
import OpenXR.Core10.Image (IsSwapchainImage(..))
import OpenXR.Extensions.XR_KHR_D3D11_enable (LUID)
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Image (SwapchainImageBaseHeader(..))
import OpenXR.Core10.Device (SystemId)
import OpenXR.Core10.Device (SystemId(..))
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_BINDING_D3D12_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_REQUIREMENTS_D3D12_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_D3D12_KHR))
import OpenXR.Extensions.XR_KHR_D3D11_enable (D3D_FEATURE_LEVEL)
import OpenXR.Extensions.XR_KHR_D3D11_enable (LUID)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetD3D12GraphicsRequirementsKHR
  :: FunPtr (Ptr Instance_T -> SystemId -> Ptr GraphicsRequirementsD3D12KHR -> IO Result) -> Ptr Instance_T -> SystemId -> Ptr GraphicsRequirementsD3D12KHR -> IO Result

-- | xrGetD3D12GraphicsRequirementsKHR - Retrieve the D3D12 feature level and
-- graphics device requirements for an instance and system
--
-- == Parameter Descriptions
--
-- = Description
--
-- The 'getD3D12GraphicsRequirementsKHR' function identifies to the
-- application what graphics device (Windows LUID) needs to be used and the
-- minimum feature level to use. The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING'
-- ('OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE' /may/ be returned
-- due to legacy behavior) on calls to 'OpenXR.Core10.Device.createSession'
-- if 'getD3D12GraphicsRequirementsKHR' has not been called for the same
-- @instance@ and @systemId@. The LUID and feature level that
-- 'getD3D12GraphicsRequirementsKHR' returns should be used to create the
-- 'ID3D12Device' that the application passes to
-- 'OpenXR.Core10.Device.createSession' in the 'GraphicsBindingD3D12KHR'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetD3D12GraphicsRequirementsKHR-extension-notenabled# The
--     @XR_KHR_D3D12_enable@ extension /must/ be enabled prior to calling
--     'getD3D12GraphicsRequirementsKHR'
--
-- -   #VUID-xrGetD3D12GraphicsRequirementsKHR-instance-parameter#
--     @instance@ /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrGetD3D12GraphicsRequirementsKHR-graphicsRequirements-parameter#
--     @graphicsRequirements@ /must/ be a pointer to an
--     'GraphicsRequirementsD3D12KHR' structure
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
-- 'GraphicsRequirementsD3D12KHR', 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'OpenXR.Core10.Device.createSession'
getD3D12GraphicsRequirementsKHR :: forall io
                                 . (MonadIO io)
                                => -- | @instance@ is an 'OpenXR.Core10.Handles.Instance' handle previously
                                   -- created with 'OpenXR.Core10.Instance.createInstance'.
                                   Instance
                                -> -- | @systemId@ is an
                                   -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
                                   -- handle for the system which will be used to create a session.
                                   SystemId
                                -> io (GraphicsRequirementsD3D12KHR)
getD3D12GraphicsRequirementsKHR instance' systemId = liftIO . evalContT $ do
  let xrGetD3D12GraphicsRequirementsKHRPtr = pXrGetD3D12GraphicsRequirementsKHR (instanceCmds (instance' :: Instance))
  lift $ unless (xrGetD3D12GraphicsRequirementsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetD3D12GraphicsRequirementsKHR is null" Nothing Nothing
  let xrGetD3D12GraphicsRequirementsKHR' = mkXrGetD3D12GraphicsRequirementsKHR xrGetD3D12GraphicsRequirementsKHRPtr
  pGraphicsRequirements <- ContT (withZeroCStruct @GraphicsRequirementsD3D12KHR)
  r <- lift $ traceAroundEvent "xrGetD3D12GraphicsRequirementsKHR" (xrGetD3D12GraphicsRequirementsKHR' (instanceHandle (instance')) (systemId) (pGraphicsRequirements))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  graphicsRequirements <- lift $ peekCStruct @GraphicsRequirementsD3D12KHR pGraphicsRequirements
  pure $ (graphicsRequirements)


-- | XrGraphicsBindingD3D12KHR - The graphics binding structure to be passed
-- at session creation to use D3D12
--
-- == Member Descriptions
--
-- = Description
--
-- When creating a D3D12-backed 'OpenXR.Core10.Handles.Session', the
-- application will provide a pointer to an 'GraphicsBindingD3D12KHR' in
-- the @next@ chain of the 'OpenXR.Core10.Device.SessionCreateInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsBindingD3D12KHR-extension-notenabled# The
--     @XR_KHR_D3D12_enable@ extension /must/ be enabled prior to using
--     'GraphicsBindingD3D12KHR'
--
-- -   #VUID-XrGraphicsBindingD3D12KHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_BINDING_D3D12_KHR'
--
-- -   #VUID-XrGraphicsBindingD3D12KHR-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrGraphicsBindingD3D12KHR-device-parameter# @device@ /must/ be
--     a pointer to an 'ID3D12Device' value
--
-- -   #VUID-XrGraphicsBindingD3D12KHR-queue-parameter# @queue@ /must/ be a
--     pointer to an 'ID3D12CommandQueue' value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Device.createSession'
data GraphicsBindingD3D12KHR = GraphicsBindingD3D12KHR
  { -- | @device@ is a pointer to a valid 'ID3D12Device' to use.
    device :: Ptr ID3D12Device
  , -- | @queue@ is a pointer to a valid 'ID3D12CommandQueue' to use.
    queue :: Ptr ID3D12CommandQueue
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsBindingD3D12KHR)
#endif
deriving instance Show GraphicsBindingD3D12KHR

instance ToCStruct GraphicsBindingD3D12KHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsBindingD3D12KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_D3D12_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ID3D12Device))) (device)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ID3D12CommandQueue))) (queue)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_D3D12_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ID3D12Device))) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ID3D12CommandQueue))) (zero)
    f

instance FromCStruct GraphicsBindingD3D12KHR where
  peekCStruct p = do
    device <- peek @(Ptr ID3D12Device) ((p `plusPtr` 16 :: Ptr (Ptr ID3D12Device)))
    queue <- peek @(Ptr ID3D12CommandQueue) ((p `plusPtr` 24 :: Ptr (Ptr ID3D12CommandQueue)))
    pure $ GraphicsBindingD3D12KHR
             device queue

instance Storable GraphicsBindingD3D12KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsBindingD3D12KHR where
  zero = GraphicsBindingD3D12KHR
           zero
           zero


-- | XrSwapchainImageD3D12KHR - D3D12-specific swapchain image structure
--
-- == Member Descriptions
--
-- = Description
--
-- If a given session was created with 'GraphicsBindingD3D12KHR', the
-- following conditions /must/ apply.
--
-- -   Calls to 'OpenXR.Core10.Image.enumerateSwapchainImages' on an
--     'OpenXR.Core10.Handles.Swapchain' in that session /must/ return an
--     array of 'SwapchainImageD3D12KHR' structures.
--
-- -   Whenever an OpenXR function accepts an
--     'OpenXR.Core10.Image.SwapchainImageBaseHeader' pointer as a
--     parameter in that session, the runtime /must/ also accept a pointer
--     to an 'SwapchainImageD3D12KHR'.
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
-- -   #VUID-XrSwapchainImageD3D12KHR-extension-notenabled# The
--     @XR_KHR_D3D12_enable@ extension /must/ be enabled prior to using
--     'SwapchainImageD3D12KHR'
--
-- -   #VUID-XrSwapchainImageD3D12KHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SWAPCHAIN_IMAGE_D3D12_KHR'
--
-- -   #VUID-XrSwapchainImageD3D12KHR-next-next# @next@ /must/ be @NULL@ or
--     a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSwapchainImageD3D12KHR-texture-parameter# @texture@ /must/
--     be a pointer to an 'ID3D12Resource' value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Image.SwapchainImageBaseHeader'
data SwapchainImageD3D12KHR = SwapchainImageD3D12KHR
  { -- | @texture@ is a pointer to a valid @ID3D12Texture2D@ to use.
    texture :: Ptr ID3D12Resource }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainImageD3D12KHR)
#endif
deriving instance Show SwapchainImageD3D12KHR

instance IsSwapchainImage SwapchainImageD3D12KHR where
  toSwapchainImageBaseHeader SwapchainImageD3D12KHR{} = SwapchainImageBaseHeader{type' = TYPE_SWAPCHAIN_IMAGE_D3D12_KHR}

instance ToCStruct SwapchainImageD3D12KHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainImageD3D12KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_D3D12_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ID3D12Resource))) (texture)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_D3D12_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ID3D12Resource))) (zero)
    f

instance FromCStruct SwapchainImageD3D12KHR where
  peekCStruct p = do
    texture <- peek @(Ptr ID3D12Resource) ((p `plusPtr` 16 :: Ptr (Ptr ID3D12Resource)))
    pure $ SwapchainImageD3D12KHR
             texture

instance Storable SwapchainImageD3D12KHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainImageD3D12KHR where
  zero = SwapchainImageD3D12KHR
           zero


-- | XrGraphicsRequirementsD3D12KHR - D3D12 feature level and LUID
-- requirements
--
-- == Member Descriptions
--
-- = Description
--
-- 'GraphicsRequirementsD3D12KHR' is populated by
-- 'getD3D12GraphicsRequirementsKHR'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsRequirementsD3D12KHR-extension-notenabled# The
--     @XR_KHR_D3D12_enable@ extension /must/ be enabled prior to using
--     'GraphicsRequirementsD3D12KHR'
--
-- -   #VUID-XrGraphicsRequirementsD3D12KHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_REQUIREMENTS_D3D12_KHR'
--
-- -   #VUID-XrGraphicsRequirementsD3D12KHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrGraphicsRequirementsD3D12KHR-adapterLuid-parameter#
--     @adapterLuid@ /must/ be a valid
--     'OpenXR.Extensions.XR_KHR_D3D11_enable.LUID' value
--
-- -   #VUID-XrGraphicsRequirementsD3D12KHR-minFeatureLevel-parameter#
--     @minFeatureLevel@ /must/ be a valid
--     'OpenXR.Extensions.XR_KHR_D3D11_enable.D3D_FEATURE_LEVEL' value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'getD3D12GraphicsRequirementsKHR'
data GraphicsRequirementsD3D12KHR = GraphicsRequirementsD3D12KHR
  { -- | @adapterLuid@ identifies what graphics device needs to be used.
    adapterLuid :: LUID
  , -- | @minFeatureLevel@ is the minimum feature level that the D3D12 device
    -- must be initialized with.
    minFeatureLevel :: D3D_FEATURE_LEVEL
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsRequirementsD3D12KHR)
#endif
deriving instance Show GraphicsRequirementsD3D12KHR

instance ToCStruct GraphicsRequirementsD3D12KHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsRequirementsD3D12KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_REQUIREMENTS_D3D12_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr LUID)) (adapterLuid)
    poke ((p `plusPtr` 24 :: Ptr D3D_FEATURE_LEVEL)) (minFeatureLevel)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_REQUIREMENTS_D3D12_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr LUID)) (zero)
    poke ((p `plusPtr` 24 :: Ptr D3D_FEATURE_LEVEL)) (zero)
    f

instance FromCStruct GraphicsRequirementsD3D12KHR where
  peekCStruct p = do
    adapterLuid <- peek @LUID ((p `plusPtr` 16 :: Ptr LUID))
    minFeatureLevel <- peek @D3D_FEATURE_LEVEL ((p `plusPtr` 24 :: Ptr D3D_FEATURE_LEVEL))
    pure $ GraphicsRequirementsD3D12KHR
             adapterLuid minFeatureLevel

instance Storable GraphicsRequirementsD3D12KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsRequirementsD3D12KHR where
  zero = GraphicsRequirementsD3D12KHR
           zero
           zero


type KHR_D3D12_enable_SPEC_VERSION = 7

-- No documentation found for TopLevel "XR_KHR_D3D12_enable_SPEC_VERSION"
pattern KHR_D3D12_enable_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_D3D12_enable_SPEC_VERSION = 7


type KHR_D3D12_ENABLE_EXTENSION_NAME = "XR_KHR_D3D12_enable"

-- No documentation found for TopLevel "XR_KHR_D3D12_ENABLE_EXTENSION_NAME"
pattern KHR_D3D12_ENABLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_D3D12_ENABLE_EXTENSION_NAME = "XR_KHR_D3D12_enable"


data ID3D12CommandQueue


data ID3D12Device


data ID3D12Resource

