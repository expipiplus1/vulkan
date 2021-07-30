{-# language CPP #-}
-- | = Name
--
-- XR_KHR_opengl_es_enable - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_opengl_es_enable  XR_KHR_opengl_es_enable>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 25
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
-- 'GraphicsBindingOpenGLESAndroidKHR', 'GraphicsRequirementsOpenGLESKHR',
-- 'SwapchainImageOpenGLESKHR', 'getOpenGLESGraphicsRequirementsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_opengl_es_enable OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_opengl_es_enable  ( getOpenGLESGraphicsRequirementsKHR
                                                  , GraphicsBindingOpenGLESAndroidKHR(..)
                                                  , SwapchainImageOpenGLESKHR(..)
                                                  , GraphicsRequirementsOpenGLESKHR(..)
                                                  , KHR_opengl_es_enable_SPEC_VERSION
                                                  , pattern KHR_opengl_es_enable_SPEC_VERSION
                                                  , KHR_OPENGL_ES_ENABLE_EXTENSION_NAME
                                                  , pattern KHR_OPENGL_ES_ENABLE_EXTENSION_NAME
                                                  , EGLDisplay
                                                  , EGLConfig
                                                  , EGLContext
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.Core10.Handles (Instance)
import OpenXR.Core10.Handles (Instance(..))
import OpenXR.Dynamic (InstanceCmds(pXrGetOpenGLESGraphicsRequirementsKHR))
import OpenXR.Core10.Handles (Instance_T)
import OpenXR.Core10.Image (IsSwapchainImage(..))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Image (SwapchainImageBaseHeader(..))
import OpenXR.Core10.Device (SystemId)
import OpenXR.Core10.Device (SystemId(..))
import OpenXR.Version (Version)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetOpenGLESGraphicsRequirementsKHR
  :: FunPtr (Ptr Instance_T -> SystemId -> Ptr GraphicsRequirementsOpenGLESKHR -> IO Result) -> Ptr Instance_T -> SystemId -> Ptr GraphicsRequirementsOpenGLESKHR -> IO Result

-- | xrGetOpenGLESGraphicsRequirementsKHR - Retrieve the OpenGL ES version
-- requirements for an instance and system
--
-- == Parameter Descriptions
--
-- = Description
--
-- The 'getOpenGLESGraphicsRequirementsKHR' function identifies to the
-- application the minimum OpenGL ES version requirement and the highest
-- known tested OpenGL ES version. The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING'
-- ('OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE' /may/ be returned
-- due to legacy behavior) on calls to 'OpenXR.Core10.Device.createSession'
-- if 'getOpenGLESGraphicsRequirementsKHR' has not been called for the same
-- @instance@ and @systemId@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetOpenGLESGraphicsRequirementsKHR-extension-notenabled# The
--     @XR_KHR_opengl_es_enable@ extension /must/ be enabled prior to
--     calling 'getOpenGLESGraphicsRequirementsKHR'
--
-- -   #VUID-xrGetOpenGLESGraphicsRequirementsKHR-instance-parameter#
--     @instance@ /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrGetOpenGLESGraphicsRequirementsKHR-graphicsRequirements-parameter#
--     @graphicsRequirements@ /must/ be a pointer to an
--     'GraphicsRequirementsOpenGLESKHR' structure
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
-- 'GraphicsRequirementsOpenGLESKHR', 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
getOpenGLESGraphicsRequirementsKHR :: forall io
                                    . (MonadIO io)
                                   => -- | @instance@ is an 'OpenXR.Core10.Handles.Instance' handle previously
                                      -- created with 'OpenXR.Core10.Instance.createInstance'.
                                      Instance
                                   -> -- | @systemId@ is an
                                      -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
                                      -- handle for the system which will be used to create a session.
                                      SystemId
                                   -> io (GraphicsRequirementsOpenGLESKHR)
getOpenGLESGraphicsRequirementsKHR instance' systemId = liftIO . evalContT $ do
  let xrGetOpenGLESGraphicsRequirementsKHRPtr = pXrGetOpenGLESGraphicsRequirementsKHR (instanceCmds (instance' :: Instance))
  lift $ unless (xrGetOpenGLESGraphicsRequirementsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetOpenGLESGraphicsRequirementsKHR is null" Nothing Nothing
  let xrGetOpenGLESGraphicsRequirementsKHR' = mkXrGetOpenGLESGraphicsRequirementsKHR xrGetOpenGLESGraphicsRequirementsKHRPtr
  pGraphicsRequirements <- ContT (withZeroCStruct @GraphicsRequirementsOpenGLESKHR)
  r <- lift $ traceAroundEvent "xrGetOpenGLESGraphicsRequirementsKHR" (xrGetOpenGLESGraphicsRequirementsKHR' (instanceHandle (instance')) (systemId) (pGraphicsRequirements))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  graphicsRequirements <- lift $ peekCStruct @GraphicsRequirementsOpenGLESKHR pGraphicsRequirements
  pure $ (graphicsRequirements)


-- | XrGraphicsBindingOpenGLESAndroidKHR - The graphics binding structure to
-- be passed at session creation to use OpenGL ES on Android
--
-- == Member Descriptions
--
-- = Description
--
-- When creating an OpenGL ES-backed 'OpenXR.Core10.Handles.Session' on
-- Android, the application will provide a pointer to an
-- 'GraphicsBindingOpenGLESAndroidKHR' structure in the @next@ chain of the
-- 'OpenXR.Core10.Device.SessionCreateInfo'.
--
-- The required window system configuration define to expose this structure
-- type is
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_USE_PLATFORM_ANDROID XR_USE_PLATFORM_ANDROID>.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsBindingOpenGLESAndroidKHR-extension-notenabled# The
--     @XR_KHR_opengl_es_enable@ extension /must/ be enabled prior to using
--     'GraphicsBindingOpenGLESAndroidKHR'
--
-- -   #VUID-XrGraphicsBindingOpenGLESAndroidKHR-type-type# @type@ /must/
--     be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR'
--
-- -   #VUID-XrGraphicsBindingOpenGLESAndroidKHR-next-next# @next@ /must/
--     be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrGraphicsBindingOpenGLESAndroidKHR-display-parameter#
--     @display@ /must/ be a valid 'EGLDisplay' value
--
-- -   #VUID-XrGraphicsBindingOpenGLESAndroidKHR-config-parameter# @config@
--     /must/ be a valid 'EGLConfig' value
--
-- -   #VUID-XrGraphicsBindingOpenGLESAndroidKHR-context-parameter#
--     @context@ /must/ be a valid 'EGLContext' value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Device.createSession'
data GraphicsBindingOpenGLESAndroidKHR = GraphicsBindingOpenGLESAndroidKHR
  { -- | @display@ is a valid Android OpenGL ES 'EGLDisplay'.
    display :: EGLDisplay
  , -- | @config@ is a valid Android OpenGL ES 'EGLConfig'.
    config :: EGLConfig
  , -- | @context@ is a valid Android OpenGL ES 'EGLContext'.
    context :: EGLContext
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsBindingOpenGLESAndroidKHR)
#endif
deriving instance Show GraphicsBindingOpenGLESAndroidKHR

instance ToCStruct GraphicsBindingOpenGLESAndroidKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsBindingOpenGLESAndroidKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr EGLDisplay)) (display)
    poke ((p `plusPtr` 24 :: Ptr EGLConfig)) (config)
    poke ((p `plusPtr` 32 :: Ptr EGLContext)) (context)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr EGLDisplay)) (zero)
    poke ((p `plusPtr` 24 :: Ptr EGLConfig)) (zero)
    poke ((p `plusPtr` 32 :: Ptr EGLContext)) (zero)
    f

instance FromCStruct GraphicsBindingOpenGLESAndroidKHR where
  peekCStruct p = do
    display <- peek @EGLDisplay ((p `plusPtr` 16 :: Ptr EGLDisplay))
    config <- peek @EGLConfig ((p `plusPtr` 24 :: Ptr EGLConfig))
    context <- peek @EGLContext ((p `plusPtr` 32 :: Ptr EGLContext))
    pure $ GraphicsBindingOpenGLESAndroidKHR
             display config context

instance Storable GraphicsBindingOpenGLESAndroidKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsBindingOpenGLESAndroidKHR where
  zero = GraphicsBindingOpenGLESAndroidKHR
           zero
           zero
           zero


-- | XrSwapchainImageOpenGLESKHR - OpenGL ES-specific swapchain image
-- structure
--
-- == Member Descriptions
--
-- = Description
--
-- If a given session was created with a @XrGraphicsBindingOpenGLES*KHR@,
-- the following conditions /must/ apply.
--
-- -   Calls to 'OpenXR.Core10.Image.enumerateSwapchainImages' on an
--     'OpenXR.Core10.Handles.Swapchain' in that session /must/ return an
--     array of 'SwapchainImageOpenGLESKHR' structures.
--
-- -   Whenever an OpenXR function accepts an
--     'OpenXR.Core10.Image.SwapchainImageBaseHeader' pointer as a
--     parameter in that session, the runtime /must/ also accept a pointer
--     to an 'SwapchainImageOpenGLESKHR' structure.
--
-- The OpenXR runtime /must/ interpret the bottom-left corner of the
-- swapchain image as the coordinate origin unless specified otherwise by
-- extension functionality.
--
-- The OpenXR runtime /must/ interpret the swapchain images in a clip space
-- of positive Y pointing up, near Z plane at -1, and far Z plane at 1.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSwapchainImageOpenGLESKHR-extension-notenabled# The
--     @XR_KHR_opengl_es_enable@ extension /must/ be enabled prior to using
--     'SwapchainImageOpenGLESKHR'
--
-- -   #VUID-XrSwapchainImageOpenGLESKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR'
--
-- -   #VUID-XrSwapchainImageOpenGLESKHR-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Image.SwapchainImageBaseHeader'
data SwapchainImageOpenGLESKHR = SwapchainImageOpenGLESKHR
  { -- | @image@ is an index indicating the current OpenGL ES swapchain image to
    -- use.
    image :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainImageOpenGLESKHR)
#endif
deriving instance Show SwapchainImageOpenGLESKHR

instance IsSwapchainImage SwapchainImageOpenGLESKHR where
  toSwapchainImageBaseHeader SwapchainImageOpenGLESKHR{} = SwapchainImageBaseHeader{type' = TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR}

instance ToCStruct SwapchainImageOpenGLESKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainImageOpenGLESKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (image)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct SwapchainImageOpenGLESKHR where
  peekCStruct p = do
    image <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ SwapchainImageOpenGLESKHR
             image

instance Storable SwapchainImageOpenGLESKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainImageOpenGLESKHR where
  zero = SwapchainImageOpenGLESKHR
           zero


-- | XrGraphicsRequirementsOpenGLESKHR - OpenGL ES API version requirements
--
-- == Member Descriptions
--
-- = Description
--
-- 'GraphicsRequirementsOpenGLESKHR' is populated by
-- 'getOpenGLESGraphicsRequirementsKHR' with the runtime’s OpenGL ES API
-- version requirements.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsRequirementsOpenGLESKHR-extension-notenabled# The
--     @XR_KHR_opengl_es_enable@ extension /must/ be enabled prior to using
--     'GraphicsRequirementsOpenGLESKHR'
--
-- -   #VUID-XrGraphicsRequirementsOpenGLESKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR'
--
-- -   #VUID-XrGraphicsRequirementsOpenGLESKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrVersion >,
-- 'getOpenGLESGraphicsRequirementsKHR'
data GraphicsRequirementsOpenGLESKHR = GraphicsRequirementsOpenGLESKHR
  { -- | @minApiVersionSupported@ is the minimum version of OpenGL ES that the
    -- runtime supports. Uses 'OpenXR.Version.MAKE_VERSION' on major and minor
    -- API version, ignoring any patch version component.
    minApiVersionSupported :: Version
  , -- | @maxApiVersionSupported@ is the maximum version of OpenGL ES that the
    -- runtime has been tested on and is known to support. Newer OpenGL ES
    -- versions might work if they are compatible. Uses
    -- 'OpenXR.Version.MAKE_VERSION' on major and minor API version, ignoring
    -- any patch version component.
    maxApiVersionSupported :: Version
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsRequirementsOpenGLESKHR)
#endif
deriving instance Show GraphicsRequirementsOpenGLESKHR

instance ToCStruct GraphicsRequirementsOpenGLESKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsRequirementsOpenGLESKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Version)) (minApiVersionSupported)
    poke ((p `plusPtr` 24 :: Ptr Version)) (maxApiVersionSupported)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Version)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Version)) (zero)
    f

instance FromCStruct GraphicsRequirementsOpenGLESKHR where
  peekCStruct p = do
    minApiVersionSupported <- peek @Version ((p `plusPtr` 16 :: Ptr Version))
    maxApiVersionSupported <- peek @Version ((p `plusPtr` 24 :: Ptr Version))
    pure $ GraphicsRequirementsOpenGLESKHR
             minApiVersionSupported maxApiVersionSupported

instance Storable GraphicsRequirementsOpenGLESKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsRequirementsOpenGLESKHR where
  zero = GraphicsRequirementsOpenGLESKHR
           zero
           zero


type KHR_opengl_es_enable_SPEC_VERSION = 7

-- No documentation found for TopLevel "XR_KHR_opengl_es_enable_SPEC_VERSION"
pattern KHR_opengl_es_enable_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_opengl_es_enable_SPEC_VERSION = 7


type KHR_OPENGL_ES_ENABLE_EXTENSION_NAME = "XR_KHR_opengl_es_enable"

-- No documentation found for TopLevel "XR_KHR_OPENGL_ES_ENABLE_EXTENSION_NAME"
pattern KHR_OPENGL_ES_ENABLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_OPENGL_ES_ENABLE_EXTENSION_NAME = "XR_KHR_opengl_es_enable"


type EGLDisplay = Ptr ()


type EGLConfig = Ptr ()


type EGLContext = Ptr ()

