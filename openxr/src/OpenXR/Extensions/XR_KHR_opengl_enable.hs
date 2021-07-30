{-# language CPP #-}
-- | = Name
--
-- XR_KHR_opengl_enable - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_opengl_enable  XR_KHR_opengl_enable>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 24
--
-- = Revision
--
-- 9
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'GraphicsBindingOpenGLWaylandKHR', 'GraphicsBindingOpenGLWin32KHR',
-- 'GraphicsBindingOpenGLXcbKHR', 'GraphicsBindingOpenGLXlibKHR',
-- 'GraphicsRequirementsOpenGLKHR', 'SwapchainImageOpenGLKHR',
-- 'getOpenGLGraphicsRequirementsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_opengl_enable OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_opengl_enable  ( getOpenGLGraphicsRequirementsKHR
                                               , GraphicsBindingOpenGLWin32KHR(..)
                                               , GraphicsBindingOpenGLXlibKHR(..)
                                               , GraphicsBindingOpenGLXcbKHR(..)
                                               , GraphicsBindingOpenGLWaylandKHR(..)
                                               , SwapchainImageOpenGLKHR(..)
                                               , GraphicsRequirementsOpenGLKHR(..)
                                               , KHR_opengl_enable_SPEC_VERSION
                                               , pattern KHR_opengl_enable_SPEC_VERSION
                                               , KHR_OPENGL_ENABLE_EXTENSION_NAME
                                               , pattern KHR_OPENGL_ENABLE_EXTENSION_NAME
                                               , HDC
                                               , HGLRC
                                               , Display
                                               , Xcb_visualid_t
                                               , Xcb_glx_fbconfig_t
                                               , Xcb_glx_drawable_t
                                               , Xcb_glx_context_t
                                               , GLXFBConfig
                                               , GLXDrawable
                                               , GLXContext
                                               , Xcb_connection_t
                                               , Wl_display
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
import OpenXR.Dynamic (InstanceCmds(pXrGetOpenGLGraphicsRequirementsKHR))
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
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_BINDING_OPENGL_WAYLAND_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_BINDING_OPENGL_WIN32_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_BINDING_OPENGL_XCB_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_BINDING_OPENGL_XLIB_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_REQUIREMENTS_OPENGL_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetOpenGLGraphicsRequirementsKHR
  :: FunPtr (Ptr Instance_T -> SystemId -> Ptr GraphicsRequirementsOpenGLKHR -> IO Result) -> Ptr Instance_T -> SystemId -> Ptr GraphicsRequirementsOpenGLKHR -> IO Result

-- | xrGetOpenGLGraphicsRequirementsKHR - Retrieve the OpenGL version
-- requirements for an instance and system
--
-- == Parameter Descriptions
--
-- = Description
--
-- The 'getOpenGLGraphicsRequirementsKHR' function identifies to the
-- application the minimum OpenGL version requirement and the highest known
-- tested OpenGL version. The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING'
-- ('OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE' /may/ be returned
-- due to legacy behavior) on calls to 'OpenXR.Core10.Device.createSession'
-- if 'getOpenGLGraphicsRequirementsKHR' has not been called for the same
-- @instance@ and @systemId@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetOpenGLGraphicsRequirementsKHR-extension-notenabled# The
--     @XR_KHR_opengl_enable@ extension /must/ be enabled prior to calling
--     'getOpenGLGraphicsRequirementsKHR'
--
-- -   #VUID-xrGetOpenGLGraphicsRequirementsKHR-instance-parameter#
--     @instance@ /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrGetOpenGLGraphicsRequirementsKHR-graphicsRequirements-parameter#
--     @graphicsRequirements@ /must/ be a pointer to an
--     'GraphicsRequirementsOpenGLKHR' structure
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
-- 'GraphicsRequirementsOpenGLKHR', 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
getOpenGLGraphicsRequirementsKHR :: forall io
                                  . (MonadIO io)
                                 => -- | @instance@ is an 'OpenXR.Core10.Handles.Instance' handle previously
                                    -- created with 'OpenXR.Core10.Instance.createInstance'.
                                    Instance
                                 -> -- | @systemId@ is an
                                    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
                                    -- handle for the system which will be used to create a session.
                                    SystemId
                                 -> io (GraphicsRequirementsOpenGLKHR)
getOpenGLGraphicsRequirementsKHR instance' systemId = liftIO . evalContT $ do
  let xrGetOpenGLGraphicsRequirementsKHRPtr = pXrGetOpenGLGraphicsRequirementsKHR (instanceCmds (instance' :: Instance))
  lift $ unless (xrGetOpenGLGraphicsRequirementsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetOpenGLGraphicsRequirementsKHR is null" Nothing Nothing
  let xrGetOpenGLGraphicsRequirementsKHR' = mkXrGetOpenGLGraphicsRequirementsKHR xrGetOpenGLGraphicsRequirementsKHRPtr
  pGraphicsRequirements <- ContT (withZeroCStruct @GraphicsRequirementsOpenGLKHR)
  r <- lift $ traceAroundEvent "xrGetOpenGLGraphicsRequirementsKHR" (xrGetOpenGLGraphicsRequirementsKHR' (instanceHandle (instance')) (systemId) (pGraphicsRequirements))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  graphicsRequirements <- lift $ peekCStruct @GraphicsRequirementsOpenGLKHR pGraphicsRequirements
  pure $ (graphicsRequirements)


-- | XrGraphicsBindingOpenGLWin32KHR - The graphics binding structure to be
-- passed at session creation to use OpenGL on Windows
--
-- == Member Descriptions
--
-- = Description
--
-- When creating an OpenGL-backed 'OpenXR.Core10.Handles.Session' on
-- Microsoft Windows, the application will provide a pointer to an
-- 'GraphicsBindingOpenGLWin32KHR' in the @next@ chain of the
-- 'OpenXR.Core10.Device.SessionCreateInfo'. As no standardized way exists
-- for OpenGL to create the graphics context on a specific GPU, the runtime
-- /must/ assume that the application uses the operating systems default
-- GPU. If the GPU used by the runtime does not match the GPU on which the
-- OpenGL context of the application got created,
-- 'OpenXR.Core10.Device.createSession' /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_GRAPHICS_DEVICE_INVALID'.
--
-- The required window system configuration define to expose this structure
-- type is
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_USE_PLATFORM_WIN32 XR_USE_PLATFORM_WIN32>.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsBindingOpenGLWin32KHR-extension-notenabled# The
--     @XR_KHR_opengl_enable@ extension /must/ be enabled prior to using
--     'GraphicsBindingOpenGLWin32KHR'
--
-- -   #VUID-XrGraphicsBindingOpenGLWin32KHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_BINDING_OPENGL_WIN32_KHR'
--
-- -   #VUID-XrGraphicsBindingOpenGLWin32KHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrGraphicsBindingOpenGLWin32KHR-hDC-parameter# @hDC@ /must/ be
--     a valid 'HDC' value
--
-- -   #VUID-XrGraphicsBindingOpenGLWin32KHR-hGLRC-parameter# @hGLRC@
--     /must/ be a valid 'HGLRC' value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Device.createSession'
data GraphicsBindingOpenGLWin32KHR = GraphicsBindingOpenGLWin32KHR
  { -- | @hDC@ is a valid Windows HW device context handle.
    hDC :: HDC
  , -- | @hGLRC@ is a valid Windows OpenGL rendering context handle.
    hGLRC :: HGLRC
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsBindingOpenGLWin32KHR)
#endif
deriving instance Show GraphicsBindingOpenGLWin32KHR

instance ToCStruct GraphicsBindingOpenGLWin32KHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsBindingOpenGLWin32KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_OPENGL_WIN32_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr HDC)) (hDC)
    poke ((p `plusPtr` 24 :: Ptr HGLRC)) (hGLRC)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_OPENGL_WIN32_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr HDC)) (zero)
    poke ((p `plusPtr` 24 :: Ptr HGLRC)) (zero)
    f

instance FromCStruct GraphicsBindingOpenGLWin32KHR where
  peekCStruct p = do
    hDC <- peek @HDC ((p `plusPtr` 16 :: Ptr HDC))
    hGLRC <- peek @HGLRC ((p `plusPtr` 24 :: Ptr HGLRC))
    pure $ GraphicsBindingOpenGLWin32KHR
             hDC hGLRC

instance Storable GraphicsBindingOpenGLWin32KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsBindingOpenGLWin32KHR where
  zero = GraphicsBindingOpenGLWin32KHR
           zero
           zero


-- | XrGraphicsBindingOpenGLXlibKHR - The graphics binding structure to be
-- passed at session creation to use OpenGL on X11 via Xlib
--
-- == Member Descriptions
--
-- = Description
--
-- When creating an OpenGL-backed 'OpenXR.Core10.Handles.Session' on any
-- Linux\/Unix platform that utilizes X11 and GLX, via the Xlib library,
-- the application will provide a pointer to an
-- 'GraphicsBindingOpenGLXlibKHR' in the @next@ chain of the
-- 'OpenXR.Core10.Device.SessionCreateInfo'.
--
-- The required window system configuration define to expose this structure
-- type is
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_USE_PLATFORM_XLIB XR_USE_PLATFORM_XLIB>.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsBindingOpenGLXlibKHR-extension-notenabled# The
--     @XR_KHR_opengl_enable@ extension /must/ be enabled prior to using
--     'GraphicsBindingOpenGLXlibKHR'
--
-- -   #VUID-XrGraphicsBindingOpenGLXlibKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_BINDING_OPENGL_XLIB_KHR'
--
-- -   #VUID-XrGraphicsBindingOpenGLXlibKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrGraphicsBindingOpenGLXlibKHR-xDisplay-parameter# @xDisplay@
--     /must/ be a pointer to a 'Display' value
--
-- -   #VUID-XrGraphicsBindingOpenGLXlibKHR-glxFBConfig-parameter#
--     @glxFBConfig@ /must/ be a valid 'GLXFBConfig' value
--
-- -   #VUID-XrGraphicsBindingOpenGLXlibKHR-glxDrawable-parameter#
--     @glxDrawable@ /must/ be a valid 'GLXDrawable' value
--
-- -   #VUID-XrGraphicsBindingOpenGLXlibKHR-glxContext-parameter#
--     @glxContext@ /must/ be a valid 'GLXContext' value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Device.createSession'
data GraphicsBindingOpenGLXlibKHR = GraphicsBindingOpenGLXlibKHR
  { -- | @xDisplay@ is a valid X11 'Display'.
    xDisplay :: Ptr Display
  , -- | @visualid@ is a valid X11 visual identifier.
    visualid :: Word32
  , -- | @glxFBConfig@ is a valid X11 OpenGL GLX 'GLXFBConfig'.
    glxFBConfig :: GLXFBConfig
  , -- | @glxDrawable@ is a valid X11 OpenGL GLX 'GLXDrawable'.
    glxDrawable :: GLXDrawable
  , -- | @glxContext@ is a valid X11 OpenGL GLX 'GLXContext'.
    glxContext :: GLXContext
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsBindingOpenGLXlibKHR)
#endif
deriving instance Show GraphicsBindingOpenGLXlibKHR

instance ToCStruct GraphicsBindingOpenGLXlibKHR where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsBindingOpenGLXlibKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_OPENGL_XLIB_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Display))) (xDisplay)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (visualid)
    poke ((p `plusPtr` 32 :: Ptr GLXFBConfig)) (glxFBConfig)
    poke ((p `plusPtr` 40 :: Ptr GLXDrawable)) (glxDrawable)
    poke ((p `plusPtr` 48 :: Ptr GLXContext)) (glxContext)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_OPENGL_XLIB_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Display))) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr GLXFBConfig)) (zero)
    poke ((p `plusPtr` 40 :: Ptr GLXDrawable)) (zero)
    poke ((p `plusPtr` 48 :: Ptr GLXContext)) (zero)
    f

instance FromCStruct GraphicsBindingOpenGLXlibKHR where
  peekCStruct p = do
    xDisplay <- peek @(Ptr Display) ((p `plusPtr` 16 :: Ptr (Ptr Display)))
    visualid <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    glxFBConfig <- peek @GLXFBConfig ((p `plusPtr` 32 :: Ptr GLXFBConfig))
    glxDrawable <- peek @GLXDrawable ((p `plusPtr` 40 :: Ptr GLXDrawable))
    glxContext <- peek @GLXContext ((p `plusPtr` 48 :: Ptr GLXContext))
    pure $ GraphicsBindingOpenGLXlibKHR
             xDisplay visualid glxFBConfig glxDrawable glxContext

instance Storable GraphicsBindingOpenGLXlibKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsBindingOpenGLXlibKHR where
  zero = GraphicsBindingOpenGLXlibKHR
           zero
           zero
           zero
           zero
           zero


-- | XrGraphicsBindingOpenGLXcbKHR - The graphics binding structure to be
-- passed at session creation to use OpenGL on X11 via XCB
--
-- == Member Descriptions
--
-- = Description
--
-- When creating an OpenGL-backed 'OpenXR.Core10.Handles.Session' on any
-- Linux\/Unix platform that utilizes X11 and GLX, via the Xlib library,
-- the application will provide a pointer to an
-- 'GraphicsBindingOpenGLXcbKHR' in the @next@ chain of the
-- 'OpenXR.Core10.Device.SessionCreateInfo'.
--
-- The required window system configuration define to expose this structure
-- type is
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_USE_PLATFORM_XCB XR_USE_PLATFORM_XCB>.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsBindingOpenGLXcbKHR-extension-notenabled# The
--     @XR_KHR_opengl_enable@ extension /must/ be enabled prior to using
--     'GraphicsBindingOpenGLXcbKHR'
--
-- -   #VUID-XrGraphicsBindingOpenGLXcbKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_BINDING_OPENGL_XCB_KHR'
--
-- -   #VUID-XrGraphicsBindingOpenGLXcbKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrGraphicsBindingOpenGLXcbKHR-connection-parameter#
--     @connection@ /must/ be a pointer to an @xcb_connection_t@ value
--
-- -   #VUID-XrGraphicsBindingOpenGLXcbKHR-fbconfigid-parameter#
--     @fbconfigid@ /must/ be a valid @xcb_glx_fbconfig_t@ value
--
-- -   #VUID-XrGraphicsBindingOpenGLXcbKHR-visualid-parameter# @visualid@
--     /must/ be a valid @xcb_visualid_t@ value
--
-- -   #VUID-XrGraphicsBindingOpenGLXcbKHR-glxDrawable-parameter#
--     @glxDrawable@ /must/ be a valid @xcb_glx_drawable_t@ value
--
-- -   #VUID-XrGraphicsBindingOpenGLXcbKHR-glxContext-parameter#
--     @glxContext@ /must/ be a valid @xcb_glx_context_t@ value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Device.createSession'
data GraphicsBindingOpenGLXcbKHR = GraphicsBindingOpenGLXcbKHR
  { -- | @connection@ is a valid @xcb_connection_t@.
    connection :: Ptr Xcb_connection_t
  , -- | @screenNumber@ is an index indicating which screen should be used for
    -- rendering.
    screenNumber :: Word32
  , -- | @fbconfigid@ is a valid XCB OpenGL GLX @xcb_glx_fbconfig_t@.
    fbconfigid :: Xcb_glx_fbconfig_t
  , -- | @visualid@ is a valid XCB OpenGL GLX @xcb_visualid_t@.
    visualid :: Xcb_visualid_t
  , -- | @glxDrawable@ is a valid XCB OpenGL GLX @xcb_glx_drawable_t@.
    glxDrawable :: Xcb_glx_drawable_t
  , -- | @glxContext@ is a valid XCB OpenGL GLX @xcb_glx_context_t@.
    glxContext :: Xcb_glx_context_t
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsBindingOpenGLXcbKHR)
#endif
deriving instance Show GraphicsBindingOpenGLXcbKHR

instance ToCStruct GraphicsBindingOpenGLXcbKHR where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsBindingOpenGLXcbKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_OPENGL_XCB_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Xcb_connection_t))) (connection)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (screenNumber)
    poke ((p `plusPtr` 28 :: Ptr Xcb_glx_fbconfig_t)) (fbconfigid)
    poke ((p `plusPtr` 32 :: Ptr Xcb_visualid_t)) (visualid)
    poke ((p `plusPtr` 36 :: Ptr Xcb_glx_drawable_t)) (glxDrawable)
    poke ((p `plusPtr` 40 :: Ptr Xcb_glx_context_t)) (glxContext)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_OPENGL_XCB_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Xcb_connection_t))) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Xcb_glx_fbconfig_t)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Xcb_visualid_t)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Xcb_glx_drawable_t)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Xcb_glx_context_t)) (zero)
    f

instance FromCStruct GraphicsBindingOpenGLXcbKHR where
  peekCStruct p = do
    connection <- peek @(Ptr Xcb_connection_t) ((p `plusPtr` 16 :: Ptr (Ptr Xcb_connection_t)))
    screenNumber <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    fbconfigid <- peek @Xcb_glx_fbconfig_t ((p `plusPtr` 28 :: Ptr Xcb_glx_fbconfig_t))
    visualid <- peek @Xcb_visualid_t ((p `plusPtr` 32 :: Ptr Xcb_visualid_t))
    glxDrawable <- peek @Xcb_glx_drawable_t ((p `plusPtr` 36 :: Ptr Xcb_glx_drawable_t))
    glxContext <- peek @Xcb_glx_context_t ((p `plusPtr` 40 :: Ptr Xcb_glx_context_t))
    pure $ GraphicsBindingOpenGLXcbKHR
             connection screenNumber fbconfigid visualid glxDrawable glxContext

instance Storable GraphicsBindingOpenGLXcbKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsBindingOpenGLXcbKHR where
  zero = GraphicsBindingOpenGLXcbKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | XrGraphicsBindingOpenGLWaylandKHR - The graphics binding structure to be
-- passed at session creation to use OpenGL on Wayland
--
-- == Member Descriptions
--
-- = Description
--
-- When creating an OpenGL-backed 'OpenXR.Core10.Handles.Session' on any
-- Linux\/Unix platform that utilizes the Wayland protocol with its
-- compositor, the application will provide a pointer to an
-- 'GraphicsBindingOpenGLWaylandKHR' in the @next@ chain of the
-- 'OpenXR.Core10.Device.SessionCreateInfo'.
--
-- The required window system configuration define to expose this structure
-- type is
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_USE_PLATFORM_WAYLAND XR_USE_PLATFORM_WAYLAND>.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsBindingOpenGLWaylandKHR-extension-notenabled# The
--     @XR_KHR_opengl_enable@ extension /must/ be enabled prior to using
--     'GraphicsBindingOpenGLWaylandKHR'
--
-- -   #VUID-XrGraphicsBindingOpenGLWaylandKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_BINDING_OPENGL_WAYLAND_KHR'
--
-- -   #VUID-XrGraphicsBindingOpenGLWaylandKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrGraphicsBindingOpenGLWaylandKHR-display-parameter# @display@
--     /must/ be a pointer to a @wl_display@ value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Device.createSession'
data GraphicsBindingOpenGLWaylandKHR = GraphicsBindingOpenGLWaylandKHR
  { -- | @display@ is a valid Wayland @wl_display@.
    display :: Ptr Wl_display }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsBindingOpenGLWaylandKHR)
#endif
deriving instance Show GraphicsBindingOpenGLWaylandKHR

instance ToCStruct GraphicsBindingOpenGLWaylandKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsBindingOpenGLWaylandKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_OPENGL_WAYLAND_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Wl_display))) (display)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_OPENGL_WAYLAND_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Wl_display))) (zero)
    f

instance FromCStruct GraphicsBindingOpenGLWaylandKHR where
  peekCStruct p = do
    display <- peek @(Ptr Wl_display) ((p `plusPtr` 16 :: Ptr (Ptr Wl_display)))
    pure $ GraphicsBindingOpenGLWaylandKHR
             display

instance Storable GraphicsBindingOpenGLWaylandKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsBindingOpenGLWaylandKHR where
  zero = GraphicsBindingOpenGLWaylandKHR
           zero


-- | XrSwapchainImageOpenGLKHR - OpenGL-specific swapchain image structure
--
-- == Member Descriptions
--
-- = Description
--
-- If a given session was created with a @XrGraphicsBindingOpenGL*KHR@, the
-- following conditions /must/ apply.
--
-- -   Calls to 'OpenXR.Core10.Image.enumerateSwapchainImages' on an
--     'OpenXR.Core10.Handles.Swapchain' in that session /must/ return an
--     array of 'SwapchainImageOpenGLKHR' structures.
--
-- -   Whenever an OpenXR function accepts an
--     'OpenXR.Core10.Image.SwapchainImageBaseHeader' pointer as a
--     parameter in that session, the runtime /must/ also accept a pointer
--     to an 'SwapchainImageOpenGLKHR'.
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
-- -   #VUID-XrSwapchainImageOpenGLKHR-extension-notenabled# The
--     @XR_KHR_opengl_enable@ extension /must/ be enabled prior to using
--     'SwapchainImageOpenGLKHR'
--
-- -   #VUID-XrSwapchainImageOpenGLKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR'
--
-- -   #VUID-XrSwapchainImageOpenGLKHR-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Image.SwapchainImageBaseHeader'
data SwapchainImageOpenGLKHR = SwapchainImageOpenGLKHR
  { -- | @image@ is the OpenGL texture handle associated with this swapchain
    -- image.
    image :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainImageOpenGLKHR)
#endif
deriving instance Show SwapchainImageOpenGLKHR

instance IsSwapchainImage SwapchainImageOpenGLKHR where
  toSwapchainImageBaseHeader SwapchainImageOpenGLKHR{} = SwapchainImageBaseHeader{type' = TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR}

instance ToCStruct SwapchainImageOpenGLKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainImageOpenGLKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (image)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct SwapchainImageOpenGLKHR where
  peekCStruct p = do
    image <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ SwapchainImageOpenGLKHR
             image

instance Storable SwapchainImageOpenGLKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainImageOpenGLKHR where
  zero = SwapchainImageOpenGLKHR
           zero


-- | XrGraphicsRequirementsOpenGLKHR - OpenGL API version requirements
--
-- == Member Descriptions
--
-- = Description
--
-- 'GraphicsRequirementsOpenGLKHR' is populated by
-- 'getOpenGLGraphicsRequirementsKHR' with the runtime’s OpenGL API version
-- requirements.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsRequirementsOpenGLKHR-extension-notenabled# The
--     @XR_KHR_opengl_enable@ extension /must/ be enabled prior to using
--     'GraphicsRequirementsOpenGLKHR'
--
-- -   #VUID-XrGraphicsRequirementsOpenGLKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_REQUIREMENTS_OPENGL_KHR'
--
-- -   #VUID-XrGraphicsRequirementsOpenGLKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrVersion >,
-- 'getOpenGLGraphicsRequirementsKHR'
data GraphicsRequirementsOpenGLKHR = GraphicsRequirementsOpenGLKHR
  { -- | @minApiVersionSupported@ is the minimum version of OpenGL that the
    -- runtime supports. Uses 'OpenXR.Version.MAKE_VERSION' on major and minor
    -- API version, ignoring any patch version component.
    minApiVersionSupported :: Version
  , -- | @maxApiVersionSupported@ is the maximum version of OpenGL that the
    -- runtime has been tested on and is known to support. Newer OpenGL
    -- versions might work if they are compatible. Uses
    -- 'OpenXR.Version.MAKE_VERSION' on major and minor API version, ignoring
    -- any patch version component.
    maxApiVersionSupported :: Version
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsRequirementsOpenGLKHR)
#endif
deriving instance Show GraphicsRequirementsOpenGLKHR

instance ToCStruct GraphicsRequirementsOpenGLKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsRequirementsOpenGLKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_REQUIREMENTS_OPENGL_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Version)) (minApiVersionSupported)
    poke ((p `plusPtr` 24 :: Ptr Version)) (maxApiVersionSupported)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_REQUIREMENTS_OPENGL_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Version)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Version)) (zero)
    f

instance FromCStruct GraphicsRequirementsOpenGLKHR where
  peekCStruct p = do
    minApiVersionSupported <- peek @Version ((p `plusPtr` 16 :: Ptr Version))
    maxApiVersionSupported <- peek @Version ((p `plusPtr` 24 :: Ptr Version))
    pure $ GraphicsRequirementsOpenGLKHR
             minApiVersionSupported maxApiVersionSupported

instance Storable GraphicsRequirementsOpenGLKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsRequirementsOpenGLKHR where
  zero = GraphicsRequirementsOpenGLKHR
           zero
           zero


type KHR_opengl_enable_SPEC_VERSION = 9

-- No documentation found for TopLevel "XR_KHR_opengl_enable_SPEC_VERSION"
pattern KHR_opengl_enable_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_opengl_enable_SPEC_VERSION = 9


type KHR_OPENGL_ENABLE_EXTENSION_NAME = "XR_KHR_opengl_enable"

-- No documentation found for TopLevel "XR_KHR_OPENGL_ENABLE_EXTENSION_NAME"
pattern KHR_OPENGL_ENABLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_OPENGL_ENABLE_EXTENSION_NAME = "XR_KHR_opengl_enable"


type HDC = Ptr ()


type HGLRC = Ptr ()


type Display = Ptr ()


type Xcb_visualid_t = Word32


type Xcb_glx_fbconfig_t = Word32


type Xcb_glx_drawable_t = Word32


type Xcb_glx_context_t = Word32


type GLXFBConfig = Ptr ()


type GLXDrawable = Word64


type GLXContext = Ptr ()


data Xcb_connection_t


data Wl_display

