{-# language CPP #-}
-- | = Name
--
-- XR_MNDX_egl_enable - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MNDX_egl_enable  XR_MNDX_egl_enable>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 49
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
-- 'GraphicsBindingEGLMNDX'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MNDX_egl_enable OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MNDX_egl_enable  ( GraphicsBindingEGLMNDX(..)
                                             , MNDX_egl_enable_SPEC_VERSION
                                             , pattern MNDX_egl_enable_SPEC_VERSION
                                             , MNDX_EGL_ENABLE_EXTENSION_NAME
                                             , pattern MNDX_EGL_ENABLE_EXTENSION_NAME
                                             , PFNEGLGETPROCADDRESSPROC
                                             , EGLDisplay
                                             , EGLConfig
                                             , EGLContext
                                             ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.String (CString)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import OpenXR.Extensions.XR_KHR_opengl_es_enable (EGLConfig)
import OpenXR.Extensions.XR_KHR_opengl_es_enable (EGLContext)
import OpenXR.Extensions.XR_KHR_opengl_es_enable (EGLDisplay)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_GRAPHICS_BINDING_EGL_MNDX))
import OpenXR.Extensions.XR_KHR_opengl_es_enable (EGLConfig)
import OpenXR.Extensions.XR_KHR_opengl_es_enable (EGLContext)
import OpenXR.Extensions.XR_KHR_opengl_es_enable (EGLDisplay)
-- | XrGraphicsBindingEGLMNDX - The graphics binding structure to be passed
-- at session creation to EGL
--
-- == Member Descriptions
--
-- = Description
--
-- When creating an EGL based 'OpenXR.Core10.Handles.Session', the
-- application will provide a pointer to an 'GraphicsBindingEGLMNDX'
-- structure in the @next@ chain of the
-- 'OpenXR.Core10.Device.SessionCreateInfo'.
--
-- The required window system configuration define to expose this structure
-- type is
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_USE_PLATFORM_EGL XR_USE_PLATFORM_EGL>.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrGraphicsBindingEGLMNDX-extension-notenabled# The @@
--     extension /must/ be enabled prior to using 'GraphicsBindingEGLMNDX'
--
-- -   #VUID-XrGraphicsBindingEGLMNDX-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_GRAPHICS_BINDING_EGL_MNDX'
--
-- -   #VUID-XrGraphicsBindingEGLMNDX-next-next# @next@ /must/ be @NULL@ or
--     a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrGraphicsBindingEGLMNDX-getProcAddress-parameter#
--     @getProcAddress@ /must/ be a valid 'PFNEGLGETPROCADDRESSPROC' value
--
-- -   #VUID-XrGraphicsBindingEGLMNDX-display-parameter# @display@ /must/
--     be a valid 'OpenXR.Extensions.XR_KHR_opengl_es_enable.EGLDisplay'
--     value
--
-- -   #VUID-XrGraphicsBindingEGLMNDX-config-parameter# @config@ /must/ be
--     a valid 'OpenXR.Extensions.XR_KHR_opengl_es_enable.EGLConfig' value
--
-- -   #VUID-XrGraphicsBindingEGLMNDX-context-parameter# @context@ /must/
--     be a valid 'OpenXR.Extensions.XR_KHR_opengl_es_enable.EGLContext'
--     value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Device.createSession'
data GraphicsBindingEGLMNDX = GraphicsBindingEGLMNDX
  { -- | @getProcAddress@ is a valid function pointer to @eglGetProcAddress@.
    getProcAddress :: PFNEGLGETPROCADDRESSPROC
  , -- | @display@ is a valid EGL
    -- 'OpenXR.Extensions.XR_KHR_opengl_es_enable.EGLDisplay'.
    display :: EGLDisplay
  , -- | @config@ is a valid EGL
    -- 'OpenXR.Extensions.XR_KHR_opengl_es_enable.EGLConfig'.
    config :: EGLConfig
  , -- | @context@ is a valid EGL
    -- 'OpenXR.Extensions.XR_KHR_opengl_es_enable.EGLContext'.
    context :: EGLContext
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsBindingEGLMNDX)
#endif
deriving instance Show GraphicsBindingEGLMNDX

instance ToCStruct GraphicsBindingEGLMNDX where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsBindingEGLMNDX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_EGL_MNDX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PFNEGLGETPROCADDRESSPROC)) (getProcAddress)
    poke ((p `plusPtr` 24 :: Ptr EGLDisplay)) (display)
    poke ((p `plusPtr` 32 :: Ptr EGLConfig)) (config)
    poke ((p `plusPtr` 40 :: Ptr EGLContext)) (context)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_GRAPHICS_BINDING_EGL_MNDX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PFNEGLGETPROCADDRESSPROC)) (zero)
    poke ((p `plusPtr` 24 :: Ptr EGLDisplay)) (zero)
    poke ((p `plusPtr` 32 :: Ptr EGLConfig)) (zero)
    poke ((p `plusPtr` 40 :: Ptr EGLContext)) (zero)
    f

instance FromCStruct GraphicsBindingEGLMNDX where
  peekCStruct p = do
    getProcAddress <- peek @PFNEGLGETPROCADDRESSPROC ((p `plusPtr` 16 :: Ptr PFNEGLGETPROCADDRESSPROC))
    display <- peek @EGLDisplay ((p `plusPtr` 24 :: Ptr EGLDisplay))
    config <- peek @EGLConfig ((p `plusPtr` 32 :: Ptr EGLConfig))
    context <- peek @EGLContext ((p `plusPtr` 40 :: Ptr EGLContext))
    pure $ GraphicsBindingEGLMNDX
             getProcAddress display config context

instance Storable GraphicsBindingEGLMNDX where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsBindingEGLMNDX where
  zero = GraphicsBindingEGLMNDX
           zero
           zero
           zero
           zero


type MNDX_egl_enable_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_MNDX_egl_enable_SPEC_VERSION"
pattern MNDX_egl_enable_SPEC_VERSION :: forall a . Integral a => a
pattern MNDX_egl_enable_SPEC_VERSION = 1


type MNDX_EGL_ENABLE_EXTENSION_NAME = "XR_MNDX_egl_enable"

-- No documentation found for TopLevel "XR_MNDX_EGL_ENABLE_EXTENSION_NAME"
pattern MNDX_EGL_ENABLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MNDX_EGL_ENABLE_EXTENSION_NAME = "XR_MNDX_egl_enable"


type PFNEGLGETPROCADDRESSPROC = FunPtr (CString -> IO (FunPtr (IO ())))

