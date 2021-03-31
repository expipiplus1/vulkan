{-# language CPP #-}
-- No documentation found for Chapter "Image"
module OpenXR.Core10.Image  ( enumerateSwapchainFormats
                            , createSwapchain
                            , withSwapchain
                            , destroySwapchain
                            , enumerateSwapchainImages
                            , acquireSwapchainImage
                            , waitSwapchainImage
                            , waitSwapchainImageSafe
                            , releaseSwapchainImage
                            , SwapchainCreateInfo(..)
                            , SwapchainImageBaseHeader(..)
                            , IsSwapchainImage(..)
                            , SwapchainImageAcquireInfo(..)
                            , SwapchainImageWaitInfo(..)
                            , SwapchainImageReleaseInfo(..)
                            ) where

import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import GHC.IO.Exception (pattern IOError)
import GHC.IO.Exception (pattern InvalidArgument)
import Control.Monad.Trans.Cont (pattern ContT)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(..))
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int64)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import OpenXR.CStruct.Utils (advancePtrBytes)
import OpenXR.CStruct.Extends (forgetExtensions)
import OpenXR.CStruct.Extends (lowerChildPointer)
import OpenXR.NamedType ((:::))
import OpenXR.CStruct.Extends (Chain)
import OpenXR.Core10.FundamentalTypes (Duration)
import OpenXR.CStruct.Extends (Extends)
import OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct.Extends (Extensible(..))
import OpenXR.CStruct.Extends (Inheritable(..))
import OpenXR.CStruct.Extends (Inherits)
import OpenXR.Dynamic (InstanceCmds(..))
import OpenXR.Dynamic (InstanceCmds(pXrAcquireSwapchainImage))
import OpenXR.Dynamic (InstanceCmds(pXrCreateSwapchain))
import OpenXR.Dynamic (InstanceCmds(pXrDestroySwapchain))
import OpenXR.Dynamic (InstanceCmds(pXrEnumerateSwapchainFormats))
import OpenXR.Dynamic (InstanceCmds(pXrReleaseSwapchainImage))
import OpenXR.Dynamic (InstanceCmds(pXrWaitSwapchainImage))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.CStruct.Extends (PeekChain)
import OpenXR.CStruct.Extends (PeekChain(..))
import OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct.Extends (PokeChain(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_secondary_view_configuration (SecondaryViewConfigurationSwapchainCreateInfoMSFT)
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.CStruct.Extends (SomeChild)
import OpenXR.CStruct.Extends (SomeChild(..))
import OpenXR.CStruct.Extends (SomeStruct)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Handles (Swapchain)
import OpenXR.Core10.Handles (Swapchain(..))
import OpenXR.Core10.Handles (Swapchain(Swapchain))
import OpenXR.Core10.Enums.SwapchainCreateFlagBits (SwapchainCreateFlags)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_D3D11_enable (SwapchainImageD3D11KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_D3D12_enable (SwapchainImageD3D12KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_es_enable (SwapchainImageOpenGLESKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_enable (SwapchainImageOpenGLKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_vulkan_enable (SwapchainImageVulkanKHR)
import OpenXR.Core10.Enums.SwapchainUsageFlagBits (SwapchainUsageFlags)
import OpenXR.Core10.Handles (Swapchain_T)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_CREATE_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_ACQUIRE_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_D3D11_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_D3D12_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_RELEASE_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SWAPCHAIN_IMAGE_WAIT_INFO))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEnumerateSwapchainFormats
  :: FunPtr (Ptr Session_T -> Word32 -> Ptr Word32 -> Ptr Int64 -> IO Result) -> Ptr Session_T -> Word32 -> Ptr Word32 -> Ptr Int64 -> IO Result

-- | xrEnumerateSwapchainFormats - Enumerates swapchain formats
--
-- == Parameter Descriptions
--
-- -   @session@ is the session that enumerates the supported formats.
--
-- -   @formatCapacityInput@ is the capacity of the @formats@, or 0 to
--     retrieve the required capacity.
--
-- -   @formatCountOutput@ is a pointer to the count of @uint64_t@ formats
--     written, or a pointer to the required capacity in the case that
--     @formatCapacityInput@ is @0@.
--
-- -   @formats@ is a pointer to an array of @int64_t@ format ids, but
--     /can/ be @NULL@ if @formatCapacityInput@ is @0@. The format ids are
--     specific to the specified graphics API.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @formats@ size.
--
-- = Description
--
-- 'enumerateSwapchainFormats' enumerates the texture formats supported by
-- the current session. The type of formats returned are dependent on the
-- graphics API specified in 'OpenXR.Core10.Device.createSession'. For
-- example, if a DirectX graphics API was specified, then the enumerated
-- formats correspond to the DXGI formats, such as
-- @DXGI_FORMAT_R8G8B8A8_UNORM_SRGB@. Texture formats /should/ be in order
-- from highest to lowest runtime preference.
--
-- With an OpenGL-based graphics API, the texture formats correspond to
-- OpenGL internal formats.
--
-- With a Direct3D-based graphics API, 'enumerateSwapchainFormats' never
-- returns typeless formats (e.g. @DXGI_FORMAT_R8G8B8A8_TYPELESS@). Only
-- concrete formats are returned, and only concrete formats may be
-- specified by applications for swapchain creation.
--
-- Runtimes /must/ always return identical buffer contents from this
-- enumeration for the lifetime of the session.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrEnumerateSwapchainFormats-session-parameter# @session@
--     /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrEnumerateSwapchainFormats-formatCountOutput-parameter#
--     @formatCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrEnumerateSwapchainFormats-formats-parameter# If
--     @formatCapacityInput@ is not @0@, @formats@ /must/ be a pointer to
--     an array of @formatCapacityInput@ @int64_t@ values
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'createSwapchain'
enumerateSwapchainFormats :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "xrEnumerateSwapchainFormats" "session"
                             Session
                          -> io (Result, ("formats" ::: Vector Int64))
enumerateSwapchainFormats session = liftIO . evalContT $ do
  let xrEnumerateSwapchainFormatsPtr = pXrEnumerateSwapchainFormats (instanceCmds (session :: Session))
  lift $ unless (xrEnumerateSwapchainFormatsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEnumerateSwapchainFormats is null" Nothing Nothing
  let xrEnumerateSwapchainFormats' = mkXrEnumerateSwapchainFormats xrEnumerateSwapchainFormatsPtr
  let session' = sessionHandle (session)
  pFormatCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrEnumerateSwapchainFormats" (xrEnumerateSwapchainFormats' session' (0) (pFormatCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  formatCountOutput <- lift $ peek @Word32 pFormatCountOutput
  pFormats <- ContT $ bracket (callocBytes @Int64 ((fromIntegral (formatCountOutput)) * 8)) free
  r' <- lift $ traceAroundEvent "xrEnumerateSwapchainFormats" (xrEnumerateSwapchainFormats' session' ((formatCountOutput)) (pFormatCountOutput) (pFormats))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  formatCountOutput' <- lift $ peek @Word32 pFormatCountOutput
  formats' <- lift $ generateM (fromIntegral (formatCountOutput')) (\i -> peek @Int64 ((pFormats `advancePtrBytes` (8 * (i)) :: Ptr Int64)))
  pure $ ((r'), formats')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateSwapchain
  :: FunPtr (Ptr Session_T -> Ptr (SomeStruct SwapchainCreateInfo) -> Ptr (Ptr Swapchain_T) -> IO Result) -> Ptr Session_T -> Ptr (SomeStruct SwapchainCreateInfo) -> Ptr (Ptr Swapchain_T) -> IO Result

-- | xrCreateSwapchain - Creates an XrSwapchain
--
-- == Parameter Descriptions
--
-- = Description
--
-- Creates an 'OpenXR.Core10.Handles.Swapchain' handle. The returned
-- swapchain handle /may/ be subsequently used in API calls. Multiple
-- 'OpenXR.Core10.Handles.Swapchain' handles may exist simultaneously, up
-- to some limit imposed by the runtime. The
-- 'OpenXR.Core10.Handles.Swapchain' handle /must/ be eventually freed via
-- the 'destroySwapchain' function. The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED' if the
-- image format specified in the 'SwapchainCreateInfo' is unsupported. The
-- runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_FEATURE_UNSUPPORTED' if any bit of the
-- create flags specified in the 'SwapchainCreateInfo' is unsupported.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_LIMIT_REACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FEATURE_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'OpenXR.Core10.Handles.Swapchain',
-- 'SwapchainCreateInfo', 'acquireSwapchainImage', 'destroySwapchain',
-- 'enumerateSwapchainFormats', 'enumerateSwapchainImages',
-- 'releaseSwapchainImage'
createSwapchain :: forall a io
                 . (Extendss SwapchainCreateInfo a, PokeChain a, MonadIO io)
                => -- | @session@ is the session that creates the image.
                   --
                   -- #VUID-xrCreateSwapchain-session-parameter# @session@ /must/ be a valid
                   -- 'OpenXR.Core10.Handles.Session' handle
                   Session
                -> -- | @createInfo@ is a pointer to an 'SwapchainCreateInfo' structure
                   -- containing parameters to be used to create the image.
                   --
                   -- #VUID-xrCreateSwapchain-createInfo-parameter# @createInfo@ /must/ be a
                   -- pointer to a valid 'SwapchainCreateInfo' structure
                   (SwapchainCreateInfo a)
                -> io (Result, Swapchain)
createSwapchain session createInfo = liftIO . evalContT $ do
  let cmds = instanceCmds (session :: Session)
  let xrCreateSwapchainPtr = pXrCreateSwapchain cmds
  lift $ unless (xrCreateSwapchainPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateSwapchain is null" Nothing Nothing
  let xrCreateSwapchain' = mkXrCreateSwapchain xrCreateSwapchainPtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pSwapchain <- ContT $ bracket (callocBytes @(Ptr Swapchain_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateSwapchain" (xrCreateSwapchain' (sessionHandle (session)) (forgetExtensions createInfo') (pSwapchain))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  swapchain <- lift $ peek @(Ptr Swapchain_T) pSwapchain
  pure $ (r, ((\h -> Swapchain h cmds ) swapchain))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSwapchain' and 'destroySwapchain'
--
-- To ensure that 'destroySwapchain' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSwapchain :: forall a io r . (Extendss SwapchainCreateInfo a, PokeChain a, MonadIO io) => Session -> SwapchainCreateInfo a -> (io (Result, Swapchain) -> ((Result, Swapchain) -> io ()) -> r) -> r
withSwapchain session createInfo b =
  b (createSwapchain session createInfo)
    (\(_, o1) -> destroySwapchain o1)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrDestroySwapchain
  :: FunPtr (Ptr Swapchain_T -> IO Result) -> Ptr Swapchain_T -> IO Result

-- | xrDestroySwapchain - Destroys an XrSwapchain
--
-- == Parameter Descriptions
--
-- = Description
--
-- All submitted graphics API commands that refer to @swapchain@ /must/
-- have completed execution. Runtimes /may/ continue to utilize swapchain
-- images after 'destroySwapchain' is called.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrDestroySwapchain-swapchain-parameter# @swapchain@ /must/ be
--     a valid 'OpenXR.Core10.Handles.Swapchain' handle
--
-- == Thread Safety
--
-- -   Access to @swapchain@, and any child handles, /must/ be externally
--     synchronized
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
-- = See Also
--
-- 'OpenXR.Core10.Handles.Swapchain', 'createSwapchain'
destroySwapchain :: forall io
                  . (MonadIO io)
                 => -- | @swapchain@ is the swapchain to destroy.
                    Swapchain
                 -> io ()
destroySwapchain swapchain = liftIO $ do
  let xrDestroySwapchainPtr = pXrDestroySwapchain (instanceCmds (swapchain :: Swapchain))
  unless (xrDestroySwapchainPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrDestroySwapchain is null" Nothing Nothing
  let xrDestroySwapchain' = mkXrDestroySwapchain xrDestroySwapchainPtr
  r <- traceAroundEvent "xrDestroySwapchain" (xrDestroySwapchain' (swapchainHandle (swapchain)))
  when (r < SUCCESS) (throwIO (OpenXrException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEnumerateSwapchainImages
  :: FunPtr (Ptr Swapchain_T -> Word32 -> Ptr Word32 -> Ptr (SomeChild SwapchainImageBaseHeader) -> IO Result) -> Ptr Swapchain_T -> Word32 -> Ptr Word32 -> Ptr (SomeChild SwapchainImageBaseHeader) -> IO Result

-- | xrEnumerateSwapchainImages - Gets images from an XrSwapchain
--
-- == Parameter Descriptions
--
-- -   @swapchain@ is the 'OpenXR.Core10.Handles.Swapchain' to get images
--     from.
--
-- -   @imageCapacityInput@ is the capacity of the @images@ array, or 0 to
--     indicate a request to retrieve the required capacity.
--
-- -   @imageCountOutput@ is a pointer to the count of @images@ written, or
--     a pointer to the required capacity in the case that
--     @imageCapacityInput@ is 0.
--
-- -   @images@ is a pointer to an array of graphics API-specific
--     @XrSwapchainImage@ structures based off of
--     'SwapchainImageBaseHeader'. It /can/ be @NULL@ if
--     @imageCapacityInput@ is 0.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @images@ size.
--
-- = Description
--
-- Fills an array of graphics API-specific @XrSwapchainImage@ structures.
-- The resources /must/ be constant and valid for the lifetime of the
-- 'OpenXR.Core10.Handles.Swapchain'.
--
-- Runtimes /must/ always return identical buffer contents from this
-- enumeration for the lifetime of the swapchain.
--
-- Note: @images@ is a pointer to an array of structures of graphics
-- API-specific type, not an array of structure pointers.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrEnumerateSwapchainImages-swapchain-parameter# @swapchain@
--     /must/ be a valid 'OpenXR.Core10.Handles.Swapchain' handle
--
-- -   #VUID-xrEnumerateSwapchainImages-imageCountOutput-parameter#
--     @imageCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrEnumerateSwapchainImages-images-parameter# If
--     @imageCapacityInput@ is not @0@, @images@ /must/ be a pointer to an
--     array of @imageCapacityInput@ 'SwapchainImageBaseHeader'-based
--     structures. See also:
--     'OpenXR.Extensions.XR_KHR_D3D11_enable.SwapchainImageD3D11KHR',
--     'OpenXR.Extensions.XR_KHR_D3D12_enable.SwapchainImageD3D12KHR',
--     'OpenXR.Extensions.XR_KHR_opengl_es_enable.SwapchainImageOpenGLESKHR',
--     'OpenXR.Extensions.XR_KHR_opengl_enable.SwapchainImageOpenGLKHR',
--     'OpenXR.Extensions.XR_KHR_vulkan_enable.SwapchainImageVulkanKHR'
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Swapchain', 'SwapchainImageBaseHeader',
-- 'createSwapchain'
enumerateSwapchainImages :: forall a io
                          . (Inherits SwapchainImageBaseHeader a, ToCStruct a, FromCStruct a, MonadIO io)
                         => -- No documentation found for Nested "xrEnumerateSwapchainImages" "swapchain"
                            Swapchain
                         -> io (Result, "images" ::: Vector a)
enumerateSwapchainImages swapchain = liftIO . evalContT $ do
  let xrEnumerateSwapchainImagesPtr = pXrEnumerateSwapchainImages (instanceCmds (swapchain :: Swapchain))
  lift $ unless (xrEnumerateSwapchainImagesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEnumerateSwapchainImages is null" Nothing Nothing
  let xrEnumerateSwapchainImages' = mkXrEnumerateSwapchainImages xrEnumerateSwapchainImagesPtr
  let swapchain' = swapchainHandle swapchain
  pImageCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrEnumerateSwapchainImages" (xrEnumerateSwapchainImages' swapchain' 0 pImageCountOutput nullPtr)
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  imageCountOutput <- lift $ peek @Word32 pImageCountOutput
  pImages <- ContT $ bracket (callocBytes @a (fromIntegral imageCountOutput * cStructSize @a)) free
  traverse_ (\i -> ContT $ pokeZeroCStruct (pImages `advancePtrBytes` (i * cStructSize @a) :: Ptr a) . ($ ())) [0..fromIntegral imageCountOutput - 1]
  r' <- lift $ traceAroundEvent "xrEnumerateSwapchainImages" (xrEnumerateSwapchainImages' swapchain' imageCountOutput pImageCountOutput (lowerChildPointer pImages))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  imageCountOutput' <- lift $ peek @Word32 pImageCountOutput
  images' <- lift $ generateM (fromIntegral imageCountOutput') (\i -> peekCStruct @a (pImages `advancePtrBytes` (cStructSize @a * i) :: Ptr a))
  pure (r', images')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrAcquireSwapchainImage
  :: FunPtr (Ptr Swapchain_T -> Ptr SwapchainImageAcquireInfo -> Ptr Word32 -> IO Result) -> Ptr Swapchain_T -> Ptr SwapchainImageAcquireInfo -> Ptr Word32 -> IO Result

-- | xrAcquireSwapchainImage - Acquire a swapchain image
--
-- == Parameter Descriptions
--
-- = Description
--
-- Acquires the image corresponding to the @index@ position in the array
-- returned by 'enumerateSwapchainImages'. The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_CALL_ORDER_INVALID' if @index@ has
-- already been acquired and not yet released with 'releaseSwapchainImage'.
-- If the @swapchain@ was created with the
-- 'OpenXR.Core10.Enums.SwapchainCreateFlagBits.SWAPCHAIN_CREATE_STATIC_IMAGE_BIT'
-- set in 'SwapchainCreateInfo'::@createFlags@, this function /must/ not
-- have been previously called for this swapchain. The runtime /must/
-- return 'OpenXR.Core10.Enums.Result.ERROR_CALL_ORDER_INVALID' if a
-- @swapchain@ created with the
-- 'OpenXR.Core10.Enums.SwapchainCreateFlagBits.SWAPCHAIN_CREATE_STATIC_IMAGE_BIT'
-- set in 'SwapchainCreateInfo'::@createFlags@ and this function has been
-- successfully called previously for this swapchain.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrAcquireSwapchainImage-swapchain-parameter# @swapchain@
--     /must/ be a valid 'OpenXR.Core10.Handles.Swapchain' handle
--
-- -   #VUID-xrAcquireSwapchainImage-acquireInfo-parameter# If
--     @acquireInfo@ is not @NULL@, @acquireInfo@ /must/ be a pointer to a
--     valid 'SwapchainImageAcquireInfo' structure
--
-- -   #VUID-xrAcquireSwapchainImage-index-parameter# @index@ /must/ be a
--     pointer to a @uint32_t@ value
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_CALL_ORDER_INVALID'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Swapchain', 'SwapchainImageAcquireInfo',
-- 'createSwapchain', 'destroySwapchain', 'enumerateSwapchainImages',
-- 'releaseSwapchainImage', 'waitSwapchainImage'
acquireSwapchainImage :: forall io
                       . (MonadIO io)
                      => -- | @swapchain@ is the swapchain from which to acquire an image.
                         Swapchain
                      -> -- | @acquireInfo@ exists for extensibility purposes, it is @NULL@ or a
                         -- pointer to a valid 'SwapchainImageAcquireInfo'.
                         ("acquireInfo" ::: Maybe SwapchainImageAcquireInfo)
                      -> io (Result, ("index" ::: Word32))
acquireSwapchainImage swapchain acquireInfo = liftIO . evalContT $ do
  let xrAcquireSwapchainImagePtr = pXrAcquireSwapchainImage (instanceCmds (swapchain :: Swapchain))
  lift $ unless (xrAcquireSwapchainImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrAcquireSwapchainImage is null" Nothing Nothing
  let xrAcquireSwapchainImage' = mkXrAcquireSwapchainImage xrAcquireSwapchainImagePtr
  acquireInfo' <- case (acquireInfo) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrAcquireSwapchainImage" (xrAcquireSwapchainImage' (swapchainHandle (swapchain)) acquireInfo' (pIndex))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  index <- lift $ peek @Word32 pIndex
  pure $ (r, index)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrWaitSwapchainImageUnsafe
  :: FunPtr (Ptr Swapchain_T -> Ptr SwapchainImageWaitInfo -> IO Result) -> Ptr Swapchain_T -> Ptr SwapchainImageWaitInfo -> IO Result

foreign import ccall
  "dynamic" mkXrWaitSwapchainImageSafe
  :: FunPtr (Ptr Swapchain_T -> Ptr SwapchainImageWaitInfo -> IO Result) -> Ptr Swapchain_T -> Ptr SwapchainImageWaitInfo -> IO Result

-- | waitSwapchainImage with selectable safeness
waitSwapchainImageSafeOrUnsafe :: forall io
                                . (MonadIO io)
                               => (FunPtr (Ptr Swapchain_T -> Ptr SwapchainImageWaitInfo -> IO Result) -> Ptr Swapchain_T -> Ptr SwapchainImageWaitInfo -> IO Result)
                               -> -- | @swapchain@ is the swapchain from which to wait for an image.
                                  --
                                  -- #VUID-xrWaitSwapchainImage-swapchain-parameter# @swapchain@ /must/ be a
                                  -- valid 'OpenXR.Core10.Handles.Swapchain' handle
                                  Swapchain
                               -> -- | @waitInfo@ is a pointer to an 'SwapchainImageWaitInfo' structure.
                                  --
                                  -- #VUID-xrWaitSwapchainImage-waitInfo-parameter# @waitInfo@ /must/ be a
                                  -- pointer to a valid 'SwapchainImageWaitInfo' structure
                                  SwapchainImageWaitInfo
                               -> io (Result)
waitSwapchainImageSafeOrUnsafe mkXrWaitSwapchainImage swapchain waitInfo = liftIO . evalContT $ do
  let xrWaitSwapchainImagePtr = pXrWaitSwapchainImage (instanceCmds (swapchain :: Swapchain))
  lift $ unless (xrWaitSwapchainImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrWaitSwapchainImage is null" Nothing Nothing
  let xrWaitSwapchainImage' = mkXrWaitSwapchainImage xrWaitSwapchainImagePtr
  waitInfo' <- ContT $ withCStruct (waitInfo)
  r <- lift $ traceAroundEvent "xrWaitSwapchainImage" (xrWaitSwapchainImage' (swapchainHandle (swapchain)) waitInfo')
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)

-- | xrWaitSwapchainImage - Wait for a swapchain image to be available
--
-- == Parameter Descriptions
--
-- = Description
--
-- Before an application can begin writing to a swapchain image, it must
-- first wait on the image to avoid writing to it before the compositor has
-- finished reading from it. 'waitSwapchainImage' will implicitly wait on
-- the oldest acquired swapchain image which has not yet been successfully
-- waited on. Once a swapchain image has been successfully waited on, it
-- /must/ be released before waiting on the next acquired swapchain image.
--
-- This function may block for longer than the timeout specified in
-- 'SwapchainImageWaitInfo' due to scheduling or contention.
--
-- If the timeout expires without the image becoming available for writing,
-- 'OpenXR.Core10.Enums.Result.TIMEOUT_EXPIRED' /must/ be returned. If
-- 'waitSwapchainImage' returns
-- 'OpenXR.Core10.Enums.Result.TIMEOUT_EXPIRED', the next call to
-- 'waitSwapchainImage' will wait on the same image index again until the
-- function succeeds with 'OpenXR.Core10.Enums.Result.SUCCESS'. Note that
-- this is not an error code; @XR_SUCCEEDED(XR_TIMEOUT_EXPIRED)@ is @true@.
--
-- The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_CALL_ORDER_INVALID' if no image has
-- been acquired by calling 'acquireSwapchainImage'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
--     -   'OpenXR.Core10.Enums.Result.TIMEOUT_EXPIRED'
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_CALL_ORDER_INVALID'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Swapchain', 'SwapchainImageWaitInfo',
-- 'acquireSwapchainImage', 'createSwapchain', 'destroySwapchain',
-- 'enumerateSwapchainImages', 'releaseSwapchainImage'
waitSwapchainImage :: forall io
                    . (MonadIO io)
                   => -- | @swapchain@ is the swapchain from which to wait for an image.
                      --
                      -- #VUID-xrWaitSwapchainImage-swapchain-parameter# @swapchain@ /must/ be a
                      -- valid 'OpenXR.Core10.Handles.Swapchain' handle
                      Swapchain
                   -> -- | @waitInfo@ is a pointer to an 'SwapchainImageWaitInfo' structure.
                      --
                      -- #VUID-xrWaitSwapchainImage-waitInfo-parameter# @waitInfo@ /must/ be a
                      -- pointer to a valid 'SwapchainImageWaitInfo' structure
                      SwapchainImageWaitInfo
                   -> io (Result)
waitSwapchainImage = waitSwapchainImageSafeOrUnsafe mkXrWaitSwapchainImageUnsafe

-- | A variant of 'waitSwapchainImage' which makes a *safe* FFI call
waitSwapchainImageSafe :: forall io
                        . (MonadIO io)
                       => -- | @swapchain@ is the swapchain from which to wait for an image.
                          --
                          -- #VUID-xrWaitSwapchainImage-swapchain-parameter# @swapchain@ /must/ be a
                          -- valid 'OpenXR.Core10.Handles.Swapchain' handle
                          Swapchain
                       -> -- | @waitInfo@ is a pointer to an 'SwapchainImageWaitInfo' structure.
                          --
                          -- #VUID-xrWaitSwapchainImage-waitInfo-parameter# @waitInfo@ /must/ be a
                          -- pointer to a valid 'SwapchainImageWaitInfo' structure
                          SwapchainImageWaitInfo
                       -> io (Result)
waitSwapchainImageSafe = waitSwapchainImageSafeOrUnsafe mkXrWaitSwapchainImageSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrReleaseSwapchainImage
  :: FunPtr (Ptr Swapchain_T -> Ptr SwapchainImageReleaseInfo -> IO Result) -> Ptr Swapchain_T -> Ptr SwapchainImageReleaseInfo -> IO Result

-- | xrReleaseSwapchainImage - Release a swapchain image
--
-- == Parameter Descriptions
--
-- = Description
--
-- If the @swapchain@ was created with the
-- 'OpenXR.Core10.Enums.SwapchainCreateFlagBits.SWAPCHAIN_CREATE_STATIC_IMAGE_BIT'
-- set in 'SwapchainCreateInfo'::@createFlags@ structure, this function
-- /must/ not have been previously called for this swapchain.
--
-- The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_CALL_ORDER_INVALID' if no image has
-- been waited on by calling 'waitSwapchainImage'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrReleaseSwapchainImage-swapchain-parameter# @swapchain@
--     /must/ be a valid 'OpenXR.Core10.Handles.Swapchain' handle
--
-- -   #VUID-xrReleaseSwapchainImage-releaseInfo-parameter# If
--     @releaseInfo@ is not @NULL@, @releaseInfo@ /must/ be a pointer to a
--     valid 'SwapchainImageReleaseInfo' structure
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_CALL_ORDER_INVALID'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Swapchain', 'SwapchainImageReleaseInfo',
-- 'acquireSwapchainImage', 'createSwapchain', 'destroySwapchain',
-- 'enumerateSwapchainImages', 'waitSwapchainImage'
releaseSwapchainImage :: forall io
                       . (MonadIO io)
                      => -- | @swapchain@ is the 'OpenXR.Core10.Handles.Swapchain' from which to
                         -- release an image.
                         Swapchain
                      -> -- | @releaseInfo@ exists for extensibility purposes, it is @NULL@ or a
                         -- pointer to a valid 'SwapchainImageReleaseInfo'.
                         ("releaseInfo" ::: Maybe SwapchainImageReleaseInfo)
                      -> io (Result)
releaseSwapchainImage swapchain releaseInfo = liftIO . evalContT $ do
  let xrReleaseSwapchainImagePtr = pXrReleaseSwapchainImage (instanceCmds (swapchain :: Swapchain))
  lift $ unless (xrReleaseSwapchainImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrReleaseSwapchainImage is null" Nothing Nothing
  let xrReleaseSwapchainImage' = mkXrReleaseSwapchainImage xrReleaseSwapchainImagePtr
  releaseInfo' <- case (releaseInfo) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  r <- lift $ traceAroundEvent "xrReleaseSwapchainImage" (xrReleaseSwapchainImage' (swapchainHandle (swapchain)) releaseInfo')
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


-- | XrSwapchainCreateInfo - Creation info for a swapchain
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.SessionCreateFlagBits.SessionCreateFlags',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Enums.SwapchainCreateFlagBits.SwapchainCreateFlags',
-- 'OpenXR.Core10.Enums.SwapchainUsageFlagBits.SwapchainUsageFlags',
-- 'OpenXR.Core10.Device.createSession', 'createSwapchain',
-- 'OpenXR.Extensions.XR_KHR_android_surface_swapchain.createSwapchainAndroidSurfaceKHR',
-- 'enumerateSwapchainFormats'
data SwapchainCreateInfo (es :: [Type]) = SwapchainCreateInfo
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR.
    --
    -- #VUID-XrSwapchainCreateInfo-next-next# @next@ /must/ be @NULL@ or a
    -- valid pointer to the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
    -- See also:
    -- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationSwapchainCreateInfoMSFT'
    next :: Chain es
  , -- | @createFlags@ is a bitmask of
    -- 'OpenXR.Core10.Enums.SwapchainCreateFlagBits.SwapchainCreateFlagBits'
    -- describing additional properties of the swapchain.
    --
    -- #VUID-XrSwapchainCreateInfo-createFlags-parameter# @createFlags@ /must/
    -- be @0@ or a valid combination of
    -- 'OpenXR.Core10.Enums.SwapchainCreateFlagBits.SwapchainCreateFlagBits'
    -- values
    createFlags :: SwapchainCreateFlags
  , -- | @usageFlags@ is a bitmask of
    -- 'OpenXR.Core10.Enums.SwapchainUsageFlagBits.SwapchainUsageFlagBits'
    -- describing the intended usage of the swapchain’s images. The usage flags
    -- define how the corresponding graphics API objects are created. A
    -- mismatch /may/ result in swapchain images that do not support the
    -- application’s usage.
    --
    -- #VUID-XrSwapchainCreateInfo-usageFlags-parameter# @usageFlags@ /must/ be
    -- @0@ or a valid combination of
    -- 'OpenXR.Core10.Enums.SwapchainUsageFlagBits.SwapchainUsageFlagBits'
    -- values
    usageFlags :: SwapchainUsageFlags
  , -- | @format@ is a graphics API-specific texture format identifier. For
    -- example, if the graphics API specified in
    -- 'OpenXR.Core10.Device.createSession' is Vulkan, then this format is a
    -- Vulkan format such as @VK_FORMAT_R8G8B8A8_SRGB@. The format identifies
    -- the format that the runtime will interpret the texture as upon
    -- submission. Valid formats are indicated by 'enumerateSwapchainFormats'.
    format :: Int64
  , -- | @sampleCount@ is the number of sub-data element samples in the image,
    -- /must/ not be @0@ or greater than the graphics API’s maximum limit.
    sampleCount :: Word32
  , -- | @width@ is the width of the image, /must/ not be @0@ or greater than the
    -- graphics API’s maximum limit.
    width :: Word32
  , -- | @height@ is the height of the image, /must/ not be @0@ or greater than
    -- the graphics API’s maximum limit.
    height :: Word32
  , -- | @faceCount@ is the number of faces, which can be either @6@ (for
    -- cubemaps) or @1@.
    faceCount :: Word32
  , -- | @arraySize@ is the number of array layers in the image or @1@ for a 2D
    -- image, /must/ not be @0@ or greater than the graphics API’s maximum
    -- limit.
    arraySize :: Word32
  , -- | @mipCount@ describes the number of levels of detail available for
    -- minified sampling of the image, /must/ not be @0@ or greater than the
    -- graphics API’s maximum limit.
    mipCount :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SwapchainCreateInfo es)

instance Extensible SwapchainCreateInfo where
  extensibleTypeName = "SwapchainCreateInfo"
  setNext x next = x{next = next}
  getNext SwapchainCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SwapchainCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SecondaryViewConfigurationSwapchainCreateInfoMSFT = Just f
    | otherwise = Nothing

instance (Extendss SwapchainCreateInfo es, PokeChain es) => ToCStruct (SwapchainCreateInfo es) where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_CREATE_INFO)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr SwapchainCreateFlags)) (createFlags)
    lift $ poke ((p `plusPtr` 24 :: Ptr SwapchainUsageFlags)) (usageFlags)
    lift $ poke ((p `plusPtr` 32 :: Ptr Int64)) (format)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (sampleCount)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (width)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (height)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (faceCount)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (arraySize)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (mipCount)
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 32 :: Ptr Int64)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    lift $ f

instance (Extendss SwapchainCreateInfo es, PeekChain es) => FromCStruct (SwapchainCreateInfo es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    createFlags <- peek @SwapchainCreateFlags ((p `plusPtr` 16 :: Ptr SwapchainCreateFlags))
    usageFlags <- peek @SwapchainUsageFlags ((p `plusPtr` 24 :: Ptr SwapchainUsageFlags))
    format <- peek @Int64 ((p `plusPtr` 32 :: Ptr Int64))
    sampleCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    width <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    faceCount <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    arraySize <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    mipCount <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    pure $ SwapchainCreateInfo
             next' createFlags usageFlags format sampleCount width height faceCount arraySize mipCount

instance es ~ '[] => Zero (SwapchainCreateInfo es) where
  zero = SwapchainCreateInfo
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | XrSwapchainImageBaseHeader - Image base header for a swapchain image
--
-- == Member Descriptions
--
-- = Description
--
-- The 'SwapchainImageBaseHeader' is a base structure that can be
-- overridden by a graphics API-specific @XrSwapchainImage*@ child
-- structure.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'enumerateSwapchainImages'
data SwapchainImageBaseHeader = SwapchainImageBaseHeader
  { -- | @type@ is the 'OpenXR.Core10.Enums.StructureType.StructureType' of this
    -- structure. This base structure itself has no associated
    -- 'OpenXR.Core10.Enums.StructureType.StructureType' value.
    --
    -- #VUID-XrSwapchainImageBaseHeader-type-type# @type@ /must/ be one of the
    -- following XrStructureType values:
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_SWAPCHAIN_IMAGE_D3D11_KHR',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_SWAPCHAIN_IMAGE_D3D12_KHR',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR'
    type' :: StructureType }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainImageBaseHeader)
#endif
deriving instance Show SwapchainImageBaseHeader

class ToCStruct a => IsSwapchainImage a where
  toSwapchainImageBaseHeader :: a -> SwapchainImageBaseHeader

instance Inheritable SwapchainImageBaseHeader where
  peekSomeCChild :: Ptr (SomeChild SwapchainImageBaseHeader) -> IO (SomeChild SwapchainImageBaseHeader)
  peekSomeCChild p = do
    ty <- peek @StructureType (castPtr @(SomeChild SwapchainImageBaseHeader) @StructureType p)
    case ty of
      TYPE_SWAPCHAIN_IMAGE_D3D12_KHR -> SomeChild <$> peekCStruct (castPtr @(SomeChild SwapchainImageBaseHeader) @SwapchainImageD3D12KHR p)
      TYPE_SWAPCHAIN_IMAGE_D3D11_KHR -> SomeChild <$> peekCStruct (castPtr @(SomeChild SwapchainImageBaseHeader) @SwapchainImageD3D11KHR p)
      TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR -> SomeChild <$> peekCStruct (castPtr @(SomeChild SwapchainImageBaseHeader) @SwapchainImageVulkanKHR p)
      TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR -> SomeChild <$> peekCStruct (castPtr @(SomeChild SwapchainImageBaseHeader) @SwapchainImageOpenGLESKHR p)
      TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR -> SomeChild <$> peekCStruct (castPtr @(SomeChild SwapchainImageBaseHeader) @SwapchainImageOpenGLKHR p)
      c -> throwIO $
        IOError
          Nothing
          InvalidArgument
          "peekSomeCChild"
          ("Illegal struct inheritance of SwapchainImageBaseHeader with " <> show c)
          Nothing
          Nothing

instance ToCStruct SwapchainImageBaseHeader where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainImageBaseHeader{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (type')
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainImageBaseHeader where
  peekCStruct p = do
    type' <- peek @StructureType ((p `plusPtr` 0 :: Ptr StructureType))
    pure $ SwapchainImageBaseHeader
             type'

instance Storable SwapchainImageBaseHeader where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainImageBaseHeader where
  zero = SwapchainImageBaseHeader
           zero


-- | XrSwapchainImageAcquireInfo - Describes a swapchain image acquisition
--
-- == Member Descriptions
--
-- = Description
--
-- Because this structure only exists to support extension-specific
-- structures, 'acquireSwapchainImage' will accept a @NULL@ argument for
-- @acquireInfo@ for applications that are not using any relevant
-- extensions.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'acquireSwapchainImage'
data SwapchainImageAcquireInfo = SwapchainImageAcquireInfo
  {}
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainImageAcquireInfo)
#endif
deriving instance Show SwapchainImageAcquireInfo

instance ToCStruct SwapchainImageAcquireInfo where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainImageAcquireInfo f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_ACQUIRE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_ACQUIRE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainImageAcquireInfo where
  peekCStruct _ = pure $ SwapchainImageAcquireInfo
                           

instance Storable SwapchainImageAcquireInfo where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainImageAcquireInfo where
  zero = SwapchainImageAcquireInfo
           


-- | XrSwapchainImageWaitInfo - Describes a swapchain image wait operation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrDuration >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'waitSwapchainImage'
data SwapchainImageWaitInfo = SwapchainImageWaitInfo
  { -- | @timeout@ indicates how many nanoseconds the call should block waiting
    -- for the image to become available for writing.
    timeout :: Duration }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainImageWaitInfo)
#endif
deriving instance Show SwapchainImageWaitInfo

instance ToCStruct SwapchainImageWaitInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainImageWaitInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_WAIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Duration)) (timeout)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_WAIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Duration)) (zero)
    f

instance FromCStruct SwapchainImageWaitInfo where
  peekCStruct p = do
    timeout <- peek @Duration ((p `plusPtr` 16 :: Ptr Duration))
    pure $ SwapchainImageWaitInfo
             timeout

instance Storable SwapchainImageWaitInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainImageWaitInfo where
  zero = SwapchainImageWaitInfo
           zero


-- | XrSwapchainImageReleaseInfo - Describes a swapchain image release
--
-- == Member Descriptions
--
-- = Description
--
-- Because this structure only exists to support extension-specific
-- structures, 'releaseSwapchainImage' will accept a @NULL@ argument for
-- @releaseInfo@ for applications that are not using any relevant
-- extensions.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'releaseSwapchainImage'
data SwapchainImageReleaseInfo = SwapchainImageReleaseInfo
  {}
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainImageReleaseInfo)
#endif
deriving instance Show SwapchainImageReleaseInfo

instance ToCStruct SwapchainImageReleaseInfo where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainImageReleaseInfo f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_RELEASE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SWAPCHAIN_IMAGE_RELEASE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainImageReleaseInfo where
  peekCStruct _ = pure $ SwapchainImageReleaseInfo
                           

instance Storable SwapchainImageReleaseInfo where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainImageReleaseInfo where
  zero = SwapchainImageReleaseInfo
           

