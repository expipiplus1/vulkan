{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_debug_marker"
module Vulkan.Extensions.VK_EXT_debug_marker  ( debugMarkerSetObjectNameEXT
                                              , debugMarkerSetObjectTagEXT
                                              , cmdDebugMarkerBeginEXT
                                              , cmdDebugMarkerEndEXT
                                              , cmdDebugMarkerInsertEXT
                                              , DebugMarkerObjectNameInfoEXT(..)
                                              , DebugMarkerObjectTagInfoEXT(..)
                                              , DebugMarkerMarkerInfoEXT(..)
                                              , EXT_DEBUG_MARKER_SPEC_VERSION
                                              , pattern EXT_DEBUG_MARKER_SPEC_VERSION
                                              , EXT_DEBUG_MARKER_EXTENSION_NAME
                                              , pattern EXT_DEBUG_MARKER_EXTENSION_NAME
                                              , DebugReportObjectTypeEXT(..)
                                              ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDebugMarkerBeginEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDebugMarkerEndEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDebugMarkerInsertEXT))
import Vulkan.Dynamic (DeviceCmds(pVkDebugMarkerSetObjectNameEXT))
import Vulkan.Dynamic (DeviceCmds(pVkDebugMarkerSetObjectTagEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDebugMarkerSetObjectNameEXT
  :: FunPtr (Ptr Device_T -> Ptr DebugMarkerObjectNameInfoEXT -> IO Result) -> Ptr Device_T -> Ptr DebugMarkerObjectNameInfoEXT -> IO Result

-- No documentation found for TopLevel "vkDebugMarkerSetObjectNameEXT"
debugMarkerSetObjectNameEXT :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "vkDebugMarkerSetObjectNameEXT" "device"
                               Device
                            -> -- No documentation found for Nested "vkDebugMarkerSetObjectNameEXT" "pNameInfo"
                               DebugMarkerObjectNameInfoEXT
                            -> io ()
debugMarkerSetObjectNameEXT device nameInfo = liftIO . evalContT $ do
  let vkDebugMarkerSetObjectNameEXTPtr = pVkDebugMarkerSetObjectNameEXT (deviceCmds (device :: Device))
  lift $ unless (vkDebugMarkerSetObjectNameEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDebugMarkerSetObjectNameEXT is null" Nothing Nothing
  let vkDebugMarkerSetObjectNameEXT' = mkVkDebugMarkerSetObjectNameEXT vkDebugMarkerSetObjectNameEXTPtr
  pNameInfo <- ContT $ withCStruct (nameInfo)
  r <- lift $ vkDebugMarkerSetObjectNameEXT' (deviceHandle (device)) pNameInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDebugMarkerSetObjectTagEXT
  :: FunPtr (Ptr Device_T -> Ptr DebugMarkerObjectTagInfoEXT -> IO Result) -> Ptr Device_T -> Ptr DebugMarkerObjectTagInfoEXT -> IO Result

-- No documentation found for TopLevel "vkDebugMarkerSetObjectTagEXT"
debugMarkerSetObjectTagEXT :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkDebugMarkerSetObjectTagEXT" "device"
                              Device
                           -> -- No documentation found for Nested "vkDebugMarkerSetObjectTagEXT" "pTagInfo"
                              DebugMarkerObjectTagInfoEXT
                           -> io ()
debugMarkerSetObjectTagEXT device tagInfo = liftIO . evalContT $ do
  let vkDebugMarkerSetObjectTagEXTPtr = pVkDebugMarkerSetObjectTagEXT (deviceCmds (device :: Device))
  lift $ unless (vkDebugMarkerSetObjectTagEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDebugMarkerSetObjectTagEXT is null" Nothing Nothing
  let vkDebugMarkerSetObjectTagEXT' = mkVkDebugMarkerSetObjectTagEXT vkDebugMarkerSetObjectTagEXTPtr
  pTagInfo <- ContT $ withCStruct (tagInfo)
  r <- lift $ vkDebugMarkerSetObjectTagEXT' (deviceHandle (device)) pTagInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDebugMarkerBeginEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DebugMarkerMarkerInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr DebugMarkerMarkerInfoEXT -> IO ()

-- No documentation found for TopLevel "vkCmdDebugMarkerBeginEXT"
cmdDebugMarkerBeginEXT :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkCmdDebugMarkerBeginEXT" "commandBuffer"
                          CommandBuffer
                       -> -- No documentation found for Nested "vkCmdDebugMarkerBeginEXT" "pMarkerInfo"
                          DebugMarkerMarkerInfoEXT
                       -> io ()
cmdDebugMarkerBeginEXT commandBuffer markerInfo = liftIO . evalContT $ do
  let vkCmdDebugMarkerBeginEXTPtr = pVkCmdDebugMarkerBeginEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdDebugMarkerBeginEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDebugMarkerBeginEXT is null" Nothing Nothing
  let vkCmdDebugMarkerBeginEXT' = mkVkCmdDebugMarkerBeginEXT vkCmdDebugMarkerBeginEXTPtr
  pMarkerInfo <- ContT $ withCStruct (markerInfo)
  lift $ vkCmdDebugMarkerBeginEXT' (commandBufferHandle (commandBuffer)) pMarkerInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDebugMarkerEndEXT
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- No documentation found for TopLevel "vkCmdDebugMarkerEndEXT"
cmdDebugMarkerEndEXT :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkCmdDebugMarkerEndEXT" "commandBuffer"
                        CommandBuffer
                     -> io ()
cmdDebugMarkerEndEXT commandBuffer = liftIO $ do
  let vkCmdDebugMarkerEndEXTPtr = pVkCmdDebugMarkerEndEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDebugMarkerEndEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDebugMarkerEndEXT is null" Nothing Nothing
  let vkCmdDebugMarkerEndEXT' = mkVkCmdDebugMarkerEndEXT vkCmdDebugMarkerEndEXTPtr
  vkCmdDebugMarkerEndEXT' (commandBufferHandle (commandBuffer))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDebugMarkerInsertEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DebugMarkerMarkerInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr DebugMarkerMarkerInfoEXT -> IO ()

-- No documentation found for TopLevel "vkCmdDebugMarkerInsertEXT"
cmdDebugMarkerInsertEXT :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkCmdDebugMarkerInsertEXT" "commandBuffer"
                           CommandBuffer
                        -> -- No documentation found for Nested "vkCmdDebugMarkerInsertEXT" "pMarkerInfo"
                           DebugMarkerMarkerInfoEXT
                        -> io ()
cmdDebugMarkerInsertEXT commandBuffer markerInfo = liftIO . evalContT $ do
  let vkCmdDebugMarkerInsertEXTPtr = pVkCmdDebugMarkerInsertEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdDebugMarkerInsertEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDebugMarkerInsertEXT is null" Nothing Nothing
  let vkCmdDebugMarkerInsertEXT' = mkVkCmdDebugMarkerInsertEXT vkCmdDebugMarkerInsertEXTPtr
  pMarkerInfo <- ContT $ withCStruct (markerInfo)
  lift $ vkCmdDebugMarkerInsertEXT' (commandBufferHandle (commandBuffer)) pMarkerInfo
  pure $ ()



-- No documentation found for TopLevel "VkDebugMarkerObjectNameInfoEXT"
data DebugMarkerObjectNameInfoEXT = DebugMarkerObjectNameInfoEXT
  { -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "objectType"
    objectType :: DebugReportObjectTypeEXT
  , -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "object"
    object :: Word64
  , -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "pObjectName"
    objectName :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugMarkerObjectNameInfoEXT)
#endif
deriving instance Show DebugMarkerObjectNameInfoEXT

instance ToCStruct DebugMarkerObjectNameInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugMarkerObjectNameInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DebugReportObjectTypeEXT)) (objectType)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word64)) (object)
    pObjectName'' <- ContT $ useAsCString (objectName)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) pObjectName''
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DebugReportObjectTypeEXT)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    pObjectName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) pObjectName''
    lift $ f

instance FromCStruct DebugMarkerObjectNameInfoEXT where
  peekCStruct p = do
    objectType <- peek @DebugReportObjectTypeEXT ((p `plusPtr` 16 :: Ptr DebugReportObjectTypeEXT))
    object <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pObjectName <- packCString =<< peek ((p `plusPtr` 32 :: Ptr (Ptr CChar)))
    pure $ DebugMarkerObjectNameInfoEXT
             objectType object pObjectName

instance Zero DebugMarkerObjectNameInfoEXT where
  zero = DebugMarkerObjectNameInfoEXT
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkDebugMarkerObjectTagInfoEXT"
data DebugMarkerObjectTagInfoEXT = DebugMarkerObjectTagInfoEXT
  { -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "objectType"
    objectType :: DebugReportObjectTypeEXT
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "object"
    object :: Word64
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "tagName"
    tagName :: Word64
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "tagSize"
    tagSize :: Word64
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "pTag"
    tag :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugMarkerObjectTagInfoEXT)
#endif
deriving instance Show DebugMarkerObjectTagInfoEXT

instance ToCStruct DebugMarkerObjectTagInfoEXT where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugMarkerObjectTagInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DebugReportObjectTypeEXT)) (objectType)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (object)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (tagName)
    poke ((p `plusPtr` 40 :: Ptr CSize)) (CSize (tagSize))
    poke ((p `plusPtr` 48 :: Ptr (Ptr ()))) (tag)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DebugReportObjectTypeEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 48 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct DebugMarkerObjectTagInfoEXT where
  peekCStruct p = do
    objectType <- peek @DebugReportObjectTypeEXT ((p `plusPtr` 16 :: Ptr DebugReportObjectTypeEXT))
    object <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    tagName <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    tagSize <- peek @CSize ((p `plusPtr` 40 :: Ptr CSize))
    pTag <- peek @(Ptr ()) ((p `plusPtr` 48 :: Ptr (Ptr ())))
    pure $ DebugMarkerObjectTagInfoEXT
             objectType object tagName ((\(CSize a) -> a) tagSize) pTag


instance Storable DebugMarkerObjectTagInfoEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DebugMarkerObjectTagInfoEXT where
  zero = DebugMarkerObjectTagInfoEXT
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkDebugMarkerMarkerInfoEXT"
data DebugMarkerMarkerInfoEXT = DebugMarkerMarkerInfoEXT
  { -- No documentation found for Nested "VkDebugMarkerMarkerInfoEXT" "pMarkerName"
    markerName :: ByteString
  , -- No documentation found for Nested "VkDebugMarkerMarkerInfoEXT" "color"
    color :: (Float, Float, Float, Float)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugMarkerMarkerInfoEXT)
#endif
deriving instance Show DebugMarkerMarkerInfoEXT

instance ToCStruct DebugMarkerMarkerInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugMarkerMarkerInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pMarkerName'' <- ContT $ useAsCString (markerName)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) pMarkerName''
    let pColor' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 4 CFloat)))
    lift $ case (color) of
      (e0, e1, e2, e3) -> do
        poke (pColor' :: Ptr CFloat) (CFloat (e0))
        poke (pColor' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pColor' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pColor' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pMarkerName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) pMarkerName''
    lift $ f

instance FromCStruct DebugMarkerMarkerInfoEXT where
  peekCStruct p = do
    pMarkerName <- packCString =<< peek ((p `plusPtr` 16 :: Ptr (Ptr CChar)))
    let pcolor = lowerArrayPtr @CFloat ((p `plusPtr` 24 :: Ptr (FixedArray 4 CFloat)))
    color0 <- peek @CFloat ((pcolor `advancePtrBytes` 0 :: Ptr CFloat))
    color1 <- peek @CFloat ((pcolor `advancePtrBytes` 4 :: Ptr CFloat))
    color2 <- peek @CFloat ((pcolor `advancePtrBytes` 8 :: Ptr CFloat))
    color3 <- peek @CFloat ((pcolor `advancePtrBytes` 12 :: Ptr CFloat))
    pure $ DebugMarkerMarkerInfoEXT
             pMarkerName ((((\(CFloat a) -> a) color0), ((\(CFloat a) -> a) color1), ((\(CFloat a) -> a) color2), ((\(CFloat a) -> a) color3)))

instance Zero DebugMarkerMarkerInfoEXT where
  zero = DebugMarkerMarkerInfoEXT
           mempty
           (zero, zero, zero, zero)


type EXT_DEBUG_MARKER_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_EXT_DEBUG_MARKER_SPEC_VERSION"
pattern EXT_DEBUG_MARKER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEBUG_MARKER_SPEC_VERSION = 4


type EXT_DEBUG_MARKER_EXTENSION_NAME = "VK_EXT_debug_marker"

-- No documentation found for TopLevel "VK_EXT_DEBUG_MARKER_EXTENSION_NAME"
pattern EXT_DEBUG_MARKER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEBUG_MARKER_EXTENSION_NAME = "VK_EXT_debug_marker"

