{-# language CPP #-}
-- No documentation found for Chapter "VK_AMD_shader_info"
module Vulkan.Extensions.VK_AMD_shader_info  ( getShaderInfoAMD
                                             , ShaderResourceUsageAMD(..)
                                             , ShaderStatisticsInfoAMD(..)
                                             , ShaderInfoTypeAMD( SHADER_INFO_TYPE_STATISTICS_AMD
                                                                , SHADER_INFO_TYPE_BINARY_AMD
                                                                , SHADER_INFO_TYPE_DISASSEMBLY_AMD
                                                                , ..
                                                                )
                                             , AMD_SHADER_INFO_SPEC_VERSION
                                             , pattern AMD_SHADER_INFO_SPEC_VERSION
                                             , AMD_SHADER_INFO_EXTENSION_NAME
                                             , pattern AMD_SHADER_INFO_EXTENSION_NAME
                                             ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.ByteString (packCStringLen)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Foreign.C.Types (CSize(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
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
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetShaderInfoAMD))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetShaderInfoAMD
  :: FunPtr (Ptr Device_T -> Pipeline -> ShaderStageFlagBits -> ShaderInfoTypeAMD -> Ptr CSize -> Ptr () -> IO Result) -> Ptr Device_T -> Pipeline -> ShaderStageFlagBits -> ShaderInfoTypeAMD -> Ptr CSize -> Ptr () -> IO Result

-- No documentation found for TopLevel "vkGetShaderInfoAMD"
getShaderInfoAMD :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkGetShaderInfoAMD" "device"
                    Device
                 -> -- No documentation found for Nested "vkGetShaderInfoAMD" "pipeline"
                    Pipeline
                 -> -- No documentation found for Nested "vkGetShaderInfoAMD" "shaderStage"
                    ShaderStageFlagBits
                 -> -- No documentation found for Nested "vkGetShaderInfoAMD" "infoType"
                    ShaderInfoTypeAMD
                 -> io (Result, ("info" ::: ByteString))
getShaderInfoAMD device pipeline shaderStage infoType = liftIO . evalContT $ do
  let vkGetShaderInfoAMDPtr = pVkGetShaderInfoAMD (deviceCmds (device :: Device))
  lift $ unless (vkGetShaderInfoAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetShaderInfoAMD is null" Nothing Nothing
  let vkGetShaderInfoAMD' = mkVkGetShaderInfoAMD vkGetShaderInfoAMDPtr
  let device' = deviceHandle (device)
  pPInfoSize <- ContT $ bracket (callocBytes @CSize 8) free
  r <- lift $ vkGetShaderInfoAMD' device' (pipeline) (shaderStage) (infoType) (pPInfoSize) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pInfoSize <- lift $ peek @CSize pPInfoSize
  pPInfo <- ContT $ bracket (callocBytes @(()) (fromIntegral (((\(CSize a) -> a) pInfoSize)))) free
  r' <- lift $ vkGetShaderInfoAMD' device' (pipeline) (shaderStage) (infoType) (pPInfoSize) (pPInfo)
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pInfoSize'' <- lift $ peek @CSize pPInfoSize
  pInfo' <- lift $ packCStringLen  (castPtr @() @CChar pPInfo, (fromIntegral (((\(CSize a) -> a) pInfoSize''))))
  pure $ ((r'), pInfo')



-- No documentation found for TopLevel "VkShaderResourceUsageAMD"
data ShaderResourceUsageAMD = ShaderResourceUsageAMD
  { -- No documentation found for Nested "VkShaderResourceUsageAMD" "numUsedVgprs"
    numUsedVgprs :: Word32
  , -- No documentation found for Nested "VkShaderResourceUsageAMD" "numUsedSgprs"
    numUsedSgprs :: Word32
  , -- No documentation found for Nested "VkShaderResourceUsageAMD" "ldsSizePerLocalWorkGroup"
    ldsSizePerLocalWorkGroup :: Word32
  , -- No documentation found for Nested "VkShaderResourceUsageAMD" "ldsUsageSizeInBytes"
    ldsUsageSizeInBytes :: Word64
  , -- No documentation found for Nested "VkShaderResourceUsageAMD" "scratchMemUsageInBytes"
    scratchMemUsageInBytes :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderResourceUsageAMD)
#endif
deriving instance Show ShaderResourceUsageAMD

instance ToCStruct ShaderResourceUsageAMD where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShaderResourceUsageAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (numUsedVgprs)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (numUsedSgprs)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (ldsSizePerLocalWorkGroup)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (ldsUsageSizeInBytes))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (scratchMemUsageInBytes))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (zero))
    f

instance FromCStruct ShaderResourceUsageAMD where
  peekCStruct p = do
    numUsedVgprs <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    numUsedSgprs <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    ldsSizePerLocalWorkGroup <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    ldsUsageSizeInBytes <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    scratchMemUsageInBytes <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    pure $ ShaderResourceUsageAMD
             numUsedVgprs numUsedSgprs ldsSizePerLocalWorkGroup ((\(CSize a) -> a) ldsUsageSizeInBytes) ((\(CSize a) -> a) scratchMemUsageInBytes)


instance Storable ShaderResourceUsageAMD where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ShaderResourceUsageAMD where
  zero = ShaderResourceUsageAMD
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkShaderStatisticsInfoAMD"
data ShaderStatisticsInfoAMD = ShaderStatisticsInfoAMD
  { -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "shaderStageMask"
    shaderStageMask :: ShaderStageFlags
  , -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "resourceUsage"
    resourceUsage :: ShaderResourceUsageAMD
  , -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "numPhysicalVgprs"
    numPhysicalVgprs :: Word32
  , -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "numPhysicalSgprs"
    numPhysicalSgprs :: Word32
  , -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "numAvailableVgprs"
    numAvailableVgprs :: Word32
  , -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "numAvailableSgprs"
    numAvailableSgprs :: Word32
  , -- No documentation found for Nested "VkShaderStatisticsInfoAMD" "computeWorkGroupSize"
    computeWorkGroupSize :: (Word32, Word32, Word32)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderStatisticsInfoAMD)
#endif
deriving instance Show ShaderStatisticsInfoAMD

instance ToCStruct ShaderStatisticsInfoAMD where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShaderStatisticsInfoAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ShaderStageFlags)) (shaderStageMask)
    poke ((p `plusPtr` 8 :: Ptr ShaderResourceUsageAMD)) (resourceUsage)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (numPhysicalVgprs)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (numPhysicalSgprs)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (numAvailableVgprs)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (numAvailableSgprs)
    let pComputeWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 56 :: Ptr (FixedArray 3 Word32)))
    case (computeWorkGroupSize) of
      (e0, e1, e2) -> do
        poke (pComputeWorkGroupSize' :: Ptr Word32) (e0)
        poke (pComputeWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pComputeWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ShaderStageFlags)) (zero)
    poke ((p `plusPtr` 8 :: Ptr ShaderResourceUsageAMD)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    let pComputeWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 56 :: Ptr (FixedArray 3 Word32)))
    case ((zero, zero, zero)) of
      (e0, e1, e2) -> do
        poke (pComputeWorkGroupSize' :: Ptr Word32) (e0)
        poke (pComputeWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pComputeWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    f

instance FromCStruct ShaderStatisticsInfoAMD where
  peekCStruct p = do
    shaderStageMask <- peek @ShaderStageFlags ((p `plusPtr` 0 :: Ptr ShaderStageFlags))
    resourceUsage <- peekCStruct @ShaderResourceUsageAMD ((p `plusPtr` 8 :: Ptr ShaderResourceUsageAMD))
    numPhysicalVgprs <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    numPhysicalSgprs <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    numAvailableVgprs <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    numAvailableSgprs <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    let pcomputeWorkGroupSize = lowerArrayPtr @Word32 ((p `plusPtr` 56 :: Ptr (FixedArray 3 Word32)))
    computeWorkGroupSize0 <- peek @Word32 ((pcomputeWorkGroupSize `advancePtrBytes` 0 :: Ptr Word32))
    computeWorkGroupSize1 <- peek @Word32 ((pcomputeWorkGroupSize `advancePtrBytes` 4 :: Ptr Word32))
    computeWorkGroupSize2 <- peek @Word32 ((pcomputeWorkGroupSize `advancePtrBytes` 8 :: Ptr Word32))
    pure $ ShaderStatisticsInfoAMD
             shaderStageMask resourceUsage numPhysicalVgprs numPhysicalSgprs numAvailableVgprs numAvailableSgprs ((computeWorkGroupSize0, computeWorkGroupSize1, computeWorkGroupSize2))


instance Storable ShaderStatisticsInfoAMD where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ShaderStatisticsInfoAMD where
  zero = ShaderStatisticsInfoAMD
           zero
           zero
           zero
           zero
           zero
           zero
           (zero, zero, zero)


-- No documentation found for TopLevel "VkShaderInfoTypeAMD"
newtype ShaderInfoTypeAMD = ShaderInfoTypeAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkShaderInfoTypeAMD" "VK_SHADER_INFO_TYPE_STATISTICS_AMD"
pattern SHADER_INFO_TYPE_STATISTICS_AMD  = ShaderInfoTypeAMD 0
-- No documentation found for Nested "VkShaderInfoTypeAMD" "VK_SHADER_INFO_TYPE_BINARY_AMD"
pattern SHADER_INFO_TYPE_BINARY_AMD      = ShaderInfoTypeAMD 1
-- No documentation found for Nested "VkShaderInfoTypeAMD" "VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD"
pattern SHADER_INFO_TYPE_DISASSEMBLY_AMD = ShaderInfoTypeAMD 2
{-# complete SHADER_INFO_TYPE_STATISTICS_AMD,
             SHADER_INFO_TYPE_BINARY_AMD,
             SHADER_INFO_TYPE_DISASSEMBLY_AMD :: ShaderInfoTypeAMD #-}

conNameShaderInfoTypeAMD :: String
conNameShaderInfoTypeAMD = "ShaderInfoTypeAMD"

enumPrefixShaderInfoTypeAMD :: String
enumPrefixShaderInfoTypeAMD = "SHADER_INFO_TYPE_"

showTableShaderInfoTypeAMD :: [(ShaderInfoTypeAMD, String)]
showTableShaderInfoTypeAMD =
  [ (SHADER_INFO_TYPE_STATISTICS_AMD , "STATISTICS_AMD")
  , (SHADER_INFO_TYPE_BINARY_AMD     , "BINARY_AMD")
  , (SHADER_INFO_TYPE_DISASSEMBLY_AMD, "DISASSEMBLY_AMD")
  ]


instance Show ShaderInfoTypeAMD where
showsPrec = enumShowsPrec enumPrefixShaderInfoTypeAMD
                          showTableShaderInfoTypeAMD
                          conNameShaderInfoTypeAMD
                          (\(ShaderInfoTypeAMD x) -> x)
                          (showsPrec 11)


instance Read ShaderInfoTypeAMD where
  readPrec =
    enumReadPrec enumPrefixShaderInfoTypeAMD showTableShaderInfoTypeAMD conNameShaderInfoTypeAMD ShaderInfoTypeAMD


type AMD_SHADER_INFO_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_SHADER_INFO_SPEC_VERSION"
pattern AMD_SHADER_INFO_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_SHADER_INFO_SPEC_VERSION = 1


type AMD_SHADER_INFO_EXTENSION_NAME = "VK_AMD_shader_info"

-- No documentation found for TopLevel "VK_AMD_SHADER_INFO_EXTENSION_NAME"
pattern AMD_SHADER_INFO_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_SHADER_INFO_EXTENSION_NAME = "VK_AMD_shader_info"

