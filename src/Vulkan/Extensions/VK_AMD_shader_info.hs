{-# language CPP #-}
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
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
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
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Text.Read.Lex (Lexeme(Ident))
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

-- | vkGetShaderInfoAMD - Get information about a shader in a pipeline
--
-- = Description
--
-- If @pInfo@ is @NULL@, then the maximum size of the information that
-- /can/ be retrieved about the shader, in bytes, is returned in
-- @pInfoSize@. Otherwise, @pInfoSize@ /must/ point to a variable set by
-- the user to the size of the buffer, in bytes, pointed to by @pInfo@, and
-- on return the variable is overwritten with the amount of data actually
-- written to @pInfo@.
--
-- If @pInfoSize@ is less than the maximum size that /can/ be retrieved by
-- the pipeline cache, then at most @pInfoSize@ bytes will be written to
-- @pInfo@, and 'getShaderInfoAMD' will return
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE'.
--
-- Not all information is available for every shader and implementations
-- may not support all kinds of information for any shader. When a certain
-- type of information is unavailable, the function returns
-- 'Vulkan.Core10.Enums.Result.ERROR_FEATURE_NOT_PRESENT'.
--
-- If information is successfully and fully queried, the function will
-- return 'Vulkan.Core10.Enums.Result.SUCCESS'.
--
-- For @infoType@ 'SHADER_INFO_TYPE_STATISTICS_AMD', a
-- 'ShaderStatisticsInfoAMD' structure will be written to the buffer
-- pointed to by @pInfo@. This structure will be populated with statistics
-- regarding the physical device resources used by that shader along with
-- other miscellaneous information and is described in further detail
-- below.
--
-- For @infoType@ 'SHADER_INFO_TYPE_DISASSEMBLY_AMD', @pInfo@ is a pointer
-- to a UTF-8 null-terminated string containing human-readable disassembly.
-- The exact formatting and contents of the disassembly string are
-- vendor-specific.
--
-- The formatting and contents of all other types of information, including
-- @infoType@ 'SHADER_INFO_TYPE_BINARY_AMD', are left to the vendor and are
-- not further specified by this extension.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   @shaderStage@ /must/ be a valid
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' value
--
-- -   @infoType@ /must/ be a valid 'ShaderInfoTypeAMD' value
--
-- -   @pInfoSize@ /must/ be a valid pointer to a @size_t@ value
--
-- -   If the value referenced by @pInfoSize@ is not @0@, and @pInfo@ is
--     not @NULL@, @pInfo@ /must/ be a valid pointer to an array of
--     @pInfoSize@ bytes
--
-- -   @pipeline@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FEATURE_NOT_PRESENT'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline',
-- 'ShaderInfoTypeAMD',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits'
getShaderInfoAMD :: forall io . MonadIO io => Device -> Pipeline -> ShaderStageFlagBits -> ShaderInfoTypeAMD -> io (Result, ("info" ::: ByteString))
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


-- | VkShaderResourceUsageAMD - Resource usage information about a particular
-- shader within a pipeline
--
-- = See Also
--
-- 'ShaderStatisticsInfoAMD'
data ShaderResourceUsageAMD = ShaderResourceUsageAMD
  { -- | @numUsedVgprs@ is the number of vector instruction general-purpose
    -- registers used by this shader.
    numUsedVgprs :: Word32
  , -- | @numUsedSgprs@ is the number of scalar instruction general-purpose
    -- registers used by this shader.
    numUsedSgprs :: Word32
  , -- | @ldsSizePerLocalWorkGroup@ is the maximum local data store size per work
    -- group in bytes.
    ldsSizePerLocalWorkGroup :: Word32
  , -- | @ldsUsageSizeInBytes@ is the LDS usage size in bytes per work group by
    -- this shader.
    ldsUsageSizeInBytes :: Word64
  , -- | @scratchMemUsageInBytes@ is the scratch memory usage in bytes by this
    -- shader.
    scratchMemUsageInBytes :: Word64
  }
  deriving (Typeable)
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


-- | VkShaderStatisticsInfoAMD - Statistical information about a particular
-- shader within a pipeline
--
-- = Description
--
-- Some implementations may merge multiple logical shader stages together
-- in a single shader. In such cases, @shaderStageMask@ will contain a
-- bitmask of all of the stages that are active within that shader.
-- Consequently, if specifying those stages as input to 'getShaderInfoAMD',
-- the same output information /may/ be returned for all such shader stage
-- queries.
--
-- The number of available VGPRs and SGPRs (@numAvailableVgprs@ and
-- @numAvailableSgprs@ respectively) are the shader-addressable subset of
-- physical registers that is given as a limit to the compiler for register
-- assignment. These values /may/ further be limited by implementations due
-- to performance optimizations where register pressure is a bottleneck.
--
-- = See Also
--
-- 'ShaderResourceUsageAMD',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags'
data ShaderStatisticsInfoAMD = ShaderStatisticsInfoAMD
  { -- | @shaderStageMask@ are the combination of logical shader stages contained
    -- within this shader.
    shaderStageMask :: ShaderStageFlags
  , -- | @resourceUsage@ is a 'ShaderResourceUsageAMD' structure describing
    -- internal physical device resources used by this shader.
    resourceUsage :: ShaderResourceUsageAMD
  , -- | @numPhysicalVgprs@ is the maximum number of vector instruction
    -- general-purpose registers (VGPRs) available to the physical device.
    numPhysicalVgprs :: Word32
  , -- | @numPhysicalSgprs@ is the maximum number of scalar instruction
    -- general-purpose registers (SGPRs) available to the physical device.
    numPhysicalSgprs :: Word32
  , -- | @numAvailableVgprs@ is the maximum limit of VGPRs made available to the
    -- shader compiler.
    numAvailableVgprs :: Word32
  , -- | @numAvailableSgprs@ is the maximum limit of SGPRs made available to the
    -- shader compiler.
    numAvailableSgprs :: Word32
  , -- | @computeWorkGroupSize@ is the local workgroup size of this shader in {
    -- X, Y, Z } dimensions.
    computeWorkGroupSize :: (Word32, Word32, Word32)
  }
  deriving (Typeable)
deriving instance Show ShaderStatisticsInfoAMD

instance ToCStruct ShaderStatisticsInfoAMD where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShaderStatisticsInfoAMD{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr ShaderStageFlags)) (shaderStageMask)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr ShaderResourceUsageAMD)) (resourceUsage) . ($ ())
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (numPhysicalVgprs)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (numPhysicalSgprs)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (numAvailableVgprs)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (numAvailableSgprs)
    let pComputeWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 56 :: Ptr (FixedArray 3 Word32)))
    lift $ case (computeWorkGroupSize) of
      (e0, e1, e2) -> do
        poke (pComputeWorkGroupSize' :: Ptr Word32) (e0)
        poke (pComputeWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pComputeWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr ShaderStageFlags)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr ShaderResourceUsageAMD)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    let pComputeWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 56 :: Ptr (FixedArray 3 Word32)))
    lift $ case ((zero, zero, zero)) of
      (e0, e1, e2) -> do
        poke (pComputeWorkGroupSize' :: Ptr Word32) (e0)
        poke (pComputeWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pComputeWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    lift $ f

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

instance Zero ShaderStatisticsInfoAMD where
  zero = ShaderStatisticsInfoAMD
           zero
           zero
           zero
           zero
           zero
           zero
           (zero, zero, zero)


-- | VkShaderInfoTypeAMD - Enum specifying which type of shader info to query
--
-- = See Also
--
-- 'getShaderInfoAMD'
newtype ShaderInfoTypeAMD = ShaderInfoTypeAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SHADER_INFO_TYPE_STATISTICS_AMD' specifies that device resources used
-- by a shader will be queried.
pattern SHADER_INFO_TYPE_STATISTICS_AMD = ShaderInfoTypeAMD 0
-- | 'SHADER_INFO_TYPE_BINARY_AMD' specifies that implementation-specific
-- information will be queried.
pattern SHADER_INFO_TYPE_BINARY_AMD = ShaderInfoTypeAMD 1
-- | 'SHADER_INFO_TYPE_DISASSEMBLY_AMD' specifies that human-readable
-- dissassembly of a shader.
pattern SHADER_INFO_TYPE_DISASSEMBLY_AMD = ShaderInfoTypeAMD 2
{-# complete SHADER_INFO_TYPE_STATISTICS_AMD,
             SHADER_INFO_TYPE_BINARY_AMD,
             SHADER_INFO_TYPE_DISASSEMBLY_AMD :: ShaderInfoTypeAMD #-}

instance Show ShaderInfoTypeAMD where
  showsPrec p = \case
    SHADER_INFO_TYPE_STATISTICS_AMD -> showString "SHADER_INFO_TYPE_STATISTICS_AMD"
    SHADER_INFO_TYPE_BINARY_AMD -> showString "SHADER_INFO_TYPE_BINARY_AMD"
    SHADER_INFO_TYPE_DISASSEMBLY_AMD -> showString "SHADER_INFO_TYPE_DISASSEMBLY_AMD"
    ShaderInfoTypeAMD x -> showParen (p >= 11) (showString "ShaderInfoTypeAMD " . showsPrec 11 x)

instance Read ShaderInfoTypeAMD where
  readPrec = parens (choose [("SHADER_INFO_TYPE_STATISTICS_AMD", pure SHADER_INFO_TYPE_STATISTICS_AMD)
                            , ("SHADER_INFO_TYPE_BINARY_AMD", pure SHADER_INFO_TYPE_BINARY_AMD)
                            , ("SHADER_INFO_TYPE_DISASSEMBLY_AMD", pure SHADER_INFO_TYPE_DISASSEMBLY_AMD)]
                     +++
                     prec 10 (do
                       expectP (Ident "ShaderInfoTypeAMD")
                       v <- step readPrec
                       pure (ShaderInfoTypeAMD v)))


type AMD_SHADER_INFO_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_SHADER_INFO_SPEC_VERSION"
pattern AMD_SHADER_INFO_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_SHADER_INFO_SPEC_VERSION = 1


type AMD_SHADER_INFO_EXTENSION_NAME = "VK_AMD_shader_info"

-- No documentation found for TopLevel "VK_AMD_SHADER_INFO_EXTENSION_NAME"
pattern AMD_SHADER_INFO_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_SHADER_INFO_EXTENSION_NAME = "VK_AMD_shader_info"

