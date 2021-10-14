{-# language CPP #-}
-- | = Name
--
-- VK_AMD_shader_info - device extension
--
-- == VK_AMD_shader_info
--
-- [__Name String__]
--     @VK_AMD_shader_info@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     43
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Jaakko Konttinen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_shader_info] @jaakkoamd%0A<<Here describe the issue or question you have about the VK_AMD_shader_info extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-10-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jaakko Konttinen, AMD
--
-- == Description
--
-- This extension adds a way to query certain information about a compiled
-- shader which is part of a pipeline. This information may include shader
-- disassembly, shader binary and various statistics about a shader’s
-- resource usage.
--
-- While this extension provides a mechanism for extracting this
-- information, the details regarding the contents or format of this
-- information are not specified by this extension and may be provided by
-- the vendor externally.
--
-- Furthermore, all information types are optionally supported, and users
-- should not assume every implementation supports querying every type of
-- information.
--
-- == New Commands
--
-- -   'getShaderInfoAMD'
--
-- == New Structures
--
-- -   'ShaderResourceUsageAMD'
--
-- -   'ShaderStatisticsInfoAMD'
--
-- == New Enums
--
-- -   'ShaderInfoTypeAMD'
--
-- == New Enum Constants
--
-- -   'AMD_SHADER_INFO_EXTENSION_NAME'
--
-- -   'AMD_SHADER_INFO_SPEC_VERSION'
--
-- == Examples
--
-- This example extracts the register usage of a fragment shader within a
-- particular graphics pipeline:
--
-- > extern VkDevice device;
-- > extern VkPipeline gfxPipeline;
-- >
-- > PFN_vkGetShaderInfoAMD pfnGetShaderInfoAMD = (PFN_vkGetShaderInfoAMD)vkGetDeviceProcAddr(
-- >     device, "vkGetShaderInfoAMD");
-- >
-- > VkShaderStatisticsInfoAMD statistics = {};
-- >
-- > size_t dataSize = sizeof(statistics);
-- >
-- > if (pfnGetShaderInfoAMD(device,
-- >     gfxPipeline,
-- >     VK_SHADER_STAGE_FRAGMENT_BIT,
-- >     VK_SHADER_INFO_TYPE_STATISTICS_AMD,
-- >     &dataSize,
-- >     &statistics) == VK_SUCCESS)
-- > {
-- >     printf("VGPR usage: %d\n", statistics.resourceUsage.numUsedVgprs);
-- >     printf("SGPR usage: %d\n", statistics.resourceUsage.numUsedSgprs);
-- > }
--
-- The following example continues the previous example by subsequently
-- attempting to query and print shader disassembly about the fragment
-- shader:
--
-- > // Query disassembly size (if available)
-- > if (pfnGetShaderInfoAMD(device,
-- >     gfxPipeline,
-- >     VK_SHADER_STAGE_FRAGMENT_BIT,
-- >     VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD,
-- >     &dataSize,
-- >     nullptr) == VK_SUCCESS)
-- > {
-- >     printf("Fragment shader disassembly:\n");
-- >
-- >     void* disassembly = malloc(dataSize);
-- >
-- >     // Query disassembly and print
-- >     if (pfnGetShaderInfoAMD(device,
-- >         gfxPipeline,
-- >         VK_SHADER_STAGE_FRAGMENT_BIT,
-- >         VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD,
-- >         &dataSize,
-- >         disassembly) == VK_SUCCESS)
-- >     {
-- >         printf((char*)disassembly);
-- >     }
-- >
-- >     free(disassembly);
-- > }
--
-- == Version History
--
-- -   Revision 1, 2017-10-09 (Jaakko Konttinen)
--
--     -   Initial revision
--
-- = See Also
--
-- 'ShaderInfoTypeAMD', 'ShaderResourceUsageAMD',
-- 'ShaderStatisticsInfoAMD', 'getShaderInfoAMD'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_shader_info Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
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
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Foreign.C.Types (CSize(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
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
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Exception (VulkanException(..))
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
-- written to @pInfo@. If @pInfoSize@ is less than the maximum size that
-- /can/ be retrieved by the pipeline cache, then at most @pInfoSize@ bytes
-- will be written to @pInfo@, and 'Vulkan.Core10.Enums.Result.INCOMPLETE'
-- will be returned, instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to
-- indicate that not all required of the pipeline cache was returned.
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
-- -   #VUID-vkGetShaderInfoAMD-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetShaderInfoAMD-pipeline-parameter# @pipeline@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-vkGetShaderInfoAMD-shaderStage-parameter# @shaderStage@ /must/
--     be a valid
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' value
--
-- -   #VUID-vkGetShaderInfoAMD-infoType-parameter# @infoType@ /must/ be a
--     valid 'ShaderInfoTypeAMD' value
--
-- -   #VUID-vkGetShaderInfoAMD-pInfoSize-parameter# @pInfoSize@ /must/ be
--     a valid pointer to a @size_t@ value
--
-- -   #VUID-vkGetShaderInfoAMD-pInfo-parameter# If the value referenced by
--     @pInfoSize@ is not @0@, and @pInfo@ is not @NULL@, @pInfo@ /must/ be
--     a valid pointer to an array of @pInfoSize@ bytes
--
-- -   #VUID-vkGetShaderInfoAMD-pipeline-parent# @pipeline@ /must/ have
--     been created, allocated, or retrieved from @device@
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_shader_info VK_AMD_shader_info>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline',
-- 'ShaderInfoTypeAMD',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits'
getShaderInfoAMD :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the device that created @pipeline@.
                    Device
                 -> -- | @pipeline@ is the target of the query.
                    Pipeline
                 -> -- | @shaderStage@ is a
                    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' specifying
                    -- the particular shader within the pipeline about which information is
                    -- being queried.
                    ShaderStageFlagBits
                 -> -- | @infoType@ describes what kind of information is being queried.
                    ShaderInfoTypeAMD
                 -> io (Result, ("info" ::: ByteString))
getShaderInfoAMD device pipeline shaderStage infoType = liftIO . evalContT $ do
  let vkGetShaderInfoAMDPtr = pVkGetShaderInfoAMD (deviceCmds (device :: Device))
  lift $ unless (vkGetShaderInfoAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetShaderInfoAMD is null" Nothing Nothing
  let vkGetShaderInfoAMD' = mkVkGetShaderInfoAMD vkGetShaderInfoAMDPtr
  let device' = deviceHandle (device)
  pPInfoSize <- ContT $ bracket (callocBytes @CSize 8) free
  r <- lift $ traceAroundEvent "vkGetShaderInfoAMD" (vkGetShaderInfoAMD' device' (pipeline) (shaderStage) (infoType) (pPInfoSize) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pInfoSize <- lift $ peek @CSize pPInfoSize
  pPInfo <- ContT $ bracket (callocBytes @(()) (fromIntegral ((coerce @CSize @Word64 pInfoSize)))) free
  r' <- lift $ traceAroundEvent "vkGetShaderInfoAMD" (vkGetShaderInfoAMD' device' (pipeline) (shaderStage) (infoType) (pPInfoSize) (pPInfo))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pInfoSize'' <- lift $ peek @CSize pPInfoSize
  pInfo' <- lift $ packCStringLen  (castPtr @() @CChar pPInfo, (fromIntegral ((coerce @CSize @Word64 pInfoSize''))))
  pure $ ((r'), pInfo')


-- | VkShaderResourceUsageAMD - Resource usage information about a particular
-- shader within a pipeline
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_shader_info VK_AMD_shader_info>,
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
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderResourceUsageAMD)
#endif
deriving instance Show ShaderResourceUsageAMD

instance ToCStruct ShaderResourceUsageAMD where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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
             numUsedVgprs numUsedSgprs ldsSizePerLocalWorkGroup (coerce @CSize @Word64 ldsUsageSizeInBytes) (coerce @CSize @Word64 scratchMemUsageInBytes)

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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_shader_info VK_AMD_shader_info>,
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
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderStatisticsInfoAMD)
#endif
deriving instance Show ShaderStatisticsInfoAMD

instance ToCStruct ShaderStatisticsInfoAMD where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
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


-- | VkShaderInfoTypeAMD - Enum specifying which type of shader information
-- to query
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_shader_info VK_AMD_shader_info>,
-- 'getShaderInfoAMD'
newtype ShaderInfoTypeAMD = ShaderInfoTypeAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SHADER_INFO_TYPE_STATISTICS_AMD' specifies that device resources used
-- by a shader will be queried.
pattern SHADER_INFO_TYPE_STATISTICS_AMD  = ShaderInfoTypeAMD 0
-- | 'SHADER_INFO_TYPE_BINARY_AMD' specifies that implementation-specific
-- information will be queried.
pattern SHADER_INFO_TYPE_BINARY_AMD      = ShaderInfoTypeAMD 1
-- | 'SHADER_INFO_TYPE_DISASSEMBLY_AMD' specifies that human-readable
-- dissassembly of a shader.
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

