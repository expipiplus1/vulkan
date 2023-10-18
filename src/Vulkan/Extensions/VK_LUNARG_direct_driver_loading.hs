{-# language CPP #-}
-- | = Name
--
-- VK_LUNARG_direct_driver_loading - instance extension
--
-- == VK_LUNARG_direct_driver_loading
--
-- [__Name String__]
--     @VK_LUNARG_direct_driver_loading@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     460
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   Charles Giessen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_LUNARG_direct_driver_loading] @charles-lunarg%0A*Here describe the issue or question you have about the VK_LUNARG_direct_driver_loading extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_LUNARG_direct_driver_loading.adoc VK_LUNARG_direct_driver_loading>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-11-29
--
-- [__Contributors__]
--
--     -   Charles Giessen, LunarG
--
-- == Description
--
-- This extension provides a mechanism for applications to add drivers to
-- the implementation. This allows drivers to be included with an
-- application without requiring installation and is capable of being used
-- in any execution environment, such as a process running with elevated
-- privileges.
--
-- == New Structures
--
-- -   'DirectDriverLoadingInfoLUNARG'
--
-- -   Extending 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo':
--
--     -   'DirectDriverLoadingListLUNARG'
--
-- == New Function Pointers
--
-- -   'PFN_vkGetInstanceProcAddrLUNARG'
--
-- == New Enums
--
-- -   'DirectDriverLoadingModeLUNARG'
--
-- == New Bitmasks
--
-- -   'DirectDriverLoadingFlagsLUNARG'
--
-- == New Enum Constants
--
-- -   'LUNARG_DIRECT_DRIVER_LOADING_EXTENSION_NAME'
--
-- -   'LUNARG_DIRECT_DRIVER_LOADING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_INFO_LUNARG'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_LIST_LUNARG'
--
-- == Version History
--
-- -   Revision 1, 2022-11-29 (Charles Giessen)
--
--     -   Initial version
--
-- == See Also
--
-- 'PFN_vkGetInstanceProcAddrLUNARG', 'DirectDriverLoadingFlagsLUNARG',
-- 'DirectDriverLoadingInfoLUNARG', 'DirectDriverLoadingListLUNARG',
-- 'DirectDriverLoadingModeLUNARG'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_LUNARG_direct_driver_loading Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_LUNARG_direct_driver_loading  ( DirectDriverLoadingInfoLUNARG(..)
                                                          , DirectDriverLoadingListLUNARG(..)
                                                          , DirectDriverLoadingFlagsLUNARG(..)
                                                          , DirectDriverLoadingModeLUNARG( DIRECT_DRIVER_LOADING_MODE_EXCLUSIVE_LUNARG
                                                                                         , DIRECT_DRIVER_LOADING_MODE_INCLUSIVE_LUNARG
                                                                                         , ..
                                                                                         )
                                                          , PFN_vkGetInstanceProcAddrLUNARG
                                                          , FN_vkGetInstanceProcAddrLUNARG
                                                          , LUNARG_DIRECT_DRIVER_LOADING_SPEC_VERSION
                                                          , pattern LUNARG_DIRECT_DRIVER_LOADING_SPEC_VERSION
                                                          , LUNARG_DIRECT_DRIVER_LOADING_EXTENSION_NAME
                                                          , pattern LUNARG_DIRECT_DRIVER_LOADING_EXTENSION_NAME
                                                          ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.FuncPointers (PFN_vkVoidFunction)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_INFO_LUNARG))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_LIST_LUNARG))
-- | VkDirectDriverLoadingInfoLUNARG - Structure specifying the information
-- required to load an additional driver
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PFN_vkGetInstanceProcAddrLUNARG',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_LUNARG_direct_driver_loading VK_LUNARG_direct_driver_loading>,
-- 'DirectDriverLoadingFlagsLUNARG', 'DirectDriverLoadingListLUNARG',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DirectDriverLoadingInfoLUNARG = DirectDriverLoadingInfoLUNARG
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkDirectDriverLoadingInfoLUNARG-flags-zerobitmask# @flags@ /must/
    -- be @0@
    flags :: DirectDriverLoadingFlagsLUNARG
  , -- | @pfnGetInstanceProcAddr@ is a 'PFN_vkGetInstanceProcAddrLUNARG' pointer
    -- to the driver 'Vulkan.Core10.DeviceInitialization.getInstanceProcAddr'
    -- function.
    pfnGetInstanceProcAddr :: PFN_vkGetInstanceProcAddrLUNARG
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DirectDriverLoadingInfoLUNARG)
#endif
deriving instance Show DirectDriverLoadingInfoLUNARG

instance ToCStruct DirectDriverLoadingInfoLUNARG where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DirectDriverLoadingInfoLUNARG{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_INFO_LUNARG)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DirectDriverLoadingFlagsLUNARG)) (flags)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkGetInstanceProcAddrLUNARG)) (pfnGetInstanceProcAddr)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_INFO_LUNARG)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DirectDriverLoadingFlagsLUNARG)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkGetInstanceProcAddrLUNARG)) (zero)
    f

instance FromCStruct DirectDriverLoadingInfoLUNARG where
  peekCStruct p = do
    flags <- peek @DirectDriverLoadingFlagsLUNARG ((p `plusPtr` 16 :: Ptr DirectDriverLoadingFlagsLUNARG))
    pfnGetInstanceProcAddr <- peek @PFN_vkGetInstanceProcAddrLUNARG ((p `plusPtr` 24 :: Ptr PFN_vkGetInstanceProcAddrLUNARG))
    pure $ DirectDriverLoadingInfoLUNARG
             flags pfnGetInstanceProcAddr

instance Storable DirectDriverLoadingInfoLUNARG where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DirectDriverLoadingInfoLUNARG where
  zero = DirectDriverLoadingInfoLUNARG
           zero
           zero


-- | VkDirectDriverLoadingListLUNARG - Structure specifying additional
-- drivers to load
--
-- = Description
--
-- When creating a Vulkan instance for which additional drivers are to be
-- included, add a 'DirectDriverLoadingListLUNARG' structure to the pNext
-- chain of the 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo'
-- structure, and include in it the list of 'DirectDriverLoadingInfoLUNARG'
-- structures which contain the information necessary to load additional
-- drivers.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_LUNARG_direct_driver_loading VK_LUNARG_direct_driver_loading>,
-- 'DirectDriverLoadingInfoLUNARG', 'DirectDriverLoadingModeLUNARG',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DirectDriverLoadingListLUNARG = DirectDriverLoadingListLUNARG
  { -- | @mode@ controls the mode in which to load the provided drivers.
    --
    -- #VUID-VkDirectDriverLoadingListLUNARG-mode-parameter# @mode@ /must/ be a
    -- valid 'DirectDriverLoadingModeLUNARG' value
    mode :: DirectDriverLoadingModeLUNARG
  , -- | @pDrivers@ is a pointer to an array of @driverCount@
    -- 'DirectDriverLoadingInfoLUNARG' structures.
    --
    -- #VUID-VkDirectDriverLoadingListLUNARG-pDrivers-parameter# @pDrivers@
    -- /must/ be a valid pointer to an array of @driverCount@ valid
    -- 'DirectDriverLoadingInfoLUNARG' structures
    drivers :: Vector DirectDriverLoadingInfoLUNARG
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DirectDriverLoadingListLUNARG)
#endif
deriving instance Show DirectDriverLoadingListLUNARG

instance ToCStruct DirectDriverLoadingListLUNARG where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DirectDriverLoadingListLUNARG{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_LIST_LUNARG)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DirectDriverLoadingModeLUNARG)) (mode)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (drivers)) :: Word32))
    pPDrivers' <- ContT $ allocaBytes @DirectDriverLoadingInfoLUNARG ((Data.Vector.length (drivers)) * 32)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDrivers' `plusPtr` (32 * (i)) :: Ptr DirectDriverLoadingInfoLUNARG) (e)) (drivers)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DirectDriverLoadingInfoLUNARG))) (pPDrivers')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_LIST_LUNARG)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DirectDriverLoadingModeLUNARG)) (zero)
    f

instance FromCStruct DirectDriverLoadingListLUNARG where
  peekCStruct p = do
    mode <- peek @DirectDriverLoadingModeLUNARG ((p `plusPtr` 16 :: Ptr DirectDriverLoadingModeLUNARG))
    driverCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pDrivers <- peek @(Ptr DirectDriverLoadingInfoLUNARG) ((p `plusPtr` 24 :: Ptr (Ptr DirectDriverLoadingInfoLUNARG)))
    pDrivers' <- generateM (fromIntegral driverCount) (\i -> peekCStruct @DirectDriverLoadingInfoLUNARG ((pDrivers `advancePtrBytes` (32 * (i)) :: Ptr DirectDriverLoadingInfoLUNARG)))
    pure $ DirectDriverLoadingListLUNARG
             mode pDrivers'

instance Zero DirectDriverLoadingListLUNARG where
  zero = DirectDriverLoadingListLUNARG
           zero
           mempty


-- | VkDirectDriverLoadingFlagsLUNARG - Reserved for future use
--
-- = Description
--
-- 'DirectDriverLoadingFlagsLUNARG' is a bitmask type for setting a mask,
-- but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_LUNARG_direct_driver_loading VK_LUNARG_direct_driver_loading>,
-- 'DirectDriverLoadingInfoLUNARG'
newtype DirectDriverLoadingFlagsLUNARG = DirectDriverLoadingFlagsLUNARG Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameDirectDriverLoadingFlagsLUNARG :: String
conNameDirectDriverLoadingFlagsLUNARG = "DirectDriverLoadingFlagsLUNARG"

enumPrefixDirectDriverLoadingFlagsLUNARG :: String
enumPrefixDirectDriverLoadingFlagsLUNARG = ""

showTableDirectDriverLoadingFlagsLUNARG :: [(DirectDriverLoadingFlagsLUNARG, String)]
showTableDirectDriverLoadingFlagsLUNARG = []

instance Show DirectDriverLoadingFlagsLUNARG where
  showsPrec =
    enumShowsPrec
      enumPrefixDirectDriverLoadingFlagsLUNARG
      showTableDirectDriverLoadingFlagsLUNARG
      conNameDirectDriverLoadingFlagsLUNARG
      (\(DirectDriverLoadingFlagsLUNARG x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DirectDriverLoadingFlagsLUNARG where
  readPrec =
    enumReadPrec
      enumPrefixDirectDriverLoadingFlagsLUNARG
      showTableDirectDriverLoadingFlagsLUNARG
      conNameDirectDriverLoadingFlagsLUNARG
      DirectDriverLoadingFlagsLUNARG

-- | VkDirectDriverLoadingModeLUNARG - Specify loader behavior of added
-- drivers
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_LUNARG_direct_driver_loading VK_LUNARG_direct_driver_loading>,
-- 'DirectDriverLoadingListLUNARG'
newtype DirectDriverLoadingModeLUNARG = DirectDriverLoadingModeLUNARG Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DIRECT_DRIVER_LOADING_MODE_EXCLUSIVE_LUNARG' specifies that the
-- provided drivers are used instead of the system-loaded drivers.
pattern DIRECT_DRIVER_LOADING_MODE_EXCLUSIVE_LUNARG = DirectDriverLoadingModeLUNARG 0

-- | 'DIRECT_DRIVER_LOADING_MODE_INCLUSIVE_LUNARG' specifies that the
-- provided drivers are used in addition to the system-loaded drivers.
pattern DIRECT_DRIVER_LOADING_MODE_INCLUSIVE_LUNARG = DirectDriverLoadingModeLUNARG 1

{-# COMPLETE
  DIRECT_DRIVER_LOADING_MODE_EXCLUSIVE_LUNARG
  , DIRECT_DRIVER_LOADING_MODE_INCLUSIVE_LUNARG ::
    DirectDriverLoadingModeLUNARG
  #-}

conNameDirectDriverLoadingModeLUNARG :: String
conNameDirectDriverLoadingModeLUNARG = "DirectDriverLoadingModeLUNARG"

enumPrefixDirectDriverLoadingModeLUNARG :: String
enumPrefixDirectDriverLoadingModeLUNARG = "DIRECT_DRIVER_LOADING_MODE_"

showTableDirectDriverLoadingModeLUNARG :: [(DirectDriverLoadingModeLUNARG, String)]
showTableDirectDriverLoadingModeLUNARG =
  [
    ( DIRECT_DRIVER_LOADING_MODE_EXCLUSIVE_LUNARG
    , "EXCLUSIVE_LUNARG"
    )
  ,
    ( DIRECT_DRIVER_LOADING_MODE_INCLUSIVE_LUNARG
    , "INCLUSIVE_LUNARG"
    )
  ]

instance Show DirectDriverLoadingModeLUNARG where
  showsPrec =
    enumShowsPrec
      enumPrefixDirectDriverLoadingModeLUNARG
      showTableDirectDriverLoadingModeLUNARG
      conNameDirectDriverLoadingModeLUNARG
      (\(DirectDriverLoadingModeLUNARG x) -> x)
      (showsPrec 11)

instance Read DirectDriverLoadingModeLUNARG where
  readPrec =
    enumReadPrec
      enumPrefixDirectDriverLoadingModeLUNARG
      showTableDirectDriverLoadingModeLUNARG
      conNameDirectDriverLoadingModeLUNARG
      DirectDriverLoadingModeLUNARG

type FN_vkGetInstanceProcAddrLUNARG = Ptr Instance_T -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
-- | PFN_vkGetInstanceProcAddrLUNARG - Type definition for
-- vkGetInstanceProcAddr
--
-- = Description
--
-- This type is compatible with the type of a pointer to the
-- 'Vulkan.Core10.DeviceInitialization.getInstanceProcAddr' command, but is
-- used only to specify device driver addresses in
-- 'DirectDriverLoadingInfoLUNARG'::@pfnGetInstanceProcAddr@.
--
-- Note
--
-- This type exists only because of limitations in the XML schema and
-- processing scripts, and its name may change in the future. Ideally we
-- would use the @PFN_vkGetInstanceProcAddr@ type generated in the
-- @vulkan_core.h@ header.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_LUNARG_direct_driver_loading VK_LUNARG_direct_driver_loading>,
-- 'DirectDriverLoadingInfoLUNARG'
type PFN_vkGetInstanceProcAddrLUNARG = FunPtr FN_vkGetInstanceProcAddrLUNARG


type LUNARG_DIRECT_DRIVER_LOADING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_LUNARG_DIRECT_DRIVER_LOADING_SPEC_VERSION"
pattern LUNARG_DIRECT_DRIVER_LOADING_SPEC_VERSION :: forall a . Integral a => a
pattern LUNARG_DIRECT_DRIVER_LOADING_SPEC_VERSION = 1


type LUNARG_DIRECT_DRIVER_LOADING_EXTENSION_NAME = "VK_LUNARG_direct_driver_loading"

-- No documentation found for TopLevel "VK_LUNARG_DIRECT_DRIVER_LOADING_EXTENSION_NAME"
pattern LUNARG_DIRECT_DRIVER_LOADING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern LUNARG_DIRECT_DRIVER_LOADING_EXTENSION_NAME = "VK_LUNARG_direct_driver_loading"

