{-# language CPP #-}
-- | = Name
--
-- VK_GGP_frame_token - device extension
--
-- == VK_GGP_frame_token
--
-- [__Name String__]
--     @VK_GGP_frame_token@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     192
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_swapchain@ to be enabled for any device-level
--         functionality
--
--     -   Requires @VK_GGP_stream_descriptor_surface@ to be enabled for
--         any device-level functionality
--
-- [__Contact__]
--
--     -   Jean-Francois Roy
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_GGP_frame_token] @jfroy%0A*Here describe the issue or question you have about the VK_GGP_frame_token extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jean-Francois Roy, Google
--
--     -   Richard O’Grady, Google
--
-- == Description
--
-- This extension allows an application that uses the @VK_KHR_swapchain@
-- extension in combination with a Google Games Platform surface provided
-- by the @VK_GGP_stream_descriptor_surface@ extension to associate a
-- Google Games Platform frame token with a present operation.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'PresentFrameTokenGGP'
--
-- == New Enum Constants
--
-- -   'GGP_FRAME_TOKEN_EXTENSION_NAME'
--
-- -   'GGP_FRAME_TOKEN_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP'
--
-- == Version History
--
-- -   Revision 1, 2018-11-26 (Jean-Francois Roy)
--
--     -   Initial revision.
--
-- == See Also
--
-- 'PresentFrameTokenGGP'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_GGP_frame_token Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_GGP_frame_token  ( PresentFrameTokenGGP(..)
                                             , GGP_FRAME_TOKEN_SPEC_VERSION
                                             , pattern GGP_FRAME_TOKEN_SPEC_VERSION
                                             , GGP_FRAME_TOKEN_EXTENSION_NAME
                                             , pattern GGP_FRAME_TOKEN_EXTENSION_NAME
                                             , GgpFrameToken
                                             ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP))
-- | VkPresentFrameTokenGGP - The Google Games Platform frame token
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_GGP_frame_token VK_GGP_frame_token>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PresentFrameTokenGGP = PresentFrameTokenGGP
  { -- | @frameToken@ is the Google Games Platform frame token.
    --
    -- #VUID-VkPresentFrameTokenGGP-frameToken-02680# @frameToken@ /must/ be a
    -- valid 'GgpFrameToken'
    frameToken :: GgpFrameToken }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentFrameTokenGGP)
#endif
deriving instance Show PresentFrameTokenGGP

instance ToCStruct PresentFrameTokenGGP where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentFrameTokenGGP{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GgpFrameToken)) (frameToken)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GgpFrameToken)) (zero)
    f

instance FromCStruct PresentFrameTokenGGP where
  peekCStruct p = do
    frameToken <- peek @GgpFrameToken ((p `plusPtr` 16 :: Ptr GgpFrameToken))
    pure $ PresentFrameTokenGGP
             frameToken

instance Storable PresentFrameTokenGGP where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PresentFrameTokenGGP where
  zero = PresentFrameTokenGGP
           zero


type GGP_FRAME_TOKEN_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_GGP_FRAME_TOKEN_SPEC_VERSION"
pattern GGP_FRAME_TOKEN_SPEC_VERSION :: forall a . Integral a => a
pattern GGP_FRAME_TOKEN_SPEC_VERSION = 1


type GGP_FRAME_TOKEN_EXTENSION_NAME = "VK_GGP_frame_token"

-- No documentation found for TopLevel "VK_GGP_FRAME_TOKEN_EXTENSION_NAME"
pattern GGP_FRAME_TOKEN_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern GGP_FRAME_TOKEN_EXTENSION_NAME = "VK_GGP_frame_token"


type GgpFrameToken = Word32

