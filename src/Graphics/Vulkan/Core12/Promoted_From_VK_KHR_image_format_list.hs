{-# language CPP #-}
module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list  ( ImageFormatListCreateInfo(..)
                                                                      , StructureType(..)
                                                                      ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.Enums.Format (Format)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkImageFormatListCreateInfo - Specify that an image /can/ be used with a
-- particular set of formats
--
-- = Description
--
-- If @viewFormatCount@ is zero, @pViewFormats@ is ignored and the image is
-- created as if the 'ImageFormatListCreateInfo' structure were not
-- included in the @pNext@ list of
-- 'Graphics.Vulkan.Core10.Image.ImageCreateInfo'.
--
-- == Valid Usage
--
-- -   If @viewFormatCount@ is not @0@, all of the formats in the
--     @pViewFormats@ array /must/ be compatible with the format specified
--     in the @format@ field of
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo', as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility compatibility table>
--
-- -   If 'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@flags@ does not
--     contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT',
--     @viewFormatCount@ /must/ be @0@ or @1@
--
-- -   If @viewFormatCount@ is not @0@,
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@format@ /must/ be
--     in @pViewFormats@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO'
--
-- -   If @viewFormatCount@ is not @0@, @pViewFormats@ /must/ be a valid
--     pointer to an array of @viewFormatCount@ valid
--     'Graphics.Vulkan.Core10.Enums.Format.Format' values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.Format.Format',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data ImageFormatListCreateInfo = ImageFormatListCreateInfo
  { -- | @pViewFormats@ is an array which lists of all formats which /can/ be
    -- used when creating views of this image.
    viewFormats :: Vector Format }
  deriving (Typeable)
deriving instance Show ImageFormatListCreateInfo

instance ToCStruct ImageFormatListCreateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageFormatListCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (viewFormats)) :: Word32))
    pPViewFormats' <- ContT $ allocaBytesAligned @Format ((Data.Vector.length (viewFormats)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewFormats' `plusPtr` (4 * (i)) :: Ptr Format) (e)) (viewFormats)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Format))) (pPViewFormats')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPViewFormats' <- ContT $ allocaBytesAligned @Format ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewFormats' `plusPtr` (4 * (i)) :: Ptr Format) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Format))) (pPViewFormats')
    lift $ f

instance FromCStruct ImageFormatListCreateInfo where
  peekCStruct p = do
    viewFormatCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pViewFormats <- peek @(Ptr Format) ((p `plusPtr` 24 :: Ptr (Ptr Format)))
    pViewFormats' <- generateM (fromIntegral viewFormatCount) (\i -> peek @Format ((pViewFormats `advancePtrBytes` (4 * (i)) :: Ptr Format)))
    pure $ ImageFormatListCreateInfo
             pViewFormats'

instance Zero ImageFormatListCreateInfo where
  zero = ImageFormatListCreateInfo
           mempty

