{-# language CPP #-}
-- | = Name
--
-- XR_KHR_binding_modification - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_binding_modification  XR_KHR_binding_modification>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 121
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
-- 'BindingModificationBaseHeaderKHR', 'BindingModificationsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_binding_modification OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_binding_modification  ( BindingModificationsKHR(..)
                                                      , BindingModificationBaseHeaderKHR(..)
                                                      , KHR_binding_modification_SPEC_VERSION
                                                      , pattern KHR_binding_modification_SPEC_VERSION
                                                      , KHR_BINDING_MODIFICATION_EXTENSION_NAME
                                                      , pattern KHR_BINDING_MODIFICATION_EXTENSION_NAME
                                                      ) where

import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import OpenXR.CStruct.Utils (advancePtrBytes)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_BINDING_MODIFICATIONS_KHR))
-- | XrBindingModificationsKHR - Suggested bindings with binding modification
-- details
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrBindingModificationsKHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using 'BindingModificationsKHR'
--
-- -   #VUID-XrBindingModificationsKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_BINDING_MODIFICATIONS_KHR'
--
-- -   #VUID-XrBindingModificationsKHR-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrBindingModificationsKHR-bindingModifications-parameter# If
--     @bindingModificationCount@ is not @0@, @bindingModifications@ /must/
--     be a pointer to an array of @bindingModificationCount@ valid
--     'BindingModificationBaseHeaderKHR'-based structures
--
-- = See Also
--
-- 'BindingModificationBaseHeaderKHR',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Input.suggestInteractionProfileBindings'
data BindingModificationsKHR = BindingModificationsKHR
  { -- | @bindingModificationCount@ is the number of binding modifications in the
    -- array pointed to by @bindingModifications@.
    bindingModificationCount :: Word32
  , -- | @bindingModifications@ is a pointer to an array of pointers to binding
    -- modification structures based on 'BindingModificationBaseHeaderKHR',
    -- that define all of the application’s suggested binding modifications for
    -- the specified interaction profile.
    bindingModifications :: Vector BindingModificationBaseHeaderKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindingModificationsKHR)
#endif
deriving instance Show BindingModificationsKHR

instance ToCStruct BindingModificationsKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindingModificationsKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_BINDING_MODIFICATIONS_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let bindingModificationsLength = Data.Vector.length $ (bindingModifications)
    bindingModificationCount'' <- lift $ if (bindingModificationCount) == 0
      then pure $ fromIntegral bindingModificationsLength
      else do
        unless (fromIntegral bindingModificationsLength == (bindingModificationCount) || bindingModificationsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "bindingModifications must be empty or have 'bindingModificationCount' elements" Nothing Nothing
        pure (bindingModificationCount)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (bindingModificationCount'')
    bindingModifications'' <- if Data.Vector.null (bindingModifications)
      then pure nullPtr
      else do
        pBindingModifications <- ContT $ allocaBytesAligned @(Ptr BindingModificationBaseHeaderKHR) (((Data.Vector.length (bindingModifications))) * 8) 8
        Data.Vector.imapM_ (\i e -> do
          bindingModifications' <- ContT $ withCStruct (e)
          lift $ poke (pBindingModifications `plusPtr` (8 * (i)) :: Ptr (Ptr BindingModificationBaseHeaderKHR)) bindingModifications') ((bindingModifications))
        pure $ pBindingModifications
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (Ptr BindingModificationBaseHeaderKHR)))) bindingModifications''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_BINDING_MODIFICATIONS_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct BindingModificationsKHR where
  peekCStruct p = do
    bindingModificationCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    bindingModifications <- peek @(Ptr (Ptr BindingModificationBaseHeaderKHR)) ((p `plusPtr` 24 :: Ptr (Ptr (Ptr BindingModificationBaseHeaderKHR))))
    let bindingModificationsLength = if bindingModifications == nullPtr then 0 else (fromIntegral bindingModificationCount)
    bindingModifications' <- generateM bindingModificationsLength (\i -> peekCStruct @BindingModificationBaseHeaderKHR =<< peek ((bindingModifications `advancePtrBytes` (8 * (i)) :: Ptr (Ptr BindingModificationBaseHeaderKHR))))
    pure $ BindingModificationsKHR
             bindingModificationCount bindingModifications'

instance Zero BindingModificationsKHR where
  zero = BindingModificationsKHR
           zero
           mempty


-- | XrBindingModificationBaseHeaderKHR - Base struct for all binding
-- modifications
--
-- == Member Descriptions
--
-- = Description
--
-- The 'BindingModificationBaseHeaderKHR' is a base structure is overridden
-- by @XrBindingModification*@ child structures.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrBindingModificationBaseHeaderKHR-extension-notenabled# The
--     @@ extension /must/ be enabled prior to using
--     'BindingModificationBaseHeaderKHR'
--
-- -   #VUID-XrBindingModificationBaseHeaderKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'BindingModificationsKHR',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Input.suggestInteractionProfileBindings'
data BindingModificationBaseHeaderKHR = BindingModificationBaseHeaderKHR
  { -- | @type@ is the 'OpenXR.Core10.Enums.StructureType.StructureType' of this
    -- structure. This base structure itself has no associated
    -- 'OpenXR.Core10.Enums.StructureType.StructureType' value.
    type' :: StructureType }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindingModificationBaseHeaderKHR)
#endif
deriving instance Show BindingModificationBaseHeaderKHR

instance ToCStruct BindingModificationBaseHeaderKHR where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindingModificationBaseHeaderKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (type')
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct BindingModificationBaseHeaderKHR where
  peekCStruct p = do
    type' <- peek @StructureType ((p `plusPtr` 0 :: Ptr StructureType))
    pure $ BindingModificationBaseHeaderKHR
             type'

instance Storable BindingModificationBaseHeaderKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindingModificationBaseHeaderKHR where
  zero = BindingModificationBaseHeaderKHR
           zero


type KHR_binding_modification_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_KHR_binding_modification_SPEC_VERSION"
pattern KHR_binding_modification_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_binding_modification_SPEC_VERSION = 1


type KHR_BINDING_MODIFICATION_EXTENSION_NAME = "XR_KHR_binding_modification"

-- No documentation found for TopLevel "XR_KHR_BINDING_MODIFICATION_EXTENSION_NAME"
pattern KHR_BINDING_MODIFICATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_BINDING_MODIFICATION_EXTENSION_NAME = "XR_KHR_binding_modification"

