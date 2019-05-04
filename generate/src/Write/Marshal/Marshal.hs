{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Write.Marshal.Marshal where

import           Control.Arrow                            (
                                                          second
                                                          )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Extra                          ( Text )
import qualified Data.Text.Extra               as T
import           Data.Maybe
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader
import           Control.Applicative                      ( Alternative(..) )
import           Control.Bool
import           Data.Foldable                            ( asum )

import           Spec.Savvy.Command
import           Spec.Savvy.Handle
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import qualified Write.Element                 as WE
import           Write.Marshal.Util
import           Write.Monad
import           Write.Monad.Lookup

marshalStructMembers :: Struct -> Write [MarshalScheme StructMember]
marshalStructMembers s@Struct {..} =
  traverseV id $ fmap (extractMarshalledMember s) sMembers

data Namespace
  = Unmarshalled
  | Marshalled

newtype Name (a :: Namespace) = Name {unName :: Text}
  deriving (Eq, Show)

-- TODO: Rename
data MarshalScheme a = MarshalScheme
  { msSchemeName :: Text
  , msParam :: a
  , msMarshalledType :: Maybe (WE (Doc ()))
  }

msUnmarshalledName :: Param a => MarshalScheme a -> Name Unmarshalled
msUnmarshalledName = name . msParam

msMarshalledName :: Param a => MarshalScheme a -> Name Marshalled
msMarshalledName = getMarshalledName . msUnmarshalledName

----------------------------------------------------------------
-- Schemes
--
-- # Optional arrys:
--
-- - Optional Count, Optional Array:
--   The count can be zero, and the array can be null
--   Represent this as @Either Word32 (Vector a)@.
--   For example: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
--
-- - Optional Count, Non Optional array:
--   The array can be null if the count is 0.
--   Represent this as @Vector a@.
--   For example VkDescriptorSetLayoutCreateInfo
--
-- - Non Optional Count, Optional array:
--   The count must be greater than zero, the array can be null
--   Represent this as @Either Word32 (Vector a)@.
--   For example: VkPresentTimesInfoGOOGLE, VkPipelineViewportStateCreateInfo
--
-- - Non Optional Count, Non Optional array:
--   The array must have a size greater than 0, represent this as @Vector a@
--   and rely on the programmer to do the right thing, the validation layers
--   will catch it otherwise.
--   Represent this as @Vector a@.
--   For example VkDescriptorPoolCreateInfo
--
-- There are oddities such as VkDrmFormatModifierPropertiesListEXT which is
-- used for returning values under a mutable pointer.
--
-- Because we don't represent things as non-empty the optionalness of the
-- count is uninteresting.
----------------------------------------------------------------

-- | This always takes priority
bespokeScheme :: Param a => Struct -> a -> MaybeT Write (MarshalScheme a)
bespokeScheme struct p = asum
  [ do
    Name "pNext" <- pure $ name p
    Ptr _ Void   <- pure $ type' p
    let tDoc = do
          tellSourceDepend (WE.TypeName "SomeVkStruct")
          pure "Maybe SomeVkStruct"
    pure $ MarshalScheme "pNext" p (Just tDoc)
  , case
    [ v
    | (s, c, v) <- fixedArrayLengths
    , s == Name (sName struct)
    , c == name p
    ]
  of
    [] -> empty
    vs -> pure $ MarshalScheme "fixed array length" p Nothing
  , case
    [ v | (s, c, v) <- cacheDataArrays, s == Name (sName struct), c == name p ]
  of
    [] -> empty
    vs -> pure $ MarshalScheme "Cache data length" p Nothing
  , case
    [ v | (s, c, v) <- cacheDataArrays, s == Name (sName struct), v == name p ]
  of
    [] -> empty
    vs -> do
      let tDoc = do
            tellImport "Data.ByteString" "ByteString"
            pure "ByteString"
      pure $ MarshalScheme "Cache data length" p (Just tDoc)
  , do
    "VkShaderModuleCreateInfo" <- pure (sName struct)
    Name "codeSize"            <- pure $ name p
    pure $ MarshalScheme "Shader module code size" p Nothing
  , do
    "VkShaderModuleCreateInfo" <- pure (sName struct)
    Name "pCode"               <- pure $ name p
    let tDoc = do
          tellImport "Data.ByteString" "ByteString"
          pure "ByteString"
    pure $ MarshalScheme "Shader module code" p (Just tDoc)
  ]
  where
     -- A list of (Struct name, length member name, array member name) for
     -- fixed sized arrays where only the first 'length member name' elements
     -- are valid'
    fixedArrayLengths
      :: [(Name Unmarshalled, Name Unmarshalled, Name Unmarshalled)]
    fixedArrayLengths =
      [ ( Name "VkPhysicalDeviceGroupProperties"
        , Name "physicalDeviceCount"
        , Name "physicalDevices"
        )
      , ( Name "VkPhysicalDeviceMemoryProperties"
        , Name "memoryTypeCount"
        , Name "memoryTypes"
        )
      , ( Name "VkPhysicalDeviceMemoryProperties"
        , Name "memoryHeapCount"
        , Name "memoryHeaps"
        )
      ]
    cacheDataArrays
      :: [(Name Unmarshalled, Name Unmarshalled, Name Unmarshalled)]
    cacheDataArrays =
      [ ( Name "VkPipelineCacheCreateInfo"
        , Name "initialDataSize"
        , Name "pInitialData"
        )
      , ( Name "VkValidationCacheCreateInfoEXT"
        , Name "initialDataSize"
        , Name "pInitialData"
        )
      ]

-- | The "default" method of marshalling something, store as ordinary member
scalarScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
scalarScheme p = do
  -- Sometimes pointers to non-optional structs are used, remove these for the
  -- marshalled version.
  t <- case type' p of
    t@(Ptr Const (TypeName n)) -> isStruct n >>= \case
      Nothing -> pure t
      Just _  -> pure $ TypeName n
    t -> pure t
  let tDoc = toHsType . deepMarshalledType $ t
  -- Some sanity checking
  guard . isNothing . values $ p
  guard . isNothing . lengths $ p
  guard ((isNothing . isOptional $ p) || ((== Just [False]) . isOptional $ p))
  guard . (/= Void) $ t
  guard $ not (isPtrType t) || isPassAsPointerType (unPtrType t)
  pure $ MarshalScheme "scalar" p (Just tDoc)

univaluedScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
univaluedScheme p = do
  [_value] <- values p
  pure $ MarshalScheme "univalued" p Nothing

arrayScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
arrayScheme p = do
  case lengths p of
    Just (NamedLength _ : _) -> do
      Ptr Const t <- pure $ type' p
      let tDoc = do
            e <- case t of
              Ptr Const c | isByteArrayElem c -> do
                tellImport "Data.ByteString" "ByteString"
                pure $ case isOptional p of
                  Just [True] -> "Maybe ByteString"
                  _           -> "ByteString"
              _ -> toHsTypePrec 10 (deepMarshalledType t)
            tellImport "Data.Vector" "Vector"
            pure $ "Vector" <+> e
      pure $ MarshalScheme "array" p (Just tDoc)
    Just [NullTerminated] -> do
      Ptr Const c <- pure $ type' p
      guard $ isByteArrayElem c
      let tDoc = do
            tellImport "Data.ByteString" "ByteString"
            pure $ case isOptional p of
              Just [True] -> "Maybe ByteString"
              _           -> "ByteString"
      pure $ MarshalScheme "C string" p (Just tDoc)
    _ -> empty

fixedArrayScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
fixedArrayScheme p = do
  guard . isNothing . lengths $ p
  case type' p of
    Array _ (SymbolicArraySize _) t | isByteArrayElem t -> do
      let tDoc = do
            tellImport "Data.ByteString" "ByteString"
            pure "ByteString"
      pure $ MarshalScheme "fixed array" p (Just tDoc)
    Array _ (SymbolicArraySize _) t -> do
      let tDoc = do
            e <- toHsTypePrec 10 (deepMarshalledType t)
            tellImport "Data.Vector" "Vector"
            pure $ "Vector" <+> e
      pure $ MarshalScheme "fixed array" p (Just tDoc)
    Array _ (NumericArraySize n) t -> do
      let tDoc = do
            e <- if isByteArrayElem t
              then do
                tellImport "Data.ByteString" "ByteString"
                pure "ByteString"
              else toHsType (deepMarshalledType t)
            pure (tupled (replicate (fromIntegral n) e))
      pure $ MarshalScheme "fixed array" p (Just tDoc)
    _ -> empty

-- | Matches if this parameter is the length of one or more vectors
lengthScheme :: Param a => [a] -> a -> MaybeT Write (MarshalScheme a)
lengthScheme ps p = do
  -- Get the vectors which are sized with this parameter
  vs <- pure $ getSizedWith (name p) ps
  -- Make sure they exist
  guard (not (null vs))
  -- Make sure they are not all void pointer, those do not have the length
  -- elided
  guard (any (\v -> type' v /= Ptr Const Void) $ vs)
  pure $ MarshalScheme "length" p Nothing

-- | An optional value with a default, so we don't need to wrap it in a Maybe
optionalDefaultScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
optionalDefaultScheme p = do
  Just [True] <- pure $ isOptional p
  guard . isNothing . lengths $ p
  guard =<< lift (isDefaultable (type' p))
  let tDoc = do
        e <- toHsType . deepMarshalledType . type' $ p
        pure e
  pure $ MarshalScheme "optional default" p (Just tDoc)

-- | An optional value to be wrapped in a Maybe
optionalScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
optionalScheme p = do
  Just [True] <- pure $ isOptional p
  guard . isNothing . lengths $ p
  let tDoc = do
        e <- toHsTypePrec 10 . deepMarshalledType . unPtrType . type' $ p
        pure ("Maybe " <> e)
  pure $ MarshalScheme "optional" p (Just tDoc)

-- | Matches const and non-const void pointers, exposes them as 'Ptr ()'
voidPointerScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
voidPointerScheme p = do
  Ptr _ Void <- pure $ type' p
  let tDoc = do
        tellImport "Foreign.Ptr" "Ptr"
        pure "Ptr ()"
  pure $ MarshalScheme "void pointer" p (Just tDoc)

returnPointerScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
returnPointerScheme p = do
  Ptr NonConst t <- pure $ type' p
  let tDoc = toHsType $ type' p
  pure $ MarshalScheme "void pointer" p (Just tDoc)

----------------------------------------------------------------
--
----------------------------------------------------------------

class Param a where
  name    :: a -> Name Unmarshalled
  values  :: Alternative f => a -> f [Text]
  type'   :: a -> Type
  lengths :: Alternative f => a -> f [ParameterLength]
  isOptional :: a -> Maybe [Bool]

instance Param StructMember where
  name       = Name . smName
  values     = maybe empty pure . smValues
  type'      = smType
  lengths    = maybe empty pure . fmap (fmap lengthStringToParamLength) . smLengths
  isOptional = smIsOptional

instance Param Parameter where
  name       = Name . pName
  values     = const empty
  type'      = pType
  lengths    = maybe empty pure . fmap pure . pLength
  isOptional = pIsOptional

data Optional = NonOptional | Optional
  deriving (Show)

getOptional :: Param a => a -> Optional
getOptional = bool NonOptional Optional . isJust . isOptional


extractMarshalledMember
  :: Struct
  -> StructMember
  -> Write (MarshalScheme StructMember)
extractMarshalledMember s@Struct {..} member = do
  let schemes =
        [ -- Anything we specifically single out should go first
          bespokeScheme s
          -- These two are for value constrained params:
        , univaluedScheme
        , lengthScheme sMembers
          -- Pointers to Void have some special handling
        , voidPointerScheme
          -- Pointers to return values in
        , returnPointerScheme
          -- Optional and non optional arrays
        , arrayScheme
        , fixedArrayScheme
          -- Optional things:
        , optionalDefaultScheme
        , optionalScheme
          -- Everything left over is treated as a boring scalar parameter
        , scalarScheme
        ]
  m <- runMaybeT . asum . fmap ($ member) $ schemes
  case m of
    Just x -> pure x
    Nothing ->
      throwError
        $     sName
        <>    "."
        <>    smName member
        T.<+> "is not handled by any marshalling scheme"

----------------------------------------------------------------
-- Type utils
----------------------------------------------------------------

marshalledType :: Type -> Type
marshalledType t
  | Just "VkBool32" <- simpleTypeName t
  = TypeName "Bool"
  | Just tyName <- simpleTypeName t, Just n <- T.dropPrefix "Vk" tyName
  = TypeName n
  | otherwise
  = t

deepMarshalledType :: Type -> Type
deepMarshalledType = \case
  Float           -> TypeName "Float"
  Ptr cv t        -> Ptr cv (deepMarshalledType t)
  Array cv size t -> Array cv size (deepMarshalledType t)
  Proto t ps' ->
    Proto (deepMarshalledType t) (second deepMarshalledType <$> ps')
  t -> marshalledType t

----------------------------------------------------------------
-- Defaulting types
----------------------------------------------------------------

isDefaultable :: Type -> Write Bool
isDefaultable t = do
  isBitmask'              <- (isJust .) <$> asks lIsBitmask
  isNonDispatchableHandle <-
    (maybe False (\h -> hHandleType h == NonDispatchable) .) <$> asks lIsHandle
  pure $ isDefaultableForeignType t || isIntegral t || maybe
    False
    (isNonDispatchableHandle <||> isBitmask')
    (simpleTypeName t)

isIntegral :: Type -> Bool
isIntegral = \case
  Int                        -> True
  Char                       -> True
  TypeName "uint8_t"         -> True
  TypeName "uint16_t"        -> True
  TypeName "uint32_t"        -> True
  TypeName "uint64_t"        -> True
  TypeName "int8_t"          -> True
  TypeName "int16_t"         -> True
  TypeName "int32_t"         -> True
  TypeName "int64_t"         -> True
  TypeName "size_t"          -> True
  TypeName "VkDeviceSize"    -> True
  TypeName "VkDeviceAddress" -> True
  _                          -> False

isDefaultableForeignType :: Type -> Bool
isDefaultableForeignType =
  (`elem` [ TypeName "HANDLE"
          , TypeName "DWORD"
          , TypeName "LPCWSTR"
          , TypeName "PFN_vkInternalAllocationNotification"
          , TypeName "PFN_vkInternalFreeNotification"
          , TypeName "PFN_vkAllocationFunction"
          , TypeName "PFN_vkReallocationFunction"
          , TypeName "PFN_vkFreeFunction"
          , Ptr Const (TypeName "SECURITY_ATTRIBUTES")
          ]
  )

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

getSizedWith :: Param a => Name Unmarshalled -> [a] -> [a]
getSizedWith lengthName ps = filter isVector ps
  where
    isVector v = case lengths v of
      Just (NamedLength len : _) | Name len == lengthName -> True
      -- ^ TODO: Change this to [NamedLength len] and think about handling
      _ -> False

allWithOthers :: (a -> [a] -> b) -> [a] -> [b]
allWithOthers f xs =
  [ f b (as ++ bs)
  | n <- [0 .. length xs - 1]
  , let (as, b : bs) = splitAt n xs
  ]

getMarshalledName :: Name Unmarshalled -> Name Marshalled
getMarshalledName (Name n) =
  Name (unReservedWord . T.lowerCaseFirst . dropPointer $ n)

unPtrType :: Type -> Type
unPtrType = \case
  Ptr _ t -> t
  t       -> t

isByteArrayElem :: Type -> Bool
isByteArrayElem = \case
  Void               -> True
  Char               -> True
  TypeName "uint8_t" -> True
  _                  -> False
