{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Write.Marshal.Type
  ( marshalStructTypes
  , marshalStructType
  , marshalCommandType
  , marshalRVType
  , MarshalType(..)
  , renderMarshalTypePrec
  ) where

import           Control.Arrow                            ( second )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Extra                          ( Text )
import qualified Data.Text.Extra               as T
import           Data.List                                ( find )
import           Data.Maybe
import           Control.Monad.Except
import           Data.List.Extra                          ( intersperse
                                                          , partition
                                                          )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader
import           Control.Applicative                      ( Alternative(..) )
import           Control.Bool
import           Data.Foldable                            ( asum
                                                          , toList
                                                          )

import           Spec.Savvy.Command
import           Spec.Savvy.Handle
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import qualified Write.Element                 as WE
import           Write.Marshal.Util
import           Write.Marshal.Marshallable
import           Write.Marshal.Scheme
import           Write.Monad
import           Write.Monad.Lookup

data MarshalType a
  = Normal Type
  | ElidedLength [a] [a] -- optional and required lengths
  | ElidedUnivalued Text
  | ElidedVoid
  | VoidPtr
  | ByteString
  | Maybe (MarshalType a)
  | Vector (MarshalType a)
  | EitherWord32 (MarshalType a)
  | Tupled Word (MarshalType a)
  | Returned (MarshalType a)
  deriving (Show)

marshalStructTypes :: Struct -> Write [MarshalType StructMember]
marshalStructTypes s = traverseV (marshalStructType s) (sMembers s)

marshalStructType :: Struct -> StructMember -> Write (MarshalType StructMember)
marshalStructType s@Struct{..} member = do
  let schemes =
        [ -- These two are for value constrained params:
          univaluedScheme
        , lengthScheme sMembers
          -- Pointers to Void have some special handling
        , voidPointerScheme
          -- Pointers to return values in, unmarshalled at the moment
        , returnPointerInStructScheme
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

marshalCommandType :: Command -> Parameter -> Write (MarshalType Parameter)
marshalCommandType s@Command{..} param = do
  let schemes =
        [ -- These two are for value constrained params:
          univaluedScheme
        , lengthScheme cParameters
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
  m <- runMaybeT . asum . fmap ($ param) $ schemes
  case m of
    Just x -> pure x
    Nothing ->
      throwError
        $     cName
        <>    "."
        <>    pName param
        T.<+> "is not handled by any marshalling scheme"

marshalRVType :: Command -> ReturnValue -> Write (MarshalType ReturnValue)
marshalRVType s@Command {..} rv = do
  let schemes =
        [ -- Returns nothing
          voidScheme
          -- Pointers to Void have some special handling
        , voidPointerScheme
          -- Everything left over is treated as a boring scalar parameter
        , scalarScheme
        ]
  m <- runMaybeT . asum . fmap ($ rv) $ schemes
  case m of
    Just x -> pure x
    Nothing ->
      throwError
        $     cName
        T.<+> "return value is not handled by any marshalling scheme"

----------------------------------------------------------------
-- Schemes
--
-- # Optional arrays:
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
--
-- The @Either Word32@ part can be dropped if there is a non-optional vector
-- with the same length
----------------------------------------------------------------

univaluedScheme :: Marshallable a => a -> MaybeT Write (MarshalType c)
univaluedScheme p = do
  [value] <- values p
  pure $ ElidedUnivalued value

-- | Matches if this parameter is the length of one or more vectors
lengthScheme :: Marshallable a => [a] -> a -> MaybeT Write (MarshalType a)
lengthScheme ps p = do
  -- Get the vectors which are sized with this parameter
  let isReturnPtr p' = case type' p' of
        Ptr NonConst _ -> True
        _              -> False
  vs <- pure $ filter (not . isReturnPtr) . getSizedWith (name p) $ ps
  -- Make sure they are not all void pointer, those do not have the length
  -- elided
  guard (any (\v -> type' v /= Ptr Const Void) $ vs)
  case partition (maybe False head . isOptional) vs of
    -- Make sure they exist
    ([], []) -> empty
    ([], vs)
      | all
        (\v -> case type' v of
          Ptr NonConst _ -> True
          _              -> False
        )
        vs
      -> empty
    (os, []) | length os > 1 ->
      throwError "TODO: handle multiple optional vectors"
    (os, vs) -> pure $ ElidedLength os vs

-- | Matches const and non-const void pointers, exposes them as 'Ptr ()'
voidPointerScheme :: Marshallable a => a -> MaybeT Write (MarshalType c)
voidPointerScheme p = do
  Ptr _ Void <- pure $ type' p
  pure VoidPtr

voidScheme :: Marshallable a => a -> MaybeT Write (MarshalType c)
voidScheme p = do
  Void <- pure $ type' p
  pure ElidedVoid

renderMarshalTypePrec :: Int -> MarshalType b -> Maybe (WE (Doc ()))
renderMarshalTypePrec prec = \case
  ElidedLength _ _  -> Nothing
  ElidedUnivalued _ -> Nothing
  ElidedVoid        -> Nothing
  VoidPtr           -> Just $ do
    tellImport "Foreign.Ptr" "Ptr"
    pure . parens' (prec >= 10) $ "Ptr ()"
  Maybe t -> do
    tDoc <- renderMarshalTypePrec 10 t
    pure $ do
      tDoc <- tDoc
      pure . parens' (prec >= 10) $ "Maybe" <+> tDoc
  ByteString -> Just $ do
    tellImport "Data.ByteString" "ByteString"
    pure "ByteString"
  Normal t -> Just $ toHsTypePrec prec t
  Vector t -> do
    tDoc <- renderMarshalTypePrec 10 t
    pure $ do
      tDoc <- tDoc
      tellImport "Data.Vector" "Vector"
      pure . parens' (prec >= 10) $ "Vector" <+> tDoc
  EitherWord32 t -> do
    tDoc <- renderMarshalTypePrec 10 t
    pure $ do
      tDoc <- tDoc
      tellImport "Data.Word"   "Word32"
      tellImport "Data.Vector" "Vector"
      pure . parens' (prec >= 10) $ "Either Word32" <+> parens
        ("Vector" <+> tDoc)
  Tupled n t -> do
    tDoc <- renderMarshalTypePrec 0 t
    pure $ do
      tDoc <- tDoc
      pure (tupled (replicate (fromIntegral n) tDoc))
  Returned t -> renderMarshalTypePrec prec t

returnPointerScheme :: Marshallable a => a -> MaybeT Write (MarshalType c)
returnPointerScheme p = do
  t@(Ptr NonConst _) <- pure $ type' p
  pure $ Returned (Normal t)

returnPointerInStructScheme
  :: Marshallable a => a -> MaybeT Write (MarshalType c)
returnPointerInStructScheme p = do
  t@(Ptr NonConst _) <- pure $ type' p
  pure $ Normal t

arrayScheme :: Marshallable a => a -> MaybeT Write (MarshalType c)
arrayScheme p = do
  case lengths p of
    -- TODO: Don't ignore the tail here
    Just (NamedLength _ : _) -> do
      -- It's a const pointer
      Ptr Const t <- pure $ type' p
      let isOpt = case isOptional p of
            Just (True : _) -> True
            _               -> False
          elemType = case t of
            -- If it's an array of bytes use a ByteString
            Ptr Const c | isByteArrayElem c -> case isOptional p of
              Just [True] -> Maybe ByteString
              _           -> ByteString
            _ -> Normal (deepMarshalledType t)
          vType = elemType
          type' = if isOpt then EitherWord32 vType else Vector vType
      pure type'
    Just [NullTerminated] -> do
      Ptr Const c <- pure $ type' p
      guard $ isByteArrayElem c
      pure $ case isOptional p of
        Just [True] -> Maybe ByteString
        _           -> ByteString
    _ -> empty

fixedArrayScheme :: Marshallable a => a -> MaybeT Write (MarshalType c)
fixedArrayScheme p = do
  guard . isNothing . lengths $ p
  case type' p of
    Array _ (SymbolicArraySize s) t
      | isByteArrayElem t -> pure $ ByteString
      | otherwise         -> pure $ Vector (Normal (deepMarshalledType t))
    Array _ (NumericArraySize n) t ->
      let
        e = if isByteArrayElem t
          then ByteString
          else Normal (deepMarshalledType t)
      in  pure $ Tupled n e
    _ -> empty

-- | An optional value with a default, so we don't need to wrap it in a Maybe
optionalDefaultScheme :: Marshallable a => a -> MaybeT Write (MarshalType c)
optionalDefaultScheme p = do
  Just [True] <- pure $ isOptional p
  guard . isNothing . lengths $ p
  guard =<< lift (isDefaultable (type' p))
  pure $ Normal (deepMarshalledType (type' p))

-- | An optional value to be wrapped in a Maybe
optionalScheme :: Marshallable a => a -> MaybeT Write (MarshalType c)
optionalScheme p = do
  Just [True] <- pure $ isOptional p
  guard . isNothing . lengths $ p
  pure $ Maybe (Normal (deepMarshalledType . unPtrType . type' $ p))

-- | The "default" method of marshalling something, store as ordinary member
scalarScheme :: Marshallable a => a -> MaybeT Write (MarshalType c)
scalarScheme p = do
  -- Sometimes pointers to non-optional structs are used, remove these for the
  -- marshalled version.
  t <- case type' p of
    t@(Ptr Const (TypeName n)) -> isStruct n >>= \case
      Nothing -> pure t
      Just _  -> pure $ TypeName n
    t -> pure t
  -- Some sanity checking
  guard . isNothing . values $ p
  guard . isNothing . lengths $ p
  guard
    (  (isNothing . isOptional $ p)
    || ((== Just [False]) . isOptional $ p)
    || isPassAsPointerType (unPtrType t)
    )
  guard . (/= Void) $ t
  guard $ not (isPtrType t) || isPassAsPointerType (unPtrType t)
  pure . Normal . deepMarshalledType $ t

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

unPtrType :: Type -> Type
unPtrType = \case
  Ptr _ t -> t
  t       -> t

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

getSizedWith :: Marshallable a => Name Unmarshalled -> [a] -> [a]
getSizedWith lengthName ps = filter isVector ps
  where
    isVector v = case lengths v of
      Just (NamedLength len : _) | Name len == lengthName -> True
      -- ^ TODO: Change this to [NamedLength len] and think about handling
      _ -> False

parens' :: Bool -> Doc () -> Doc ()
parens' = \case
  True -> parens
  False -> id

isByteArrayElem :: Type -> Bool
isByteArrayElem = \case
  Void               -> True
  Char               -> True
  TypeName "uint8_t" -> True
  _                  -> False
