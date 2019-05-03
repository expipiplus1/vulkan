{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Write.Marshal.Marshal where

import           Control.Arrow                            ( (&&&)
                                                          , second
                                                          )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Extra                          ( Text )
import qualified Data.Text.Extra               as T
import           Data.List.Extra                          ( partition )
import           Data.Maybe
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader
import           Control.Applicative                      ( Alternative(..) )
import           Data.Traversable
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
----------------------------------------------------------------

-- | This always takes priority
bespokeScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
bespokeScheme p = do
  Name "pNext" <- pure $ name p
  Ptr _ Void   <- pure $ type' p
  let tDoc = do
        tellSourceDepend (WE.TypeName "SomeVkStruct")
        pure "Maybe SomeVkStruct"
  pure $ MarshalScheme "pNext" p (Just tDoc)

scalarScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
scalarScheme p = do
  let tDoc = toHsType . deepMarshalledType . type' $ p
  guard . isNothing . values $ p
  guard . isNothing . lengths $ p
  guard . isNothing . isOptional $ p
  guard . (/= Void) . unPtrType . type' $ p
  pure $ MarshalScheme "scalar" p (Just tDoc)

univaluedScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
univaluedScheme p = do
  [_value] <- values p
  pure $ MarshalScheme "univalued" p Nothing

arrayScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
arrayScheme p = do
  lens <- lengths p
  Ptr _ t <- pure $ type' p
  guard . (/= Void) . unPtrType . type' $ p
  let tDoc = do
        e <- toHsTypePrec 10 (deepMarshalledType t)
        tellImport "Data.Vector" "Vector"
        pure $ "Vector" <+> e
  pure $ MarshalScheme "array" p (Just tDoc)

lengthScheme :: Param a => [a] -> a -> MaybeT Write (MarshalScheme a)
lengthScheme ps p = do
  vs                  <- pure $ getSizedWith (name p) ps
  guard (not (null vs))
  pure $ MarshalScheme "length" p Nothing

-- | An optional value with a default
optionalDefaultScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
optionalDefaultScheme p = do
  Just [True] <- pure $ isOptional p
  guard . isNothing . lengths $ p
  guard . (/= Void) . unPtrType . type' $ p
  let tDoc = do
        e <- toHsType . deepMarshalledType . unPtrType . type' $ p
        pure e
  pure $ MarshalScheme "optional default" p (Just tDoc)

voidPointerScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
voidPointerScheme p = do
  Ptr _ Void <- pure $ type' p
  let tDoc = do
        tellImport "Foreign.Ptr" "Ptr"
        pure "Ptr ()"
  pure $ MarshalScheme "void pointer" p (Just tDoc)

-- -- | A pointer to a single optional value
-- optionalPointerScheme :: Param a => a -> MaybeT Write (MarshalScheme a)
-- optionalPointerScheme p = do
--   Just [True] <- pure $ isOptional p
--   guard . isNothing . lengths $ p
--   Ptr _ t <- pure $ type' p
--   let tDoc = do
--         e <- toHsTypePrec 10 (deepMarshalledType t)
--         pure $ "Maybe" <+> e
--   pure $ MarshalScheme "optional pointer" p (Just tDoc)

----------------------------------------------------------------
--
----------------------------------------------------------------

data MarshalledParam a = MarshalledParam
  { mmSchemeName :: Text
    -- ^ For debugging
  , mmUnmarshalled :: a
    -- ^ The member/parameter this was created from
  , mmType :: MarshalType
    -- ^ How is this to be marshalled
  }

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

data MarshalType
  = Univalued (Name Unmarshalled) (Doc ())
  | Scalar Optional (Name Unmarshalled) Type ScalarType
  | Length Optional [(Name Unmarshalled, Type)] [(Name Unmarshalled, Type)]
  | Bespoke
  deriving (Show)

data Optional = NonOptional | Optional
  deriving (Show)

getOptional :: Param a => a -> Optional
getOptional = bool NonOptional Optional . isJust . isOptional

data ScalarType
  = PNext
  | Preserved
  | Bitmask
  | Struct'
  | OutputScalar
  | Bool
  | Enum'
  | Handle'
  | OptionalScalar
  | ArrayFixed
  | ArrayFixedNum Word
  | CString
  | FD
  | UnsizedVoidPtr
  | FunctionPtr
  | Array'
  | ArrayNonConst
  deriving (Show)

scalarParam :: Param a => ScalarType -> a -> MarshalledParam a
scalarParam t p = MarshalledParam
  (T.tShow t)
  p
  (Scalar (getOptional p) (name p) (type' p) t)

extractMarshalledMember
  :: Struct
  -> StructMember
  -> Write (MarshalScheme StructMember)
extractMarshalledMember Struct {..} member = do
  let schemes =
        [ -- Anything we specifically single out should go first
          bespokeScheme
          -- These two are for value constrained params:
        , univaluedScheme
        , lengthScheme sMembers
          -- Pointers to Void have some special handling
        , voidPointerScheme
          -- Optional and non optional arrays are next
        , arrayScheme
          -- Optional things:
        , optionalDefaultScheme
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

extractMarshalledMember'
  :: Struct
  -> StructMember
  -> [StructMember]
  -> Write (MarshalledParam StructMember)
extractMarshalledMember' Struct {..} member others = do
  let schemes =
        (   (const .)
          <$> [ univalued
              , pNext
              , enumeration
              , bitmask
              , struct
              , outputScalar
              , boolean
              , handle
              , fixedArray
              , optionalScalar
              , cString
              , array
              , nonConstArray
              , fd
              , unsizedVoidPtr
              , functionPtr
              , bespoke
              ]
          )
          ++ [basicType, arrayLength, arrayVoidLength]
  fs <- traverseV runMaybeT [ s member others | s <- schemes ]
  case catMaybes fs of
    [x] -> pure x
    [] ->
      throwError
        $     sName
        <>    "."
        <>    smName member
        T.<+> "is not handled by any marshalling scheme"
    xs ->
      throwError
        $     sName
        <>    "."
        <>    smName member
        T.<+> "is handled by multiple marshalling schemes:"
        T.<+> T.tShow (mmSchemeName <$> xs)

----------------------------------------------------------------
-- Schemes
----------------------------------------------------------------

type Scheme1 a = a -> MaybeT Write (MarshalledParam a)
type Scheme a = a -> [a] -> MaybeT Write (MarshalledParam a)

univalued :: Param a => Scheme1 a
univalued p = do
  [value] <- values p
  pure $ MarshalledParam "univalued" p (Univalued (name p) (pretty value))

pNext :: Scheme1 StructMember
pNext p = do
  Name "pNext" <- pure (name p)
  Ptr _ Void   <- pure $ type' p
  pure $ scalarParam PNext p
--------------------------------------------------------------------------------------------------------------------
basicType :: Param a => Scheme a
basicType p ps =
  let t = type' p
  in  case t of
        TypeName n
          | n `elem` ["uint32_t", "size_t"], [] <- getSizedWith (name p) ps
          -> pure $ scalarParam Preserved p
        n | n `elem` basicTypes -> pure $ scalarParam Preserved p
        _                       -> empty
  where
    basicTypes =
      [ TypeName "VkDeviceAddress"
      , TypeName "VkDeviceSize"
      , TypeName "int32_t"
      , TypeName "uint8_t"
      , TypeName "uint64_t"
      , TypeName "DWORD"
      , TypeName "HINSTANCE"
      , TypeName "HMONITOR"
      , TypeName "HWND"
      , TypeName "HANDLE"
      , TypeName "LPCWSTR" -- TODO: remove
      , TypeName "GgpFrameToken"
      , TypeName "GgpStreamDescriptor"
      , TypeName "zx_handle_t"
      , TypeName "xcb_window_t"
      , TypeName "Window"
      , Ptr Const (TypeName "CAMetalLayer")
      , Ptr Const (TypeName "SECURITY_ATTRIBUTES")
      , Float
      ]

fd :: Param a => Scheme1 a
fd p = do
  Int <- pure $ type' p
  Name "fd" <- pure $ name p
  pure $ scalarParam FD p

unsizedVoidPtr :: Param a => Scheme1 a
unsizedVoidPtr p = do
  Ptr _ Void <- pure $ type' p
  guard (isNothing . lengths $ p)
  guard ((/= Name "pNext") . name $ p)
  pure $ scalarParam UnsizedVoidPtr p

functionPtr :: Param a => Scheme1 a
functionPtr p = do
  TypeName t <- pure $ type' p
  guard ("PFN_" `T.isPrefixOf` t)
  pure $ scalarParam FunctionPtr p

enumeration :: Param a => Scheme1 a
enumeration p = do
  TypeName t <- pure $ type' p
  _          <- MaybeT $ isEnum t
  Nothing    <- pure $ isOptional p
  guard (name p /= Name "sType")
  pure $ scalarParam Enum' p

bitmask :: Param a => Scheme1 a
bitmask p = do
  TypeName t <- pure $ type' p
  _          <- MaybeT $ isBitmask t
  pure $ scalarParam Bitmask p

struct :: Param a => Scheme1 a
struct p = do
  TypeName t <- pure $ type' p
  _          <- MaybeT $ isStruct t
  pure $ scalarParam Struct' p

handle :: Param a => Scheme1 a
handle p = do
  TypeName t <- pure $ type' p
  _          <- MaybeT $ isHandle t
  pure $ scalarParam Handle' p

outputScalar :: Param a => Scheme1 a
outputScalar p = do
  Ptr NonConst t <- pure $ type' p
  guard (name p /= Name "pNext")
  guard (t /= Void)
  Nothing <- pure $ lengths p
  Nothing <- pure $ isOptional p
  pure $ scalarParam OutputScalar p

boolean :: Param a => Scheme1 a
boolean p = do
  TypeName "VkBool32" <- pure $ type' p
  Nothing             <- pure $ isOptional p
  pure $ scalarParam Bool p

arrayLength :: forall a . Param a => Scheme a
arrayLength p ps = do
  TypeName "uint32_t" <- pure $ type' p
  vs                  <- pure $ getSizedWith (name p) ps
  guard (not (null vs))
  let optionalSize                    = getOptional p
  let (optionalVecs, nonOptionalVecs) = partition (isJust . isOptional) vs
  pure $ MarshalledParam
    "array"
    p
    (Length optionalSize
            ((name &&& type') <$> optionalVecs)
            ((name &&& type') <$> nonOptionalVecs)
    )

arrayVoidLength :: forall a . Param a => Scheme a
arrayVoidLength p ps = do
  TypeName "size_t" <- pure $ type' p
  vs                <- pure $ getSizedWith (name p) ps
  guard (not (null vs))
  let optional = getOptional p
  guard (all (isNothing . isOptional) vs)
  guard (all ((== (Ptr Const Void)) . type') vs)
  pure $ MarshalledParam "void array"
                         p
                         (Length optional [] ((name &&& type') <$> vs))

fixedArray :: Param a => Scheme1 a
fixedArray p = do
  Array _ arraySize _t <- pure $ type' p
  let t = case arraySize of
        NumericArraySize n -> ArrayFixedNum n
        SymbolicArraySize _ -> ArrayFixed
  pure $ scalarParam t p

optionalScalar :: Param a => Scheme1 a
optionalScalar p = do
  Ptr Const (TypeName t) <- pure $ type' p
  _                      <- MaybeT $ isStruct t
  guard (isNothing . lengths $ p)
  pure $ scalarParam OptionalScalar p

cString :: Param a => Scheme1 a
cString p = do
  Ptr Const         Char <- pure $ type' p
  [   NullTerminated]    <- lengths p
  pure $ scalarParam CString p

array :: Param a => Scheme1 a
array p = do
  guard $ name p `notElem` [Name "pCode", Name "pSampleMask"]
  Ptr Const t <- pure $ type' p
  NamedLength _ : _ <- lengths p
  pure $ scalarParam Array' p

nonConstArray :: Param a => Scheme1 a
nonConstArray p = do
  Ptr NonConst t <- pure $ type' p
  [NamedLength _] <- lengths p
  pure $ scalarParam ArrayNonConst p

bespoke :: Param a => Scheme1 a
bespoke p = do
  guard $ name p `elem` [Name "pSampleMask", Name "pCode"]
  pure $ MarshalledParam "bespoke" p Bespoke

----------------------------------------------------------------
-- Rendering Marshalled type
----------------------------------------------------------------

renderMarshalledType
  :: MarshalType -> WE (Either (Doc ()) (Name Unmarshalled, Doc ()))
renderMarshalledType = \case
  Univalued _ _                 -> pure (Left "-- Univalued member elided")
  Scalar NonOptional n _t PNext -> do
    tellSourceDepend (WE.TypeName "SomeVkStruct")
    pure (Right (n, "Maybe SomeVkStruct"))
  Scalar _ n (Ptr Const (Ptr Const Char)) Array' -> do
    tellImport "Data.Vector"     "Vector"
    tellImport "Data.ByteString" "ByteString"
    pure (Right (n, "Vector ByteString"))
  Scalar _ n (Array _ _ t) (ArrayFixedNum len) -> do
    tDoc <- toHsType (deepMarshalledType t)
    pure (Right (n, tupled (replicate (fromIntegral len) tDoc)))
  Scalar _ n (Ptr Const Char) ArrayFixed -> do
    tellImport "Data.ByteString" "ByteString"
    pure (Right (n, "ByteString"))
  Scalar _ n (Ptr Const t) ArrayFixed -> do
    tDoc <- toHsTypePrec 10 (deepMarshalledType t)
    tellImport "Data.Vector" "Vector"
    pure (Right (n, "Vector" <+> tDoc))
  Scalar _ n (Ptr Const t) Array' -> do
    tDoc <- toHsTypePrec 10 (deepMarshalledType t)
    tellImport "Data.Vector" "Vector"
    pure (Right (n, "Vector" <+> tDoc))
  Scalar Optional n (Ptr _ t) _ -> do
    tDoc <- toHsTypePrec 10 (deepMarshalledType t)
    pure (Right (n, "Maybe" <+> tDoc))
  Scalar NonOptional n (Ptr _ t) _ -> do
    tDoc <- toHsType (deepMarshalledType t)
    pure (Right (n, tDoc))
  Scalar NonOptional n t _ -> do
    tDoc <- toHsType (deepMarshalledType t)
    pure (Right (n, tDoc))
  Scalar Optional n t _ -> do
    isDefaultable t >>= \case
      False -> do
        tDoc <- toHsTypePrec 10 (deepMarshalledType t)
        pure (Right (n, "Maybe" <+> tDoc))
      True -> do
        tDoc <- toHsType (deepMarshalledType t)
        pure (Right (n, tDoc))
  Length _ _ _ -> do
    pure (Left "-- Length valued member elided")
  -- Array' _ [] [(n, Ptr _ Void)] -> do
  --   tellImport "Foreign.Ptr" "Ptr"
  --   pure [(n, "Ptr ()")]
  -- Array' True [] [(n, Ptr Const (Ptr Const Char))] -> do
  --   tellImport "Data.Vector"     "Vector"
  --   tellImport "Data.Bytestring" "Bytestring"
  --   pure [(n, "Vector Bytestring")]
  -- Array' _ [] [(n, Ptr Const t)] -> do
  --   tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
  --   tellImport "Data.Vector" "Vector"
  --   pure [(n, "Vector" <+> tDoc)]
  -- -- TODO: Improve this case V
  -- Array' _ ovs vs
  --   | all
  --     ( (\case
  --         Ptr Const _ -> True
  --         _           -> False
  --       )
  --     . snd
  --     )
  --     (ovs ++ vs)
  --   -> for (ovs ++ vs) $ \(n, Ptr Const t) -> do
  --     tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
  --     tellImport "Data.Vector" "Vector"
  --     pure (n, "Vector" <+> tDoc)
  -- Array' _ [(n, Ptr Const t)] [] -> do
  --   tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
  --   tellImport "Data.Vector" "Vector"
  --   pure [(n, "Vector" <+> tDoc)]
  -- Array' _ [] [(n, Ptr NonConst t)] -> do
  --   tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
  --   tellImport "Foreign.Ptr" "Ptr"
  --   pure [(n, "Ptr" <+> tDoc)]
  -- Array' _ [(n, Ptr NonConst t)] [] -> do
  --   tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
  --   tellImport "Foreign.Ptr" "Ptr"
  --   pure [(n, "Ptr" <+> tDoc)]
  -- Array' _ [(n, Ptr NonConst t)] vs
  --   | all
  --     ( (\case
  --         Ptr Const _ -> True
  --         _           -> False
  --       )
  --     . snd
  --     )
  --     vs
  --   -> do
  --     vs' <- for vs $ \(vn, Ptr Const t) -> do
  --       tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
  --       tellImport "Data.Vector" "Vector"
  --       pure (vn, "Vector" <+> tDoc)
  --     tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
  --     tellImport "Foreign.Ptr" "Ptr"
  --     pure $ (n, "Ptr" <+> tDoc) : vs'
  Bespoke -> pure (Right (Name "member", "type"))
  t       -> throwError ("Unable to render marshalled type:" T.<+> T.tShow t)

marshalledMarshalType :: MarshalType -> MarshalType
marshalledMarshalType = \case
  Scalar o n t s -> Scalar o n (deepMarshalledType t) s
  Length o vs ovs ->
    Length o (second deepMarshalledType <$> vs) (second deepMarshalledType <$> ovs)
  m -> m

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
  Ptr cv t        -> Ptr cv (deepMarshalledType t)
  Array cv size t -> Array cv size (deepMarshalledType t)
  Proto t ps' ->
    Proto (deepMarshalledType t) (second deepMarshalledType <$> ps')
  t -> marshalledType t

----------------------------------------------------------------
-- Defaulting types
----------------------------------------------------------------

isDefaultable :: Type -> WE Bool
isDefaultable t = do
  isBitmask'              <- (isJust .) <$> asks lIsBitmask
  isNonDispatchableHandle <-
    (maybe False (\h -> hHandleType h == NonDispatchable) .) <$> asks lIsHandle
  pure $ maybe
    False
    (    isNonDispatchableHandle
    -- <||> isMarshalledNonDispatchableHandle
    <||> isBitmask'
    <||> isDefaultableForeignType
    -- <||> isMarshalledBitmask
    )
    (simpleTypeName t)

isDefaultableForeignType :: Text -> Bool
isDefaultableForeignType =
  (`elem` [ "HANDLE"
          , "DWORD"
          , "PFN_vkInternalAllocationNotification"
          , "PFN_vkInternalFreeNotification"
          , "PFN_vkAllocationFunction"
          , "PFN_vkReallocationFunction"
          , "PFN_vkFreeFunction"
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
