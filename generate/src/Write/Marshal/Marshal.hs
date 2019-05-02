{-# LANGUAGE RecordWildCards #-}
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
import qualified Data.MultiMap                 as MultiMap
import           Data.List.Extra                          ( partition )
import           Data.MultiMap                            ( MultiMap )
import           Data.Maybe
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader
import           Control.Applicative                      ( Alternative(..) )
import           Data.Traversable
import           Control.Bool

import           Spec.Savvy.Command
import           Spec.Savvy.Handle
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import qualified Write.Element                 as WE
import           Write.Marshal.Util
import           Write.Monad
import           Write.Monad.Lookup

marshalStructMembers :: Struct -> Write [MarshalledParam StructMember]
marshalStructMembers Struct {..} = do
  mms <- fmap concat . traverseV id $ allWithOthers extractMarshalledMember sMembers
  let schemeMap :: MultiMap Text Text
      schemeMap = MultiMap.fromList
        [ (smName, mmSchemeName)
        | MarshalledParam {..} <- mms
        , StructMember {..}    <- mmUnmarshalled
        ]
      assertUnique :: StructMember -> Write ()
      assertUnique StructMember {..} = case MultiMap.lookup smName schemeMap of
        [_] -> pure ()
        [] ->
          throwError
            $     sName
            <>    "."
            <>    smName
            T.<+> "is not handled by any marshalling scheme"
        xs ->
          throwError
            $     sName
            <>    "."
            <>    smName
            T.<+> "is handled by multiple marshalling schemes:"
            T.<+> T.tShow xs
  _ <- traverseV assertUnique sMembers
  pure mms

data Namespace
  = Unmarshalled
  | Marshalled

newtype Name (a :: Namespace) = Name {unName :: Text}
  deriving (Eq, Show)

data MarshalledParam a = MarshalledParam
  { mmSchemeName :: Text
  , mmUnmarshalled :: [a]
  , mmType :: MarshalType
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
  | Scalar Bool (Name Unmarshalled) Type ScalarType
  | Array' Bool [(Name Unmarshalled, Type)] [(Name Unmarshalled, Type)]
  | Bespoke
  deriving (Show)

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
  | CString
  | FD
  | UnsizedVoidPtr
  | FunctionPtr
  deriving (Show)

scalarParam :: Param a => ScalarType -> a -> MarshalledParam a
scalarParam t p = MarshalledParam
  (T.tShow t)
  [p]
  (Scalar (isJust . isOptional $ p) (name p) (type' p) t)

extractMarshalledMember
  :: StructMember -> [StructMember] -> Write [MarshalledParam StructMember]
extractMarshalledMember member others = do
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
              , fd
              , unsizedVoidPtr
              , functionPtr
              , bespoke
              ]
          )
          ++ [basicType, array, arrayVoid]
  fs <- traverseV runMaybeT [ s member others | s <- schemes ]
  pure $ catMaybes fs

----------------------------------------------------------------
-- Schemes
----------------------------------------------------------------

type Scheme1 a = a -> MaybeT Write (MarshalledParam a)
type Scheme a = a -> [a] -> MaybeT Write (MarshalledParam a)

univalued :: Param a => Scheme1 a
univalued p = do
  [value] <- values p
  pure $ MarshalledParam "univalued" [p] (Univalued (name p) (pretty value))

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

array :: forall a . Param a => Scheme a
array p ps = do
  TypeName "uint32_t" <- pure $ type' p
  vs                  <- pure $ getSizedWith (name p) ps
  guard (not (null vs))
  let optionalSize                    = isJust . isOptional $ p
  let (optionalVecs, nonOptionalVecs) = partition (isJust . isOptional) vs
  pure $ MarshalledParam
    "array"
    (p : vs)
    (Array' optionalSize
            ((name &&& type') <$> optionalVecs)
            ((name &&& type') <$> nonOptionalVecs)
    )

arrayVoid :: forall a . Param a => Scheme a
arrayVoid p ps = do
  TypeName "size_t" <- pure $ type' p
  vs                <- pure $ getSizedWith (name p) ps
  guard (not (null vs))
  let optional = isJust . isOptional $ p
  guard (all (isNothing . isOptional) vs)
  guard (all ((== (Ptr Const Void)) . type') vs)
  pure $ MarshalledParam "void array"
                         (p : vs)
                         (Array' optional [] ((name &&& type') <$> vs))

fixedArray :: Param a => Scheme1 a
fixedArray p = do
  Array NonConst _arraySize _t <- pure $ type' p
  pure $ scalarParam ArrayFixed p

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

bespoke :: Param a => Scheme1 a
bespoke p = do
  guard $ name p `elem` [Name "pSampleMask", Name "pCode"]
  pure $ MarshalledParam "bespoke" [p] Bespoke

----------------------------------------------------------------
-- Rendering Marshalled type
----------------------------------------------------------------

renderMarshalledType :: MarshalType -> WE [(Name Unmarshalled, Doc ())]
renderMarshalledType = \case
  Univalued _ _           -> pure []
  Scalar False n _t PNext -> do
    tellSourceDepend (WE.TypeName "SomeVkStruct")
    pure [(n, "Maybe SomeVkStruct")]
  Scalar False n t _ -> do
    tDoc <- toHsType (deepMarshalledType t)
    pure [(n, tDoc)]
  Scalar True n t _ -> do
    isDefaultable t >>= \case
      False -> do
        tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
        pure [(n, "Maybe" <+> tDoc)]
      True -> do
        tDoc <- toHsType (deepMarshalledType t)
        pure [(n, tDoc)]
  Array' _ [] [(n, Ptr _ Void)] -> do
    tellImport "Foreign.Ptr" "Ptr"
    pure [(n, "Ptr ()")]
  Array' True [] [(n, Ptr Const (Ptr Const Char))] -> do
    tellImport "Data.Vector"     "Vector"
    tellImport "Data.Bytestring" "Bytestring"
    pure [(n, "Vector Bytestring")]
  Array' _ [] [(n, Ptr Const t)] -> do
    tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
    tellImport "Data.Vector" "Vector"
    pure [(n, "Vector" <+> tDoc)]
  -- TODO: Improve this case V
  Array' _ ovs vs
    | all
      ( (\case
          Ptr Const _ -> True
          _           -> False
        )
      . snd
      )
      (ovs ++ vs)
    -> for (ovs ++ vs) $ \(n, Ptr Const t) -> do
      tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
      tellImport "Data.Vector" "Vector"
      pure (n, "Vector" <+> tDoc)
  Array' _ [(n, Ptr Const t)] [] -> do
    tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
    tellImport "Data.Vector" "Vector"
    pure [(n, "Vector" <+> tDoc)]
  Array' _ [] [(n, Ptr NonConst t)] -> do
    tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
    tellImport "Foreign.Ptr" "Ptr"
    pure [(n, "Ptr" <+> tDoc)]
  Array' _ [(n, Ptr NonConst t)] [] -> do
    tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
    tellImport "Foreign.Ptr" "Ptr"
    pure [(n, "Ptr" <+> tDoc)]
  Array' _ [(n, Ptr NonConst t)] vs
    | all
      ( (\case
          Ptr Const _ -> True
          _           -> False
        )
      . snd
      )
      vs
    -> do
      vs' <- for vs $ \(vn, Ptr Const t) -> do
        tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
        tellImport "Data.Vector" "Vector"
        pure (vn, "Vector" <+> tDoc)
      tDoc <- toHsTypePrec (-1) (deepMarshalledType t)
      tellImport "Foreign.Ptr" "Ptr"
      pure $ (n, "Ptr" <+> tDoc) : vs'
  Bespoke -> pure [(Name "member", "type")]
  t       -> throwError ("Unable to render marshalled type:" T.<+> T.tShow t)

marshalledMarshalType :: MarshalType -> MarshalType
marshalledMarshalType = \case
  Scalar o n t s -> Scalar o n (deepMarshalledType t) s
  Array' o vs ovs ->
    Array' o (second deepMarshalledType <$> vs) (second deepMarshalledType <$> ovs)
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
