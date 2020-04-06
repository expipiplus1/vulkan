module Marshal.Scheme
  where

import           Relude                  hiding ( Const
                                                , uncons
                                                )
import qualified Prelude                       as P
import           Polysemy
import           Polysemy.Input
import           Polysemy.NonDet         hiding ( Empty )
import           Polysemy.Fail
import           Data.Vector.Extra              ( Vector
                                                , pattern Empty
                                                , pattern Singleton
                                                , pattern (:<|)
                                                )
import qualified Data.Vector.Extra             as V
import           Data.Text.Prettyprint.Doc

import           Marshal.Marshalable
import           Render.Stmts
import           Render.Element
import           Render.SpecInfo
import           Render.Names
import           Render.Stmts.Poke.SiblingInfo
import           Spec.Types
import           Error
import           CType
import           Haskell                       as H

-- | @MarshalScheme a@ represents how we will marshal some @a@ (a struct
-- member or a command parameter (both referred to as parameter here))
data MarshalScheme a
  = Unit
    -- ^ Replaced with Unit, useful for quick testing
  | Preserve CType
    -- ^ Passed exactly as is
  | Normal CType
    -- ^ Stays the same but uses more idiomatic Haskell types, for example
    -- Float instead of CFloat
  | ElidedLength (Vector a) (Vector a) -- optional and required lengths
    -- ^ This parameter only appears on the C side, it's value can be inferred
    -- from the lengths of some optional and required vectors
  | ElidedUnivalued Text
    -- ^ This parameter is constrained to have one value so it only appears on
    -- the C side
  | ElidedVoid
    -- ^ This parameter is of @void@ type in C (used as a return type)
  | VoidPtr
    -- ^ A pointer to void, on the Haskell side left as (Ptr ())
  | ByteString
    -- ^ An array of @char@, @uint8_t@ or @int8_t@ on the C side
  | Maybe (MarshalScheme a)
    -- ^ Any other scheme, but optional on the C side, This usually means it's
    -- represented as 0 (integral 0 or NULL) there
  | Vector (MarshalScheme a)
    -- ^ A pointer on the C side, usually paired with an ElidedLength scheme,
    -- The sub-scheme here is for elements
  | EitherWord32 (MarshalScheme a)
    -- ^ On the haskell side this is either a length or a vector. It's
    -- necessary because the vector may be optional but the length is required.
    -- Only used if the only way to get the corresponding length is from an
    -- optional vector. This is assumed in "Poke" and @elidedLengthPoke@ should
    -- be updated if this assumption changes
    -- The sub-scheme here is for elements
  | Tupled Word (MarshalScheme a)
    -- ^ A small fixed sized array, represented as a n-length tuple of the same
    -- element
  | WrappedStruct CName
    -- ^ A struct to be wrapped in a GADT to hide a variable
  | Returned (MarshalScheme a)
    -- ^ A non-const pointer to some allocated memory, used to return
    -- additional values.
    -- Not typically used for structs
  | InOutCount (MarshalScheme a)
    -- ^ A non-const pointer to some count value, used to send and return
    -- additional the length of a vector.
    -- Not typically used for structs
  | Custom (CustomScheme a)
    -- ^ A non-elided scheme with some complex behavior
  | ElidedCustom (CustomSchemeElided a)
    -- ^ An elided scheme with some complex behavior
  deriving (Show)

data CustomScheme a = CustomScheme
  { csName :: Text
    -- ^ A name for debugging
  , csZero :: Maybe (Doc ())
    -- ^ The 'zero' value for this scheme if possible
  , csType
      :: forall r
       . (HasErr r, HasRenderParams r, HasSpecInfo r)
      => Sem r H.Type
  , csDirectPoke
      :: forall k (s :: k) r
       . ( Marshalable a
         , HasRenderElem r
         , HasRenderParams r
         , HasErr r
         , HasSpecInfo r
         , HasSiblingInfo a r
         , HasStmts r
         , HasRenderedNames r
         , Show a
         )
      => Ref s ValueDoc
      -> Stmt s r (Ref s ValueDoc)
  , csPeek
      :: forall k r (s :: k)
       . ( HasErr r
         , HasRenderElem r
         , HasSpecInfo r
         , HasRenderParams r
         , HasSiblingInfo a r
         , HasStmts r
         , Show a
         )
      => Ref s AddrDoc
      -> Stmt s r (Ref s ValueDoc)
  }

data CustomSchemeElided a = CustomSchemeElided
  { cseName :: Text
  , cseDirectPoke
      :: forall k (s :: k) r
       . ( Marshalable a
         , HasRenderElem r
         , HasRenderParams r
         , HasErr r
         , HasSpecInfo r
         , HasStmts r
         , HasRenderedNames r
         , Show a
         )
      => Stmt s r (Ref s ValueDoc)
  , csePeek
      :: forall k r (s :: k)
       . ( HasErr r
         , HasRenderElem r
         , HasSpecInfo r
         , HasRenderParams r
         , HasStmts r
         )
      => Maybe (Ref s AddrDoc -> Stmt s r (Ref s ValueDoc))
  }

instance P.Show (CustomScheme a) where
  showsPrec d (CustomScheme name _ _ _ _) =
    P.showParen (d > 10)
      $ P.showString "CustomScheme "
      . P.showsPrec 11 name
      . P.showString " _ _ _ _"

instance P.Show (CustomSchemeElided a) where
  showsPrec d (CustomSchemeElided name _ _) =
    P.showParen (d > 10)
      $ P.showString "CustomSchemeElided "
      . P.showsPrec 11 name
      . P.showString " _ _"


type ND r a
  =  (MemberWithError (Input MarshalParams) r, HasSpecInfo r)
  => Sem (Fail ': NonDet ': r) a

type HasMarshalParams r = MemberWithError (Input MarshalParams) r

-- | Some functions to control the marshaling
data MarshalParams = MarshalParams
  { isDefaultable       :: CType -> Bool
  , isPassAsPointerType :: CType -> Bool
  , getBespokeScheme
      :: forall a . Marshalable a => CName -> a -> Maybe (MarshalScheme a)
  }

instance Semigroup MarshalParams where
  mp1 <> mp2 = MarshalParams
    { isDefaultable       = getAny . concatBoth (Any .: isDefaultable)
    , isPassAsPointerType = getAny . concatBoth (Any .: isPassAsPointerType)
    , getBespokeScheme    = \p x ->
                              getBespokeScheme mp1 p x <|> getBespokeScheme mp2 p x
    }
   where
    concatBoth :: Monoid a => (MarshalParams -> a) -> a
    concatBoth f = f mp1 <> f mp2
    (.:) = (.) . (.)

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

-- | Matches if this is constrained to have a single value
univaluedScheme :: Marshalable a => a -> ND r (MarshalScheme a)
univaluedScheme = fmap ElidedUnivalued . singleValue

-- | Matches if this parameter is the length of one or more vectors
lengthScheme
  :: (Marshalable a, HasErr r)
  => Vector a
  -- ^ A set of things which have a length to draw from
  -> a
  -- ^ The potential length parameter
  -> ND r (MarshalScheme a)
lengthScheme ps p = do
  -- Get the vectors which are sized with this parameter
  let vs = V.filter (not . isReturnPtr) . getSizedWith (name p) $ ps
  -- Make sure they are not all void pointer, those do not have the length
  -- elided
  guard (any (\v -> type' v /= Ptr Const Void) vs)
  case V.partition isTopOptional vs of
    -- Make sure they exist
    (Empty, Empty)                     -> empty
    (Empty, rs) | all isReturnPtr rs -> empty
    (os, Empty) | length os > 1 ->
      throw "TODO: Handle multiple optional vectors without any required ones"
    (os, rs) -> pure $ ElidedLength os rs

-- | Matches const and non-const void pointers, exposes them as 'Ptr ()'
voidPointerScheme :: Marshalable a => a -> ND r (MarshalScheme a)
voidPointerScheme p = do
  Ptr _ Void <- pure $ type' p
  pure VoidPtr

-- | TODO: This should be fleshed out a bit more
returnPointerScheme :: Marshalable a => a -> ND r (MarshalScheme a)
returnPointerScheme p = do
  Ptr NonConst t <- pure $ type' p
  let inout = do
        Empty                <- pure $ lengths p
        False :<| True :<| _ <- pure $ isOptional p
        -- FIXME: Check if any siblings are sized by this
        guard (t == TypeName "uint32_t" || t == TypeName "size_t")
        pure $ InOutCount (Normal t)
      normal = do
        Empty <- pure $ lengths p
        pure $ Returned (Normal t)
      array = do
        _ :<| _ <- pure $ lengths p
        pure $ Returned (Vector (Normal t))
  asum [inout, normal, array]

-- | If we have a non-const pointer in a struct leave it as it is
returnPointerInStructScheme
  :: Marshalable a => a -> ND r (MarshalScheme a)
returnPointerInStructScheme p = do
  t@(Ptr NonConst _) <- pure $ type' p
  pure $ Preserve t

-- | Matches pointers with lengths
arrayScheme
  :: Marshalable a
  => WrapExtensibleStructs
  -> WrapDispatchableHandles
  -> a
  -> ND r (MarshalScheme a)
arrayScheme wes wdh p = case lengths p of
  -- Not an array
  Empty                    -> empty

  -- A string of some kind
  NullTerminated :<| Empty -> do
    Ptr Const c <- pure $ type' p
    guard $ isByteArrayElem c
    -- TODO: What's the impact of isTopOptional here
    pure $ case isOptional p of
      Singleton True -> Maybe ByteString
      _              -> ByteString

  -- TODO: Don't ignore the tail here
  -- TODO: Handle NamedMemberLength here
  NamedLength _ :<| _ -> do
    -- It's a const pointer
    Ptr Const t <- pure $ type' p
    -- TODO: What's the impact of isTopOptional here
    let isOpt = case isOptional p of
          True :<| _ -> True
          _          -> False
    elemType <- case t of
          -- If it's an array of bytes use a ByteString
      Ptr Const c | isByteArrayElem c -> pure $ case isOptional p of
        Singleton True -> Maybe ByteString
        _              -> ByteString
      _ -> dropPtrToStruct t >>= innerType wes wdh
    pure $ if isOpt then EitherWord32 elemType else Vector elemType

  _ -> empty

-- | Arrays with a predetermined length
fixedArrayScheme
  :: Marshalable a
  => WrapExtensibleStructs
  -> WrapDispatchableHandles
  -> a
  -> ND r (MarshalScheme a)
fixedArrayScheme wes wdh p = do
  guard . V.null . lengths $ p
  case type' p of
    Array _ (SymbolicArraySize _) t
      | isByteArrayElem t -> pure ByteString
      | otherwise         -> Vector <$> innerType wes wdh t
    Array _ (NumericArraySize n) t -> do
      e <- if isByteArrayElem t then pure ByteString else innerType wes wdh t
      pure $ Tupled n e
    _ -> empty

-- | An optional value with a default, so we don't need to wrap it in a Maybe
optionalDefaultScheme
  :: Marshalable a
  => WrapExtensibleStructs
  -> WrapDispatchableHandles
  -> a
  -> ND r (MarshalScheme a)
optionalDefaultScheme wes wds p = do
  MarshalParams {..} <- input
  Singleton True     <- pure $ isOptional p
  guard . V.null . lengths $ p
  guard $ isDefaultable (type' p)
  innerType wes wds (type' p)

-- | An optional value to be wrapped in a Maybe
optionalScheme
  :: Marshalable a
  => WrapExtensibleStructs
  -> WrapDispatchableHandles
  -> a
  -> ND r (MarshalScheme a)
optionalScheme wes wdh p = do
  Singleton True <- pure $ isOptional p
  guard . V.null . lengths $ p
  Maybe <$> innerType wes wdh (unPtrType (type' p))

-- | A struct to be wrapped in "SomeStruct"
extensibleStruct :: Marshalable a => a -> ND r (MarshalScheme a)
extensibleStruct p = do
  MarshalParams {..} <- input
  TypeName n         <- dropPtrToStruct (type' p)
  Just     s         <- getStruct n
  guard (not (V.null (sExtendedBy s)))
  pure $ WrappedStruct n

rawDispatchableHandles :: Marshalable a => a -> ND r (MarshalScheme a)
rawDispatchableHandles p = do
  MarshalParams {..} <- input
  t@(TypeName n)     <- dropPtrToStruct (type' p)
  Just h             <- getHandle n
  guard (Dispatchable == hDispatchable h)
  normalCheck t p
  pure . Preserve $ t

-- | The "default" method of marshalling something, store as ordinary member
scalarScheme :: Marshalable a => a -> ND r (MarshalScheme a)
scalarScheme p = do
  t <- dropPtrToStruct (type' p)
  normalCheck t p
  pure $ Normal t

normalCheck :: Marshalable a => CType -> a -> ND r ()
normalCheck t p = do
  MarshalParams {..} <- input
  -- Some sanity checking
  guard . V.null . values $ p
  guard . V.null . lengths $ p
  guard
    (  (V.null . isOptional $ p)
    || ((== Singleton False) . isOptional $ p)
    || isPassAsPointerType (unPtrType t)
    )
  guard . (/= Void) $ t
  guard $ not (isPtrType t) || isPassAsPointerType (unPtrType t)


-- Sometimes pointers to non-optional structs are used, remove these for the
-- marshalled version.
dropPtrToStruct :: HasSpecInfo r => CType -> Sem r CType
dropPtrToStruct t = do
  let stripConstPtr = \case
        Ptr Const t -> stripConstPtr t
        t           -> t
  case stripConstPtr t of
    TypeName n ->
      liftA2 (||) (isJust <$> getStruct n) (isJust <$> getUnion n) <&> \case
        True  -> TypeName n
        False -> t
    _ -> pure t

----------------------------------------------------------------
-- Extensible structs
----------------------------------------------------------------

data WrapExtensibleStructs = WrapExtensibleStructs | DoNotWrapExtensibleStructs
  deriving (Eq)
data WrapDispatchableHandles = WrapDispatchableHandles | DoNotWrapDispatchableHandles
  deriving (Eq)

innerType
  :: WrapExtensibleStructs
  -> WrapDispatchableHandles
  -> CType
  -> ND r (MarshalScheme a)
innerType wes wdh t = do
  r <-
    runNonDetMaybe
    .  asum @[]
    $  [ unwrapDispatchableHandles t | wdh == DoNotWrapDispatchableHandles ]
    <> [ wrapExtensibleStruct t | wes == WrapExtensibleStructs ]

  pure $ fromMaybe (Normal t) r

unwrapDispatchableHandles
  :: (Member NonDet r, HasSpecInfo r) => CType -> Sem r (MarshalScheme a)
unwrapDispatchableHandles t = failToNonDet $ do
  TypeName n <- pure t
  Just     h <- getHandle n
  pure $ case hDispatchable h of
    Dispatchable    -> Preserve t
    NonDispatchable -> Normal t

-- If this is an extensible struct, wrap it, otherwise return Normal
wrapExtensibleStruct
  :: (Member NonDet r, HasSpecInfo r) => CType -> Sem r (MarshalScheme a)
wrapExtensibleStruct t = failToNonDet $ do
  TypeName n <- dropPtrToStruct t
  Just     s <- getStruct n
  guard (not (V.null (sExtendedBy s)))
  pure $ WrappedStruct n

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

unPtrType :: CType -> CType
unPtrType = \case
  Ptr _ t -> t
  t       -> t

-- | Is this optional at the top level
isTopOptional :: Marshalable a => a -> Bool
isTopOptional x = case isOptional x of
  Empty   -> False
  b :<| _ -> b

-- | Is this a non-const pointer only used for returning
isReturnPtr :: Marshalable a => a -> Bool
isReturnPtr p' = case type' p' of
  Ptr NonConst _ -> True
  _              -> False

-- | Get all the @a@s which are sized with this name
getSizedWith :: Marshalable a => CName -> Vector a -> Vector a
getSizedWith lengthName = V.filter $ \v -> case lengths v of
  (NamedLength len :<| _) | len == lengthName -> True
  -- ^ TODO: Change this to [NamedLength len] and think about handling
  _ -> False

-- | Element types for which arrays of should be translated into @ByteString@s
isByteArrayElem :: CType -> Bool
isByteArrayElem = (`elem` [Void, Char, TypeName "uint8_t", TypeName "int8_t"])

isElided :: MarshalScheme a -> Bool
isElided = \case
  Unit              -> False
  Preserve _        -> False
  Normal   _        -> False
  ElidedLength _ _  -> True
  ElidedUnivalued _ -> True
  ElidedVoid        -> True
  VoidPtr           -> False
  ByteString        -> False
  Maybe        _    -> False
  Vector       _    -> False
  EitherWord32 _    -> False
  Tupled _ _        -> False
  Returned      _   -> False
  InOutCount    _   -> False
  WrappedStruct _   -> False
  Custom        _   -> False
  ElidedCustom  _   -> True
