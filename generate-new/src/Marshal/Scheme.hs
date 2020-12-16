module Marshal.Scheme where

import           Data.Text.Prettyprint.Doc
import           Data.Vector.Extra              ( pattern (:<|)
                                                , pattern Empty
                                                , pattern Singleton
                                                , Vector
                                                )
import qualified Data.Vector.Extra             as V
import           Polysemy
import           Polysemy.Fail
import           Polysemy.Input
import           Polysemy.NonDet         hiding ( Empty )
import qualified Prelude                       as P
import           Relude                  hiding ( Const
                                                , uncons
                                                )

import           CType
import qualified Data.Text                     as T
import           Error
import           Haskell                       as H
import           Marshal.Marshalable
import           Render.Element
import           Render.Names
import           Render.SpecInfo
import           Render.State                   ( HasRenderState )
import           Render.Stmts
import {-# SOURCE #-} Render.Stmts.Poke.SiblingInfo
import           Spec.Types

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
  | Length CType (Vector a) (Vector a)
    -- ^ The length of a set of optional and required vectors, the same as
    -- Normal but with some checks on the size. Additionally if the given
    -- length is zero, it will instead be poked as the number of elements in
    -- the associated vector.  This is used over 'ElidedLength' when having the
    -- length explicit would be clearer.
  | ElidedLength CType (Vector a) (Vector a) -- optional and required lengths
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
  | Vector Nullable (MarshalScheme a)
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
    -- ^ A extensible struct to be wrapped in a GADT to hide a variable
  | WrappedChildStruct CName
    -- ^ An inherited struct to be wrapped in a GADT to hide the precise
    -- identity
  | Returned (MarshalScheme a)
    -- ^ A non-const pointer to some allocated memory, used to return
    -- additional values.
    -- Not typically used for structs
  | InOutCount (MarshalScheme a)
    -- ^ A non-const pointer to some count value, used to send and return
    -- additional the length of a vector.
    -- - Not typically used for structs
  | OutCount (MarshalScheme a)
    -- ^ A non-const pointer to some count value, used to return the length of
    -- a vector, which when input has a equal or longer length
    -- - Not typically used for structs
  | Custom (CustomScheme a)
    -- ^ A non-elided scheme with some complex behavior
  | ElidedCustom (CustomSchemeElided a)
    -- ^ An elided scheme with some complex behavior
  deriving (Show, Eq, Ord)

data Nullable = Nullable | NotNullable
  deriving (Show, Eq, Ord)

data CustomScheme a = CustomScheme
  { csName       :: Text
    -- ^ A name for Eq and Ord, also useful for debugging
  , csZero       :: Maybe (Doc ())
    -- ^ The 'zero' value for this scheme if possible
  , csZeroIsZero :: Bool
    -- ^ Does the 'zero' value write zero bytes (and can be omitted if we know
    -- the bytes are zero already).
    -- TODO, better name for this
  , csType
      :: forall r
       . (HasErr r, HasRenderParams r, HasSpecInfo r)
      => Sem r H.Type
  , csDirectPoke :: CSPoke a
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

data CSPoke a
  = NoPoke
  | APoke
       (forall k (s :: k) r
       . ( Marshalable a
         , HasRenderElem r
         , HasRenderParams r
         , HasErr r
         , HasSpecInfo r
         , HasSiblingInfo a r
         , HasStmts r
         , HasRenderedNames r
         , HasRenderState r
         , Show a
         )
      => Ref s ValueDoc -> Stmt s r (Ref s ValueDoc))

instance Eq (CustomScheme a) where
  (==) = (==) `on` csName

instance Ord (CustomScheme a) where
  compare = compare `on` csName

data CustomSchemeElided a = CustomSchemeElided
  { cseName :: Text
    -- ^ A name for Eq and Ord, also useful for debugging
  , cseDirectPoke
      :: forall k (s :: k) r
       . ( Marshalable a
         , HasRenderElem r
         , HasRenderParams r
         , HasErr r
         , HasSpecInfo r
         , HasStmts r
         , HasRenderedNames r
         , HasSiblingInfo a r
         , HasRenderState r
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

instance Eq (CustomSchemeElided a) where
  (==) = (==) `on` cseName

instance Ord (CustomSchemeElided a) where
  compare = compare `on` cseName

instance P.Show (CustomScheme a) where
  showsPrec d (CustomScheme name _ _ _ _ _) =
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
  , isForeignStruct     :: CType -> Bool
    -- ^ Foreign structs we've defined explicitly
  , getBespokeScheme
      :: forall a . Marshalable a => CName -> a -> Maybe (MarshalScheme a)
  }

instance Semigroup MarshalParams where
  mp1 <> mp2 = MarshalParams
    { isDefaultable       = getAny . concatBoth (Any .: isDefaultable)
    , isPassAsPointerType = getAny . concatBoth (Any .: isPassAsPointerType)
    , isForeignStruct     = getAny . concatBoth (Any .: isPassAsPointerType)
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

data ElideReturnedVectorLengths
  = DoNotElideReturnedVectorLengths
  | DoElideReturnedVectorLengths

elidedInputCountScheme
  :: (Marshalable a, HasErr r)
  => Vector a
  -- ^ A set of things which have a length to draw from
  -> a
  -- ^ The potential length parameter
  -> ND r (MarshalScheme a)
elidedInputCountScheme ps p = do
  -- Get the vectors which are sized with this parameter
  let vs = getSizedWith (name p) ps
  -- Make sure they are not all void pointer, those do not have the length
  -- elided
  guard (any (\v -> type' v /= Ptr Const Void) vs)
  guard ("CapacityInput" `T.isSuffixOf` unCName (name p))
  case V.partition isTopOptional vs of
    -- Make sure they exist
    (Empty, Empty) -> empty
    (os   , rs   ) -> pure $ ElidedLength (type' p) os rs

-- | Matches if this parameter is the length of one or more vectors
lengthScheme
  :: (Marshalable a, HasErr r)
  => ElideReturnedVectorLengths
  -- ^ Should we elide lengths which are specified just by returned vectors
  -- we may want to do this if the lengths are specified in some other way
  -- (like in a dual-purpose command)
  -> Vector a
  -- ^ A set of things which have a length to draw from
  -> a
  -- ^ The potential length parameter
  -> ND r (MarshalScheme a)
lengthScheme elideReturnVectorLengths ps p = do
  -- Get the vectors which are sized with this parameter
  let vs = case elideReturnVectorLengths of
        DoNotElideReturnedVectorLengths ->
          V.filter (not . isReturnPtr) . getSizedWith (name p) $ ps
        DoElideReturnedVectorLengths -> getSizedWith (name p) ps
  -- Make sure they are not all void pointer, those do not have the length
  -- elided
  guard (any (\v -> type' v /= Ptr Const Void) vs)
  case V.partition isTopOptional vs of
    -- Make sure they exist
    (Empty, Empty) -> empty
    (Empty, rs)
      | DoNotElideReturnedVectorLengths <- elideReturnVectorLengths
      , all isReturnPtr rs
      -> empty
    -- If we have a just optional vectors then preserve the length member
    (os, rs@Empty) -> pure $ Length (type' p) os rs
    (os, rs      ) -> pure $ ElidedLength (type' p) os rs

-- | Matches const and non-const void pointers, exposes them as 'Ptr ()'
voidPointerScheme :: Marshalable a => a -> ND r (MarshalScheme a)
voidPointerScheme p = do
  Ptr _ Void <- pure $ type' p
  pure VoidPtr

-- | TODO: This should be fleshed out a bit more
returnPointerScheme :: Marshalable a => a -> ND r (MarshalScheme a)
returnPointerScheme p = do
  MarshalParams {..} <- input
  Ptr NonConst t     <- pure $ type' p
  guard (not (isPassAsPointerType t))
  let inout = do
        Empty                <- pure $ lengths p
        False :<| True :<| _ <- pure $ isOptional p
        -- FIXME: Check if any siblings are sized by this
        guard (t == TypeName "uint32_t" || t == TypeName "size_t")
        pure $ InOutCount (Normal t)
      outLength = do
        Empty <- pure $ lengths p
        Empty <- pure $ isOptional p
        guard ("CountOutput" `T.isSuffixOf` unCName (name p))
        guard (t == TypeName "uint32_t" || t == TypeName "size_t")
        pure $ OutCount (Normal t)
      normal = do
        Empty <- pure $ lengths p
        pure $ Returned (Normal t)
      array = do
        _ :<| _ <- pure $ lengths p
        pure $ Returned (Vector NotNullable (Normal t))
  asum [inout, outLength, normal, array]

returnArrayScheme :: Marshalable a => a -> ND r (MarshalScheme a)
returnArrayScheme p = do
  MarshalParams {..}          <- input
  Array NonConst _arraySize t <- pure $ type' p
  guard (not (isPassAsPointerType t))
  let string = do
        Char  <- pure t
        Empty <- pure $ lengths p
        pure $ Returned ByteString
  asum [string]

-- | If we have a non-const pointer in a struct leave it as it is
returnPointerInStructScheme :: Marshalable a => a -> ND r (MarshalScheme a)
returnPointerInStructScheme p = do
  t@(Ptr NonConst _) <- pure $ type' p
  pure $ Preserve t

-- | Matches pointers with lengths
arrayScheme
  :: Marshalable a
  => WrapExtensibleStructs
  -> WrapDispatchableHandles
  -> Vector a
  -- ^ Siblings
  -> a
  -> ND r (MarshalScheme a)
arrayScheme wes wdh sibs p = case lengths p of
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
  l :<| _
    | case l of
      NamedLength _         -> True
      NamedMemberLength _ _ -> True
      _                     -> False
    -> do
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

      -- Is the length constrained by a non-optional sibling
      let constrainedLengthBySibling = not isOpt || (not . null)
            [ ()
            | s        <- toList sibs
            , l' :<| _ <- pure $ lengths s
            , l == l'
            , not (isTopOptional s)
            ]
          -- It is often clearer to preserve the length member for optional
          -- arrays
          constrainedLengthByClarity = True
          nullable                   = bool NotNullable Nullable isOpt
      pure $ if constrainedLengthByClarity || constrainedLengthBySibling
        then Vector nullable elemType
        else EitherWord32 elemType

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
      | otherwise         -> Vector NotNullable <$> innerType wes wdh t
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
  TypeName n <- dropPtrToStruct (type' p)
  Just     s <- getStruct n
  guard (not (V.null (sExtendedBy s)))
  pure $ WrappedStruct n

-- | A struct to be wrapped in "SomeHaptic"/"Some..."
inheritingStruct :: Marshalable a => a -> ND r (MarshalScheme a)
inheritingStruct p = do
  TypeName n <- dropPtrToStruct (type' p)
  Just     s <- getStruct n
  guard (not (V.null (sInheritedBy s)))
  pure $ WrappedChildStruct n

rawDispatchableHandles :: Marshalable a => a -> ND r (MarshalScheme a)
rawDispatchableHandles p = do
  t@(TypeName n) <- dropPtrToStruct (type' p)
  Just h         <- getHandle n
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
dropPtrToStruct :: (HasMarshalParams r, HasSpecInfo r) => CType -> Sem r CType
dropPtrToStruct t = do
  MarshalParams {..} <- input
  let stripConstPtr = \case
        Ptr Const t -> stripConstPtr t
        t           -> t
  case stripConstPtr t of
    t | isForeignStruct t -> pure t
    TypeName n ->
      (isJust <$> getStruct n) <||> (isJust <$> getUnion n) <&> \case
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
    <> [ do
           i <- raise $ innerType wes wdh tElem
           pure (Tupled n i)
       | Array _ (NumericArraySize n) tElem <- pure t
       ]

  pure $ fromMaybe (Normal t) r

unwrapDispatchableHandles
  :: (Member NonDet r, HasSpecInfo r) => CType -> Sem r (MarshalScheme a)
unwrapDispatchableHandles t = failToNonDet $ do
  TypeName n <- pure t
  Just     h <- getHandle n
  pure $ case hDispatchable h of
    Dispatchable    -> Preserve t
    NonDispatchable -> Normal t

-- If this is an extensible or inherited struct, wrap it, otherwise return
-- Normal
wrapExtensibleStruct
  :: (Member NonDet r, HasSpecInfo r, HasMarshalParams r)
  => CType
  -> Sem r (MarshalScheme a)
wrapExtensibleStruct t = failToNonDet $ do
  TypeName n <- dropPtrToStruct t
  Just     s <- getStruct n
  let isExtended  = not (V.null (sExtendedBy s))
      isInherited = not (V.null (sInheritedBy s))
  guard (isExtended || isInherited)
  pure $ if isInherited then WrappedChildStruct n else WrappedStruct n

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
  --  ^ TODO: Change this to [NamedLength len] and think about handling
  _ -> False

-- | Element types for which arrays of should be translated into @ByteString@s
isByteArrayElem :: CType -> Bool
isByteArrayElem = (`elem` [Void, Char, TypeName "uint8_t", TypeName "int8_t"])

isElided :: MarshalScheme a -> Bool
isElided = \case
  Unit                 -> False
  Preserve _           -> False
  Normal   _           -> False
  Length{}             -> False
  ElidedLength{}       -> True
  ElidedUnivalued _    -> True
  ElidedVoid           -> True
  VoidPtr              -> False
  ByteString           -> False
  Maybe _              -> False
  Vector _ _           -> False
  EitherWord32 _       -> False
  Tupled _ _           -> False
  Returned           _ -> False
  InOutCount         _ -> False
  OutCount           _ -> False
  WrappedStruct      _ -> False
  WrappedChildStruct _ -> False
  Custom             _ -> False
  ElidedCustom       _ -> True

isNegative :: MarshalScheme a -> Bool
isNegative = \case
  Unit                 -> True
  Preserve _           -> True
  Normal   _           -> True
  Length{}             -> True
  ElidedLength{}       -> False
  ElidedUnivalued _    -> False
  ElidedVoid           -> False
  VoidPtr              -> True
  ByteString           -> True
  Maybe _              -> True
  Vector _ _           -> True
  EitherWord32 _       -> True
  Tupled _ _           -> True
  Returned           _ -> False
  -- TODO: We should probably be more careful with InOutCount
  InOutCount         _ -> True
  OutCount           _ -> False
  WrappedStruct      _ -> True
  WrappedChildStruct _ -> True
  Custom             _ -> True
  ElidedCustom       _ -> False

-- | A bit of an ad-hoc test
isSimple :: HasSpecInfo r => MarshalScheme a -> Sem r Bool
isSimple = \case
  Unit       -> pure True
  Preserve _ -> pure True
  Normal (TypeName n) ->
    (isNothing <$> getStruct n) <&&> (isNothing <$> getUnion n)
  Normal _             -> pure True
  Length{}             -> pure True
  ElidedLength{}       -> pure True
  ElidedUnivalued _    -> pure True
  ElidedVoid           -> pure True
  VoidPtr              -> pure False
  ByteString           -> pure False
  Maybe s              -> isSimple s
  Vector _ _           -> pure False
  EitherWord32 _       -> pure False
  Tupled _ s           -> isSimple s
  Returned           _ -> pure False
  InOutCount         _ -> pure False
  OutCount           _ -> pure False
  WrappedStruct      _ -> pure False
  WrappedChildStruct _ -> pure False
  Custom             _ -> pure False
  ElidedCustom       _ -> pure False

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
