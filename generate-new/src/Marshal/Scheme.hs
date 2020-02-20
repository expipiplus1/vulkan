module Marshal.Scheme
  where

import           Relude                  hiding ( Const
                                                , uncons
                                                , Reader
                                                , ask
                                                )
import           Polysemy
import           Polysemy.Reader
import           Polysemy.NonDet         hiding ( Empty )
import           Polysemy.Fail
import           Data.Vector.Extra              ( Vector
                                                , pattern Empty
                                                , pattern Singleton
                                                , pattern (:<|)
                                                )
import qualified Data.Vector.Extra             as V

import           Marshal.Marshalable
import           Error
import           CType

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
    -- The sub-scheme here is for elements
  | Tupled Word (MarshalScheme a)
    -- ^ A small fixed sized array, represented as a n-length tuple of the same
    -- element
  | Returned (MarshalScheme a)
    -- ^ A non-const pointer to some allocated memory, used to return
    -- additional values.
    -- Not typically used for structs
  deriving (Show)

type ND r a
  = (MemberWithError (Reader MarshalParams) r) => Sem (Fail ': NonDet ': r) a

-- | Some functions to control the marshaling
data MarshalParams = MarshalParams
  { isDefaultable       :: CType -> Bool
  , isStruct            :: Text -> Bool
  , isPassAsPointerType :: CType -> Bool
  }

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
univaluedScheme :: Marshalable a => a -> ND r (MarshalScheme c)
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
voidPointerScheme :: Marshalable a => a -> ND r (MarshalScheme c)
voidPointerScheme p = do
  Ptr _ Void <- pure $ type' p
  pure VoidPtr

returnPointerScheme :: Marshalable a => a -> ND r (MarshalScheme c)
returnPointerScheme p = do
  t@(Ptr NonConst _) <- pure $ type' p
  pure $ Returned (Normal t)

-- | If we have a non-const pointer in a struct leave it as it is
returnPointerInStructScheme
  :: Marshalable a => a -> ND r (MarshalScheme c)
returnPointerInStructScheme p = do
  t@(Ptr NonConst _) <- pure $ type' p
  pure $ Preserve t

-- | Matches pointers with lengths
arrayScheme :: Marshalable a => a -> ND r (MarshalScheme c)
arrayScheme p = case lengths p of
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
  NamedLength _ :<| _ -> do
    -- It's a const pointer
    Ptr Const t <- pure $ type' p
    -- TODO: What's the impact of isTopOptional here
    let isOpt = case isOptional p of
          True :<| _ -> True
          _          -> False
        elemType = case t of
          -- If it's an array of bytes use a ByteString
          Ptr Const c | isByteArrayElem c -> case isOptional p of
            Singleton True -> Maybe ByteString
            _              -> ByteString
          _ -> Normal t
        vType = elemType
    pure $ if isOpt then EitherWord32 vType else Vector vType

  _ -> empty

-- | Arrays with a predetermined length
fixedArrayScheme :: Marshalable a => a -> ND r (MarshalScheme c)
fixedArrayScheme p = do
  guard . V.null . lengths $ p
  case type' p of
    Array _ (SymbolicArraySize _) t | isByteArrayElem t -> pure ByteString
                                    | otherwise -> pure $ Vector (Normal t)
    Array _ (NumericArraySize n) t ->
      let e = if isByteArrayElem t then ByteString else Normal t
      in  pure $ Tupled n e
    _ -> empty

-- | An optional value with a default, so we don't need to wrap it in a Maybe
optionalDefaultScheme :: Marshalable a => a -> ND r (MarshalScheme c)
optionalDefaultScheme p = do
  MarshalParams {..} <- ask
  Singleton True     <- pure $ isOptional p
  guard . V.null . lengths $ p
  guard $ isDefaultable (type' p)
  pure $ Normal (type' p)

-- | An optional value to be wrapped in a Maybe
optionalScheme :: Marshalable a => a -> ND r (MarshalScheme c)
optionalScheme p = do
  Singleton True <- pure $ isOptional p
  guard . V.null . lengths $ p
  pure $ Maybe (Normal . unPtrType . type' $ p)

-- | The "default" method of marshalling something, store as ordinary member
scalarScheme :: Marshalable a => a -> ND r (MarshalScheme c)
scalarScheme p = do
  MarshalParams {..} <- ask
  -- Sometimes pointers to non-optional structs are used, remove these for the
  -- marshalled version.
  let t = case type' p of
        (Ptr Const (TypeName n)) | isStruct n -> TypeName n
        t' -> t'
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
  pure . Normal $ t

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

-- | Is this a non-const pointer
isReturnPtr :: Marshalable a => a -> Bool
isReturnPtr p' = case type' p' of
  Ptr NonConst _ -> True
  _              -> False

-- | Get all the @a@s which are sized with this name
getSizedWith :: Marshalable a => Text -> Vector a -> Vector a
getSizedWith lengthName = V.filter $ \v -> case lengths v of
  (NamedLength len :<| _) | len == lengthName -> True
  -- ^ TODO: Change this to [NamedLength len] and think about handling
  _ -> False

-- | Element types for which arrays of should be translated into @ByteString@s
isByteArrayElem :: CType -> Bool
isByteArrayElem = (`elem` [Void, Char, TypeName "uint8_t", TypeName "int8_t"])
