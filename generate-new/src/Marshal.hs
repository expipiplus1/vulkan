module Marshal
  where

import Relude hiding(Reader)
import Polysemy
import Polysemy.NonDet
import Polysemy.Reader
import Polysemy.Fail
import Data.Vector as V

import CType
import Haskell
import TrackDepends
import Spec.Parse
import Error
import Marshal.Scheme

type M = Sem '[Err, TrackDepends]

data MarshaledStruct = MarshaledStruct
  { msName :: Text
  , msMembers :: Vector MarshaledStructMember
  }

data MarshaledStructMember =
  MarshaledStructMember
    { msmStructMember :: StructMember
    , msmScheme :: MarshalScheme StructMember
    }

marshalStruct
  :: (MemberWithError (Reader MarshalParams) r, HasErr r)
  => Struct
  -> Sem r MarshaledStruct
marshalStruct s@Struct {..} = contextShow structName $ do
  let msName = structName
  msMembers <- forV structMembers $ \sm -> contextShow (smName sm) $ do
    scheme <- structMemberScheme s sm
    pure $ MarshaledStructMember sm scheme
  pure MarshaledStruct { .. }

structMemberScheme
  :: (MemberWithError (Reader MarshalParams) r, HasErr r)
  => Struct
  -> StructMember
  -> Sem r (MarshalScheme StructMember)
structMemberScheme s@Struct {..} member = do
  let schemes =
        [ -- These two are for value constrained params:
          univaluedScheme
        , lengthScheme structMembers
          -- Pointers to Void have some special handling
        , voidPointerScheme
          -- Pointers to return values in, unmarshaled at the moment
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
  m <- runNonDet . failToNonDet . asum . fmap ($ member) $ schemes
  case m of
    Just x  -> pure x
    Nothing -> throw
      ("Not handled by any marshaling scheme. Type: " <> show (smType member))

----------------------------------------------------------------
--
----------------------------------------------------------------

-- data MarshaledFun = MarshaledFun
--   { marshaledCType :: Sem '[TrackDepends] HType
--   , marshaledHType :: Sem '[TrackDepends] HType
--   , marshaledToC :: Sem '[TrackDepends] Text
--   }

-- data Parameter = Parameter
--   { paramName :: Text
--   , paramType :: CType
--   , paramLength :: Maybe ParameterLength
--   , paramIsOptional :: Maybe Bool
--   }
--   deriving (Show, Eq)

-- data ParameterLength
--   = NullTerminated
--   | NamedLength Text
--   | NamedMemberLength Text Text
--     -- ^ The length is specified by a member of another (struct) parameter
--   deriving (Show, Eq)

-- marshalFunction :: CType -> Parameter -> MarshaledFun
-- marshalFunction ret args = undefined

-- qualifiedCon
--   :: Member TrackDepends r
--   => Text -> Text -> Sem r HType
-- qualifiedCon mod' t = do
--   tell [External (ModuleName mod') t]
--   pure (Con t)
