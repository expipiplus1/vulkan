module Marshal.Struct
  ( marshalStruct
  , MarshaledStruct(..)
  , MarshaledStructMember(..)
  ) where

import Relude hiding(Reader)
import Polysemy
import Polysemy.NonDet
import Polysemy.Reader
import Polysemy.Fail
import Data.Vector as V

import Spec.Parse
import Error
import Marshal.Scheme

data MarshaledStruct = MarshaledStruct
  { msName :: Text
  , msStruct :: Struct
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
marshalStruct s@Struct {..} = contextShow sName $ do
  let msName = sName
      msStruct = s
  msMembers <- forV sMembers $ \sm -> contextShow (smName sm) $ do
    scheme <- structMemberScheme s sm
    pure $ MarshaledStructMember sm scheme
  pure MarshaledStruct { .. }

structMemberScheme
  :: (MemberWithError (Reader MarshalParams) r, HasErr r)
  => Struct
  -> StructMember
  -> Sem r (MarshalScheme StructMember)
structMemberScheme Struct {..} member = do
  let schemes =
        [ -- These two are for value constrained params:
          univaluedScheme
        , lengthScheme sMembers
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

