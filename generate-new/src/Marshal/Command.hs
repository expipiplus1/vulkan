module Marshal.Command
  ( marshalCommand
  , MarshaledCommand(..)
  , MarshaledParam(..)
  ) where

import           Relude                  hiding ( Reader
                                                , ask
                                                )
import           Polysemy
import           Polysemy.NonDet
import           Polysemy.Reader
import           Polysemy.Fail
import qualified Data.Vector                   as V

import           CType
import           Spec.Parse
import           Error
import           Marshal.Scheme

data MarshaledCommand = MarshaledCommand
  { mcName    :: Text
  , mcParams  :: V.Vector MarshaledParam
  , mcReturn  :: CType
    -- ^ TOOD: Change
  , mcCommand :: Command
  }
  deriving Show

data MarshaledParam =
  MarshaledParam
    { mpParam :: Parameter
    , mpScheme :: MarshalScheme Parameter
    }
  deriving (Show)

marshalCommand
  :: (MemberWithError (Reader MarshalParams) r, HasErr r)
  => Command
  -> Sem r MarshaledCommand
marshalCommand c@Command {..} = contextShow cName $ do
  let mcName    = cName
      mcReturn  = cReturnType
      mcCommand = c
  mcParams <- forV cParameters $ \p -> contextShow (pName p) $ do
    scheme <- parameterScheme c p
    pure $ MarshaledParam p scheme
  pure MarshaledCommand { .. }

parameterScheme
  :: (MemberWithError (Reader MarshalParams) r, HasErr r)
  => Command
  -> Parameter
  -> Sem r (MarshalScheme Parameter)
parameterScheme Command {..} param = do
  MarshalParams {..} <- ask
  let schemes =
        [ maybe empty pure . getBespokeScheme cName
          -- These two are for value constrained params:
        , univaluedScheme
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
  m <- runNonDet . failToNonDet . asum . fmap ($ param) $ schemes
  case m of
    Just x  -> pure x
    Nothing -> throw
      ("Not handled by any marshaling scheme. Type: " <> show (pType param))

