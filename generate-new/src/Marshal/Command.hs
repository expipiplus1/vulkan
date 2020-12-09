module Marshal.Command
  ( marshalCommand
  , MarshaledCommand(..)
  , MarshaledParam(..)
  , marshaledCommandShouldIncludeReturnValue
  , marshaledCommandInputTypes
  , marshaledCommandReturnTypes
  , marshaledParamType
  , marshaledParamTypeNegative
  , marshaledParamTypePositive
  ) where

import qualified Data.Vector                   as V
import           Polysemy
import           Polysemy.Fail
import           Polysemy.Input
import           Polysemy.NonDet
import           Relude

import           CType
import           Error
import           Haskell                       as H
import           Marshal.Scheme
import           Render.Element
import           Render.Scheme
import           Render.SpecInfo
import           Render.Type
import           Spec.Parse

data MarshaledCommand = MarshaledCommand
  { mcName    :: CName
  , mcParams  :: V.Vector MarshaledParam
  , mcReturn  :: CType
    -- ^ TOOD: Change
  , mcCommand :: Command
  }
  deriving Show

data MarshaledParam = MarshaledParam
  { mpParam  :: Parameter
  , mpScheme :: MarshalScheme Parameter
  }
  deriving Show

marshalCommand
  :: (HasMarshalParams r, HasErr r, HasSpecInfo r)
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
  :: (HasMarshalParams r, HasErr r, HasSpecInfo r)
  => Command
  -> Parameter
  -> Sem r (MarshalScheme Parameter)
parameterScheme Command {..} param = do
  MarshalParams {..} <- input
  let
    schemes =
      [ maybe empty pure . getBespokeScheme cName
        -- These two are for value constrained params:
      , univaluedScheme
      , lengthScheme cParameters
        -- Pointers to Void have some special handling
      , voidPointerScheme
        -- Pointers to return values in
      , returnPointerScheme
      , returnArrayScheme
        -- Optional and non optional arrays
      , arrayScheme WrapExtensibleStructs WrapDispatchableHandles cParameters
      , fixedArrayScheme WrapExtensibleStructs WrapDispatchableHandles
        -- Optional things:
      , optionalDefaultScheme DoNotWrapExtensibleStructs WrapDispatchableHandles
      , optionalScheme DoNotWrapExtensibleStructs WrapDispatchableHandles
        -- Everything left over is treated as a boring scalar parameter
      , scalarScheme
      ]
  m <- runNonDet . failToNonDet . asum . fmap ($ param) $ schemes
  case m of
    Just x  -> pure x
    Nothing -> throw
      ("Not handled by any marshaling scheme. Type: " <> show (pType param))

----------------------------------------------------------------
-- Getting type information for high level function
----------------------------------------------------------------

-- | We include the return value if
--
-- - It is not Void and it is not the exit code type
-- - It is the exit code type and success other than the default is possible
marshaledCommandShouldIncludeReturnValue
  :: HasRenderParams r => MarshaledCommand -> Sem r Bool
marshaledCommandShouldIncludeReturnValue MarshaledCommand {..} = do
  RenderParams {..} <- input
  pure
    $  mcReturn
    /= Void
    && (mcReturn == successCodeType --> any isSuccessCodeReturned
                                            (cSuccessCodes mcCommand)
       )

marshaledCommandInputTypes
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => MarshaledCommand
  -> Sem r (V.Vector H.Type)
marshaledCommandInputTypes MarshaledCommand {..} =
  V.mapMaybe id <$> traverseV marshaledParamTypeNegative mcParams

-- | Get a list of the types of values which should be in the return value of
-- this command.
marshaledCommandReturnTypes
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Bool
  -- ^ Should we return the value of 'InOutCount' parameters
  -> MarshaledCommand
  -> Sem r (V.Vector H.Type)
marshaledCommandReturnTypes includeInOutCountTypes m@MarshaledCommand {..} = do
  includeReturnType <- marshaledCommandShouldIncludeReturnValue m
  pts               <-
    V.mapMaybe id
      <$> traverseV (marshaledParamTypePositive includeInOutCountTypes) mcParams
  r <- case mcReturn of
    Void                      -> pure V.empty
    _ | not includeReturnType -> pure V.empty
    r                         -> V.singleton <$> cToHsType DoNotPreserve r
  pure (r <> pts)

marshaledParamTypePositive
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Bool
  -- ^ Consider 'InOutCount' params as positive
  -> MarshaledParam
  -> Sem r (Maybe H.Type)
marshaledParamTypePositive includeInOutCountTypes =
  let pos = \case
        InOutCount _ | not includeInOutCountTypes -> pure Nothing
        s -> schemeTypePositive s
  in  marshaledParamType pos

marshaledParamTypeNegative
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => MarshaledParam
  -> Sem r (Maybe H.Type)
marshaledParamTypeNegative = marshaledParamType schemeTypeNegative

-- | A helper to annotate a parameter with a name
marshaledParamType
  :: (HasErr r, HasRenderParams r)
  => (MarshalScheme Parameter -> Sem r (Maybe H.Type))
  -> MarshaledParam
  -> Sem r (Maybe H.Type)
marshaledParamType st MarshaledParam {..} = contextShow (pName mpParam) $ do
  RenderParams {..} <- input
  let Parameter {..} = mpParam
  n <- st mpScheme
  pure $ namedTy (unName . mkParamName $ pName) <$> n

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

infixr 2 -->
(-->) :: Bool -> Bool -> Bool
a --> b = not a || b
