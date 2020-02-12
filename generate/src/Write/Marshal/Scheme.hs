{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Write.Marshal.Scheme where

import           Control.Arrow                            ( second )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Extra                          ( Text )
import qualified Data.Text.Extra               as T
import           Data.Maybe
import           Control.Monad.Except
import           Data.List.Extra                          ( intersperse
                                                          , partition
                                                          )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader
import           Control.Applicative                      ( Alternative(..) )
import           Control.Bool
import           Data.Foldable                            ( asum , toList)

import           Spec.Savvy.Command
import           Spec.Savvy.Handle
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import qualified Write.Element                 as WE
import           Write.Marshal.Util
import           Write.Monad
import           Write.Monad.Lookup
import           Write.Marshal.Marshallable

-- TODO: Rename
data MarshalScheme a = MarshalScheme
  { msSchemeName     :: Text
  , msParam          :: a
  , msMarshalledType :: MarshalData
    -- ^ The type and its zero value
  , msPosition       :: Position
  }

data Position = Negative [Poke] | Positive

data MarshalData
  = Elided
  | Present (WE (Doc ())) ZeroType

data ZeroType
  = Zero
  | ZeroNothing
  | ZeroMempty
  | ZeroNullPtr
  | ZeroTuple Word ZeroType
  | ZeroFalse
  | ZeroLeftZero

data Poke
  = SimplePoke
  | ComplexPoke (Doc () -> WE (Doc ()))
  | ComplexContTPoke (Doc () -> WE (Doc ()))
  | ContTPoke (WE (Doc ()))
    -- ^ A poke which doesn't bind any variables taking place in ContT
  | IOPoke (WE (Doc ()))
    -- ^ A poke which doesn't bind any variables
  | WrappedPokeIO (WE (Doc ()))
  | WrappedPokeContT (WE (Doc ()))
  | WrappedPokePure (WE (Doc ()))

msUnmarshalledName :: Marshallable a => MarshalScheme a -> Name Unmarshalled
msUnmarshalledName = name . msParam

msMarshalledName :: Marshallable a => MarshalScheme a -> Name Marshalled
msMarshalledName = getMarshalledName . msUnmarshalledName

isNegative :: Position -> Bool
isNegative = \case
  Negative _ -> True
  _          -> False

----------------------------------------------------------------
-- Rendering
----------------------------------------------------------------

data PokeSet = PokeSet
  { pokeSetContT :: [Doc ()]
  , pokeSetIO    :: [Doc ()]
  , pokeSetPure  :: [Doc ()]
  , pokeSetPoke  :: [Doc ()]
  }

instance Semigroup PokeSet where
  PokeSet a1 b1 c1 d1 <> PokeSet a2 b2 c2 d2 =
    PokeSet (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance Monoid PokeSet where
  mempty = PokeSet [] [] [] []

renderStructMemberPoke :: MarshalScheme StructMember -> WE PokeSet
renderStructMemberPoke ms@MarshalScheme {..} = do
  let StructMember {..} = msParam
      name =
        pretty
          . unReservedWord
          . T.lowerCaseFirst
          . unName
          . msUnmarshalledName
          $ ms
  tellImport "Foreign.Ptr" "plusPtr"
  let offset            = "p `plusPtr`" <+> pretty (show smOffset)
      offsetWithoutType = parens offset
      offsetWithType    = do
        tyDoc' <- toHsTypePrec 10 smType
        pure . parens $ offset <+> "::" <+> "Ptr" <+> tyDoc'
  case msPosition of
    Positive -> pure mempty
    Negative [WrappedPokePure value', SimplePoke] -> do
      value          <- value'
      tyDoc          <- toHsType smType
      offsetWithType <- offsetWithType
      pure $ mempty
        { pokeSetPoke = [ "poke" <+> offsetWithType <+> parens
                            (value <+> "::" <+> tyDoc)
                        ]
        }
    Negative pokes -> flip foldMap pokes $ \case
      SimplePoke -> do
        tellImport "Foreign.Storable" "poke"
        offsetWithType <- offsetWithType
        pure $ mempty { pokeSetPoke = ["poke" <+> offsetWithType <+> name] }
      ComplexContTPoke d -> do
        -- TODO: This "with/without type" is arbitrary, make it selectable by
        -- the scheme
        poke <- d offsetWithoutType
        pure $ mempty { pokeSetContT = [poke] }
      ComplexPoke d -> do
        poke <- d offsetWithoutType
        pure $ mempty { pokeSetPoke = [poke] }
      ContTPoke poke' -> do
        poke <- poke'
        pure $ mempty { pokeSetContT = [poke] }
      IOPoke poke' -> do
        poke <- poke'
        pure $ mempty { pokeSetIO = [poke] }
      WrappedPokeContT wrapFun' -> do
        wrapFun <- wrapFun'
        tellImport "Control.Monad.Trans.Cont" "ContT(..)"
        pure $ mempty { pokeSetContT = [name <+> "<- ContT $" <+> wrapFun] }
      WrappedPokeIO value' -> do
        value <- value'
        pure $ mempty { pokeSetIO = [name <+> "<-" <+> value] }
      WrappedPokePure value' -> do
        value <- value'
        pure $ mempty { pokeSetPure = ["let" <+> name <+> "=" <+> value] }

renderZero :: MarshalScheme StructMember -> Maybe (WE (Doc ()))
renderZero MarshalScheme {..} = case msMarshalledType of
  Elided      -> Nothing
  Present _ z -> Just $ go z
  where
    go = \case
      Zero -> do
        tellDepend (WE.TypeName "Zero")
        pure "zero"
      ZeroNothing -> pure "Nothing"
      ZeroMempty  -> pure "mempty"
      ZeroNullPtr -> do
        tellImport "Foreign.Ptr" "nullPtr"
        pure "nullPtr"
      ZeroTuple n z -> do
        z' <- go z
        pure $ tupled (replicate (fromIntegral n) z')
      ZeroFalse -> pure "False"
      ZeroLeftZero -> pure (parens "Left 0")
