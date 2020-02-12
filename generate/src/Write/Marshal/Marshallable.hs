{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Write.Marshal.Marshallable where

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

----------------------------------------------------------------
--
----------------------------------------------------------------

class Marshallable a where
  name    :: a -> Name Unmarshalled
  values  :: Alternative f => a -> f [Text]
  type'   :: a -> Type
  lengths :: Alternative f => a -> f [ParameterLength]
  isOptional :: a -> Maybe [Bool]

instance Marshallable StructMember where
  name       = Name . smName
  values     = maybe empty pure . smValues
  type'      = smType
  lengths    = maybe empty pure . fmap (fmap lengthStringToParamLength) . smLengths
  isOptional = smIsOptional

instance Marshallable Parameter where
  name       = Name . pName
  values     = const empty
  type'      = pType
  lengths    = maybe empty pure . fmap pure . pLength
  isOptional = pIsOptional

newtype ReturnValue = ReturnValue { unReturnValue :: Type }
  deriving (Show)

instance Marshallable ReturnValue where
  name       = error "return value name"
  values     = const empty
  type'      = unReturnValue
  lengths    = const empty
  isOptional = const empty

----------------------------------------------------------------
-- Name
----------------------------------------------------------------

data Namespace
  = Unmarshalled
  | Marshalled

newtype Name (a :: Namespace) = Name {unName :: Text}
  deriving (Eq, Show)

getMarshalledName :: Name Unmarshalled -> Name Marshalled
getMarshalledName (Name n) =
  Name (unReservedWord . T.lowerCaseFirst . dropPointer $ n)

unmarshalledNameDoc :: Marshallable a => a -> Doc ()
unmarshalledNameDoc = pretty . unName . name

marshalledNameDoc :: Marshallable a => a -> Doc ()
marshalledNameDoc = pretty . unName . getMarshalledName . name
