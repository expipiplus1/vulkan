{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Write.Monad.Lookup
  where

import           Prelude                           hiding ( Enum )
import           Data.Text.Extra                   (Text)
import           Control.Monad.Reader

import           Spec.Savvy.Enum
import           Spec.Savvy.Struct
import           Spec.Savvy.Handle

----------------------------------------------------------------
-- Lookup
----------------------------------------------------------------

data Lookup = Lookup
  { lIsEnum    :: Text -> Maybe Enum
  , lIsBitmask :: Text -> Maybe Enum
  , lIsStruct  ::  Text -> Maybe Struct
  , lIsHandle  ::  Text -> Maybe Handle
  }

emptyLookup :: Lookup
emptyLookup =
  Lookup (const Nothing) (const Nothing) (const Nothing) (const Nothing)

isEnum :: MonadReader Lookup m => Text -> m (Maybe Enum)
isEnum t = ($ t) <$> asks lIsEnum

isBitmask :: MonadReader Lookup m => Text -> m (Maybe Enum)
isBitmask t = ($ t) <$> asks lIsBitmask

isStruct :: MonadReader Lookup m => Text -> m (Maybe Struct)
isStruct t = ($ t) <$>  asks lIsStruct

isHandle :: MonadReader Lookup m => Text -> m (Maybe Handle)
isHandle t = ($ t) <$>  asks lIsHandle
