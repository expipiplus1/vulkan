{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Write.WriteMonad
  ( Write
  , WriteOutput
  , ReadInput
  , RequiredName(..)
  , WildCard(..)
  , FileType(..)
  , runWrite
  , askTypeEnv
  , isBoot
  , tellRequiredName
  , tellRequiredNames
  , tellExtension
  , requireStorable
  , doesDeriveStorable
  ) where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Hashable
import           Data.HashSet         as S
import           GHC.Generics         (Generic)
import           Spec.TypeEnv
import           Write.Utils

data RequiredName = ExternalName ModuleName String
                  | InternalName WildCard String
  deriving(Eq, Generic)

data WildCard = WildCard
              | NoWildCard
  deriving(Eq, Generic)

instance Hashable WildCard

instance Hashable RequiredName

data FileType = Normal
              | Boot
  deriving(Eq)

type ExtensionName = String

type WriteOutput = (HashSet RequiredName, HashSet ExtensionName)

data ReadInput = ReadInput { readTypeEnv  :: TypeEnv
                           , readFileType :: FileType
                           }

type Write = ReaderT ReadInput (Writer WriteOutput)

runWrite :: TypeEnv -> FileType -> Write a
         -> (a, (HashSet RequiredName, HashSet ExtensionName))
runWrite env boot m = runWriter (runReaderT m (ReadInput env boot))

askTypeEnv :: Write TypeEnv
askTypeEnv = asks readTypeEnv

isBoot :: Write Bool
isBoot = asks ((== Boot) . readFileType)

doesDeriveStorable :: Write ()
doesDeriveStorable =
  requireStorable >> tellExtension "GeneralizedNewtypeDeriving"

requireStorable :: Write ()
requireStorable = tellRequiredName (ExternalName (ModuleName "Foreign.Storable") "Storable(..)")

tellRequiredName :: MonadWriter WriteOutput m  => RequiredName -> m ()
tellRequiredName rn = tell (S.singleton rn, mempty)

tellRequiredNames :: [RequiredName] -> Write ()
tellRequiredNames rns = tell (S.fromList rns, mempty)

tellExtension :: ExtensionName -> Write ()
tellExtension en = tell (mempty, S.singleton en)

