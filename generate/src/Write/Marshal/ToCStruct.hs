{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.ToCStruct
  ( toCStructWriteElement
  ) where

import           Control.Arrow                            ( (&&&) )
import           Control.Monad
import           Control.Monad.Except
import           Data.Traversable
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.Functor.Identity
import           Data.List                                ( intersperse )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Text                                ( Text )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented
import           Control.Monad.State.Strict

import           Spec.Savvy.Handle
import           Spec.Savvy.Platform
import           Spec.Savvy.Struct
import           Write.Element                     hiding ( TypeName )
import qualified Write.Element                 as WE
import           Write.Util
import           Write.Monad
import           Write.Marshal.Struct.Utils
import           Write.Marshal.Util
import           Write.Marshal.Scheme
import           Write.Marshal.ToC

toCStructWriteElement
  :: [(Struct, [MarshalScheme StructMember])]
  -> Write WriteElement
toCStructWriteElement ss =
  runWE "ToCStruct class and instances" $ do
  c <- classDoc
  instances <- traverse (uncurry instanceDoc) ss
  pure $ \_ -> vcat (intersperse line (c : instances))

classDoc :: WE (Doc ())
classDoc = do
  tellExport $ WithConstructors (WE.TypeName "ToCStruct")
  traverse_ tellExtension ["FunctionalDependencies", "DefaultSignatures"]
  tellImport "Foreign.Marshal.Alloc" "alloca"
  tellImport "Foreign.Storable"      "Storable"
  tellImport "Foreign.Ptr"           "Ptr"
  pure [qci|
    -- | A class for types which can be marshalled into a C style
    -- structure.
    class ToCStruct marshalled c | marshalled -> c, c -> marshalled where
      -- | Allocates a C type structure and all dependencies and passes
      -- it to a continuation. The space is deallocated when this
      -- continuation returns and the C type structure must not be
      -- returned out of it.
      withCStruct :: marshalled -> (Ptr c -> IO a) -> IO a
      default withCStruct :: Storable c => marshalled -> (Ptr c -> IO a) -> IO a
      withCStruct x f = alloca $ \p -> pokeCStruct p x (f p)

      -- | Write a C type struct into some existing memory and run a
      -- continuation. The pointed to structure is not necessarily valid
      -- outside the continuation as additional allocations may have been
      -- made.
      pokeCStruct :: Ptr c -> marshalled -> IO a -> IO a
  |]

instanceDoc :: Struct -> [MarshalScheme StructMember] -> WE (Doc ())
instanceDoc s schemes = do
  let marshalledName = dropVkType (sName s)
  tellDepend (WE.TypeName ("Vk" <> marshalledName))
  tellDepend (WE.TypeName marshalledName)
  case sStructOrUnion s of
    AStruct -> do
      tellImport "Control.Monad.IO.Class"   "liftIO"
      tellImport "Control.Monad.Trans.Cont" "ContT(..)"
      tellImport "Foreign.Ptr"              "Ptr"
      tellExtension "RecordWildCards"
      tellExtension "InstanceSigs"
      tellSourceDepend (WE.TypeName "ToCStruct")
      PokeSet {..} <- foldMap renderStructMemberPoke schemes
      let ioPokes = pokeSetIO <> pokeSetPure <> pokeSetPoke
          io      = if null ioPokes
            then mempty
            else "liftIO $ do" <> line <> indent 2 (vsep $ ioPokes)
      pure [qci|
        instance ToCStruct {marshalledName} Vk{marshalledName} where
          pokeCStruct :: Ptr Vk{marshalledName} -> {marshalledName} -> IO a -> IO a
          pokeCStruct p {marshalledName}\{..} = (. const) . runContT ${if null pokeSetContT then "" else " do" <> line}{indent 4 . vsep $ pokeSetContT}
        {indent 4 io}
        |]
    AUnion -> do
      let renderMember scheme@MarshalScheme {..} = do
            PokeSet {..} <- renderStructMemberPoke scheme
            let n       = T.upperCaseFirst $ smName msParam
                v       = smName msParam
                ioPokes = pokeSetIO <> pokeSetPure <> pokeSetPoke
                io      = if null ioPokes
                  then mempty
                  else "liftIO $ do" <> line <> indent 2 (vsep $ ioPokes)
            pure
              $   pretty n
              <+> pretty v
              <+> "->"
              <+> (if null pokeSetContT then "" else " do" <> line)
              <>  (indent 4 . vsep $ pokeSetContT)
              <>  line
              <>  (indent 2 io)
      rs <- traverse renderMember schemes
      pure [qci|
        instance ToCStruct {marshalledName} Vk{marshalledName} where
          pokeCStruct :: Ptr Vk{marshalledName} -> {marshalledName} -> IO a -> IO a
          pokeCStruct p u = (. const) . runContT $ case u of
        {indent 4 $ vsep rs}
      |]
