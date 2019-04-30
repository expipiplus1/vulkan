{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.SomeVkStruct
  ( someVkStructWriteElement
  , vkPeekStructWriteElement
  ) where

import           Control.Arrow                            ( (&&&) )
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Writer.Class
import           Data.Traversable
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.Functor.Identity
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Text                                ( Text )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented
import           Control.Monad.State.Strict

import           Spec.Savvy.Error
import           Spec.Savvy.Handle
import           Spec.Savvy.Platform
import           Spec.Savvy.Struct
import           Write.Element                     hiding ( TypeName )
import qualified Write.Element                 as WE
import           Write.Util
import           Write.Monad
import           Write.Marshal.Struct.Utils
import           Write.Marshal.Util

someVkStructWriteElement
  :: (Text -> Maybe Handle)
  -- ^ Get a handle by name
  -> [Platform]
  -- ^ Platform guard info
  -> [Struct]
  -> Write WriteElement
someVkStructWriteElement getHandle platforms structs
  = let
      containsUnion = doesStructContainUnion structs
      containsDispatchableHandle =
        isJust
          . doesStructContainDispatchableHandle (getHandle <=< simpleTypeName)
                                                structs
      guardMap :: Text -> Maybe Text
      guardMap =
        (`Map.lookup` Map.fromList
          ((Spec.Savvy.Platform.pName &&& pProtect) <$> platforms)
        )
      name     = "ToCStruct class declaration"
      go       = do
        tellBootElem someVkStructBootElement
        tellImports
          [ Import "Control.Applicative"   ["(<|>)"]
          , Import "Control.Exception"     ["throwIO"]
          , Import "Data.Type.Equality"    ["(:~:)(Refl)"]
          , Import "Data.Typeable"         ["Typeable", "cast", "eqT"]
          , Import "Foreign.Marshal.Alloc" ["alloca"]
          , Import "Foreign.Ptr"           ["Ptr", "castPtr"]
          , Import "GHC.IO.Exception"
                   ["IOException(..)", "IOErrorType(InvalidArgument)"]
          ]
        traverse_ tellExport
          $
          [ WithConstructors $ WE.TypeName "ToCStruct"
              , WithConstructors $ WE.TypeName "FromCStruct"
              , WithConstructors $ WE.TypeName "SomeVkStruct"
              , WithConstructors $ WE.TypeName "HasNext"
              , Term "SomeVkStruct"
              , Term "fromSomeVkStruct"
              , Term "fromSomeVkStructChain"
              , Term "withSomeVkStruct"
              , Term "withCStructPtr"
              , Term "fromCStructPtr"
              , Term "fromCStructPtrElem"
              ]
        traverse_
          tellExtension
          [ "FunctionalDependencies"
          , "DataKinds"
          , "ExplicitNamespaces"
          , "FlexibleContexts"
          , "GADTs"
          , "LambdaCase"
          , "RankNTypes"
          , "ScopedTypeVariables"
          , "StandaloneDeriving"
          , "TypeApplications"
          , "TypeOperators"
          , "DuplicateRecordFields"
          ]
        instances <-
          traverse (writeSomeStructInstances guardMap containsUnion containsDispatchableHandle) structs
        pure $ \_ -> [qci|
          -- | A class for types which can be marshalled into a C style
          -- structure.
          class ToCStruct marshalled c | marshalled -> c, c -> marshalled where
            -- | Allocates a C type structure and all dependencies and passes
            -- it to a continuation. The space is deallocated when this
            -- continuation returns and the C type structure must not be
            -- returned out of it.
            withCStruct :: marshalled -> (c -> IO a) -> IO a

          -- | A class for converting C type structures to the marshalled types
          class FromCStruct marshalled c | marshalled -> c, c -> marshalled where
            -- | Read a C type structure and dependencies
            fromCStruct :: c -> IO marshalled

          -- | A class for types which can be present in a @pNext@ chain
          class HasNext a where
            getNext :: a -> Maybe SomeVkStruct

          -- | A wrapper for holding any Vulkan struct, this is used to
          -- implement structure @pNext@ chains.
          data SomeVkStruct where
            SomeVkStruct
              :: (ToCStruct a b, Storable b, Show a, Eq a, Typeable a, HasNext a)
              => a
              -> SomeVkStruct

          instance HasNext SomeVkStruct where
            getNext (SomeVkStruct s) = getNext s

          deriving instance Show SomeVkStruct

          instance Eq SomeVkStruct where
            SomeVkStruct (s1 :: s1) == SomeVkStruct (s2 :: s2) = case eqT @s1 @s2 of
              Nothing   -> False
              Just Refl -> s1 == s2

          withCStructPtr :: (Storable c, ToCStruct a c) => a -> (Ptr c -> IO b) -> IO b
          withCStructPtr s f = withCStruct s (\c -> alloca (\p -> poke p c *> f p))

          fromCStructPtr :: (Storable c, FromCStruct a c) => Ptr c -> IO a
          fromCStructPtr p = fromCStruct =<< peek p

          fromCStructPtrElem :: (Storable c, FromCStruct a c) => Ptr c -> Int -> IO a
          fromCStructPtrElem p o = fromCStruct =<< peekElemOff p o

          -- | Convert a 'SomeVkStruct' to a structure of a known type if
          -- possible.
          fromSomeVkStruct :: Typeable a => SomeVkStruct -> Maybe a
          fromSomeVkStruct (SomeVkStruct s) = cast s

          -- | Search the whole pointer chain for a structure of a particular
          -- type and return that if possible
          fromSomeVkStructChain :: Typeable a => SomeVkStruct -> Maybe a
          fromSomeVkStructChain s =
            fromSomeVkStruct s <|> (getNext s >>= fromSomeVkStructChain)

          -- | Allocate space for the value contained in a 'SomeVkStruct' and
          -- use that in continuation.
          withSomeVkStruct :: SomeVkStruct -> (Ptr () -> IO a) -> IO a
          withSomeVkStruct (SomeVkStruct s) f = withCStructPtr s (f . castPtr)

          ----------------------------------------------------------------
          -- Instances
          ----------------------------------------------------------------
          {vcat instances}
        |]
  in runWE name go

writeSomeStructInstances
  :: (Text -> Maybe Text)
  -- ^ guard map
  -> (Text -> Bool)
  -- ^ Does a struct contain a union
  -> (Text -> Bool)
  -- ^ Does a struct contain a dispatchable handle
  -> Struct
  -> WE (Doc ())
writeSomeStructInstances guardMap containsUnion containsDispatchableHandle s@Struct{..}
  = censorGuarded guardMap s $ do
    marshalledName <- case T.dropPrefix "Vk" sName of
                        Nothing -> throwError "Struct without a Vk Prefix"
                        Just x -> pure x
    toCStructDoc <- do
      let withC = "withCStruct" <> marshalledName
      tellDepend (TermName withC)
      tellDepend (WE.TypeName marshalledName)
      tellDepend (WE.TypeName sName)
      pure [qci|
        instance ToCStruct {marshalledName} {sName} where
          withCStruct = {withC}
      |]
    fromCStructDoc <- if
          | containsUnion sName -> pure [qci|
              -- No FromCStruct instance for {sName} as it contains a union type|]
          | containsDispatchableHandle sName -> pure [qci|
              -- No FromCStruct instance for {sName} as it contains a dispatchable handle|]
          | otherwise -> do
              let fromC = "fromCStruct" <> marshalledName
              tellDepend (TermName fromC)
              tellDepend (WE.TypeName marshalledName)
              tellDepend (WE.TypeName sName)
              pure [qci|
                instance FromCStruct {marshalledName} {sName} where
                  fromCStruct = {fromC}
              |]
    hasNextDoc <-
          if any (\case
                     StructMember {smName = "pNext"} -> True
                     _ -> False) sMembers
            then do
              tellDepend (WE.TypeName marshalledName)
              pure [qci|
                instance HasNext {marshalledName} where
                  getNext s = next (s :: {marshalledName})
              |]
            else pure mempty
    pure $ vcat [toCStructDoc, fromCStructDoc, hasNextDoc]

someVkStructBootElement :: WriteElement
someVkStructBootElement =
  let
    weName                 = "SomeVkStruct boot declaration"
    weImports              = [ Unguarded $ Import "Foreign.Ptr"           ["Ptr"] ]
    weProvides             = [ Unguarded (WithoutConstructors $ WE.TypeName "SomeVkStruct")
                             , Unguarded (Term "withSomeVkStruct")
                             ]
    weUndependableProvides = []
    weSourceDepends        = []
    weBootElement          = Nothing
    weDepends              = []
    weExtensions           = []
    weDoc = pure [qci|
      data SomeVkStruct
      instance Show SomeVkStruct
      instance Eq SomeVkStruct

      withSomeVkStruct :: SomeVkStruct -> (Ptr () -> IO a) -> IO a
    |]
  in WriteElement{..}

----------------------------------------------------------------
-- peekVkStruct
----------------------------------------------------------------

vkPeekStructWriteElement
  :: (Text -> Maybe Handle)
  -> [Platform]
  -> [Struct]
  -> Write WriteElement
vkPeekStructWriteElement getHandle platforms structs
  = let
      containsUnion = doesStructContainUnion structs
      containsDispatchableHandle =
        isJust
          . doesStructContainDispatchableHandle (getHandle <=< simpleTypeName)
                                                structs
      guardMap :: Text -> Maybe Text
      guardMap =
        (`Map.lookup` Map.fromList
          ((Spec.Savvy.Platform.pName &&& pProtect) <$> platforms)
        )
      go            = do
        tellBootElem peekBootElement
        tellImports
          [ Import "Data.Typeable"         ["Typeable", "cast", "eqT"]
          , Import "Foreign.Marshal.Alloc" ["alloca"]
          , Import "Foreign.Ptr"           ["Ptr", "castPtr"]
          , Import "Foreign.Storable"      ["Storable", "poke", "peek", "peekElemOff"]
          ]
        tellExport (Term "peekVkStruct")
        tellDepends $
            [ WE.TypeName "SomeVkStruct"
            , WE.TypeName "VkStructureType"
            ]
        tellExtension "LambdaCase"
        peeks <- fmap catMaybes $
          for structs $ \s -> do
            doc <- fmap (indent 4) <$> writeSomeStructPeek guardMap containsUnion containsDispatchableHandle s
            let guard' = (\d -> "defined(" <> d <> ")") <$> (guardMap =<< sPlatform s)
            pure ((, guard') <$> doc)
        pure $ \_ -> [qci|
          -- | Read the @sType@ member of a Vulkan struct and marshal the struct into
          -- a 'SomeVkStruct'
          --
          -- Make sure that you only pass this a pointer to a Vulkan struct with a
          -- @sType@ member at offset 0 otherwise the behaviour is undefined.
          --
          -- - Throws an 'InvalidArgument' 'IOException' if given a pointer to a
          --   struct with an unrecognised @sType@ member.
          -- - Throws an 'InvalidArgument' 'IOException' if given a pointer to a
          --   struct which can't be marshalled (those containing union types)
          peekVkStruct :: Ptr SomeVkStruct -> IO SomeVkStruct
          peekVkStruct p = do
            peek (castPtr p :: Ptr VkStructureType) >>= \case
          {separatedWithGuards "" peeks}
              t -> throwIO (IOError Nothing InvalidArgument "" ("Unknown VkStructureType: " ++ show t) Nothing Nothing)
        |]
    in
      runWE "peekVkStruct declaration" go


writeSomeStructPeek
  :: (Text -> Maybe Text)
  -- ^ guard map
  -> (Text -> Bool)
  -- ^ Does a struct contain a union
  -> (Text -> Bool)
  -- ^ Does a struct contain a dispatchable handle
  -> Struct
  -> WE (Maybe (Doc ()))
writeSomeStructPeek guardMap containsUnion containsDispatchableHandle s@Struct{..}
  = censorGuardedNoCPPF guardMap s $ do
    case sMembers of
      StructMember {smName = "sType", smValues = Just [enum]} : _ ->
        Just <$> if
          | containsUnion sName
          -> do
            tellImport "GHC.IO.Exception" "IOException(..)"
            tellImport "GHC.IO.Exception" "IOErrorType(InvalidArgument)"
            tellImport "Control.Exception" "throwIO"
            tellDepend (PatternName enum)
            pure [qci|
              -- We are not able to marshal this type back into Haskell as we don't know which union component to use
              {enum} -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing unions: " ++ show {enum}) Nothing Nothing)
            |]
          | containsDispatchableHandle sName
          -> do
            tellImport "GHC.IO.Exception" "IOException(..)"
            tellImport "GHC.IO.Exception" "IOErrorType(InvalidArgument)"
            tellImport "Control.Exception" "throwIO"
            tellDepend (PatternName enum)
            pure [qci|
              -- We are not able to marshal this type back into Haskell as we don't have the command table for it
              {enum} -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing dispatchable handles: " ++ show {enum}) Nothing Nothing)
            |]
          | otherwise
          -> do
            let fromC = "fromCStruct" <> T.dropPrefix' "Vk" sName
            tellDepend (WE.TypeName "SomeVkStruct")
            tellDepend (PatternName enum)
            tellDepend (TermName fromC)
            tellImport "Foreign.Storable" "peek"
            tellImport "Foreign.Ptr" "Ptr"
            tellImport "Foreign.Ptr" "castPtr"
            pure [qci|{enum} -> SomeVkStruct <$> ({fromC} =<< peek (castPtr p :: Ptr {sName}))|]
      _ -> pure Nothing

peekBootElement :: WriteElement
peekBootElement =
  let
    weName                 = "peekVkStruct boot declaration"
    weImports              = [ Unguarded $ Import "Foreign.Ptr"           ["Ptr"] ]
    weProvides             = [Unguarded (Term "peekVkStruct")]
    weUndependableProvides = []
    weSourceDepends        = []
    weBootElement          = Nothing
    weDepends              = []
    weExtensions           = []
    weDoc = pure [qci|
      peekVkStruct :: Ptr SomeVkStruct -> IO SomeVkStruct
    |]
  in WriteElement{..}

censorGuarded
  :: (Text -> Maybe Text)
  -- ^ platform to guard
  -> Struct
  -> WE (Doc ())
  -- ^ The doc will be wrapped in guards if the struct is platform specific
  -> WE (Doc ())
censorGuarded gp s = fmap runIdentity . censorGuardedF gp s . fmap Identity

censorGuardedF
  :: Functor f
  => (Text -> Maybe Text)
  -- ^ platform to guard
  -> Struct
  -> WE (f (Doc ()))
  -- ^ The doc will be wrapped in guards if the struct is platform specific
  -> WE (f (Doc ()))
censorGuardedF gp struct@Struct {..} =
  let guardContents = case gp =<< sPlatform of
        Nothing -> id
        Just g  -> guarded (Just g)
  in  fmap (fmap guardContents) . censorGuardedNoCPPF gp struct

censorGuardedNoCPPF
  :: Functor f
  => (Text -> Maybe Text)
  -- ^ platform to guard
  -> Struct
  -> WE (f (Doc ()))
  -- ^ The doc will be wrapped in guards if the struct is platform specific
  -> WE (f (Doc ()))
censorGuardedNoCPPF gp Struct {..} =
  let censorer = case gp =<< sPlatform of
        Nothing -> id
        Just g ->
          let replaceGuards :: [Guarded a] -> [Guarded a]
              replaceGuards = fmap (Guarded (Guard g) . unGuarded)
          in  \we -> we
                { weProvides             = replaceGuards (weProvides we)
                , weUndependableProvides = replaceGuards
                                             (weUndependableProvides we)
                , weDepends              = replaceGuards (weDepends we)
                , weSourceDepends        = replaceGuards (weSourceDepends we)
                }
  in  WE . mapStateT (fmap (fmap censorer)) . unWE
