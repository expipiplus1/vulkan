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

import           Control.Arrow                            ((&&&))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Writer.Class
import Data.Traversable
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.Functor.Identity
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Error
import           Spec.Savvy.Handle
import           Spec.Savvy.Platform
import           Spec.Savvy.Struct
import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE
import Write.Util
import           Write.Marshal.Monad
import           Write.Marshal.Struct.Utils
import           Write.Marshal.Util

someVkStructWriteElement
  :: (Text -> Maybe Handle)
  -- ^ Get a handle by name
  -> [Platform]
  -- ^ Platform guard info
  -> [Struct] -> Either [SpecError] WriteElement
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
      weName        = "ToCStruct class declaration"
      weBootElement = Just someVkStructBootElement
      go            = do
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
          $   Unguarded
          <$> [ WithConstructors $ WE.TypeName "ToCStruct"
              , WithConstructors $ WE.TypeName "FromCStruct"
              , WithConstructors $ WE.TypeName "SomeVkStruct"
              , WithConstructors $ WE.TypeName "HasPNext"
              , Term "SomeVkStruct"
              , Term "fromSomeVkStruct"
              , Term "fromSomeVkStructChain"
              , Term "withSomeVkStruct"
              , Term "withCStructPtr"
              , Term "fromCStructPtr"
              , Term "fromCStructPtrElem"
              ]
        -- tellDepends
        --   [ d
        --   | Struct {..} <- structs
        --   , d           <-
        --     Unguarded
        --     <$> [ WE.TypeName sName
        --         , WE.TypeName (T.dropPrefix' "Vk" sName)
        --         , WE.TermName ("withCStruct" <> T.dropPrefix' "Vk" sName)
        --         ]
        --     ++  [ WE.TermName ("fromCStruct" <> T.dropPrefix' "Vk" sName)
        --         | not (containsUnion sName)
        --         , not (containsDispatchableHandle sName)
        --         ]
        --   ]
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
          class ToCStruct marshalled c | marshalled -> c, c -> marshalled where
            withCStruct :: marshalled -> (c -> IO a) -> IO a

          class FromCStruct marshalled c | marshalled -> c, c -> marshalled where
            fromCStruct :: c -> IO marshalled

          class HasPNext a where
            getPNext :: a -> Maybe SomeVkStruct

          data SomeVkStruct where
            SomeVkStruct
              :: (ToCStruct a b, Storable b, Show a, Eq a, Typeable a, HasPNext a)
              => a
              -> SomeVkStruct

          instance HasPNext SomeVkStruct where
            getPNext (SomeVkStruct a) = getPNext a

          deriving instance Show SomeVkStruct

          instance Eq SomeVkStruct where
            SomeVkStruct (a :: a) == SomeVkStruct (b :: b) = case eqT @a @b of
              Nothing   -> False
              Just Refl -> a == b

          withCStructPtr :: (Storable c, ToCStruct a c) => a -> (Ptr c -> IO b) -> IO b
          withCStructPtr a f = withCStruct a (\c -> alloca (\p -> poke p c *> f p))

          fromCStructPtr :: (Storable c, FromCStruct a c) => Ptr c -> IO a
          fromCStructPtr p = fromCStruct =<< peek p

          fromCStructPtrElem :: (Storable c, FromCStruct a c) => Ptr c -> Int -> IO a
          fromCStructPtrElem p o = fromCStruct =<< peekElemOff p o

          fromSomeVkStruct :: Typeable a => SomeVkStruct -> Maybe a
          fromSomeVkStruct (SomeVkStruct a) = cast a

          fromSomeVkStructChain :: Typeable a => SomeVkStruct -> Maybe a
          fromSomeVkStructChain a =
            fromSomeVkStruct a <|> (getPNext a >>= fromSomeVkStructChain)

          withSomeVkStruct :: SomeVkStruct -> (Ptr () -> IO a) -> IO a
          withSomeVkStruct (SomeVkStruct a) f = withCStructPtr a (f . castPtr)

          ----------------------------------------------------------------
          -- Instances
          ----------------------------------------------------------------
          {vcat instances}
        |]
  in wrapMToWriteElements weName weBootElement go

writeSomeStructInstances
  :: (Text -> Maybe Text)
  -- ^ guard map
  -> (Text -> Bool)
  -- ^ Does a struct contain a union
  -> (Text -> Bool)
  -- ^ Does a struct contain a dispatchable handle
  -> Struct
  -> WrapM (Doc ())
writeSomeStructInstances guardMap containsUnion containsDispatchableHandle s@Struct{..}
  = censorGuarded guardMap s $ do
    marshalledName <- case T.dropPrefix "Vk" sName of
                        Nothing -> throwError [Other "Struct without a Vk Prefix"]
                        Just x -> pure x
    toCStructDoc <- do
      let withC = "withCStruct" <> marshalledName
      tellDepend (Unguarded (TermName withC))
      tellDepend (Unguarded (WE.TypeName marshalledName))
      tellDepend (Unguarded (WE.TypeName sName))
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
              tellDepend (Unguarded (TermName fromC))
              tellDepend (Unguarded (WE.TypeName marshalledName))
              tellDepend (Unguarded (WE.TypeName sName))
              pure [qci|
                instance FromCStruct {marshalledName} {sName} where
                  fromCStruct = {fromC}
              |]
    hasPNextDoc <-
          if any (\case
                     StructMember {smName = "pNext"} -> True
                     _ -> False) sMembers
            then do
              tellDepend (Unguarded (WE.TypeName marshalledName))
              pure [qci|
                instance HasPNext {marshalledName} where
                  getPNext a = vkPNext (a :: {marshalledName})
              |]
            else pure mempty
    pure $ vcat [toCStructDoc, fromCStructDoc, hasPNextDoc]

someVkStructBootElement :: WriteElement
someVkStructBootElement =
  let
    weName                 = "SomeVkStruct boot declaration"
    weImports              = [ Import "Foreign.Ptr"           ["Ptr"] ]
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
  -> Either [SpecError] WriteElement
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
      weName        = "peekVkStruct declaration"
      weBootElement = Just peekBootElement
      go            = do
        tellImports
          [ Import "Data.Typeable"         ["Typeable", "cast", "eqT"]
          , Import "Foreign.Marshal.Alloc" ["alloca"]
          , Import "Foreign.Ptr"           ["Ptr", "castPtr"]
          , Import "Foreign.Storable"      ["Storable", "poke", "peek", "peekElemOff"]
          ]
        tellExport (Unguarded (Term "peekVkStruct"))
        tellDepends $
            [ Unguarded (WE.TypeName "SomeVkStruct")
            , Unguarded (WE.TypeName "VkStructureType")
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
      wrapMToWriteElements weName weBootElement go


writeSomeStructPeek
  :: (Text -> Maybe Text)
  -- ^ guard map
  -> (Text -> Bool)
  -- ^ Does a struct contain a union
  -> (Text -> Bool)
  -- ^ Does a struct contain a dispatchable handle
  -> Struct
  -> WrapM (Maybe (Doc ()))
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
            tellDepend (Unguarded (PatternName enum))
            pure [qci|
              -- We are not able to marshal this type back into Haskell as we don't know which union component to use
              {enum} -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing unions: " ++ show {enum}) Nothing Nothing)
            |]
          | containsDispatchableHandle sName
          -> do
            tellImport "GHC.IO.Exception" "IOException(..)"
            tellImport "GHC.IO.Exception" "IOErrorType(InvalidArgument)"
            tellImport "Control.Exception" "throwIO"
            tellDepend (Unguarded (PatternName enum))
            pure [qci|
              -- We are not able to marshal this type back into Haskell as we don't have the command table for it
              {enum} -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing dispatchable handles: " ++ show {enum}) Nothing Nothing)
            |]
          | otherwise
          -> do
            let fromC = "fromCStruct" <> T.dropPrefix' "Vk" sName
            tellDepend (Unguarded (WE.TypeName "SomeVkStruct"))
            tellDepend (Unguarded (PatternName enum))
            tellDepend (Unguarded (TermName fromC))
            tellImport "Foreign.Storable" "peek"
            tellImport "Foreign.Ptr" "Ptr"
            tellImport "Foreign.Ptr" "castPtr"
            pure [qci|{enum} -> SomeVkStruct <$> ({fromC} =<< peek (castPtr p :: Ptr {sName}))|]
      _ -> pure Nothing

peekBootElement :: WriteElement
peekBootElement =
  let
    weName                 = "peekVkStruct boot declaration"
    weImports              = [ Import "Foreign.Ptr"           ["Ptr"] ]
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
  -> WrapM (Doc ())
  -- ^ The doc will be wrapped in guards if the struct is platform specific
  -> WrapM (Doc ())
censorGuarded gp s = fmap runIdentity . censorGuardedF gp s . fmap Identity

censorGuardedF
  :: Functor f
  => (Text -> Maybe Text)
  -- ^ platform to guard
  -> Struct
  -> WrapM (f (Doc ()))
  -- ^ The doc will be wrapped in guards if the struct is platform specific
  -> WrapM (f (Doc ()))
censorGuardedF gp Struct {..}
  = let
      (censorer, guardContents) = case gp =<< sPlatform of
        Nothing -> (id, id)
        Just g
          -> let
               replaceGuards :: [Guarded a] -> [Guarded a]
               replaceGuards = fmap (Guarded g . unGuarded)
               cen =
                 \(a, (exports, undependableExports), (depends, sourceDepends), d, e) ->
                   ( a
                   , (replaceGuards exports, replaceGuards undependableExports)
                   , (replaceGuards depends, replaceGuards sourceDepends)
                   , d
                   , e
                   )
             in
               (cen, guarded (Just g))
    in  fmap (fmap guardContents) . censor censorer

censorGuardedNoCPPF
  :: Functor f
  => (Text -> Maybe Text)
  -- ^ platform to guard
  -> Struct
  -> WrapM (f (Doc ()))
  -- ^ The doc will be wrapped in guards if the struct is platform specific
  -> WrapM (f (Doc ()))
censorGuardedNoCPPF gp Struct {..}
  = let
      censorer = case gp =<< sPlatform of
        Nothing -> id
        Just g ->
          let replaceGuards :: [Guarded a] -> [Guarded a]
              replaceGuards = fmap (Guarded g . unGuarded)
          in  \(a, (exports, undependableExports), (depends, sourceDepends), d, e) ->
                ( a
                , (replaceGuards exports, replaceGuards undependableExports)
                , (replaceGuards depends, replaceGuards sourceDepends)
                , d
                , e
                )
    in  censor censorer
