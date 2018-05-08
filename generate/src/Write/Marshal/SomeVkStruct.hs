{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.SomeVkStruct
  ( someVkStructWriteElement
  , vkPeekStructWriteElement
  ) where

import Control.Monad
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Struct
import           Spec.Savvy.Handle

import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE
import           Write.Marshal.Struct.Utils
import           Write.Marshal.Util

someVkStructWriteElement :: (Text -> Maybe Handle) -> [Struct] -> WriteElement
someVkStructWriteElement getHandle structs =
  let
    containsUnion = doesStructContainUnion structs
    containsDispatchableHandle = isJust . doesStructContainDispatchableHandle (getHandle <=< simpleTypeName) structs
    weName        = "ToCStruct class declaration"
    weImports
      = [ Import "Control.Applicative"       ["(<|>)"]
        , Import "Control.Exception"         ["throwIO"]
        , Import "Data.Type.Equality"        ["(:~:)(Refl)"]
        , Import "Data.Typeable" ["Typeable", "cast", "eqT"]
        , Import "Foreign.Marshal.Alloc" ["alloca"]
        , Import "Foreign.Ptr"           ["Ptr", "castPtr"]
        , Import "GHC.IO.Exception" ["IOException(..)", "IOErrorType(InvalidArgument)"]
        ]
    weProvides =
      Unguarded
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
    weUndependableProvides = []
    weSourceDepends        = []
    weBootElement          = Just someVkStructBootElement
    weDepends =
      [ d
      | Struct{..} <- structs
      , d <- Unguarded <$>
          [ WE.TypeName sName
          , WE.TypeName (T.dropPrefix' "Vk" sName)
          , WE.TermName ("withCStruct" <> T.dropPrefix' "Vk" sName)
          ] ++
          [ WE.TermName ("fromCStruct" <> T.dropPrefix' "Vk" sName)
          | not (containsUnion sName)
          , not (containsDispatchableHandle sName)
          ]
      ]
    weExtensions =
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
    weDoc = pure [qci|
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
      {vcat . mapMaybe (writeSomeStructInstances containsUnion containsDispatchableHandle) $ structs}
    |]
  in WriteElement{..}

writeSomeStructInstances
  :: (Text -> Bool)
  -- ^ Does a struct contain a union
  -> (Text -> Bool)
  -- ^ Does a struct contain a dispatchable handle
  -> Struct
  -> Maybe (Doc ())
writeSomeStructInstances containsUnion containsDispatchableHandle Struct{..}
  = do
    marshalledName <- T.dropPrefix "Vk" sName
    let toCStructDoc = [qci|
          instance ToCStruct {marshalledName} {sName} where
            withCStruct = withCStruct{marshalledName}
          |]
    let fromCStructDoc = if containsUnion sName
          then [qci|-- No FromCStruct instance for {sName} as it contains a union type|]
          else if containsDispatchableHandle sName
            then [qci|-- No FromCStruct instance for {sName} as it contains a dispatchable handle|]
            else [qci|
              instance FromCStruct {marshalledName} {sName} where
                fromCStruct = fromCStruct{marshalledName}
              |]
    let hasPNextDoc =
          if any (\case
                     StructMember {smName = "pNext"} -> True
                     _ -> False) sMembers
            then [qci|
                   instance HasPNext {marshalledName} where
                     getPNext a = vkPNext (a :: {marshalledName})
                 |]
            else mempty
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

vkPeekStructWriteElement :: (Text -> Maybe Handle) -> [Struct] -> WriteElement
vkPeekStructWriteElement getHandle structs =
  let
    containsUnion = doesStructContainUnion structs
    containsDispatchableHandle = isJust . doesStructContainDispatchableHandle (getHandle <=< simpleTypeName) structs

    weName        = "peekVkStruct declaration"
    weImports
      = [ Import "Control.Exception"         ["throwIO"]
        , Import "Data.Typeable" ["Typeable", "cast", "eqT"]
        , Import "Foreign.Marshal.Alloc" ["alloca"]
        , Import "Foreign.Ptr"           ["Ptr", "castPtr"]
        , Import "Foreign.Storable" ["Storable", "poke", "peek", "peekElemOff"]
        , Import "GHC.IO.Exception" ["IOException(..)", "IOErrorType(InvalidArgument)"]
        ]
    weProvides             = [Unguarded (Term "peekVkStruct")]
    weUndependableProvides = []
    weSourceDepends        = []
    weBootElement          = Just peekBootElement
    weDepends =
      [ Unguarded (WE.TypeName "SomeVkStruct")
      , Unguarded (WE.TypeName "VkStructureType")
      ] ++
      [ d
      | Struct{..} <- structs
      , StructMember {smName = "sType", smValues = Just [enum]} : _ <- pure sMembers
      , d <- Unguarded <$>
          [ PatternName enum
          , WE.TypeName sName
          ] ++
          [ WE.TermName ("fromCStruct" <> T.dropPrefix' "Vk" sName)
          | not (containsUnion sName)
          , not (containsDispatchableHandle sName)
          ]
      ]
    weExtensions = ["LambdaCase"]
    weDoc = pure [qci|
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
          {indent 0 . vcat $ mapMaybe (writeSomeStructPeek containsUnion containsDispatchableHandle) $ structs}
          t -> throwIO (IOError Nothing InvalidArgument "" ("Unknown VkStructureType: " ++ show t) Nothing Nothing)
    |]
  in WriteElement{..}

writeSomeStructPeek
  :: (Text -> Bool)
  -- ^ Does a struct contain a union
  -> (Text -> Bool)
  -- ^ Does a struct contain a dispatchable handle
  -> Struct
  -> Maybe (Doc ())
writeSomeStructPeek containsUnion containsDispatchableHandle Struct{..}
  = do
    StructMember {smName = "sType", smValues = Just [enum]} : _ <- pure sMembers
    pure $ if containsUnion sName
      then [qci|
             -- We are not able to marshal this type back into Haskell as we don't know which union component to use
             {enum} -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing unions: " ++ show {enum}) Nothing Nothing)
           |]
      else if containsDispatchableHandle sName
        then [qci|
               -- We are not able to marshal this type back into Haskell as we don't have the command table for it
               {enum} -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing dispatchable handles: " ++ show {enum}) Nothing Nothing)
             |]
        else [qci|{enum} -> SomeVkStruct <$> (fromCStruct{T.dropPrefix' "Vk" sName} =<< peek (castPtr p :: Ptr {sName}))|]

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
