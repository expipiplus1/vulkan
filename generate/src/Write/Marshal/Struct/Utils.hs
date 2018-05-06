{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Struct.Utils
  ( vkStructWriteElement
  , vkPeekStructWriteElement
  , doesStructContainUnion
  ) where

import           Control.Monad
import           Data.Closure
import           Data.Function
import           Data.Functor
import           Data.Maybe
import qualified Data.MultiMap                            as MultiMap
import qualified Data.Set                                 as Set
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Struct
import           Spec.Savvy.Type

import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE

vkStructWriteElement :: [Struct] -> WriteElement
vkStructWriteElement structs =
  let
    weName        = "ToCStruct class declaration"
    weImports
      = [ Import "Control.Applicative"       ["(<|>)"]
        , Import "Control.Exception"         ["throwIO"]
        , Import "Data.Proxy"                ["Proxy(Proxy)"]
        , Import "Data.Type.Equality"        ["(:~:)(Refl)"]
        , Import "Data.Word"                 ["Word8"]
        , Import "Foreign.C.Types"           ["CChar(..)"]
        , Import "Data.ByteString" ["ByteString", "take", "unpack", "packCString"]
        , Import "Data.Typeable" ["Typeable", "cast", "eqT"]
        , Import "Data.Vector"           ["Vector", "ifoldr"]
        , Import "Foreign.Marshal.Alloc" ["alloca"]
        , Import "Foreign.Marshal.Array" ["allocaArray"]
        , Import "Foreign.Ptr"           ["Ptr", "castPtr"]
        , Import "Foreign.Storable" ["Storable", "poke", "pokeElemOff", "peek", "peekElemOff"]
        , Import "GHC.IO.Exception" ["IOException(..)", "IOErrorType(InvalidArgument)"]
        , Import "GHC.TypeNats" ["natVal", "KnownNat", "type (<=)"]
        , QualifiedImport "Data.Vector"           ["length"]
        , QualifiedImport "Data.Vector.Generic" ["length", "take", "replicate", "fromList", "Vector", "(++)"]
        , QualifiedImport "Data.Vector.Generic" ["snoc", "empty"]
        , QualifiedImport "Data.Vector.Generic.Sized" ["Vector", "fromSized"]
        , QualifiedImport "Data.Vector.Sized" ["Vector"]
        , QualifiedImport "Data.Vector.Generic.Sized.Internal" ["Vector(Vector)"]
        ]
    weProvides =
      Unguarded
        <$> [ WithConstructors $ WE.TypeName "ToCStruct"
            , WithConstructors $ WE.TypeName "FromCStruct"
            , WithConstructors $ WE.TypeName "SomeVkStruct"
            , WithConstructors $ WE.TypeName "HasPNext"
            , Term "SomeVkStruct"
            , Term "withCStructPtr"
            , Term "fromCStructPtr"
            , Term "fromCStructPtrElem"
            , Term "fromSomeVkStruct"
            , Term "fromSomeVkStructChain"
            , Term "withSomeVkStruct"
            , Term "withVec"
            , Term "withArray"
            , Term "withSizedArray"
            , Term "byteStringToSizedVector"
            , Term "byteStringToNullTerminatedSizedVector"
            , Term "padSized"
            , Term "padVector"
            , Term "packCStringElemOff"
            ]
    weUndependableProvides = [Unguarded (Term "peekVkStruct")]
    weSourceDepends        = [Unguarded (TermName "peekVkStruct")]
    weBootElement          = Nothing
    weDepends = []
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

      fromSomeVkStruct :: Typeable a => SomeVkStruct -> Maybe a
      fromSomeVkStruct (SomeVkStruct a) = cast a

      fromSomeVkStructChain :: Typeable a => SomeVkStruct -> Maybe a
      fromSomeVkStructChain a =
        fromSomeVkStruct a <|> (getPNext a >>= fromSomeVkStructChain)

      withSomeVkStruct :: SomeVkStruct -> (Ptr () -> IO a) -> IO a
      withSomeVkStruct (SomeVkStruct a) f = withCStructPtr a (f . castPtr)

      withCStructPtr :: (Storable c, ToCStruct a c) => a -> (Ptr c -> IO b) -> IO b
      withCStructPtr a f = withCStruct a (\c -> alloca (\p -> poke p c *> f p))

      fromCStructPtr :: (Storable c, FromCStruct a c) => Ptr c -> IO a
      fromCStructPtr p = fromCStruct =<< peek p

      fromCStructPtrElem :: (Storable c, FromCStruct a c) => Ptr c -> Int -> IO a
      fromCStructPtrElem p o = fromCStruct =<< peekElemOff p o

      packCStringElemOff :: Ptr (Ptr CChar) -> Int -> IO ByteString
      packCStringElemOff p o = packCString =<< peekElemOff p o

      withArray
        :: forall a b d
         . (a -> (b -> IO d) -> IO d)
        -> Vector a
        -> (Vector b -> IO d)
        -> IO d
      withArray alloc v cont =
        let go :: a -> (Vector b -> IO d) -> (Vector b -> IO d)
            go x complete bs = alloc x (\b -> complete (Data.Vector.Generic.snoc bs b))
        in  foldr go cont v (Data.Vector.Generic.empty)

      withSizedArray
        :: forall a b d n
         . (a -> (b -> IO d) -> IO d)
        -> Data.Vector.Sized.Vector n a
        -> (Data.Vector.Sized.Vector n b -> IO d)
        -> IO d
      withSizedArray alloc v cont = withArray
        alloc
        (Data.Vector.Generic.Sized.fromSized v)
        (cont . Data.Vector.Generic.Sized.Internal.Vector)

      withVec
        :: forall a b d
         . Storable b
        => (a -> (b -> IO d) -> IO d)
        -> Vector a
        -> (Ptr b -> IO d)
        -> IO d
      withVec alloc v cont = allocaArray (Data.Vector.length v) $ \p ->
        let go :: Int -> a -> IO d -> IO d
            go index x complete = alloc x (\b -> pokeElemOff p index b *> complete)
        in  ifoldr go (cont p) v

      -- | Pad or truncate a vector so that it has the required size
      padSized
        :: forall n a v
         . (KnownNat n, Data.Vector.Generic.Vector v a)
        => a
        -- ^ The value with which to pad if the given vector is too short
        -> v a
        -- ^ The vector to pad or truncate
        -> Data.Vector.Generic.Sized.Vector v n a
      padSized p v = Data.Vector.Generic.Sized.Internal.Vector padded
        where
          padded :: v a
          padded = let n = fromIntegral (natVal (Proxy @n))
                   in padVector p n v

      -- | Make sure a vector is at least a certain length
      padVector
        :: (Data.Vector.Generic.Vector v a)
        => a
        -> Int
        -> v a
        -> v a
      padVector p n v =
        let m = Data.Vector.Generic.length v
        in case m `compare` n of
             LT -> v Data.Vector.Generic.++ (Data.Vector.Generic.replicate (n - m) p)
             EQ -> v
             GT -> Data.Vector.Generic.take n v

      -- | Convert a bytestring to a null terminated sized vector. If the bytestring
      -- is too long it will be truncated.
      byteStringToNullTerminatedSizedVector
        :: forall n v
         . (KnownNat n, 1 <= n, Data.Vector.Generic.Vector v CChar)
        => ByteString
        -> Data.Vector.Generic.Sized.Vector v n CChar
      byteStringToNullTerminatedSizedVector bs = padSized
        (CChar 0)
        (byteStringToVector (Data.ByteString.take predN bs))
        where
          predN = pred (fromIntegral (natVal (Proxy @n)))
          byteStringToVector =
            Data.Vector.Generic.fromList . fmap fromIntegral . Data.ByteString.unpack

      -- | Convert a bytestring to a sized vector. If the bytestring is too
      -- long it will be truncated. If it is too short it will be zero padded
      byteStringToSizedVector
        :: forall n v
         . (KnownNat n, Data.Vector.Generic.Vector v Word8)
        => ByteString
        -> Data.Vector.Generic.Sized.Vector v n Word8
      byteStringToSizedVector bs = padSized
        0
        (byteStringToVector (Data.ByteString.take n bs))
        where
          n                  = fromIntegral (natVal (Proxy @n))
          byteStringToVector = Data.Vector.Generic.fromList . Data.ByteString.unpack

    |]
  in WriteElement{..}

-- | Returns the names of all structs containing unions
doesStructContainUnion :: [Struct] -> Text -> Bool
doesStructContainUnion structs =
  let
    unionNames = [ sName s | s <- structs, sStructOrUnion s == AUnion ]

    -- The list of struct names which contain this type
    contains :: Text -> [Text]
    contains = (`MultiMap.lookup` m)
      where
        m = MultiMap.fromList
          [ (containee, sName container)
          | container <- structs
          , WE.TypeName containee <- typeDepends . smType =<< sMembers container
          ]

    structWithUnions = closeL contains unionNames
  in (`Set.member` Set.fromList structWithUnions)

----------------------------------------------------------------
-- peekVkStruct
----------------------------------------------------------------

vkPeekStructWriteElement :: [Struct] -> WriteElement
vkPeekStructWriteElement structs =
  let
    containsUnion = doesStructContainUnion structs

    weName        = "peekVkStruct declaration"
    weImports
      = [ Import "Control.Exception"         ["throwIO"]
        , Import "Data.Typeable" ["Typeable", "cast", "eqT"]
        , Import "Foreign.Marshal.Alloc" ["alloca"]
        , Import "Foreign.Marshal.Array" ["allocaArray"]
        , Import "Foreign.Ptr"           ["Ptr", "castPtr"]
        , Import "Foreign.Storable" ["Storable", "poke", "pokeElemOff", "peek", "peekElemOff"]
        , Import "GHC.IO.Exception" ["IOException(..)", "IOErrorType(InvalidArgument)"]
        ]
    weProvides = [Unguarded (Term "peekVkStruct")]
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
          , WE.TermName ("fromCStruct" <> T.dropPrefix' "Vk" sName)
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
          {indent 0 . vcat $ mapMaybe (writeSomeStructPeek containsUnion) $ structs}
          t -> throwIO (IOError Nothing InvalidArgument "" ("Unknown VkStructureType: " ++ show t) Nothing Nothing)
    |]
  in WriteElement{..}

writeSomeStructPeek
  :: (Text -> Bool)
  -- ^ Does a struct contain a union
  -> Struct
  -> Maybe (Doc ())
writeSomeStructPeek containsUnion Struct{..}
  = do
    StructMember {smName = "sType", smValues = Just [enum]} : _ <- pure sMembers
    pure $ if containsUnion sName
      then [qci|
             -- We are not able to marshal this type back into Haskell as we don't know which union component to use
             {enum} -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing unions: " ++ show {enum}) Nothing Nothing)
           |]
      else [qci|{enum} -> SomeVkStruct <$> (fromCStruct{T.dropPrefix' "Vk" sName} =<< peek (castPtr p :: Ptr {sName}))|]

peekBootElement :: WriteElement
peekBootElement =
  let
    weName                 = "peekVkStruct boot declaration"
    weImports              = [ Import "Foreign.Ptr"           ["Ptr"] ]
    weProvides             = [Unguarded (Term "peekVkStruct")]
    weUndependableProvides = []
    weSourceDepends        = [Unguarded (WE.TypeName "SomeVkStruct")]
    weBootElement          = Nothing
    weDepends              = []
    weExtensions           = []
    weDoc = pure [qci|
      peekVkStruct :: Ptr SomeVkStruct -> IO SomeVkStruct
    |]
  in WriteElement{..}
