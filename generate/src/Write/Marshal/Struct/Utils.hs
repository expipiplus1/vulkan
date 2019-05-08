{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Struct.Utils
  ( vkStructWriteElement
  , doesStructContainUnion
  , doesStructContainDispatchableHandle
  ) where

import           Control.Monad
import           Data.Closure
import           Data.Function
import           Data.Functor
import           Data.List.Extra
import qualified Data.Map                                 as Map
import           Data.Maybe
import qualified Data.MultiMap                            as MultiMap
import qualified Data.Set                                 as Set
import           Data.Text                                (Text)
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Handle
import           Spec.Savvy.Struct
import           Spec.Savvy.Type

import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE

vkStructWriteElement :: WriteElement
vkStructWriteElement =
  let
    weName        = "ToCStruct class declaration"
    weImports
      = Unguarded <$>
        [ Import "Data.Proxy"                ["Proxy(Proxy)"]
        , Import "Data.Word"                 ["Word8"]
        , Import "Foreign.C.Types"           ["CChar(..)"]
        , Import "Data.ByteString" ["ByteString", "take", "unpack", "packCString"]
        , Import "Data.ByteString.Unsafe" ["unsafeUseAsCString"]
        , QualifiedImport "Data.ByteString" ["length"]
        , Import "Data.Vector"           ["Vector", "ifoldr"]
        , Import "Foreign.Marshal.Array" ["allocaArray"]
        , Import "Foreign.Marshal.Utils" ["copyBytes"]
        , Import "Foreign.Ptr"           ["Ptr", "castPtr"]
        , Import "Foreign.Storable" ["Storable", "pokeElemOff", "peekElemOff"]
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
        <$> [
              Term "withVec"
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

      pokeFixedLengthNullTerminatedByteString :: Int -> Ptr CChar -> ByteString -> IO ()
      pokeFixedLengthNullTerminatedByteString maxLength to bs =
        unsafeUseAsCString bs $ \from -> do
          let len = min maxLength (Data.ByteString.length bs)
              end = min (maxLength - 1) len
          -- Copy the entire string into the buffer
          copyBytes to from len
          -- Make the last byte (the one following the string, or the
          -- one at the end of the buffer)
          pokeElemOff to end 0

      pokeFixedLengthByteString :: Int -> Ptr Word8 -> ByteString -> IO ()
      pokeFixedLengthByteString maxLength to bs =
        unsafeUseAsCString bs $ \from -> do
          let len = min maxLength (Data.ByteString.length bs)
          copyBytes to (castPtr @CChar @Word8 from) len

      sameLength :: [Int] -> [Maybe Int] -> IO Int
      sameLength required optional = _
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

-- | Returns the names of all structs containing unions
doesStructContainDispatchableHandle
  :: (Type -> Maybe Handle) -> [Struct] -> Text -> Maybe Handle
doesStructContainDispatchableHandle getHandle structs =
  let dispatchableHandleNames = nubOrd
        [ hName h
        | WE.TypeName t <- nubOrd
          (typeDepends . smType =<< sMembers =<< structs)
        , Just        h <- pure (getHandle (TypeName t))
        , Dispatchable  <- pure (hHandleType h)
        ]

      -- The list of struct names which contain this type
      contains :: Text -> [Text]
      contains = (`MultiMap.lookup` m)
        where
          m = MultiMap.fromList
            [ (containee, sName container)
            | container             <- structs
            , WE.TypeName containee <-
              typeDepends . smType =<< sMembers container
            ]

      structsWithDispatchableHandles = Map.fromList
        [ (s, n)
        | h      <- dispatchableHandleNames
        , s      <- closeNonReflexiveL contains [h]
        , Just n <- pure $ getHandle (TypeName h)
        ]
  in  (`Map.lookup` structsWithDispatchableHandles)
