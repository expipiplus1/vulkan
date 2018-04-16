{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Savvy.Type.Packing
  ( typeSize
  , typeAlignment
  ) where

import           Control.Monad.Except
import           Data.Int
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import           Spec.Savvy.Error
import           Spec.Savvy.Type

-- | Get the size of a type in bytes
typeSize
  :: (Text -> Maybe Type)
  -> (Text -> Maybe Word)
  -- ^ Lookup a constant value
  -> (Type -> Either [SpecError] Word)
  -- ^ Lookup the size of a sub type
  -> Type
  -> Either [SpecError] Word
typeSize namedType constantValue lookupSub = \case
  Float -> pure $ fromIntegral (sizeOf (undefined :: CFloat))
  Char -> pure $ fromIntegral (sizeOf (undefined :: CChar))
  Int -> pure $ fromIntegral (sizeOf (undefined :: CInt))
  Ptr _ -> pure $ fromIntegral (sizeOf (undefined :: Ptr ()))
  Array (NumericArraySize len) t -> do
    tSize <- lookupSub t
    pure (len * tSize)
  Array (SymbolicArraySize n) t -> do
    tSize <- lookupSub t
    len <- maybe (throwError [UnknownConstantValue n]) pure (constantValue n)
    pure (len * tSize)
  TypeName n
    | Just (SomeStorable x) <- knownNames n
    -> pure $ fromIntegral (sizeOf x)
    | Just t <- namedType n
    -> lookupSub t
    | otherwise
    -> throwError [UnknownTypeSize n]
  t -> throwError [SizingBadType (T.pack (show t))]

-- | Get the alignment of a type in bytes
typeAlignment
  :: (Text -> Maybe Type)
  -> (Type -> Either [SpecError] Word)
  -> Type
  -> Either [SpecError] Word
typeAlignment namedType lookupSub = \case
  Float     -> pure $ fromIntegral (alignment (undefined :: CFloat))
  Char      -> pure $ fromIntegral (alignment (undefined :: CChar))
  Int       -> pure $ fromIntegral (alignment (undefined :: CInt))
  Ptr _     -> pure $ fromIntegral (alignment (undefined :: Ptr ()))
  Array _ t -> lookupSub t
  TypeName n
    | Just (SomeStorable x) <- knownNames n -> pure $ fromIntegral (alignment x)
    | Just t <- namedType n -> lookupSub t
    | otherwise -> throwError [UnknownTypeAlignment n]
  t -> throwError [AligningBadType (T.pack (show t))]

knownNames :: Text -> Maybe SomeStorable
knownNames = \case
  "uint8_t"      -> Just (SomeStorable (0 :: Word8))
  "uint32_t"     -> Just (SomeStorable (0 :: Word32))
  "uint64_t"     -> Just (SomeStorable (0 :: Word64))
  "int32_t"      -> Just (SomeStorable (0 :: Int32))
  "size_t"       -> Just (SomeStorable (0 :: CSize))
  -- Windows
  "HWND"         -> Just (SomeStorable (nullPtr :: Ptr ()))
  "HINSTANCE"    -> Just (SomeStorable (nullPtr :: Ptr ()))
  "HANDLE"       -> Just (SomeStorable (nullPtr :: Ptr ()))
  "LPCWSTR"      -> Just (SomeStorable (nullPtr :: Ptr Word16))
  "DWORD"        -> Just (SomeStorable (0 :: Word32))
  -- Xlib
  "Window"       -> Just (SomeStorable (0 :: Word32))
  -- xcb
  "xcb_window_t" -> Just (SomeStorable (0 :: Word32))
  _              -> Nothing

data SomeStorable where
  SomeStorable :: Storable a => a -> SomeStorable
