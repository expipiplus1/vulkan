{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Bespoke
  ( bespokeWriteElements
  ) where

import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Write.Element

bespokeWriteElements :: [WriteElement]
bespokeWriteElements = [versions, nullHandle, aNativeWindow]

versions :: WriteElement
versions =
  let weDoc = [qci|
        pattern VK_MAKE_VERSION :: Word32 -> Word32 -> Word32 -> Word32
        pattern VK_MAKE_VERSION major minor patch <- ((== VK_MAKE_VERSION major minor patch) -> True)
          where VK_MAKE_VERSION major minor patch = major `shiftL` 22 .||. minor `shiftL` 12 .||. patch

        pattern VK_API_VERSION_1_0 :: Word32
        pattern VK_API_VERSION_1_0 = VK_MAKE_VERSION 1 0 0

        pattern VK_API_VERSION_1_1 :: Word32
        pattern VK_API_VERSION_1_1 = VK_MAKE_VERSION 1 1 0

        pattern VK_VERSION_MAJOR :: Word32 -> Word32
        pattern VK_VERSION_MAJOR v <- ((== VK_VERSION_MAJOR v) -> True)
          where VK_VERSION_MAJOR v = v `shiftR` 22

        pattern VK_VERSION_MINOR :: Word32 -> Word32
        pattern VK_VERSION_MINOR v <- ((== VK_VERSION_MINOR v) -> True)
          where VK_VERSION_MINOR v = v `shiftR` 12 .&&. 0x3ff

        pattern VK_VERSION_PATCH :: Word32 -> Word32
        pattern VK_VERSION_PATCH v <- ((== VK_VERSION_PATCH v) -> True)
          where VK_VERSION_PATCH v = v .&&. 0xfff

|]
      weImports = [ Import "Data.Word" ["Word32"]
                  , Import "Data.Bits" ["(.&&.)", "(.||.)", "shiftL", "shiftR"]
                  ]
      weExtensions = ["PatternSynonyms", "ViewPatterns"]
      weName = "Version Macros"
      weProvides = Pattern <$> [ "VK_MAKE_VERSION"
                               , "VK_API_VERSION_1_0"
                               , "VK_API_VERSION_1_1"
                               , "VK_VERSION_MAJOR"
                               , "VK_VERSION_MINOR"
                               , "VK_VERSION_PATCH"
                               ]
      weDepends = []
  in WriteElement{..}

nullHandle :: WriteElement
nullHandle =
  let weDoc = [qci|
        pattern VK_NULL_HANDLE :: Ptr a
        pattern VK_NULL_HANDLE <- ((== nullPtr) -> True)
          where VK_NULL_HANDLE = nullPtr
|]
      weImports = [Import "Foreign.Ptr" ["Ptr", "nullPtr"]]
      weExtensions = ["PatternSynonyms", "ViewPatterns"]
      weName = "Null handle"
      weProvides = [Pattern "VK_NULL_HANDLE"]
      weDepends = []
  in WriteElement{..}

aNativeWindow :: WriteElement
aNativeWindow =
  let weDoc = [qci|
        data ANativeWindow
|]
      weImports = []
      weExtensions = []
      weName = "ANativeWindow"
      weProvides = [Type "ANativeWindow"]
      weDepends = []
  in WriteElement{..}
