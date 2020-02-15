module Main
  where

import Relude
import Say

import Spec.Parse

import Text.Show.Pretty

main :: IO ()
main = do
  specText <- readFileBS "./Vulkan-Docs/xml/vk.xml"
  case parseSpec specText of
    Left e -> sayErr e
    Right r -> pPrint r

-- Marshalling C to idiomatic Haskell
