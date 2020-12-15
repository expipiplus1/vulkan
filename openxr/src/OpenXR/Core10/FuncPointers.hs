{-# language CPP #-}
-- No documentation found for Chapter "FuncPointers"
module OpenXR.Core10.FuncPointers  ( PFN_xrVoidFunction
                                   , FN_xrVoidFunction
                                   ) where

import Foreign.Ptr (FunPtr)

type FN_xrVoidFunction = () -> IO ()
-- | PFN_xrVoidFunction - Generic function pointer type returned by queries
--
-- == Parameter Descriptions
--
-- -   no parameters.
--
-- = Description
--
-- 'PFN_xrVoidFunction' is a generic function pointer type returned by
-- queries, specifically those to
-- 'OpenXR.Core10.Instance.getInstanceProcAddr'.
--
-- = See Also
--
-- 'OpenXR.Core10.Instance.getInstanceProcAddr'
type PFN_xrVoidFunction = FunPtr FN_xrVoidFunction

