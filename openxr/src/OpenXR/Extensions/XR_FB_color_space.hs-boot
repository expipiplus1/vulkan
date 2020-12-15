{-# language CPP #-}
-- | = Name
--
-- XR_FB_color_space - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_FB_color_space  XR_FB_color_space>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 109
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'ColorSpaceFB', 'SystemColorSpacePropertiesFB',
-- 'enumerateColorSpacesFB', 'setColorSpaceFB'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_FB_color_space OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_FB_color_space  ( SystemColorSpacePropertiesFB
                                            , ColorSpaceFB
                                            ) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data SystemColorSpacePropertiesFB

instance ToCStruct SystemColorSpacePropertiesFB
instance Show SystemColorSpacePropertiesFB

instance FromCStruct SystemColorSpacePropertiesFB


data ColorSpaceFB

