{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Shader
  ( VkShaderModuleCreateFlags(..)
  , VkShaderModule
  , vkCreateShaderModule
  , vkDestroyShaderModule
  , VkShaderModuleCreateInfo(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )


-- ** VkShaderModuleCreateFlags

-- | VkShaderModuleCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkShaderModuleCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkShaderModuleCreateInfo'
newtype VkShaderModuleCreateFlags = VkShaderModuleCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkShaderModuleCreateFlags where
  
  showsPrec p (VkShaderModuleCreateFlags x) = showParen (p >= 11) (showString "VkShaderModuleCreateFlags " . showsPrec 11 x)

instance Read VkShaderModuleCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkShaderModuleCreateFlags")
                        v <- step readPrec
                        pure (VkShaderModuleCreateFlags v)
                        )
                    )


-- | Dummy data to tag the 'Ptr' with
data VkShaderModule_T
-- | VkShaderModule - Opaque handle to a shader module object
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'vkCreateShaderModule', 'vkDestroyShaderModule'
type VkShaderModule = Ptr VkShaderModule_T
-- | vkCreateShaderModule - Creates a new shader module object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that creates the shader module.
--
-- -   @pCreateInfo@ parameter is a pointer to an instance of the
--     @VkShaderModuleCreateInfo@ structure.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- -   @pShaderModule@ points to a @VkShaderModule@ handle in which the
--     resulting shader module object is returned.
--
-- = Description
-- #_description#
--
-- Once a shader module has been created, any entry points it contains
-- /can/ be used in pipeline shader stages as described in
-- <{html_spec_relative}#pipelines-compute Compute Pipelines> and
-- <{html_spec_relative}#pipelines-graphics Graphics Pipelines>.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkShaderModuleCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pShaderModule@ /must/ be a valid pointer to a @VkShaderModule@
--     handle
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_INVALID_SHADER_NV@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkShaderModule', 'VkShaderModuleCreateInfo'
foreign import ccall "vkCreateShaderModule" vkCreateShaderModule :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkShaderModuleCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pShaderModule" ::: Ptr VkShaderModule) -> IO VkResult
-- | vkDestroyShaderModule - Destroy a shader module module
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that destroys the shader module.
--
-- -   @shaderModule@ is the handle of the shader module to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- = Description
-- #_description#
--
-- A shader module /can/ be destroyed while pipelines created using its
-- shaders are still in use.
--
-- == Valid Usage
--
-- -   If @VkAllocationCallbacks@ were provided when @shaderModule@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @shaderModule@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @shaderModule@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @shaderModule@
--     /must/ be a valid @VkShaderModule@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @shaderModule@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @shaderModule@ /must/ be externally synchronized
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkShaderModule'
foreign import ccall "vkDestroyShaderModule" vkDestroyShaderModule :: ("device" ::: VkDevice) -> ("shaderModule" ::: VkShaderModule) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | VkShaderModuleCreateInfo - Structure specifying parameters of a newly
-- created shader module
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @codeSize@ /must/ be greater than 0
--
-- -   @codeSize@ /must/ be a multiple of 4
--
-- -   @pCode@ /must/ point to valid SPIR-V code, formatted and packed as
--     described by the
--     <{html_spec_relative}#spirv-spec Khronos SPIR-V Specification>
--
-- -   @pCode@ /must/ adhere to the validation rules described by the
--     <{html_spec_relative}#spirvenv-module-validation Validation Rules within a Module>
--     section of the
--     <{html_spec_relative}#spirvenv-capabilities SPIR-V Environment>
--     appendix
--
-- -   @pCode@ /must/ declare the @Shader@ capability for SPIR-V code
--
-- -   @pCode@ /must/ not declare any capability that is not supported by
--     the API, as described by the
--     <{html_spec_relative}#spirvenv-module-validation Capabilities>
--     section of the
--     <{html_spec_relative}#spirvenv-capabilities SPIR-V Environment>
--     appendix
--
-- -   If @pCode@ declares any of the capabilities listed as /optional/ in
--     the
--     <{html_spec_relative}#spirvenv-capabilities-table SPIR-V Environment>
--     appendix, the corresponding feature(s) /must/ be enabled.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.VkShaderModuleValidationCacheCreateInfoEXT'
--
-- -   @flags@ /must/ be @0@
--
-- -   @pCode@ /must/ be a valid pointer to an array of
--     <<data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAARoAAABCCAYAAACIGRmUAAAABmJLR0QA/wD/AP+gvaeTAAAQE0lEQVR4nO2dabAVxRWAv/vgyQMeIiqLEBWMhpSARiUxatwNGKMmISbErayUS1yiJiJquYFRy7iVIZaaRaMxRdRE44IakxhBIVEQBDcUUYKioj5EHiCKPt7Nj3On7kxPz70zPT0zb+mvqkvfcPvMmZ6Znu7T55wGh8PhCDIG+BvwGVDWlLXAxUBjUQo6HI7OSwNwGdCOdCjvApcC44GDgcmVY16Hc3+ljsPhcMSiB3Av1U7kUaCf5nfDgA98vzs9LwUdDkfn53dUO495QK8av73Q99s3slfN4XB0Bb5HteNoA0bV+f0+BG02w7NUzuFwdH6agHeodhp3xqizI8GO5vB6FZwhx+Ho3hwDDPX9fVOMOupqU1O9Cq6jcTi6N6f4/v9NYG6MOsOUv8v1KvRMopEjE64Ctqnx763A2Tnp4uhebAl81ff3VsCrMeqpq1Hv1KvgOppi2QyYRG3Hp5k56dKR6AGMBHYGBgD9kVWQdYiz2NvAksp/635NLdAfMZj2Be4D3svhnHmwH8FZTTPS7klZbkUbR2bsgd7z0l+uKUy7fNkCOBF4HPiE+u1SBj4GZgFXAocBfTLQa1dgpe+cK5AXsiswmWB7fq1YdRx58SXCL9PEQjXKnt7ARchIJU7nUqt8AjwCHG1Jtwbgdc15vm9JftHcSPC6tsjqRG7q1LHYQ3NsQe5a5MfOwEPAF5Xjr1SOL0SmR2uR6WU/ZGl1LPADxKbgpwkZ2awH7rKg3xiNbgAfWpDd0WgD1hSthCMfriP4hVkDlArVKDv2JzyKeR44KGb9zZC4nE2ERxznWdJxBNW4H6/Mpeus1l5O9bo2Jqh3DnA38OsslHJkzyyCD/UThWqTHTsAqwhe64PINCopFxHuaA62oyYgQYWfIV/8+4BBFmUXzREE263W6qfHSKpR3T/PTjVHVpSQpWz/jb+2UI2y4z8Er/NlzA25jcD7irwBFnT004sYTmmdkGZgNdV2O6X2z+kDPEt1ZOdML50QnSH4R4VqlA2HE77OI1PKvMMn6/WUsrob/pWn94DtIn43DJhd+d2bwLa5aOewztGEX8CdCtUoG/5B8BpbSG/zuMQn756UsrobJcRw7rXfauAKxKj+DcTo/hvEh6mMRHZHdUaOTsD1dH1DcF/gU4LXea8Fuf60BbYMwd2JBuAswnYzf1kM/ATD6ZKbY3Uc1KXt55Ab3JXYm3Cek7csyJ2NjGoAHrAgr7vRjqwg3YyMYkYirgMbkYx6C4DX0pzARkfTiMRLbAcMBDZHli3XIMq9hHhw5oGqSzPi8/A+4pNh46GOS1/kxdoWGAx8jty0+YRvWgnYTTmWl/9MH2BPJIJ3MNKGLZUyD2k7W+imgja8eWdXiiMdbcjK56xi1ahSQmI/ZlCdu0WVTchLczFi8MxClwnAwzF0WYQMEbMcye2DfFU31NBjAfBNX528DcE9gOOAfxGeyvhLO9JBn1apk5apmnMsoWP4pQyi9rPjLcHndS6TksX7VRiHI6MU3YVuJDpzuvfgzsBeTMURdXTROXOVgRcQr0+bDAL+GnG+qLY4s1L3GM2/Z2UInoC83DqdNhB2UPO3mc5LNgkXR8i+MKVcGxxG/Xt2qaVzfSvGuZKWVjqwTS/Jl70X4rn6U+X4DCQr15NUjUlbIl/2Mwl+uUtIR7UUGZabEqXLgz5dPkSubwSyonMeMp0B6WQeR6JXl6TQw2MXxGV+e+X46srxF5G22Qpxn/9ORZdpiC1Gtc+sxf4ybRNwA3Cq79jnwO3IqsM8pKNpRFI5Hk9w9DcGcSDcF/Mp6LKI41ciX+PzsTtVS8p03/8PBg5R/t3WdNZ/v+9COve4DATGaY5fgbx7nZpm4L8Ee9AW9BesciXh3vfYlLqoDl8tBDs0HbsQdohbiriyp2Es4SnbRsQ9PsrJ6wvA05XfziHsETwzpU4q/YBnlHO8isQa1eIgwiPUx1LoMRTp3KK+yhuQpVTVXlUEJxHWb4gl2fdX5K1IWG9LJExD1WuKJb0KpTfhF+F96j+kHg2EpzdfNtSlCfmq+mV9QP1kyh6nE75J5xjqAmJ09qcQ8F6WQ2PUHUR124qNiozrUuik0kT4/r2CPLRxuJZwm+2bQh9/tv1aZTHyAtme4sblFkWfusmdEvBWRWYSm09/ZCFBbadfWtSrUKYTvLB24o1k/Pj9HNZhbvy7U6NLnJfaoy9hI+27hrr0QH/jk4zWvOGuWmylOQD4kyJ7AxIBHZeRGv1uTqFPX8Kjq3plKXA1wWxwWTNP0eEhS3IH+mTGtfnoZhRlZOrdJfgu4Yv7vYEcf/DWU4a6HKnR5TYDOf/WyDH5ap6tkXN3Qhl7aWSUsbd6oGuzCxLK0MVgvZBSr2bENpSks/GPxiaTbfKpRsKrcVMsyT7UJ/PbMX6vm1GUkZFhhzX+JqEP4WnBRszcj/fzybjBUBf/VpxlxHYw3EDWHYRvWtLd9rYm/PK1kWykAMGvm1dsrR7o2qwFM78Vb6jvlU0W9ANJFeHFzyQtLcAZlvRQ+YrmfHE6hTg0I8/tcOrvXd0L+KdGlz9i9oxM1ciyXaYmVeoMjZDpNWtEMxDx2zgOs5ykOtuKaWKjaRpZl9SsEeYCjYykoxmQjGaqnJkGcnTo2uxyQ1lqR1MmvRHdz26IZ6p/q9W45SFq76poQpaG4Lg0Iiu6qh73YO7TNFUjr9COpgHxXlWFHGZ4gWkooff7MP3C3KaRdXWC+g1IMmZVRt1NtDQM0cixYQiOajMTI3wJvW+Ubl/mtPRARjnTkAjhuA+3SSdfiywNwXFQ98H2ygOkczadqpGZS0cTpfRehB3GWpFhXN7sRdhm0YpEAZvQX3PsswT19yXsL2OqzwjNMRu+GnsTbrONJLfPgHxZ1SF+G9mElWxCfKCeRGxgYxFnxqOpPaKYCPwWe6NB1a8pz3SqDciih5qX+DHgh0jbdzqiOhrdqtIcirnI8ZpjaXTRGVqXJ6iv89eZg/iH2NDFxkOtu3+9gBMsyAbx/0jiZGbK/Eo5HwnJuJRoO9ix2OloGhGfKz95dTQlZLHlGOX4E0jIT5IPYqdAt5RWVPi9TV0a0G/lkST1o+doZ0OfGxQ5a7FjCNa1mc1ia+SQlCbgDxE6LbR0Dp0h2GRabMJNmnPPoerR3qUokf5l7Ki6jNbI2kR857US+iDEuAm1VZ5S5MwylOMnqs32tyC7I1BCRhjq9dkK2ThRIzsPQ7Caj6iM+PJsnsO5C2Eo+i+Gut9uZ9RFt5IWZ69hj20j9Blaq1IEPQmHLlxvIEclqs3yXjXJkvMIX5+t6c3Nitw8DMG6MJ2F2M99XBg6G832mmMfk7/lHfS6fIK5LgdqjiUx4u6gObYBM+/isYSdzmy8LLo2W09x27gOQa4VZCUzVQKlCis1x/5nQS7kbwi+hHD0+mLEFvhRxufODV0ogM4noaiNpXS6tBrK6kd4eb4dsfDHRefsttZQH51Reb6hLD8d6f6BRIHPqJQk4SK1UDeOAzuJr3qSryF4EvAL5dhSxDSwKsPz5o5uRLNecyyvDHkqNnWZQHjfoAdJNrfXrSx9aqjP8crf65CHLC26NjPVEWREMtr393ySdVz+EYKtFBC7a4793YLc0YQj7rPqaM4g7DO1HLH3FTX6zJURhOeLpoGHlyE98ypkSXRrC7q0GOhRQuJz/HLakRSWSdhTo4/JCGucRs4sAzk6hmtkm7SZh5fSoIx4CCf1wl3qq//jFHp4eKlG/de3yIJc0BuC42yqlpSTCCcYW4Her0rHnsBRSF6jrGkieA+t5kl6m2AjtFE/LkNlIMF4INNIU1WXMsnjdSZqZJhs59kHfT6VJB1oA9XE47YNwR66kAGT1Yv9CL4QJyWs31+pbxoC4ec0wtd2lAW5kI8h+DjCmR9XkiyQ1ossf966dmFUQ7XVjuZ2wjdTzTZWD396iZXo59WmuiTZcGwI4RiapZj7JuiWVk9IUP8yTf0yYSetNOjCLJLmIB6APFRe/Zkk9/E5UNHhJQMZfnYiuLNiGVk1tBXFPFeRbSs1hMdRyEdbHW3GzacEwchyk+wFSdiZcPiJ1Y5mDOFeN8n+O5N89dpJ5/Ck0yXufLwf4vCkfj2SRln70QUrLiZekOHJROfkNQk2jWIU4Qf6aeIH4/VDwgC8uh9gFrV/LuHrPMtADoj9ZJkiaxX6VTYTehL2P5piSTbIx1F9aVcjDoJJ8DsUJs06kIQS+sh66zuB6jKhnVynTi8kQNFfJ00Gu1q61HtgdyScvChJNr4oeiPGOlWf+4geJQ1EYnG8h0uNyrXlEexHnQaUgV9RP+nYbkiOY69OK3rjaxz8OyB6ZRPyAquG+Sj6I6PAjxU5q0mX6U9lV42utjyCxxN29GzFLEm/346U1MaYhFMq51gDPOI7p1FHU+vh7g08ChygHJ+OvDSLkJWSRiQqeDwyf/Z8TdqQkY2JLSSuLvcgbtsLkdWWzZEHZiJiT/AbLmcj05O3LehzEBJgqo4QViGRxK8gL8ZgJCh0HGLfWYO00zUEPXU/IjhMb0c69TR5X3ohW9CoU96ngRuR+BnPqDoI+DoSLzTBd13LKn+b2gJeI3o3hw+RUfJc5OFtRb74zUjHPBrpSMYRNkAvQQyhaRLLTyKY8Gw7wn5WDxA09k8jeajD/sgIXO1YF2HWrrsjerchI880K4pRDEJySg9AVsd2Rzo4gDdINyPQ0oy8zLqhfplwrluvvIwYEvPUJWqbl1VIUJ6NfYn8nIDe1T+qLEBeOl3GOrXY2JkBpHNTU7H6SxvhKVYZMXjfguTLScMoJOewmoDLtKwCfoadXDgrDM5vsgXOwwbniVNsrbTp+HPlHM8gI+Bbfee1PnXycwjiQRvVsZSRr/BTyAuY5QZtcXRpQ76U55JtysfRyEhL97J65TlkWdebsug2i1OLaYKxKA6o6Flrs7gy8vJdi/2NyHogW61OQUaW6jSoVmkB/oKkTYg73aqHyQZupnuhq1kqbZWsDMGe68XnyOwALHQ0SRuuH5LrZBtkeNuA3IA3SO7IlRZVF++hfA+xzeTpvj0EmSINQ0YB6xHnq2cpJnQjCm+b3qHIy9YDGV2tRKYEb+akRwmZqoys6NKvUsrIdHwtch9fRB9u4MiG3sjK4A6IM+HkyvFbyXjq5HA4ug9XIZ39coILG7lNnRwOR9dmNFU7p7ralrqj6QibqzscjmIpISvJjYibxsPFquNwOLoipyKjlbXocz25qZPD4UjFEGThpAycGfEb19E4HI5U3I10IM8SbUpxHY3D4TDG2563jdphJs4Y7HA4jOiDxMOBhKQ8V6AuDoeji+IFP79Ffe/53D2DHQ5H12AlYgheh0TC12Irqp1RG2Fv9xeokyMqy5gkh8PRcfEGGV74R1x6Es4B1KUSqTscjmJwxmCHw9HxcR2Nw+HIHNfROByOzHEdjcPhyBy3vO1wOFSmE9xhdAiSJB4k894y5fejSJff2uFwdEPULYrqlbpuMv8HtK+/BrL8EjoAAAAASUVORK5CYII= stem 3c191c7a3fc8d91b8c1ec0b877b089a5>>
--     @uint32_t@ values
--
-- = See Also
-- #_see_also#
--
-- 'VkShaderModuleCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateShaderModule'
data VkShaderModuleCreateInfo = VkShaderModuleCreateInfo
  { -- No documentation found for Nested "VkShaderModuleCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkShaderModuleCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkShaderModuleCreateInfo" "vkFlags"
  vkFlags :: VkShaderModuleCreateFlags
  , -- No documentation found for Nested "VkShaderModuleCreateInfo" "vkCodeSize"
  vkCodeSize :: CSize
  , -- No documentation found for Nested "VkShaderModuleCreateInfo" "vkPCode"
  vkPCode :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkShaderModuleCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkShaderModuleCreateInfo <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkCodeSize (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPCode (poked :: VkShaderModuleCreateInfo))
