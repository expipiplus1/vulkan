/*
Trimmed down Enum part of https://github.com/KhronosGroup/KTX-Software/blob/master/lib/gl_format.h
*/

/*
================================================================================================

Description	:	OpenGL formats/types and properties.
Author		:	J.M.P. van Waveren
Date		:	07/17/2016
Language	:	C99
Format		:	Real tabs with the tab size equal to 4 spaces.
Copyright	:	Copyright (c) 2016 Oculus VR, LLC. All Rights reserved.


LICENSE
=======

Copyright (c) 2016 Oculus VR, LLC.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.


DESCRIPTION
===========

This header stores the OpenGL formats/types and two simple routines
to derive the format/type from an internal format. These routines
are useful to verify the data in a KTX container files. The OpenGL
constants are generally useful to convert files like KTX and glTF
to different graphics APIs.

This header stores the OpenGL formats/types that are used as parameters
to the following OpenGL functions:

void glTexImage2D( GLenum target, GLint level, GLint internalFormat,
	GLsizei width, GLsizei height, GLint border,
	GLenum format, GLenum type, const GLvoid * data );
void glTexImage3D( GLenum target, GLint level, GLint internalFormat,
	GLsizei width, GLsizei height, GLsizei depth, GLint border,
	GLenum format, GLenum type, const GLvoid * data );
void glCompressedTexImage2D( GLenum target, GLint level, GLenum internalformat,
	GLsizei width, GLsizei height, GLint border,
	GLsizei imageSize, const GLvoid * data );
void glCompressedTexImage3D( GLenum target, GLint level, GLenum internalformat,
	GLsizei width, GLsizei height, GLsizei depth, GLint border,
	GLsizei imageSize, const GLvoid * data );
void glTexStorage2D( GLenum target, GLsizei levels, GLenum internalformat,
	GLsizei width, GLsizei height );
void glTexStorage3D( GLenum target, GLsizei levels, GLenum internalformat,
	GLsizei width, GLsizei height, GLsizei depth );
void glVertexAttribPointer( GLuint index, GLint size, GLenum type, GLboolean normalized,
	GLsizei stride, const GLvoid * pointer);


IMPLEMENTATION
==============

This file does not include OpenGL / OpenGL ES headers because:

  1. Including OpenGL / OpenGL ES headers is platform dependent and
     may require a separate installation of an OpenGL SDK.
  2. The OpenGL format/type constants are the same between extensions and core.
  3. The OpenGL format/type constants are the same between OpenGL and OpenGL ES.
  4. The OpenGL constants in this header are also used to derive Vulkan formats
     from the OpenGL formats/types stored in files like KTX and glTF. These file
     formats may use OpenGL formats/types that are not supported by the OpenGL
     implementation on the platform but are supported by the Vulkan implementation.
*/

#if !defined( GL_INVALID_VALUE )
#define GL_INVALID_VALUE                0x0501
#endif

/*
================================================================================================================================

Format to glTexImage2D and glTexImage3D.

================================================================================================================================
*/

#if !defined( GL_RED )
#define GL_RED                      0x1903
#endif
#if !defined( GL_GREEN )
#define GL_GREEN                    0x1904
#endif
#if !defined( GL_BLUE )
#define GL_BLUE                      0x1905
#endif
#if !defined( GL_ALPHA )
#define GL_ALPHA                    0x1906
#endif
#if !defined( GL_LUMINANCE )
#define GL_LUMINANCE                  0x1909
#endif
#if !defined( GL_SLUMINANCE )
#define GL_SLUMINANCE                  0x8C46
#endif
#if !defined( GL_LUMINANCE_ALPHA )
#define GL_LUMINANCE_ALPHA                0x190A
#endif
#if !defined( GL_SLUMINANCE_ALPHA )
#define GL_SLUMINANCE_ALPHA                0x8C44
#endif
#if !defined( GL_INTENSITY )
#define GL_INTENSITY                  0x8049
#endif
#if !defined( GL_RG )
#define GL_RG                      0x8227
#endif
#if !defined( GL_RGB )
#define GL_RGB                      0x1907
#endif
#if !defined( GL_BGR )
#define GL_BGR                      0x80E0
#endif
#if !defined( GL_RGBA )
#define GL_RGBA                      0x1908
#endif
#if !defined( GL_BGRA )
#define GL_BGRA                      0x80E1
#endif
#if !defined( GL_RED_INTEGER )
#define GL_RED_INTEGER                  0x8D94
#endif
#if !defined( GL_GREEN_INTEGER )
#define GL_GREEN_INTEGER                0x8D95
#endif
#if !defined( GL_BLUE_INTEGER )
#define GL_BLUE_INTEGER                  0x8D96
#endif
#if !defined( GL_ALPHA_INTEGER )
#define GL_ALPHA_INTEGER                0x8D97
#endif
#if !defined( GL_LUMINANCE_INTEGER )
#define GL_LUMINANCE_INTEGER              0x8D9C
#endif
#if !defined( GL_LUMINANCE_ALPHA_INTEGER )
#define GL_LUMINANCE_ALPHA_INTEGER            0x8D9D
#endif
#if !defined( GL_RG_INTEGER )
#define GL_RG_INTEGER                  0x8228
#endif
#if !defined( GL_RGB_INTEGER )
#define GL_RGB_INTEGER                  0x8D98
#endif
#if !defined( GL_BGR_INTEGER )
#define GL_BGR_INTEGER                  0x8D9A
#endif
#if !defined( GL_RGBA_INTEGER )
#define GL_RGBA_INTEGER                  0x8D99
#endif
#if !defined( GL_BGRA_INTEGER )
#define GL_BGRA_INTEGER                  0x8D9B
#endif
#if !defined( GL_COLOR_INDEX )
#define GL_COLOR_INDEX                  0x1900
#endif
#if !defined( GL_STENCIL_INDEX )
#define GL_STENCIL_INDEX                0x1901
#endif
#if !defined( GL_DEPTH_COMPONENT )
#define GL_DEPTH_COMPONENT                0x1902
#endif
#if !defined( GL_DEPTH_STENCIL )
#define GL_DEPTH_STENCIL                0x84F9
#endif

/*
================================================================================================================================

Type to glTexImage2D, glTexImage3D and glVertexAttribPointer.

================================================================================================================================
*/

#if !defined( GL_BYTE )
#define GL_BYTE                      0x1400
#endif
#if !defined( GL_UNSIGNED_BYTE )
#define GL_UNSIGNED_BYTE                0x1401
#endif
#if !defined( GL_SHORT )
#define GL_SHORT                    0x1402
#endif
#if !defined( GL_UNSIGNED_SHORT )
#define GL_UNSIGNED_SHORT                0x1403
#endif
#if !defined( GL_INT )
#define GL_INT                      0x1404
#endif
#if !defined( GL_UNSIGNED_INT )
#define GL_UNSIGNED_INT                  0x1405
#endif
#if !defined( GL_INT64 )
#define GL_INT64                    0x140E
#endif
#if !defined( GL_UNSIGNED_INT64 )
#define GL_UNSIGNED_INT64                0x140F
#endif
#if !defined( GL_HALF_FLOAT )
#define GL_HALF_FLOAT                  0x140B
#endif
#if !defined( GL_HALF_FLOAT_OES )
#define GL_HALF_FLOAT_OES                0x8D61
#endif
#if !defined( GL_FLOAT )
#define GL_FLOAT                    0x1406
#endif
#if !defined( GL_DOUBLE )
#define GL_DOUBLE                    0x140A
#endif
#if !defined( GL_UNSIGNED_BYTE_3_3_2 )
#define GL_UNSIGNED_BYTE_3_3_2              0x8032
#endif
#if !defined( GL_UNSIGNED_BYTE_2_3_3_REV )
#define GL_UNSIGNED_BYTE_2_3_3_REV            0x8362
#endif
#if !defined( GL_UNSIGNED_SHORT_5_6_5 )
#define GL_UNSIGNED_SHORT_5_6_5              0x8363
#endif
#if !defined( GL_UNSIGNED_SHORT_5_6_5_REV )
#define GL_UNSIGNED_SHORT_5_6_5_REV            0x8364
#endif
#if !defined( GL_UNSIGNED_SHORT_4_4_4_4 )
#define GL_UNSIGNED_SHORT_4_4_4_4            0x8033
#endif
#if !defined( GL_UNSIGNED_SHORT_4_4_4_4_REV )
#define GL_UNSIGNED_SHORT_4_4_4_4_REV          0x8365
#endif
#if !defined( GL_UNSIGNED_SHORT_5_5_5_1 )
#define GL_UNSIGNED_SHORT_5_5_5_1            0x8034
#endif
#if !defined( GL_UNSIGNED_SHORT_1_5_5_5_REV )
#define GL_UNSIGNED_SHORT_1_5_5_5_REV          0x8366
#endif
#if !defined( GL_UNSIGNED_INT_8_8_8_8 )
#define GL_UNSIGNED_INT_8_8_8_8              0x8035
#endif
#if !defined( GL_UNSIGNED_INT_8_8_8_8_REV )
#define GL_UNSIGNED_INT_8_8_8_8_REV            0x8367
#endif
#if !defined( GL_UNSIGNED_INT_10_10_10_2 )
#define GL_UNSIGNED_INT_10_10_10_2            0x8036
#endif
#if !defined( GL_UNSIGNED_INT_2_10_10_10_REV )
#define GL_UNSIGNED_INT_2_10_10_10_REV          0x8368
#endif
#if !defined( GL_UNSIGNED_INT_10F_11F_11F_REV )
#define GL_UNSIGNED_INT_10F_11F_11F_REV          0x8C3B
#endif
#if !defined( GL_UNSIGNED_INT_5_9_9_9_REV )
#define GL_UNSIGNED_INT_5_9_9_9_REV            0x8C3E
#endif
#if !defined( GL_UNSIGNED_INT_24_8 )
#define GL_UNSIGNED_INT_24_8              0x84FA
#endif
#if !defined( GL_FLOAT_32_UNSIGNED_INT_24_8_REV )
#define GL_FLOAT_32_UNSIGNED_INT_24_8_REV        0x8DAD
#endif

/*
================================================================================================================================

Internal format to glTexImage2D, glTexImage3D, glCompressedTexImage2D, glCompressedTexImage3D, glTexStorage2D, glTexStorage3D

================================================================================================================================
*/

/*

*/

#if !defined( GL_R8 )
#define GL_R8                      0x8229
#endif
#if !defined( GL_RG8 )
#define GL_RG8                      0x822B
#endif
#if !defined( GL_RGB8 )
#define GL_RGB8                      0x8051
#endif
#if !defined( GL_RGBA8 )
#define GL_RGBA8                    0x8058
#endif

#if !defined( GL_R8_SNORM )
#define GL_R8_SNORM                    0x8F94
#endif
#if !defined( GL_RG8_SNORM )
#define GL_RG8_SNORM                  0x8F95
#endif
#if !defined( GL_RGB8_SNORM )
#define GL_RGB8_SNORM                  0x8F96
#endif
#if !defined( GL_RGBA8_SNORM )
#define GL_RGBA8_SNORM                  0x8F97
#endif

#if !defined( GL_R8UI )
#define GL_R8UI                      0x8232
#endif
#if !defined( GL_RG8UI )
#define GL_RG8UI                    0x8238
#endif
#if !defined( GL_RGB8UI )
#define GL_RGB8UI                    0x8D7D
#endif
#if !defined( GL_RGBA8UI )
#define GL_RGBA8UI                    0x8D7C
#endif

#if !defined( GL_R8I )
#define GL_R8I                      0x8231
#endif
#if !defined( GL_RG8I )
#define GL_RG8I                      0x8237
#endif
#if !defined( GL_RGB8I )
#define GL_RGB8I                    0x8D8F
#endif
#if !defined( GL_RGBA8I )
#define GL_RGBA8I                    0x8D8E
#endif

#if !defined( GL_SR8 )
#define GL_SR8                      0x8FBD
#endif
#if !defined( GL_SRG8 )
#define GL_SRG8                      0x8FBE
#endif
#if !defined( GL_SRGB8 )
#define GL_SRGB8                    0x8C41
#endif
#if !defined( GL_SRGB8_ALPHA8 )
#define GL_SRGB8_ALPHA8                  0x8C43
#endif

/*

*/

#if !defined( GL_R16 )
#define GL_R16                      0x822A
#endif
#if !defined( GL_RG16 )
#define GL_RG16                      0x822C
#endif
#if !defined( GL_RGB16 )
#define GL_RGB16                    0x8054
#endif
#if !defined( GL_RGBA16 )
#define GL_RGBA16                    0x805B
#endif

#if !defined( GL_R16_SNORM )
#define GL_R16_SNORM                  0x8F98
#endif
#if !defined( GL_RG16_SNORM )
#define GL_RG16_SNORM                  0x8F99
#endif
#if !defined( GL_RGB16_SNORM )
#define GL_RGB16_SNORM                  0x8F9A
#endif
#if !defined( GL_RGBA16_SNORM )
#define GL_RGBA16_SNORM                  0x8F9B
#endif

#if !defined( GL_R16UI )
#define GL_R16UI                    0x8234
#endif
#if !defined( GL_RG16UI )
#define GL_RG16UI                    0x823A
#endif
#if !defined( GL_RGB16UI )
#define GL_RGB16UI                    0x8D77
#endif
#if !defined( GL_RGBA16UI )
#define GL_RGBA16UI                    0x8D76
#endif

#if !defined( GL_R16I )
#define GL_R16I                      0x8233
#endif
#if !defined( GL_RG16I )
#define GL_RG16I                    0x8239
#endif
#if !defined( GL_RGB16I )
#define GL_RGB16I                    0x8D89
#endif
#if !defined( GL_RGBA16I )
#define GL_RGBA16I                    0x8D88
#endif

#if !defined( GL_R16F )
#define GL_R16F                      0x822D
#endif
#if !defined( GL_RG16F )
#define GL_RG16F                    0x822F
#endif
#if !defined( GL_RGB16F )
#define GL_RGB16F                    0x881B
#endif
#if !defined( GL_RGBA16F )
#define GL_RGBA16F                    0x881A
#endif

/*

*/

#if !defined( GL_R32UI )
#define GL_R32UI                    0x8236
#endif
#if !defined( GL_RG32UI )
#define GL_RG32UI                    0x823C
#endif
#if !defined( GL_RGB32UI )
#define GL_RGB32UI                    0x8D71
#endif
#if !defined( GL_RGBA32UI )
#define GL_RGBA32UI                    0x8D70
#endif

#if !defined( GL_R32I )
#define GL_R32I                      0x8235
#endif
#if !defined( GL_RG32I )
#define GL_RG32I                    0x823B
#endif
#if !defined( GL_RGB32I )
#define GL_RGB32I                    0x8D83
#endif
#if !defined( GL_RGBA32I )
#define GL_RGBA32I                    0x8D82
#endif

#if !defined( GL_R32F )
#define GL_R32F                      0x822E
#endif
#if !defined( GL_RG32F )
#define GL_RG32F                    0x8230
#endif
#if !defined( GL_RGB32F )
#define GL_RGB32F                    0x8815
#endif
#if !defined( GL_RGBA32F )
#define GL_RGBA32F                    0x8814
#endif

/*

*/

#if !defined( GL_R3_G3_B2 )
#define GL_R3_G3_B2                    0x2A10
#endif
#if !defined( GL_RGB4 )
#define GL_RGB4                      0x804F
#endif
#if !defined( GL_RGB5 )
#define GL_RGB5                      0x8050
#endif
#if !defined( GL_RGB565 )
#define GL_RGB565                    0x8D62
#endif
#if !defined( GL_RGB10 )
#define GL_RGB10                    0x8052
#endif
#if !defined( GL_RGB12 )
#define GL_RGB12                    0x8053
#endif
#if !defined( GL_RGBA2 )
#define GL_RGBA2                    0x8055
#endif
#if !defined( GL_RGBA4 )
#define GL_RGBA4                    0x8056
#endif
#if !defined( GL_RGBA12 )
#define GL_RGBA12                    0x805A
#endif
#if !defined( GL_RGB5_A1 )
#define GL_RGB5_A1                    0x8057
#endif
#if !defined( GL_RGB10_A2 )
#define GL_RGB10_A2                    0x8059
#endif
#if !defined( GL_RGB10_A2UI )
#define GL_RGB10_A2UI                  0x906F
#endif
#if !defined( GL_R11F_G11F_B10F )
#define GL_R11F_G11F_B10F                0x8C3A
#endif
#if !defined( GL_RGB9_E5 )
#define GL_RGB9_E5                    0x8C3D
#endif

/*

*/

#if !defined( GL_ALPHA4 )
#define GL_ALPHA4                    0x803B
#endif
#if !defined( GL_ALPHA8 )
#define GL_ALPHA8                    0x803C
#endif
#if !defined( GL_ALPHA8_SNORM )
#define GL_ALPHA8_SNORM                  0x9014
#endif
#if !defined( GL_ALPHA8UI_EXT )
#define GL_ALPHA8UI_EXT                  0x8D7E
#endif
#if !defined( GL_ALPHA8I_EXT )
#define GL_ALPHA8I_EXT                  0x8D90
#endif
#if !defined( GL_ALPHA12 )
#define GL_ALPHA12                    0x803D
#endif
#if !defined( GL_ALPHA16 )
#define GL_ALPHA16                    0x803E
#endif
#if !defined( GL_ALPHA16_SNORM )
#define GL_ALPHA16_SNORM                0x9018
#endif
#if !defined( GL_ALPHA16UI_EXT )
#define GL_ALPHA16UI_EXT                0x8D78
#endif
#if !defined( GL_ALPHA16I_EXT )
#define GL_ALPHA16I_EXT                  0x8D8A
#endif
#if !defined( GL_ALPHA16F_ARB )
#define GL_ALPHA16F_ARB                  0x881C
#endif
#if !defined( GL_ALPHA32UI_EXT )
#define GL_ALPHA32UI_EXT                0x8D72
#endif
#if !defined( GL_ALPHA32I_EXT )
#define GL_ALPHA32I_EXT                  0x8D84
#endif
#if !defined( GL_ALPHA32F_ARB )
#define GL_ALPHA32F_ARB                  0x8816
#endif

/*

*/

#if !defined( GL_LUMINANCE4 )
#define GL_LUMINANCE4                  0x803F
#endif
#if !defined( GL_LUMINANCE8 )
#define GL_LUMINANCE8                  0x8040
#endif
#if !defined( GL_LUMINANCE8_SNORM )
#define GL_LUMINANCE8_SNORM                0x9015
#endif
#if !defined( GL_SLUMINANCE8 )
#define GL_SLUMINANCE8                  0x8C47
#endif
#if !defined( GL_LUMINANCE8UI_EXT )
#define GL_LUMINANCE8UI_EXT                0x8D80
#endif
#if !defined( GL_LUMINANCE8I_EXT )
#define GL_LUMINANCE8I_EXT                0x8D92
#endif
#if !defined( GL_LUMINANCE12 )
#define GL_LUMINANCE12                  0x8041
#endif
#if !defined( GL_LUMINANCE16 )
#define GL_LUMINANCE16                  0x8042
#endif
#if !defined( GL_LUMINANCE16_SNORM )
#define GL_LUMINANCE16_SNORM              0x9019
#endif
#if !defined( GL_LUMINANCE16UI_EXT )
#define GL_LUMINANCE16UI_EXT              0x8D7A
#endif
#if !defined( GL_LUMINANCE16I_EXT )
#define GL_LUMINANCE16I_EXT                0x8D8C
#endif
#if !defined( GL_LUMINANCE16F_ARB )
#define GL_LUMINANCE16F_ARB                0x881E
#endif
#if !defined( GL_LUMINANCE32UI_EXT )
#define GL_LUMINANCE32UI_EXT              0x8D74
#endif
#if !defined( GL_LUMINANCE32I_EXT )
#define GL_LUMINANCE32I_EXT                0x8D86
#endif
#if !defined( GL_LUMINANCE32F_ARB )
#define GL_LUMINANCE32F_ARB                0x8818
#endif

/*

*/

#if !defined( GL_LUMINANCE4_ALPHA4 )
#define GL_LUMINANCE4_ALPHA4              0x8043
#endif
#if !defined( GL_LUMINANCE6_ALPHA2 )
#define GL_LUMINANCE6_ALPHA2              0x8044
#endif
#if !defined( GL_LUMINANCE8_ALPHA8 )
#define GL_LUMINANCE8_ALPHA8              0x8045
#endif
#if !defined( GL_LUMINANCE8_ALPHA8_SNORM )
#define GL_LUMINANCE8_ALPHA8_SNORM            0x9016
#endif
#if !defined( GL_SLUMINANCE8_ALPHA8 )
#define GL_SLUMINANCE8_ALPHA8              0x8C45
#endif
#if !defined( GL_LUMINANCE_ALPHA8UI_EXT )
#define GL_LUMINANCE_ALPHA8UI_EXT            0x8D81
#endif
#if !defined( GL_LUMINANCE_ALPHA8I_EXT )
#define GL_LUMINANCE_ALPHA8I_EXT            0x8D93
#endif
#if !defined( GL_LUMINANCE12_ALPHA4 )
#define GL_LUMINANCE12_ALPHA4              0x8046
#endif
#if !defined( GL_LUMINANCE12_ALPHA12 )
#define GL_LUMINANCE12_ALPHA12              0x8047
#endif
#if !defined( GL_LUMINANCE16_ALPHA16 )
#define GL_LUMINANCE16_ALPHA16              0x8048
#endif
#if !defined( GL_LUMINANCE16_ALPHA16_SNORM )
#define GL_LUMINANCE16_ALPHA16_SNORM          0x901A
#endif
#if !defined( GL_LUMINANCE_ALPHA16UI_EXT )
#define GL_LUMINANCE_ALPHA16UI_EXT            0x8D7B
#endif
#if !defined( GL_LUMINANCE_ALPHA16I_EXT )
#define GL_LUMINANCE_ALPHA16I_EXT            0x8D8D
#endif
#if !defined( GL_LUMINANCE_ALPHA16F_ARB )
#define GL_LUMINANCE_ALPHA16F_ARB            0x881F
#endif
#if !defined( GL_LUMINANCE_ALPHA32UI_EXT )
#define GL_LUMINANCE_ALPHA32UI_EXT            0x8D75
#endif
#if !defined( GL_LUMINANCE_ALPHA32I_EXT )
#define GL_LUMINANCE_ALPHA32I_EXT            0x8D87
#endif
#if !defined( GL_LUMINANCE_ALPHA32F_ARB )
#define GL_LUMINANCE_ALPHA32F_ARB            0x8819
#endif

/*

*/

#if !defined( GL_INTENSITY4 )
#define GL_INTENSITY4                  0x804A
#endif
#if !defined( GL_INTENSITY8 )
#define GL_INTENSITY8                  0x804B
#endif
#if !defined( GL_INTENSITY8_SNORM )
#define GL_INTENSITY8_SNORM                0x9017
#endif
#if !defined( GL_INTENSITY8UI_EXT )
#define GL_INTENSITY8UI_EXT                0x8D7F
#endif
#if !defined( GL_INTENSITY8I_EXT )
#define GL_INTENSITY8I_EXT                0x8D91
#endif
#if !defined( GL_INTENSITY12 )
#define GL_INTENSITY12                  0x804C
#endif
#if !defined( GL_INTENSITY16 )
#define GL_INTENSITY16                  0x804D
#endif
#if !defined( GL_INTENSITY16_SNORM )
#define GL_INTENSITY16_SNORM              0x901B
#endif
#if !defined( GL_INTENSITY16UI_EXT )
#define GL_INTENSITY16UI_EXT              0x8D79
#endif
#if !defined( GL_INTENSITY16I_EXT )
#define GL_INTENSITY16I_EXT                0x8D8B
#endif
#if !defined( GL_INTENSITY16F_ARB )
#define GL_INTENSITY16F_ARB                0x881D
#endif
#if !defined( GL_INTENSITY32UI_EXT )
#define GL_INTENSITY32UI_EXT              0x8D73
#endif
#if !defined( GL_INTENSITY32I_EXT )
#define GL_INTENSITY32I_EXT                0x8D85
#endif
#if !defined( GL_INTENSITY32F_ARB )
#define GL_INTENSITY32F_ARB                0x8817
#endif

/*

*/

#if !defined( GL_COMPRESSED_RED )
#define GL_COMPRESSED_RED                0x8225
#endif
#if !defined( GL_COMPRESSED_ALPHA )
#define GL_COMPRESSED_ALPHA                0x84E9
#endif
#if !defined( GL_COMPRESSED_LUMINANCE )
#define GL_COMPRESSED_LUMINANCE              0x84EA
#endif
#if !defined( GL_COMPRESSED_SLUMINANCE )
#define GL_COMPRESSED_SLUMINANCE            0x8C4A
#endif
#if !defined( GL_COMPRESSED_LUMINANCE_ALPHA )
#define GL_COMPRESSED_LUMINANCE_ALPHA          0x84EB
#endif
#if !defined( GL_COMPRESSED_SLUMINANCE_ALPHA )
#define GL_COMPRESSED_SLUMINANCE_ALPHA          0x8C4B
#endif
#if !defined( GL_COMPRESSED_INTENSITY )
#define GL_COMPRESSED_INTENSITY              0x84EC
#endif
#if !defined( GL_COMPRESSED_RG )
#define GL_COMPRESSED_RG                0x8226
#endif
#if !defined( GL_COMPRESSED_RGB )
#define GL_COMPRESSED_RGB                0x84ED
#endif
#if !defined( GL_COMPRESSED_RGBA )
#define GL_COMPRESSED_RGBA                0x84EE
#endif
#if !defined( GL_COMPRESSED_SRGB )
#define GL_COMPRESSED_SRGB                0x8C48
#endif
#if !defined( GL_COMPRESSED_SRGB_ALPHA )
#define GL_COMPRESSED_SRGB_ALPHA            0x8C49
#endif

/*

*/

#if !defined( GL_COMPRESSED_RGB_FXT1_3DFX )
#define GL_COMPRESSED_RGB_FXT1_3DFX            0x86B0
#endif
#if !defined( GL_COMPRESSED_RGBA_FXT1_3DFX )
#define GL_COMPRESSED_RGBA_FXT1_3DFX          0x86B1
#endif

/*

*/

#if !defined( GL_COMPRESSED_RGB_S3TC_DXT1_EXT )
#define GL_COMPRESSED_RGB_S3TC_DXT1_EXT          0x83F0
#endif
#if !defined( GL_COMPRESSED_RGBA_S3TC_DXT1_EXT )
#define GL_COMPRESSED_RGBA_S3TC_DXT1_EXT        0x83F1
#endif
#if !defined( GL_COMPRESSED_RGBA_S3TC_DXT3_EXT )
#define GL_COMPRESSED_RGBA_S3TC_DXT3_EXT        0x83F2
#endif
#if !defined( GL_COMPRESSED_RGBA_S3TC_DXT5_EXT )
#define GL_COMPRESSED_RGBA_S3TC_DXT5_EXT        0x83F3
#endif

#if !defined( GL_COMPRESSED_SRGB_S3TC_DXT1_EXT )
#define GL_COMPRESSED_SRGB_S3TC_DXT1_EXT        0x8C4C
#endif
#if !defined( GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT )
#define GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT      0x8C4D
#endif
#if !defined( GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT )
#define GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT      0x8C4E
#endif
#if !defined( GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT )
#define GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT      0x8C4F
#endif

#if !defined( GL_COMPRESSED_LUMINANCE_LATC1_EXT )
#define GL_COMPRESSED_LUMINANCE_LATC1_EXT        0x8C70
#endif
#if !defined( GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT )
#define GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT      0x8C72
#endif
#if !defined( GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT )
#define GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT    0x8C71
#endif
#if !defined( GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT )
#define GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT  0x8C73
#endif

#if !defined( GL_COMPRESSED_RED_RGTC1 )
#define GL_COMPRESSED_RED_RGTC1              0x8DBB
#endif
#if !defined( GL_COMPRESSED_RG_RGTC2 )
#define GL_COMPRESSED_RG_RGTC2              0x8DBD
#endif
#if !defined( GL_COMPRESSED_SIGNED_RED_RGTC1 )
#define GL_COMPRESSED_SIGNED_RED_RGTC1          0x8DBC
#endif
#if !defined( GL_COMPRESSED_SIGNED_RG_RGTC2 )
#define GL_COMPRESSED_SIGNED_RG_RGTC2          0x8DBE
#endif

#if !defined( GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT )
#define GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT        0x8E8E
#endif
#if !defined( GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT )
#define GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT      0x8E8F
#endif
#if !defined( GL_COMPRESSED_RGBA_BPTC_UNORM )
#define GL_COMPRESSED_RGBA_BPTC_UNORM          0x8E8C
#endif
#if !defined( GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM )
#define GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM        0x8E8D
#endif

/*

*/

#if !defined( GL_ETC1_RGB8_OES )
#define GL_ETC1_RGB8_OES                0x8D64
#endif

#if !defined( GL_COMPRESSED_RGB8_ETC2 )
#define GL_COMPRESSED_RGB8_ETC2              0x9274
#endif
#if !defined( GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 )
#define GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2    0x9276
#endif
#if !defined( GL_COMPRESSED_RGBA8_ETC2_EAC )
#define GL_COMPRESSED_RGBA8_ETC2_EAC          0x9278
#endif

#if !defined( GL_COMPRESSED_SRGB8_ETC2 )
#define GL_COMPRESSED_SRGB8_ETC2            0x9275
#endif
#if !defined( GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 )
#define GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2  0x9277
#endif
#if !defined( GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC )
#define GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC        0x9279
#endif

#if !defined( GL_COMPRESSED_R11_EAC )
#define GL_COMPRESSED_R11_EAC              0x9270
#endif
#if !defined( GL_COMPRESSED_RG11_EAC )
#define GL_COMPRESSED_RG11_EAC              0x9272
#endif
#if !defined( GL_COMPRESSED_SIGNED_R11_EAC )
#define GL_COMPRESSED_SIGNED_R11_EAC          0x9271
#endif
#if !defined( GL_COMPRESSED_SIGNED_RG11_EAC )
#define GL_COMPRESSED_SIGNED_RG11_EAC          0x9273
#endif

/*

*/

#if !defined( GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG )
#define GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG        0x8C01
#define GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG        0x8C00
#define GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG        0x8C03
#define GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG        0x8C02
#endif
#if !defined( GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG )
#define GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG        0x9137
#define GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG        0x9138
#endif
#if !defined( GL_COMPRESSED_SRGB_PVRTC_2BPPV1_EXT )
#define GL_COMPRESSED_SRGB_PVRTC_2BPPV1_EXT        0x8A54
#define GL_COMPRESSED_SRGB_PVRTC_4BPPV1_EXT        0x8A55
#define GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV1_EXT    0x8A56
#define GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV1_EXT    0x8A57
#endif
#if !defined( GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV2_IMG )
#define GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV2_IMG    0x93F0
#define GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV2_IMG    0x93F1
#endif

/*

*/

#if !defined( GL_COMPRESSED_RGBA_ASTC_4x4_KHR )
#define GL_COMPRESSED_RGBA_ASTC_4x4_KHR          0x93B0
#define GL_COMPRESSED_RGBA_ASTC_5x4_KHR          0x93B1
#define GL_COMPRESSED_RGBA_ASTC_5x5_KHR          0x93B2
#define GL_COMPRESSED_RGBA_ASTC_6x5_KHR          0x93B3
#define GL_COMPRESSED_RGBA_ASTC_6x6_KHR          0x93B4
#define GL_COMPRESSED_RGBA_ASTC_8x5_KHR          0x93B5
#define GL_COMPRESSED_RGBA_ASTC_8x6_KHR          0x93B6
#define GL_COMPRESSED_RGBA_ASTC_8x8_KHR          0x93B7
#define GL_COMPRESSED_RGBA_ASTC_10x5_KHR        0x93B8
#define GL_COMPRESSED_RGBA_ASTC_10x6_KHR        0x93B9
#define GL_COMPRESSED_RGBA_ASTC_10x8_KHR        0x93BA
#define GL_COMPRESSED_RGBA_ASTC_10x10_KHR        0x93BB
#define GL_COMPRESSED_RGBA_ASTC_12x10_KHR        0x93BC
#define GL_COMPRESSED_RGBA_ASTC_12x12_KHR        0x93BD
#endif

#if !defined( GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR )
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR      0x93D0
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR      0x93D1
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR      0x93D2
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR      0x93D3
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR      0x93D4
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR      0x93D5
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR      0x93D6
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR      0x93D7
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR    0x93D8
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR    0x93D9
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR    0x93DA
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR    0x93DB
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR    0x93DC
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR    0x93DD
#endif

#if !defined( GL_COMPRESSED_RGBA_ASTC_3x3x3_OES )
#define GL_COMPRESSED_RGBA_ASTC_3x3x3_OES        0x93C0
#define GL_COMPRESSED_RGBA_ASTC_4x3x3_OES        0x93C1
#define GL_COMPRESSED_RGBA_ASTC_4x4x3_OES        0x93C2
#define GL_COMPRESSED_RGBA_ASTC_4x4x4_OES        0x93C3
#define GL_COMPRESSED_RGBA_ASTC_5x4x4_OES        0x93C4
#define GL_COMPRESSED_RGBA_ASTC_5x5x4_OES        0x93C5
#define GL_COMPRESSED_RGBA_ASTC_5x5x5_OES        0x93C6
#define GL_COMPRESSED_RGBA_ASTC_6x5x5_OES        0x93C7
#define GL_COMPRESSED_RGBA_ASTC_6x6x5_OES        0x93C8
#define GL_COMPRESSED_RGBA_ASTC_6x6x6_OES        0x93C9
#endif

#if !defined( GL_COMPRESSED_SRGB8_ALPHA8_ASTC_3x3x3_OES )
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_3x3x3_OES    0x93E0
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x3x3_OES    0x93E1
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4x3_OES    0x93E2
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4x4_OES    0x93E3
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4x4_OES    0x93E4
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5x4_OES    0x93E5
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5x5_OES    0x93E6
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5x5_OES    0x93E7
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6x5_OES    0x93E8
#define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6x6_OES    0x93E9
#endif

/*

*/

#if !defined( GL_ATC_RGB_AMD )
#define GL_ATC_RGB_AMD                  0x8C92
#define GL_ATC_RGBA_EXPLICIT_ALPHA_AMD          0x8C93
#define GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD        0x87EE
#endif

/*

*/

#if !defined( GL_PALETTE4_RGB8_OES )
#define GL_PALETTE4_RGB8_OES              0x8B90
#define GL_PALETTE4_RGBA8_OES              0x8B91
#define GL_PALETTE4_R5_G6_B5_OES            0x8B92
#define GL_PALETTE4_RGBA4_OES              0x8B93
#define GL_PALETTE4_RGB5_A1_OES              0x8B94
#define GL_PALETTE8_RGB8_OES              0x8B95
#define GL_PALETTE8_RGBA8_OES              0x8B96
#define GL_PALETTE8_R5_G6_B5_OES            0x8B97
#define GL_PALETTE8_RGBA4_OES              0x8B98
#define GL_PALETTE8_RGB5_A1_OES              0x8B99
#endif

/*

*/

#if !defined( GL_COLOR_INDEX1_EXT )
#define GL_COLOR_INDEX1_EXT                0x80E2
#define GL_COLOR_INDEX2_EXT                0x80E3
#define GL_COLOR_INDEX4_EXT                0x80E4
#define GL_COLOR_INDEX8_EXT                0x80E5
#define GL_COLOR_INDEX12_EXT              0x80E6
#define GL_COLOR_INDEX16_EXT              0x80E7
#endif

/*

*/

#if !defined( GL_DEPTH_COMPONENT16 )
#define GL_DEPTH_COMPONENT16              0x81A5
#endif
#if !defined( GL_DEPTH_COMPONENT24 )
#define GL_DEPTH_COMPONENT24              0x81A6
#endif
#if !defined( GL_DEPTH_COMPONENT32 )
#define GL_DEPTH_COMPONENT32              0x81A7
#endif
#if !defined( GL_DEPTH_COMPONENT32F )
#define GL_DEPTH_COMPONENT32F              0x8CAC
#endif
#if !defined( GL_DEPTH_COMPONENT32F_NV )
#define GL_DEPTH_COMPONENT32F_NV            0x8DAB
#endif
#if !defined( GL_STENCIL_INDEX1 )
#define GL_STENCIL_INDEX1                0x8D46
#endif
#if !defined( GL_STENCIL_INDEX4 )
#define GL_STENCIL_INDEX4                0x8D47
#endif
#if !defined( GL_STENCIL_INDEX8 )
#define GL_STENCIL_INDEX8                0x8D48
#endif
#if !defined( GL_STENCIL_INDEX16 )
#define GL_STENCIL_INDEX16                0x8D49
#endif
#if !defined( GL_DEPTH24_STENCIL8 )
#define GL_DEPTH24_STENCIL8                0x88F0
#endif
#if !defined( GL_DEPTH32F_STENCIL8 )
#define GL_DEPTH32F_STENCIL8              0x8CAD
#endif
#if !defined( GL_DEPTH32F_STENCIL8_NV )
#define GL_DEPTH32F_STENCIL8_NV              0x8DAC
#endif
