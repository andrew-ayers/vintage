Attribute VB_Name = "Declarations"
'***********************************************************
'*                                                         *
'* Copyright (C) 1999 by Andrew L. Ayers                   *
'*                                                         *
'* This program is free software; you can redistribute it  *
'* and/or modify it under the terms of the GNU General     *
'* Public License as published by the Free Software        *
'* Foundation; either version 2 of the License, or any     *
'* later version.                                          *
'*                                                         *
'* This program is distributed in the hope that it will be *
'* useful, but WITHOUT ANY WARRANTY; without even the      *
'* implied warranty of MERCHANTABILITY or FITNESS FOR A    *
'* PARTICULAR PURPOSE.  See the GNU General Public License *
'* for more details.                                       *
'*                                                         *
'* You should have received a copy of the GNU General      *
'* Public License along with this program; if not, write   *
'* to the Free Software Foundation, Inc. at:               *
'*                                                         *
'*           Free Software Foundation, Inc.                *
'*           59 Temple Place - Suite 330                   *
'*           Boston, MA  02111-1307, USA.                  *
'*                                                         *
'***********************************************************
Public Viewport() As Byte          ' Viewport bitmap array
Public Textures() As Byte          ' Textures bitmap array
'
' Set up array descriptors
'
Public sa1 As SAFEARRAY1D          ' Foreground
Public sa2 As SAFEARRAY1D          ' Textures
'
Public bmp1 As BITMAP
Public bmp2 As BITMAP
'
Public FPS As Long
'
Public Const MAX_WIDTH As Long = 320
Public Const MAX_HEIGHT As Long = 200
Public Const VCX As Long = 160
Public Const VCY As Long = 100
Public Const MAX_AREA As Long = 64000
Public Sub ClearViewport()
  '
  Dim tt As Long
  '
  For tt = 0 To MAX_AREA - 1
    Viewport(tt) = 0
  Next
  '
End Sub

