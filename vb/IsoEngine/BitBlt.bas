Attribute VB_Name = "BitBlit"
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
Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long
Declare Function StretchBlt Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal nSrcWidth As Long, ByVal nSrcHeight As Long, ByVal dwRop As Long) As Long
'
Public Const SRCAND = &H8800C6
Public Const SRCCOPY = &HCC0020
Public Const SRCERASE = &H440328
Public Const SRCINVERT = &H660046
Public Const SRCPAINT = &HEE0086
Public Sub PlaceSprite(ToForm As Object, FromPic As PictureBox, from_x As Long, from_y As Long, to_x As Long, to_y As Long, from_wid As Long, from_hgt As Long)
  '
  Dim From_hDC As Long, To_hDC As Long, res As Long
  '
  ' Set up device contexts
  '
  From_hDC = FromPic.hdc: To_hDC = ToForm.hdc
  '
  ' Place the sprite
  '
  res = BitBlt(To_hDC, to_x, to_y, from_wid, from_hgt, From_hDC, from_x, from_y + from_hgt, SRCAND)
  res = BitBlt(To_hDC, to_x, to_y, from_wid, from_hgt, From_hDC, from_x, from_y, SRCPAINT)
  '
End Sub
Public Sub FlipBuffer(from_ob As Object, to_ob As Object)
  '
  Dim From_hDC As Long, To_hDC As Long, res As Long
  '
  ' Set up device contexts
  '
  From_hDC = from_ob.hdc: To_hDC = to_ob.hdc
  '
  ' Place the sprite
  '
  res = BitBlt(To_hDC, 0, 0, from_ob.ScaleWidth, from_ob.ScaleHeight, From_hDC, 0, 0, SRCCOPY)
  '
End Sub

