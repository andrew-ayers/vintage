Attribute VB_Name = "PerspTriModule"
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
Private Type Vertex
  '
  u As Single
  v As Single
  '
  sx As Long
  sy As Long
  sz As Single
  su As Single
  sv As Single
  '
End Type
Public Type Triangle
  '
  v(2) As Vertex
  '
End Type
Public Sub DrawPerspTriangle(poly As Triangle)
  '
  Dim tp As Long, a As Long, b As Long, i As Long, na As Long, nb As Long
  '
  tp = 0
  '
  Dim dx_A As Single, dx_B As Single
  Dim du_A As Single, du_B As Single
  Dim dv_A As Single, dv_B As Single
  Dim dz_A As Single, dz_B As Single
  '
  For i = 1 To 2
    '
    If poly.v(i).sy < poly.v(tp).sy Then tp = i
    '
  Next
  '
  a = tp + 1
  b = tp - 1
  If a > 2 Then a = 0
  If b < 0 Then b = 2
  '
  Dim y As Long
  Dim x1 As Single, x2 As Single
  Dim u1 As Single, u2 As Single
  Dim v1 As Single, v2 As Single
  Dim z1 As Single, z2 As Single
  '
  y = CLng(poly.v(tp).sy)
  x1 = poly.v(tp).sx
  x2 = x1
  u1 = poly.v(tp).su
  u2 = u1
  v1 = poly.v(tp).sv
  v2 = v1
  z1 = poly.v(tp).sz
  z2 = z1
  '
  Dim height_A As Long, height_B As Long
  '
  height_A = CLng(poly.v(a).sy - poly.v(tp).sy)
  height_B = CLng(poly.v(b).sy - poly.v(tp).sy)
  '
  If height_A <> 0 Then
    '
    dx_A = (poly.v(a).sx - poly.v(tp).sx) / height_A
    du_A = (poly.v(a).su - poly.v(tp).su) / height_A
    dv_A = (poly.v(a).sv - poly.v(tp).sv) / height_A
    dz_A = (poly.v(a).sz - poly.v(tp).sz) / height_A
    '
  End If
  '
  If height_B <> 0 Then
    '
    dx_B = (poly.v(b).sx - poly.v(tp).sx) / height_B
    du_B = (poly.v(b).su - poly.v(tp).su) / height_B
    dv_B = (poly.v(b).sv - poly.v(tp).sv) / height_B
    dz_B = (poly.v(b).sz - poly.v(tp).sz) / height_B
    '
  End If
  '
  i = 2
  '
  Do Until i < 0
    '
    Do While (height_A > 0 And height_B > 0)
      '
      If y >= 0 And y < MAX_HEIGHT Then
        '
        Call DrawPerspScanlineVB(x1, x2, u1, u2, v1, v2, z1, z2, y)
        '
      End If
      '
      y = y + 1
      '
      height_A = height_A - 1
      height_B = height_B - 1
      '
      x1 = x1 + dx_A
      x2 = x2 + dx_B
      u1 = u1 + du_A
      u2 = u2 + du_B
      v1 = v1 + dv_A
      v2 = v2 + dv_B
      z1 = z1 + dz_A
      z2 = z2 + dz_B
      '
    Loop
    '
    If height_A = 0 Then
      '
      na = a + 1
      '
      If na > 2 Then na = 0
      '
      height_A = CLng(poly.v(na).sy - poly.v(a).sy)
      '
      If height_A Then
        '
        dx_A = (poly.v(na).sx - poly.v(a).sx) / height_A
        du_A = (poly.v(na).su - poly.v(a).su) / height_A
        dv_A = (poly.v(na).sv - poly.v(a).sv) / height_A
        dz_A = (poly.v(na).sz - poly.v(a).sz) / height_A
        '
      End If
      '
      x1 = poly.v(a).sx
      u1 = poly.v(a).su
      v1 = poly.v(a).sv
      z1 = poly.v(a).sz
      '
      i = i - 1
      a = na
      '
    End If
    '
    If height_B = 0 Then
      '
      nb = b - 1
      '
      If nb < 0 Then nb = 2
      '
      height_B = CLng(poly.v(nb).sy - poly.v(b).sy)
      '
      If height_B Then
        '
        dx_B = (poly.v(nb).sx - poly.v(b).sx) / height_B
        du_B = (poly.v(nb).su - poly.v(b).su) / height_B
        dv_B = (poly.v(nb).sv - poly.v(b).sv) / height_B
        dz_B = (poly.v(nb).sz - poly.v(b).sz) / height_B
        '
      End If
      '
      x2 = poly.v(b).sx
      u2 = poly.v(b).su
      v2 = poly.v(b).sv
      z2 = poly.v(b).sz
      '
      i = i - 1
      b = nb
      '
    End If
    '
  Loop
  '
End Sub
Private Sub DrawPerspScanlineVB(ByVal x1 As Single, ByVal x2 As Single, ByVal u1 As Single, ByVal u2 As Single, ByVal v1 As Single, ByVal v2 As Single, ByVal z1 As Single, ByVal z2 As Single, ByVal y As Long)
  '
  Dim width As Single, du As Single, dv As Single, dz As Single
  Dim x As Long, sx As Long, ex As Long, yoff As Long
  Dim tmp As Single
  '
  If x2 < x1 Then
    '
    tmp = x1: x1 = x2: x2 = tmp
    tmp = u1: u1 = u2: u2 = tmp
    tmp = v1: v1 = v2: v2 = tmp
    tmp = z1: z1 = z2: z2 = tmp
    '
  End If
  '
  width = (x2 - x1)
  '
  If width = 0 Then Exit Sub
  '
  width = 1 / width
  '
  du = (u2 - u1) * width
  dv = (v2 - v1) * width
  dz = (z2 - z1) * width
  '
  yoff = ((MAX_HEIGHT - 1) - y) * MAX_WIDTH
  '
  sx = CLng(x1): ex = CLng(x2) - 1
  '
  For x = sx To ex
    '
    If x >= 0 And x < MAX_WIDTH Then
      '
      Viewport(yoff + x) = Textures((199 - CLng(v1 / z1)) * 320 + CLng(u1 / z1))
      '
    End If
    '
    u1 = u1 + du
    v1 = v1 + dv
    z1 = z1 + dz
    '
  Next
  '
End Sub
