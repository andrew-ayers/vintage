Attribute VB_Name = "MainModule"
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
Public Type Point3D
  '
  x As Long
  y As Long
  z As Long
  '
  sx As Long
  sy As Long
  '
End Type
'
Public Grid(99, 99) As Point3D
Public Sub CreateGrid()
  '
  For l = 0 To 5
    '
    For y = 0 To 19
      '
      For x = 0 To 19
        '
        If l = 0 Then
          '
          Grid(x, y).x = (x - 10) * 10
          Grid(x, y).z = (y - 10) * 10
          '
        End If
        '
        Grid(x, y).y = Grid(x, y).y + Sin(x) * Sin(y)
        '
      Next
      '
    Next
    '
  Next
  '
End Sub
Public Sub DrawMesh(yaw As Single, pit As Single, rol As Single)
  '
  Dim x As Long, y As Long
  Dim x1 As Long, y1 As Long, z1 As Long
  '
  For y = 0 To 19
    '
    For x = 0 To 19
      '
      x1 = Grid(x, y).x: y1 = Grid(x, y).y: z1 = Grid(x, y).z
      '
      Call Rotate3D(x1, y1, z1, yaw, pit, rol)
      '
      z1 = -z1 - 200
      '
      If z1 = 0 Then z1 = 1
      '
      Grid(x, y).sx = (MainForm.ScaleWidth \ 2) + ((x1 * 300) \ z1)
      Grid(x, y).sy = (MainForm.ScaleHeight \ 2) + ((y1 * 300) \ z1)
      '
    Next
    '
  Next
  '
  For y = 0 To 18
    '
    For x = 0 To 18
      '
      MainForm.Line (Grid(x, y).sx, Grid(x, y).sy)-(Grid(x + 1, y).sx, Grid(x + 1, y).sy), RGB(255, 255, 255)
      MainForm.Line -(Grid(x + 1, y + 1).sx, Grid(x + 1, y + 1).sy), RGB(255, 255, 255)
      MainForm.Line -(Grid(x, y + 1).sx, Grid(x, y + 1).sy), RGB(255, 255, 255)
      MainForm.Line -(Grid(x, y).sx, Grid(x, y).sy), RGB(255, 255, 255)
      '
    Next
    '
  Next
  '
End Sub
Private Sub Rotate3D(x1 As Long, y1 As Long, z1 As Long, yaw As Single, pit As Single, rol As Single)
  '
  Dim x2 As Long, y2 As Long, z2 As Long
  Dim x3 As Long, y3 As Long, z3 As Long
  Dim x4 As Long, y4 As Long, z4 As Long
  '
  ' Yaw
  '
  x2 = x1 * Cos(yaw) + z1 * Sin(yaw)
  y2 = y1
  z2 = x1 * Sin(yaw) - z1 * Cos(yaw)
  '
  ' Pitch
  '
  x3 = x2
  y3 = y2 * Cos(pit) + z2 * Sin(pit)
  z3 = y2 * Sin(pit) - z2 * Cos(pit)
  '
  ' Roll
  '
  x4 = x3 * Cos(rol) + y3 * Sin(rol)
  y4 = x3 * Sin(rol) - y3 * Cos(rol)
  z4 = z3
  '
  x1 = x4: y1 = y4: z1 = z4
  '
End Sub
