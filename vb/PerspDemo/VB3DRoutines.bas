Attribute VB_Name = "VB3DRoutines"
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
Private Type Point3D
  '
  x As Single
  y As Single
  Z As Single
  '
  sx As Long
  sy As Long
  sz As Long
  '
End Type
'
Private Type TextCoords
  '
  u As Single
  v As Single
  '
End Type
'
Private Type Plane
  '
  PointPtr(2) As Long
  '
  tcoords(2) As TextCoords
  visible As Boolean
  '
End Type
'
Private Type Object3D
  '
  Name As String
  NumPoints As Long
  PointList(7) As Point3D
  NumPlanes As Long
  PlaneList(11) As Plane
  '
  xoff As Long
  yoff As Long
  zoff As Long
  '
  xscal As Single
  yscal As Single
  zscal As Single
  '
  yaw As Single
  pit As Single
  rol As Single
  '
End Type
'
Private Type World3D
  '
  NumObjects As Long
  ObjectList(9) As Object3D
  '
End Type
'
Private Type Camera3D
  '
  x As Single
  y As Single
  Z As Single
  '
  yaw As Single
  pit As Single
  rol As Single
  '
  hth As Single
  yon As Single
  '
End Type
'
Private SinTable(1023) As Long, CosTable(1023) As Long
Private Demo As World3D
Private Camera(2) As Camera3D
Private Sub BuildTrigTables()
  '
  Dim tt As Long, part As Single
  '
  part = ((3.14159 * 2) \ 1024)
  '
  For tt = 0 To 1023
    '
    SinTable(tt) = CLng(Sin(part * tt) * 256)
    CosTable(tt) = CLng(Cos(part * tt) * 256)
    '
  Next
  '
End Sub
Public Sub LoadWorld()
  '
  ' Number of objects in world
  '
  Demo.NumObjects = 0
  '
  ' Object name
  '
  Demo.ObjectList(0).Name = "Cube"
  '
  ' Total number of points in object
  '
  Demo.ObjectList(0).NumPoints = 7
  '
  ' Top four points
  '
  Demo.ObjectList(0).PointList(0).x = -10: Demo.ObjectList(0).PointList(0).y = 10: Demo.ObjectList(0).PointList(0).Z = 10
  Demo.ObjectList(0).PointList(1).x = 10: Demo.ObjectList(0).PointList(1).y = 10: Demo.ObjectList(0).PointList(1).Z = 10
  Demo.ObjectList(0).PointList(2).x = 10: Demo.ObjectList(0).PointList(2).y = 10: Demo.ObjectList(0).PointList(2).Z = -10
  Demo.ObjectList(0).PointList(3).x = -10: Demo.ObjectList(0).PointList(3).y = 10: Demo.ObjectList(0).PointList(3).Z = -10
  '
  ' Bottom four points
  '
  Demo.ObjectList(0).PointList(4).x = -10: Demo.ObjectList(0).PointList(4).y = -10: Demo.ObjectList(0).PointList(4).Z = 10
  Demo.ObjectList(0).PointList(5).x = 10: Demo.ObjectList(0).PointList(5).y = -10: Demo.ObjectList(0).PointList(5).Z = 10
  Demo.ObjectList(0).PointList(6).x = 10: Demo.ObjectList(0).PointList(6).y = -10: Demo.ObjectList(0).PointList(6).Z = -10
  Demo.ObjectList(0).PointList(7).x = -10: Demo.ObjectList(0).PointList(7).y = -10: Demo.ObjectList(0).PointList(7).Z = -10
  '
  ' Number of planes in object
  '
  Demo.ObjectList(0).NumPlanes = 11
  '
  ' Top
  '
  Demo.ObjectList(0).PlaneList(0).PointPtr(0) = 1: Demo.ObjectList(0).PlaneList(0).PointPtr(1) = 0: Demo.ObjectList(0).PlaneList(0).PointPtr(2) = 3
  Demo.ObjectList(0).PlaneList(0).tcoords(0).u = 63: Demo.ObjectList(0).PlaneList(0).tcoords(0).v = 0
  Demo.ObjectList(0).PlaneList(0).tcoords(1).u = 0: Demo.ObjectList(0).PlaneList(0).tcoords(1).v = 0
  Demo.ObjectList(0).PlaneList(0).tcoords(2).u = 0: Demo.ObjectList(0).PlaneList(0).tcoords(2).v = 63
  Demo.ObjectList(0).PlaneList(1).PointPtr(0) = 3: Demo.ObjectList(0).PlaneList(1).PointPtr(1) = 2: Demo.ObjectList(0).PlaneList(1).PointPtr(2) = 1
  Demo.ObjectList(0).PlaneList(1).tcoords(0).u = 0: Demo.ObjectList(0).PlaneList(1).tcoords(0).v = 63
  Demo.ObjectList(0).PlaneList(1).tcoords(1).u = 63: Demo.ObjectList(0).PlaneList(1).tcoords(1).v = 63
  Demo.ObjectList(0).PlaneList(1).tcoords(2).u = 63: Demo.ObjectList(0).PlaneList(1).tcoords(2).v = 0
  '
  ' Bottom
  '
  Demo.ObjectList(0).PlaneList(2).PointPtr(0) = 7: Demo.ObjectList(0).PlaneList(2).PointPtr(1) = 4: Demo.ObjectList(0).PlaneList(2).PointPtr(2) = 5
  Demo.ObjectList(0).PlaneList(2).tcoords(0).u = 0: Demo.ObjectList(0).PlaneList(2).tcoords(0).v = 0
  Demo.ObjectList(0).PlaneList(2).tcoords(1).u = 0: Demo.ObjectList(0).PlaneList(2).tcoords(1).v = 63
  Demo.ObjectList(0).PlaneList(2).tcoords(2).u = 63: Demo.ObjectList(0).PlaneList(2).tcoords(2).v = 63
  Demo.ObjectList(0).PlaneList(3).PointPtr(0) = 5: Demo.ObjectList(0).PlaneList(3).PointPtr(1) = 6: Demo.ObjectList(0).PlaneList(3).PointPtr(2) = 7
  Demo.ObjectList(0).PlaneList(3).tcoords(0).u = 63: Demo.ObjectList(0).PlaneList(3).tcoords(0).v = 63
  Demo.ObjectList(0).PlaneList(3).tcoords(1).u = 63: Demo.ObjectList(0).PlaneList(3).tcoords(1).v = 0
  Demo.ObjectList(0).PlaneList(3).tcoords(2).u = 0: Demo.ObjectList(0).PlaneList(3).tcoords(2).v = 0
  '
  ' Left
  '
  Demo.ObjectList(0).PlaneList(4).PointPtr(0) = 0: Demo.ObjectList(0).PlaneList(4).PointPtr(1) = 4: Demo.ObjectList(0).PlaneList(4).PointPtr(2) = 7
  Demo.ObjectList(0).PlaneList(4).tcoords(0).u = 0: Demo.ObjectList(0).PlaneList(4).tcoords(0).v = 0
  Demo.ObjectList(0).PlaneList(4).tcoords(1).u = 0: Demo.ObjectList(0).PlaneList(4).tcoords(1).v = 63
  Demo.ObjectList(0).PlaneList(4).tcoords(2).u = 63: Demo.ObjectList(0).PlaneList(4).tcoords(2).v = 63
  Demo.ObjectList(0).PlaneList(5).PointPtr(0) = 7: Demo.ObjectList(0).PlaneList(5).PointPtr(1) = 3: Demo.ObjectList(0).PlaneList(5).PointPtr(2) = 0
  Demo.ObjectList(0).PlaneList(5).tcoords(0).u = 63: Demo.ObjectList(0).PlaneList(5).tcoords(0).v = 63
  Demo.ObjectList(0).PlaneList(5).tcoords(1).u = 63: Demo.ObjectList(0).PlaneList(5).tcoords(1).v = 0
  Demo.ObjectList(0).PlaneList(5).tcoords(2).u = 0: Demo.ObjectList(0).PlaneList(5).tcoords(2).v = 0
  '
  ' Right
  '
  Demo.ObjectList(0).PlaneList(6).PointPtr(0) = 1: Demo.ObjectList(0).PlaneList(6).PointPtr(1) = 2: Demo.ObjectList(0).PlaneList(6).PointPtr(2) = 6
  Demo.ObjectList(0).PlaneList(6).tcoords(0).u = 63: Demo.ObjectList(0).PlaneList(6).tcoords(0).v = 0
  Demo.ObjectList(0).PlaneList(6).tcoords(1).u = 0: Demo.ObjectList(0).PlaneList(6).tcoords(1).v = 0
  Demo.ObjectList(0).PlaneList(6).tcoords(2).u = 0: Demo.ObjectList(0).PlaneList(6).tcoords(2).v = 63
  Demo.ObjectList(0).PlaneList(7).PointPtr(0) = 6: Demo.ObjectList(0).PlaneList(7).PointPtr(1) = 5: Demo.ObjectList(0).PlaneList(7).PointPtr(2) = 1
  Demo.ObjectList(0).PlaneList(7).tcoords(0).u = 0: Demo.ObjectList(0).PlaneList(7).tcoords(0).v = 63
  Demo.ObjectList(0).PlaneList(7).tcoords(1).u = 63: Demo.ObjectList(0).PlaneList(7).tcoords(1).v = 63
  Demo.ObjectList(0).PlaneList(7).tcoords(2).u = 63: Demo.ObjectList(0).PlaneList(7).tcoords(2).v = 0
  '
  ' Back
  '
  Demo.ObjectList(0).PlaneList(8).PointPtr(0) = 0: Demo.ObjectList(0).PlaneList(8).PointPtr(1) = 1: Demo.ObjectList(0).PlaneList(8).PointPtr(2) = 4
  Demo.ObjectList(0).PlaneList(8).tcoords(0).u = 63: Demo.ObjectList(0).PlaneList(8).tcoords(0).v = 0
  Demo.ObjectList(0).PlaneList(8).tcoords(1).u = 0: Demo.ObjectList(0).PlaneList(8).tcoords(1).v = 0
  Demo.ObjectList(0).PlaneList(8).tcoords(2).u = 63: Demo.ObjectList(0).PlaneList(8).tcoords(2).v = 63
  Demo.ObjectList(0).PlaneList(9).PointPtr(0) = 4: Demo.ObjectList(0).PlaneList(9).PointPtr(1) = 1: Demo.ObjectList(0).PlaneList(9).PointPtr(2) = 5
  Demo.ObjectList(0).PlaneList(9).tcoords(0).u = 63: Demo.ObjectList(0).PlaneList(9).tcoords(0).v = 63
  Demo.ObjectList(0).PlaneList(9).tcoords(1).u = 0: Demo.ObjectList(0).PlaneList(9).tcoords(1).v = 0
  Demo.ObjectList(0).PlaneList(9).tcoords(2).u = 0: Demo.ObjectList(0).PlaneList(9).tcoords(2).v = 63
  '
  ' Front
  '
  Demo.ObjectList(0).PlaneList(10).PointPtr(0) = 7: Demo.ObjectList(0).PlaneList(10).PointPtr(1) = 2: Demo.ObjectList(0).PlaneList(10).PointPtr(2) = 3
  Demo.ObjectList(0).PlaneList(10).tcoords(0).u = 0: Demo.ObjectList(0).PlaneList(10).tcoords(0).v = 63
  Demo.ObjectList(0).PlaneList(10).tcoords(1).u = 63: Demo.ObjectList(0).PlaneList(10).tcoords(1).v = 0
  Demo.ObjectList(0).PlaneList(10).tcoords(2).u = 0: Demo.ObjectList(0).PlaneList(10).tcoords(2).v = 0
  Demo.ObjectList(0).PlaneList(11).PointPtr(0) = 7: Demo.ObjectList(0).PlaneList(11).PointPtr(1) = 6: Demo.ObjectList(0).PlaneList(11).PointPtr(2) = 2
  Demo.ObjectList(0).PlaneList(11).tcoords(0).u = 0: Demo.ObjectList(0).PlaneList(11).tcoords(0).v = 63
  Demo.ObjectList(0).PlaneList(11).tcoords(1).u = 63: Demo.ObjectList(0).PlaneList(11).tcoords(1).v = 63
  Demo.ObjectList(0).PlaneList(11).tcoords(2).u = 63: Demo.ObjectList(0).PlaneList(11).tcoords(2).v = 0
  '
  ' X/Y/Z Offsets of an object
  '
  Demo.ObjectList(0).xoff = 0
  Demo.ObjectList(0).yoff = 0
  Demo.ObjectList(0).zoff = 0
  '
  ' X/Y/Z Scale of an object
  '
  Demo.ObjectList(0).xscal = 1
  Demo.ObjectList(0).yscal = 1
  Demo.ObjectList(0).zscal = 1
  '
  ' Yaw/Pitch/Roll of an object
  '
  Demo.ObjectList(0).yaw = 0
  Demo.ObjectList(0).pit = 0
  Demo.ObjectList(0).rol = 0
  '
  ' Set up a camera
  '
  Camera(0).x = 0
  Camera(0).y = 0
  Camera(0).Z = -45
  Camera(0).yaw = 0
  Camera(0).pit = 0
  Camera(0).rol = 0
  Camera(0).hth = 0
  Camera(0).yon = 500
  '
End Sub
Public Sub DrawWorld()
  '
  Dim obj As Object3D, cam As Camera3D, ob As Long
  '
  For ob = 0 To Demo.NumObjects
    '
    obj = Demo.ObjectList(ob)
    cam = Camera(0)
    '
    ' Is the object visible?
    '
    If Not CullObject3D(obj, cam) Then
      '
      Call ScaleObject3D(obj)                 ' Scale the object
      Call RotateObject3D(obj)                ' Rotate the object
      Call TranslateObject3D(obj)             ' Move the object according to object movement
      Call TranslateCamera3D(obj, cam)        ' Move the object according to camera movement
      Call RotateCamera3D(obj, cam)           ' Rotate the object according to camera rotation
      Call ProjectObject3D(obj)               ' Project the object from 3D to 2D
      Call DrawObject3D(obj, cam)       ' Draw the object on the screen
      '
    End If
    '
    Call AnimateObject3D(Demo.ObjectList(ob)) ' Perform any desired animation on the object
    '
  Next
  '
End Sub
Private Function CullObject3D(obj As Object3D, cam As Camera3D) As Boolean
  '
  ' Cull objects based on camera view and other settings
  '
  CullObject3D = False
  '
End Function
Private Sub AnimateObject3D(obj As Object3D)
  '
  Select Case obj.Name
    Case "Cube"
      '
      obj.yaw = obj.yaw + 0.07: If obj.yaw > 6.282 Then obj.yaw = 0
      obj.pit = obj.pit + 0.03: If obj.pit > 6.282 Then obj.pit = 0
      obj.rol = obj.rol + 0.05: If obj.rol > 6.282 Then obj.rol = 0
      '
    Case Else
      '
      '
  End Select
  '
End Sub
Private Sub TranslateObject3D(obj As Object3D)
  '
  Dim ptn As Long
  '
  For ptn = 0 To obj.NumPoints
    '
    obj.PointList(ptn).x = obj.PointList(ptn).x + obj.xoff
    obj.PointList(ptn).y = obj.PointList(ptn).y + obj.yoff
    obj.PointList(ptn).Z = obj.PointList(ptn).Z + obj.zoff
    '
  Next
  '
End Sub
Private Sub TranslateCamera3D(obj As Object3D, cam As Camera3D)
  '
  Dim ptn As Long
  '
  For ptn = 0 To obj.NumPoints
    '
    obj.PointList(ptn).x = obj.PointList(ptn).x - cam.x
    obj.PointList(ptn).y = obj.PointList(ptn).y - cam.y
    obj.PointList(ptn).Z = obj.PointList(ptn).Z - cam.Z
    '
  Next
  '
End Sub
Private Sub ScaleObject3D(obj As Object3D)
  '
  Dim ptn As Long
  '
  For ptn = 0 To obj.NumPoints
    '
    obj.PointList(ptn).x = obj.PointList(ptn).x * obj.xscal
    obj.PointList(ptn).y = obj.PointList(ptn).y * obj.yscal
    obj.PointList(ptn).Z = obj.PointList(ptn).Z * obj.zscal
    '
  Next
  '
End Sub
Private Sub RotateObject3D(obj As Object3D)
  '
  ' These rotations follow the "right-hand" rule for this implementation of the 3D Cartesian Coordinate
  ' System, in which (facing the screen), Positive X extends to the right, Negative X extends to the left,
  ' Positive Y extends to the top, Negative Y extends to the bottom, Positive Z extends in toward the
  ' monitor, and Negative Z extends out toward the user.
  '
  ' Using the right hand rule, the fingers of the right hand curl around the axis which the object is
  ' being rotated about, with the thumb pointing toward the positive direction of the axis. The direct-
  ' ional curl of the fingers indicate the direction which rotation will progress.
  '
  Dim ptn As Long
  '
  Dim x1 As Single, y1 As Single, z1 As Single
  Dim x2 As Single, y2 As Single, z2 As Single
  Dim x3 As Single, y3 As Single, z3 As Single
  '
  For ptn = 0 To obj.NumPoints
    '
    x1 = Cos(-obj.yaw) * obj.PointList(ptn).x + Sin(-obj.yaw) * obj.PointList(ptn).Z
    y1 = obj.PointList(ptn).y
    z1 = Sin(-obj.yaw) * obj.PointList(ptn).x - Cos(-obj.yaw) * obj.PointList(ptn).Z
    '
    x2 = x1
    y2 = Cos(obj.pit) * y1 + Sin(obj.pit) * z1
    z2 = Sin(obj.pit) * y1 - Cos(obj.pit) * z1
    '
    x3 = Cos(-obj.rol) * x2 + Sin(-obj.rol) * y2
    y3 = Sin(-obj.rol) * x2 - Cos(-obj.rol) * y2
    z3 = z2
    '
    obj.PointList(ptn).x = x3
    obj.PointList(ptn).y = y3
    obj.PointList(ptn).Z = z3
    '
  Next
  '
End Sub
Private Sub RotateCamera3D(obj As Object3D, cam As Camera3D)
  '
  ' These rotations follow the "right-hand" rule for this implementation of the 3D Cartesian Coordinate
  ' System, in which (facing the screen), Positive X extends to the right, Negative X extends to the left,
  ' Positive Y extends to the top, Negative Y extends to the bottom, Positive Z extends in toward the
  ' monitor, and Negative Z extends out toward the user.
  '
  ' Using the right hand rule, the fingers of the right hand curl around the axis which the object is
  ' being rotated about, with the thumb pointing toward the positive direction of the axis. The direct-
  ' ional curl of the fingers indicate the direction which rotation will progress.
  '
  Dim ptn As Long
  '
  Dim x1 As Single, y1 As Single, z1 As Single
  Dim x2 As Single, y2 As Single, z2 As Single
  Dim x3 As Single, y3 As Single, z3 As Single
  '
  For ptn = 0 To obj.NumPoints
    '
    x1 = Cos(cam.yaw) * obj.PointList(ptn).x + Sin(cam.yaw) * obj.PointList(ptn).Z
    y1 = obj.PointList(ptn).y
    z1 = Sin(cam.yaw) * obj.PointList(ptn).x - Cos(cam.yaw) * obj.PointList(ptn).Z
    '
    x2 = x1
    y2 = Cos(cam.pit) * y1 + Sin(cam.pit) * z1
    z2 = Sin(cam.pit) * y1 - Cos(cam.pit) * z1
    '
    x3 = Cos(-cam.rol) * x2 + Sin(-cam.rol) * y2
    y3 = Sin(-cam.rol) * x2 - Cos(-cam.rol) * y2
    z3 = z2
    '
    obj.PointList(ptn).x = x3
    obj.PointList(ptn).y = y3
    obj.PointList(ptn).Z = z3
    '
  Next
  '
End Sub
Private Function CullPlane3D(pnt0 As Point3D, pnt1 As Point3D, pnt2 As Point3D, cam As Camera3D) As Boolean
  '
  Dim dx1 As Currency, dx2 As Currency
  Dim dy1 As Currency, dy2 As Currency
  Dim dz1 As Currency
  '
  CullPlane3D = False
  '
  dx1 = pnt2.sx - pnt0.sx
  dy1 = pnt2.sy - pnt0.sy
  dx2 = pnt2.sx - pnt1.sx
  dy2 = pnt2.sy - pnt1.sy
  dz1 = (pnt0.Z + pnt1.Z + pnt2.Z) * 0.333
  '
  ' Find the z (sign) value of the cross product - if positive, then plane is not visible
  '
  If ((dx1 * (dy2 - dy1) - (dx2 - dx1) * dy1) > 0) Or (dz1 < cam.Z + cam.hth) Or (dz1 > cam.Z + cam.yon) Then CullPlane3D = True
  '
End Function
Private Sub ProjectObject3D(obj As Object3D)
  '
  Dim ptn As Long
  '
  For ptn = 0 To obj.NumPoints
    '
    zz = obj.PointList(ptn).Z + Not (obj.PointList(ptn).Z) * 0.0001
    '
    obj.PointList(ptn).sx = VCX + ((obj.PointList(ptn).x * 200) / zz)
    obj.PointList(ptn).sy = VCY - ((obj.PointList(ptn).y * 200) / zz)
    obj.PointList(ptn).sz = zz
    '
  Next
  '
End Sub
Private Sub DrawObject3D(obj As Object3D, cam As Camera3D)
  '
  Dim pl As Long, poly As Triangle
  '
  For pl = 0 To obj.NumPlanes
    '
    If Not CullPlane3D(obj.PointList(obj.PlaneList(pl).PointPtr(0)), obj.PointList(obj.PlaneList(pl).PointPtr(1)), obj.PointList(obj.PlaneList(pl).PointPtr(2)), cam) Then
      '
      poly.v(0).sx = obj.PointList(obj.PlaneList(pl).PointPtr(0)).sx
      poly.v(0).sy = obj.PointList(obj.PlaneList(pl).PointPtr(0)).sy
      poly.v(0).sz = 1 / obj.PointList(obj.PlaneList(pl).PointPtr(0)).sz
      poly.v(0).u = obj.PlaneList(pl).tcoords(0).u
      poly.v(0).v = obj.PlaneList(pl).tcoords(0).v
      poly.v(0).su = obj.PlaneList(pl).tcoords(0).u / obj.PointList(obj.PlaneList(pl).PointPtr(0)).sz
      poly.v(0).sv = obj.PlaneList(pl).tcoords(0).v / obj.PointList(obj.PlaneList(pl).PointPtr(0)).sz
      '
      poly.v(1).sx = obj.PointList(obj.PlaneList(pl).PointPtr(1)).sx
      poly.v(1).sy = obj.PointList(obj.PlaneList(pl).PointPtr(1)).sy
      poly.v(1).sz = 1 / obj.PointList(obj.PlaneList(pl).PointPtr(1)).sz
      poly.v(1).u = obj.PlaneList(pl).tcoords(1).u
      poly.v(1).v = obj.PlaneList(pl).tcoords(1).v
      poly.v(1).su = obj.PlaneList(pl).tcoords(1).u / obj.PointList(obj.PlaneList(pl).PointPtr(1)).sz
      poly.v(1).sv = obj.PlaneList(pl).tcoords(1).v / obj.PointList(obj.PlaneList(pl).PointPtr(1)).sz
      '
      poly.v(2).sx = obj.PointList(obj.PlaneList(pl).PointPtr(2)).sx
      poly.v(2).sy = obj.PointList(obj.PlaneList(pl).PointPtr(2)).sy
      poly.v(2).sz = 1 / obj.PointList(obj.PlaneList(pl).PointPtr(2)).sz
      poly.v(2).u = obj.PlaneList(pl).tcoords(2).u
      poly.v(2).v = obj.PlaneList(pl).tcoords(2).v
      poly.v(2).su = obj.PlaneList(pl).tcoords(2).u / obj.PointList(obj.PlaneList(pl).PointPtr(2)).sz
      poly.v(2).sv = obj.PlaneList(pl).tcoords(2).v / obj.PointList(obj.PlaneList(pl).PointPtr(2)).sz
      '
      Call DrawPerspTriangle(poly)
      '
    End If
    '
  Next
  '
End Sub
Private Function ShadePlane3D(col As Long, pnt0 As Point3D, pnt1 As Point3D, pnt2 As Point3D) As Long
  '
  Dim colr As Long, colg As Long, colb As Long
  '
  If ShadeMode Then
    '
    ' "Z-Flat" Shading Model - Not the best, but does in a pinch!
    '
    colr = Val("&H" + Mid$(Hex$(col), 1, 2))
    colg = Val("&H" + Mid$(Hex$(col), 3, 2))
    colb = Val("&H" + Mid$(Hex$(col), 5, 2))
    '
    colr = colr - (pnt0.Z - pnt1.Z - pnt2.Z) / 0.2: If colr < 0 Then colr = 0
    colg = colg - (pnt0.Z - pnt1.Z - pnt2.Z) / 0.2: If colg < 0 Then colg = 0
    colb = colb - (pnt0.Z - pnt1.Z - pnt2.Z) / 0.2: If colb < 0 Then colb = 0
    '
    ShadePlane3D = RGB(colr, colg, colb)
    '
  Else
    '
    ' No shading - use color of plane
    '
    ShadePlane3D = col
    '
  End If
  '
End Function
