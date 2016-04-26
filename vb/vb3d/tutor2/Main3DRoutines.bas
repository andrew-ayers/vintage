Attribute VB_Name = "Main3DRoutines"
'
' Define data types for object data
'
' Effective data type layout will help to ensure a smoothly
' running engine, as well as one which may be easily updated
' and expanded at later points. Thinking about this up front
' (and I am not saying mine is the best or most widely used -
' in fact, I wouldn't be suprised if mine was really bad, but
' it is what I have come up with) will save a lot of problems
' in the future (I know - BTDT)...
'
' First we define our elementary data type for a 3D point in
' space (object space). There will end up being one vertex
' for each vertex in the object (duh!). In the vertex data
' type will also be space for the calculation of the screen
' coordinates for each vertex in the object.
'
Private Type Vertex3D   ' Vertex Data
  '
  X As Single               ' X object coordinate of point
  Y As Single               ' Y object coordinate of point
  z As Single               ' Z object coordinate of point
  '
  sx As Long                ' X screen coordinate of point
  sy As Long                ' Y screen coordinate of point
  '
End Type
'
' Now we define a data type for our polygons. Our data type
' will need to hold a few bits of information. We are going
' to use a list of three pointers back to the vertex list in
' the object. We also have a color value data area so we
' know what color to paint the triangle that forms the poly.
'
Private Type Polygon3D  ' Polygon Data
  '
  v_pointer(2) As Long      ' Pointer to vertex (in object)
  '
  colr As Long              ' Color of polygon
  style As Long             ' Style of polygon - 0=wireframe,1=solid,2=transparent
  '
End Type
'
' Finally we have our object data type. This data type is
' composed of two lists, one for vertices, and a second for
' polygons. We use this kind of arrangement so we can quickly
' calculate the positions for each vertex in the object, rather
' than looping through each polygon and calculating each vertex
' that way (because you would end up calculating some vertices
' more than one time, and that takes up a bit of time).
'
Public Type Object3D    ' Object Data
  '
  name As String            ' Name of object
  vertex() As Vertex3D      ' List of vertices for object
  Polygon() As Polygon3D    ' List of polygons for object
  '
End Type
'
Type Coord
  '
  X As Long
  Y As Long
  '
End Type
'
Declare Function Polygon Lib "gdi32" (ByVal hdc As Long, lpPoints As Any, ByVal nCount As Long) As Long
Public Sub LoadMPLGObject(filename As String, obj As Object3D)
  '
  ' This routine will load our MPLG format object from a disk file
  '
  Dim filenum As Long
  '
  filenum = FreeFile
  '
  Open filename For Input As filenum
  '
  ' Get the name, number of vertices, and polygons
  '
  Do
    Line Input #filenum, valu$
    '
    If Trim$(Left$(valu$, 1)) <> "" And Trim$(Left$(valu$, 1)) <> "#" Then
      '
      ' Clean up line to remove extra spaces
      '
      valu$ = TrimExtraSpace(valu$)
      '
      ' Get values from line
      '
      name$ = Field(valu$, " ", 1)
      numv& = Val(Field(valu$, " ", 2))
      nump& = Val(Field(valu$, " ", 3))
      '
      Exit Do
      '
    End If
    '
  Loop
  '
  ' Load vertex data
  '
  ReDim obj.vertex(numv& - 1)
  '
  vnum& = 0
  '
  Do
    Line Input #filenum, valu$
    '
    If Left$(Trim$(valu$), 1) <> "" And Left$(Trim$(valu$), 1) <> "#" Then
      '
      ' Clean up line to remove extra spaces
      '
      valu$ = TrimExtraSpace(valu$)
      '
      ' Get values from line
      '
      obj.vertex(vnum&).X = Val(Field(valu$, " ", 1))
      obj.vertex(vnum&).Y = Val(Field(valu$, " ", 2))
      obj.vertex(vnum&).z = Val(Field(valu$, " ", 3))
      '
      vnum& = vnum& + 1
      '
      If vnum& > numv& - 1 Then Exit Do
      '
    End If
    '
  Loop
  '
  ' Load polygon data
  '
  ReDim obj.Polygon(nump& - 1)
  '
  pnum& = 0
  '
  Do
    Line Input #filenum, valu$
    '
    If Left$(Trim$(valu$), 1) <> "" And Left$(Trim$(valu$), 1) <> "#" Then
      '
      ' Clean up line to remove extra spaces
      '
      valu$ = TrimExtraSpace(valu$)
      '
      ' Get color for polygon
      '
      colrstrg$ = Field(valu$, " ", 1)
      '
      colrtype& = Val(Mid$(colrstrg$, 1, 2))
      '
      cvred& = "&h" + Mid$(colrstrg$, 3, 2)
      cvgrn& = "&h" + Mid$(colrstrg$, 5, 2)
      cvblu& = "&h" + Mid$(colrstrg$, 7, 2)
      '
      Select Case colrtype&
        Case 0 ' Wireframe
          obj.Polygon(pnum&).colr = RGB(cvred&, cvgrn&, cvblu&)
          obj.Polygon(pnum&).style = 0
        Case 1 ' Solid
          obj.Polygon(pnum&).colr = RGB(cvred&, cvgrn&, cvblu&)
          obj.Polygon(pnum&).style = 1
        Case 2 ' Solid/Transparent
          obj.Polygon(pnum&).colr = RGB(cvred&, cvgrn&, cvblu&)
          obj.Polygon(pnum&).style = 2
        Case Else ' Wireframe white
          obj.Polygon(pnum&).colr = RGB(255, 255, 255)
          obj.Polygon(pnum&).style = 0
      End Select
      '
      ' Get vertex pointer values
      '
      obj.Polygon(pnum&).v_pointer(0) = Val(Field(valu$, " ", 2))
      obj.Polygon(pnum&).v_pointer(1) = Val(Field(valu$, " ", 3))
      obj.Polygon(pnum&).v_pointer(2) = Val(Field(valu$, " ", 4))
      '
      pnum& = pnum& + 1
      '
      If pnum& > nump& - 1 Then Exit Do
      '
    End If
    '
  Loop
  '
  Close filenum
  '
End Sub
Public Sub DisplayObject(obj As Object3D, style As Long, frm As Object)
  '
  ' Draw the object on the form, one polygon (triangle) at
  ' a time...
  '
  Dim poly As Long
  Dim v0 As Vertex3D
  Dim v1 As Vertex3D
  Dim v2 As Vertex3D
  '
  For poly = 0 To UBound(obj.Polygon())
    '
    v0 = obj.vertex(obj.Polygon(poly).v_pointer(0))
    v1 = obj.vertex(obj.Polygon(poly).v_pointer(1))
    v2 = obj.vertex(obj.Polygon(poly).v_pointer(2))
    '
    If Not CullPlane3D(v0, v1, v2) Or style = 0 Then
      '
      Call DrawTriangle(obj, poly, style, frm)
      '
    End If
    '
  Next
  '
End Sub
Private Sub DrawTriangle(obj As Object3D, polyg As Long, style As Long, frm As Object)
  '
  Dim poly(0 To 2) As Coord
  '
  poly(0).X = obj.vertex(obj.Polygon(polyg).v_pointer(0)).sx
  poly(0).Y = obj.vertex(obj.Polygon(polyg).v_pointer(0)).sy
  poly(1).X = obj.vertex(obj.Polygon(polyg).v_pointer(1)).sx
  poly(1).Y = obj.vertex(obj.Polygon(polyg).v_pointer(1)).sy
  poly(2).X = obj.vertex(obj.Polygon(polyg).v_pointer(2)).sx
  poly(2).Y = obj.vertex(obj.Polygon(polyg).v_pointer(2)).sy
  '
  Select Case style
    Case 0, 1
      frm.Line (poly(0).X, poly(0).Y)-(poly(1).X, poly(1).Y), obj.Polygon(polyg).colr
      frm.Line -(poly(2).X, poly(2).Y), obj.Polygon(polyg).colr
      frm.Line -(poly(0).X, poly(0).Y), obj.Polygon(polyg).colr
    Case 2
      HoldFillStyle& = frm.FillStyle
      HoldFillColor& = frm.FillColor
      HoldForeColor& = frm.ForeColor
      '
      ' Solid Mode
      '
      frm.FillStyle = 0
      frm.FillColor = obj.Polygon(polyg).colr
      frm.ForeColor = obj.Polygon(polyg).colr
      '
      foo& = Polygon(frm.hdc, poly(0), 3)
      '
      frm.FillStyle = HoldFileStyle&
      frm.FillColor = HoldFillColor&
      frm.ForeColor = HoldForeColor&
  End Select
  '
End Sub
Public Sub ProjectObject(obj As Object3D, frm As Object)
  '
  ' Project the object from "world" space to screen space...
  '
  Dim pnt As Long, dz As Single
  '
  For pnt = 0 To UBound(obj.vertex())
    '
    ' For each vertex, figure out the screen X and Y coordinates
    ' to position it onscreen...
    '
    ' Control "divide by zero" problem - without this line, at
    ' certain distances the projection calculation will fail...
    '
    dz = obj.vertex(pnt).z: If dz <= 0 Then dz = 0.001
    '
    ' Project the point
    '
    obj.vertex(pnt).sx = (frm.ScaleWidth / 2) + ((obj.vertex(pnt).X * 300) / dz)
    obj.vertex(pnt).sy = (frm.ScaleHeight / 2) + ((obj.vertex(pnt).Y * 300) / dz)
    '
  Next
  '
End Sub
Public Sub RotateObject(yaw As Single, pit As Single, rol As Single, obj As Object3D)
  '
  ' For each point in the object, rotate it by input amount
  ' This is very important. If you learn nothing else, learn
  ' this section. If you notice, for the axis you want the
  ' object to rotate around, just leave it alone. You should
  ' also notice that the calculation for yaw, pitch and roll
  ' are _identical_. They all derive from the same equation
  ' for two dimensional rotation, only with a third component
  ' added.
  '
  Dim pnt As Long
  Dim x0 As Single, y0 As Single, z0 As Single
  Dim x1 As Single, y1 As Single, z1 As Single
  Dim x2 As Single, y2 As Single, z2 As Single
  '
  For pnt = 0 To UBound(obj.vertex())
    '
    ' Yaw - We rotate about the Y axis (in our system, the
    ' Y axis is vertical, and the X and Z axis' extend in the
    ' horizontal plane. Some systems, like the one used in
    ' 3DStudio, have the Z axis vertical, with the X and Y
    ' axis' extending in the horizontal plane. This may be
    ' an issue when we develop a 3DS model loader in a later
    ' tutorial)...
    '
    x0 = Cos(yaw) * obj.vertex(pnt).X + Sin(yaw) * obj.vertex(pnt).z
    y0 = obj.vertex(pnt).Y
    z0 = Sin(yaw) * obj.vertex(pnt).X - Cos(yaw) * obj.vertex(pnt).z
    '
    ' Pitch
    '
    x1 = x0
    y1 = Cos(pit) * y0 + Sin(pit) * z0
    z1 = Sin(pit) * y0 - Cos(pit) * z0
    '
    ' Roll
    '
    x2 = Cos(rol) * x1 + Sin(rol) * y1
    y2 = Sin(rol) * x1 - Cos(rol) * y1
    z2 = z1
    '
    ' Assign the new values back to the object
    '
    obj.vertex(pnt).X = x2
    obj.vertex(pnt).Y = y2
    obj.vertex(pnt).z = z2
    '
  Next
  '
End Sub
Public Sub ScaleObject(xs As Single, ys As Single, zs As Single, obj As Object3D)
  '
  ' Scale an object by simply multiplying each vertex by
  ' a scalar value...
  '
  Dim pnt As Long
  '
  For pnt = 0 To UBound(obj.vertex())
    '
    obj.vertex(pnt).X = obj.vertex(pnt).X * xs
    obj.vertex(pnt).Y = obj.vertex(pnt).Y * ys
    obj.vertex(pnt).z = obj.vertex(pnt).z * zs
    '
  Next
  '
End Sub
Public Sub TranslateObject(tx As Long, ty As Long, tz As Long, obj As Object3D)
  '
  ' Translate an object by simply adding a translation value
  ' to each vertex...
  '
  ' This routine can be used to perform movement and camera
  ' placement (via actually moving the objects in relation to
  ' the camera - the camera never actually moves). In this
  ' tutorial it is used to move an object from 0,0,0 away from
  ' the "camera" (also at 0,0,0) to 0,0,50 - because otherwise
  ' the camera appears to be inside the object, which can cause
  ' a crash...
  '
  Dim pnt As Long
  '
  For pnt = 0 To UBound(obj.vertex())
    '
    obj.vertex(pnt).X = obj.vertex(pnt).X + tx
    obj.vertex(pnt).Y = obj.vertex(pnt).Y + ty
    obj.vertex(pnt).z = obj.vertex(pnt).z + tz
    '
  Next
  '
End Sub
Private Function CullPlane3D(pnt0 As Vertex3D, pnt1 As Vertex3D, pnt2 As Vertex3D) As Boolean
  '
  Dim dx1 As Long, dx2 As Long
  Dim dy1 As Long, dy2 As Long
  Dim dz1 As Long
  '
  CullPlane3D = False
  '
  dx1 = pnt2.sx - pnt0.sx
  dy1 = pnt2.sy - pnt0.sy
  dx2 = pnt2.sx - pnt1.sx
  dy2 = pnt2.sy - pnt1.sy
  dz1 = (pnt0.z + pnt1.z + pnt2.z) \ 3
  '
  ' Find the z (sign) value of the cross product - if negative,
  ' then the plane is not visible
  '
  If ((dx1 * (dy2 - dy1) - (dx2 - dx1) * dy1) < 0) Then CullPlane3D = True
  '
End Function

