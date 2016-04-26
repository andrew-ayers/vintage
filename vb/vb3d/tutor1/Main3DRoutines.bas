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
  x As Single               ' X object coordinate of point
  y As Single               ' Y object coordinate of point
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
  vertex() As Vertex3D      ' List of vertices for object
  polygon() As Polygon3D    ' List of polygons for object
  '
End Type
Public Sub LoadObject(obj As Object3D)
  '
  ' Here we are "loading" our object. In a future tutorial,
  ' I will show you how to load an "object file" from disk,
  ' but for this, a quick 'n' dirty aproach is used.
  '
  ' Since we are going to show a cube, we need to define the
  ' number of vertices and polygons make up the cube. So, we
  ' come up with eight vertices, and 12 polygons (6 faces,
  ' each face is composed of 2 triangles, ergo 12 polys).
  '
  ReDim obj.vertex(7), obj.polygon(11)
  '
  ' Now we load up out vertex data (see tutor1.gif for more
  ' point layout details)
  '
  obj.vertex(0).x = -1: obj.vertex(0).y = -1: obj.vertex(0).z = -1
  obj.vertex(1).x = -1: obj.vertex(1).y = -1: obj.vertex(1).z = 1
  obj.vertex(2).x = 1: obj.vertex(2).y = -1: obj.vertex(2).z = 1
  obj.vertex(3).x = 1: obj.vertex(3).y = -1: obj.vertex(3).z = -1
  obj.vertex(4).x = -1: obj.vertex(4).y = 1: obj.vertex(4).z = -1
  obj.vertex(5).x = -1: obj.vertex(5).y = 1: obj.vertex(5).z = 1
  obj.vertex(6).x = 1: obj.vertex(6).y = 1: obj.vertex(6).z = 1
  obj.vertex(7).x = 1: obj.vertex(7).y = 1: obj.vertex(7).z = -1
  '
  ' Finally we set up our vertex pointer. If you notice, we are
  ' listing the vertices in a counter-clockwise manner, so that
  ' later on we will be able to make use of this (in another
  ' tutorial) to determine when a surface is hidden or not.
  ' This isn't necessary for this tutorial, but I am putting it in
  ' here now for the future...
  '
  ' Top
  '
  obj.polygon(0).v_pointer(0) = 0: obj.polygon(0).v_pointer(1) = 2: obj.polygon(0).v_pointer(2) = 1
  obj.polygon(0).colr = RGB(255, 0, 0)
  obj.polygon(1).v_pointer(0) = 0: obj.polygon(1).v_pointer(1) = 3: obj.polygon(1).v_pointer(2) = 2
  obj.polygon(1).colr = RGB(255, 0, 0)
  '
  ' Left
  '
  obj.polygon(2).v_pointer(0) = 4: obj.polygon(2).v_pointer(1) = 0: obj.polygon(2).v_pointer(2) = 1
  obj.polygon(2).colr = RGB(255, 0, 0)
  obj.polygon(3).v_pointer(0) = 4: obj.polygon(3).v_pointer(1) = 1: obj.polygon(3).v_pointer(2) = 5
  obj.polygon(3).colr = RGB(255, 0, 0)
  '
  ' Back
  '
  obj.polygon(4).v_pointer(0) = 5: obj.polygon(4).v_pointer(1) = 1: obj.polygon(4).v_pointer(2) = 2
  obj.polygon(4).colr = RGB(255, 0, 0)
  obj.polygon(5).v_pointer(0) = 5: obj.polygon(5).v_pointer(1) = 2: obj.polygon(5).v_pointer(2) = 6
  obj.polygon(5).colr = RGB(255, 0, 0)
  '
  ' Right
  '
  obj.polygon(6).v_pointer(0) = 7: obj.polygon(6).v_pointer(1) = 2: obj.polygon(6).v_pointer(2) = 3
  obj.polygon(6).colr = RGB(255, 0, 0)
  obj.polygon(7).v_pointer(0) = 7: obj.polygon(7).v_pointer(1) = 6: obj.polygon(7).v_pointer(2) = 2
  obj.polygon(7).colr = RGB(255, 0, 0)
  '
  ' Bottom
  '
  obj.polygon(8).v_pointer(0) = 4: obj.polygon(8).v_pointer(1) = 5: obj.polygon(8).v_pointer(2) = 6
  obj.polygon(8).colr = RGB(255, 0, 0)
  obj.polygon(9).v_pointer(0) = 4: obj.polygon(9).v_pointer(1) = 6: obj.polygon(9).v_pointer(2) = 7
  obj.polygon(9).colr = RGB(255, 0, 0)
  '
  ' Front
  '
  obj.polygon(10).v_pointer(0) = 4: obj.polygon(10).v_pointer(1) = 3: obj.polygon(10).v_pointer(2) = 0
  obj.polygon(10).colr = RGB(255, 0, 0)
  obj.polygon(11).v_pointer(0) = 4: obj.polygon(11).v_pointer(1) = 7: obj.polygon(11).v_pointer(2) = 3
  obj.polygon(11).colr = RGB(255, 0, 0)
  '
End Sub
Public Sub DisplayObject(obj As Object3D, frm As Object)
  '
  ' Draw the object on the form, one polygon (triangle) at
  ' a time...
  '
  Dim poly As Long
  '
  For poly = 0 To UBound(obj.polygon())
    '
    Call DrawTriangle(obj, poly, frm)
    '
  Next
  '
End Sub
Private Sub DrawTriangle(obj As Object3D, poly As Long, frm As Object)
  '
  ' Draw a triangle on the form...
  '
  Dim x0 As Long, y0 As Long
  Dim x1 As Long, y1 As Long
  Dim x2 As Long, y2 As Long
  '
  x0 = obj.vertex(obj.polygon(poly).v_pointer(0)).sx
  y0 = obj.vertex(obj.polygon(poly).v_pointer(0)).sy
  x1 = obj.vertex(obj.polygon(poly).v_pointer(1)).sx
  y1 = obj.vertex(obj.polygon(poly).v_pointer(1)).sy
  x2 = obj.vertex(obj.polygon(poly).v_pointer(2)).sx
  y2 = obj.vertex(obj.polygon(poly).v_pointer(2)).sy
  '
  frm.Line (x0, y0)-(x1, y1), obj.polygon(poly).colr
  frm.Line -(x2, y2), obj.polygon(poly).colr
  frm.Line -(x0, y0), obj.polygon(poly).colr
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
    obj.vertex(pnt).sx = (frm.ScaleWidth / 2) + ((obj.vertex(pnt).x * 300) / dz)
    obj.vertex(pnt).sy = (frm.ScaleHeight / 2) + ((obj.vertex(pnt).y * 300) / dz)
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
    x0 = Cos(yaw) * obj.vertex(pnt).x + Sin(yaw) * obj.vertex(pnt).z
    y0 = obj.vertex(pnt).y
    z0 = Sin(yaw) * obj.vertex(pnt).x - Cos(yaw) * obj.vertex(pnt).z
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
    obj.vertex(pnt).x = x2
    obj.vertex(pnt).y = y2
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
    obj.vertex(pnt).x = obj.vertex(pnt).x * xs
    obj.vertex(pnt).y = obj.vertex(pnt).y * ys
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
    obj.vertex(pnt).x = obj.vertex(pnt).x + tx
    obj.vertex(pnt).y = obj.vertex(pnt).y + ty
    obj.vertex(pnt).z = obj.vertex(pnt).z + tz
    '
  Next
  '
End Sub
