# Cube in Modified (MPLG) format

cube 8 12       # 8 vertices, 12 polygons

# Vertex data
#
# Modifications to the standard PLG format occur in this section.
# The vertex data is presented as an X,Y,Z triple followed by a
# color value for the vertex. This color value is defined as
# follows:
#
#   RRGGBBAA
#
#   RR=Red Value (00-FF)
#   GG=Green Value (00-FF)
#   BB=Blue Value (00-FF)
#   AA=Alpha Value (00-FF)
#
# The only value I haven't played with extensively has been the
# alpha value - but presumably, setting it and the other vertex
# alpha values for a polygon would allow a "see-through" effect.
# Play with it, and see - tell me what you find...

-1 -1 -1 FF000000
-1 -1  1 00FF0000
 1 -1  1 0000FF00
 1 -1 -1 FFFF0000
-1  1 -1 FF00FF00
-1  1  1 00FFFF00
 1  1  1 FFFFFF00
 1  1 -1 00000000

# Polygon data
#
# Modifications to the standard PLG format also occur in this
# section. First off, the color number convention has changed so
# that VB color values can be supported. In this scheme, the
# color is represented by four bytes - if the first byte is zero,
# then the next three bytes represent an RGB color value:
#
#   00RRGGBB
#
# Other values for the first byte will be defined in the future.
#
# The last change is that number of vertices per polygon is fixed
# at three, and so that value position has been dropped from the
# standard...

00FF0000 0 2 1       # color number FF0000, 3 vertices
00FF0000 0 3 2       # color number FF0000, 3 vertices
0000FF00 4 0 1
0000FF00 4 1 5
000000FF 5 1 2
000000FF 5 2 6
00FFFFFF 7 2 3
00FFFFFF 7 6 2
00FFFF00 4 5 6
00FFFF00 4 6 7
00FF00FF 4 3 0
00FF00FF 4 7 3

 