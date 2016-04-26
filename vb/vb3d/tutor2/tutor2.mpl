# Cube in Modified (MPLG) format

cube 8 12       # 8 vertices, 12 polygons

# Vertex data

-1 -1 -1
-1 -1  1
 1 -1  1
 1 -1 -1
-1  1 -1
-1  1  1
 1  1  1
 1  1 -1

# Polygon data
#
# The modifications to the standard PLG format occur in this
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

 