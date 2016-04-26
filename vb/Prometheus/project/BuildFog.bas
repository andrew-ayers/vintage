Attribute VB_Name = "BuildFog"
'***********************************************************
'*                                                         *
'* Copyright (C) 1999-2000 by Andrew L. Ayers              *
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
'
' To hold transparency lookup table for 255 levels of transparency
'
Public trans(255, 255, 8) As Byte
Public GAMMA As Integer, FOG_ENABLE As Boolean
Public USE_COLOR As Boolean, FORE_COLOR As Integer, FADE_TO_COL As Integer
Public CEILING_COLOR As Integer, FLOOR_COLOR As Integer
'
Dim pal1(8) As Byte
Dim pal2(8) As Byte
Sub BuildFogTransTable()
  '
  ' Set up the default rgb levels of a 3-3-2 colour palette
  '
  pal1(1) = 115: pal1(2) = 200: pal1(3) = 255
  pal2(1) = 36: pal2(2) = 73: pal2(3) = 109
  pal2(4) = 146: pal2(5) = 182: pal2(6) = 219
  pal2(7) = 255
  '
  ' These hold the RGB levels of
  ' a (source), b (destination) and c(combination)
  '
  Dim R_a%, B_a%, G_a%
  Dim R_b%, B_b%, G_b%
  Dim R_c%, B_c%, G_c%
  '
  ' Work out transparency/alpha levels
  ' for each possible 8 bit source/destination/alpha level
  '
  For a% = 0 To 255
    '
    ' Work out RGB levels of the source palette
    '
    B_a% = a% Mod 4
    G_a% = (a% \ 4) Mod 8
    R_a% = a% \ 32
    '
    For b% = 0 To 255
      '
      ' Work out RGB levels of the destination palette
      '
      B_b% = b% Mod 4
      G_b% = (b% \ 4) Mod 8
      R_b% = b% \ 32
      '
      For C% = 0 To 8
        '
        ' Work out the average RGB levels
        '
        R_c% = ((pal2(R_a%) * (8 - C%)) + (pal2(R_b%) * C%)) / 8
        B_c% = ((pal1(B_a%) * (8 - C%)) + (pal1(B_b%) * C%)) / 8
        G_c% = ((pal2(G_a%) * (8 - C%)) + (pal2(G_b%) * C%)) / 8
        '
        ' Work out the closest colour match in the 3-3-2 palette
        ' and store the resulting colour in the lookup-table
        '
        trans(a%, b%, C%) = CByte((B_c% \ 85) + (G_c% \ 36) * 4 + (R_c% \ 36) * 32)
        '
      Next
      '
    Next
    '
  Next
  '
End Sub
