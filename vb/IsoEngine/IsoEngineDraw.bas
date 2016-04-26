Attribute VB_Name = "IsoEngineDraw"
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
Private Type MAP_STRUCTURE
  '
  num As Byte
  tile(10) As Byte
  height(10) As Byte
  '
End Type
'
Private map(30, 30) As MAP_STRUCTURE
Private block_width(10) As Long, block_height(10) As Long
Private max_x As Long, max_y As Long
Public Sub LoadTiles()
  '
  MainForm.Picture1.Picture = LoadPicture(App.Path + "\tiles1.bmp")
  '
  block_width(0) = 32: block_height(0) = 15
  block_width(1) = 32: block_height(1) = 15
  block_width(2) = 32: block_height(2) = 30
  '
End Sub
Public Sub LoadMap()
  '
  max_x = 30: max_y = 30
  '
  Dim a$(30), x As Long, y As Long
  '
  a$(0) = "..............................."
  a$(1) = "..............................."
  a$(2) = "..............................."
  a$(3) = ".X----X........................"
  a$(4) = ".------o......................."
  a$(5) = ".------oooooooooo.............."
  a$(6) = ".X----X....o....o.............."
  a$(7) = "...o.......o....o.............."
  a$(8) = "...o.......o....o.............."
  a$(9) = "...ooooooooo....o.............."
  a$(10) = "..............ooo.............."
  a$(11) = ".............oO.Oo............."
  a$(12) = ".............o.X.o............."
  a$(13) = ".............oO.Oo............."
  a$(14) = "..............ooo.............."
  a$(15) = "..............................."
  a$(16) = "..............................."
  a$(17) = "..............................."
  a$(18) = "..............................."
  a$(19) = "..............................."
  a$(20) = "..............................."
  a$(21) = "..............................."
  a$(22) = "..............................."
  a$(23) = "..............................."
  a$(24) = "..............................."
  a$(25) = "..............................."
  a$(26) = "..............................."
  a$(27) = "..............................."
  a$(28) = "..............................."
  a$(29) = "..............................."
  a$(30) = "..............................."
  '
  For y = 0 To max_y
    '
    lin$ = a$(y)
    '
    For x = 0 To max_x
      '
      tile$ = Mid$(lin$, x + 1, 1)
      '
      Select Case tile$
        Case "."
          '
          ' Grass
          '
          map(x, y).num = 1
          map(x, y).tile(0) = 0
          map(x, y).height(0) = 0
          '
        Case "o"
          '
          ' Path
          '
          map(x, y).num = 1
          map(x, y).tile(0) = 1
          map(x, y).height(0) = 0
          '
        Case "O"
          '
          ' Wall
          '
          map(x, y).num = 1
          map(x, y).tile(0) = 2
          map(x, y).height(0) = 0
          '
        Case "X"
          '
          ' Tall Wall
          '
          map(x, y).num = 2
          map(x, y).tile(0) = 2
          map(x, y).height(0) = 0
          map(x, y).tile(1) = 2
          map(x, y).height(1) = 14
          '
        Case "-"
          '
          ' Lentil
          '
          map(x, y).num = 2
          map(x, y).tile(0) = 0
          map(x, y).height(0) = 0
          map(x, y).tile(1) = 2
          map(x, y).height(1) = 14
          '
      End Select
      '
    Next
    '
  Next
  '
End Sub
Public Sub DrawMap(px As Long, py As Long)
  '
  Dim screenx As Long, screeny As Long
  Dim x As Long, y As Long, k As Long
  Dim temp_px As Long, temp_py As Long
  Dim wid As Long, hgt As Long
  Dim tile_to_draw As Byte
  Dim height_to_draw As Byte
  '
  MainForm.Picture2.Cls
  '
  screeny = 8
  '
  For y = 0 To 46
    '
    temp_px = px: temp_py = py
    '
    If y Mod 2 <> 0 Then screenx = 0 Else screenx = 16
    '
    For x = 0 To 11
      '
      For k = 0 To map(temp_px, temp_py).num - 1
        '
        tile_to_draw = map(temp_px, temp_py).tile(k)
        height_to_draw = map(temp_px, temp_py).height(k)
        wid = block_width(tile_to_draw)
        hgt = block_height(tile_to_draw)
        '
        Call Block_Draw(tile_to_draw, screenx - wid, screeny - hgt - height_to_draw, wid, hgt)
        '
      Next
      '
      screenx = screenx + 32
      '
      temp_px = temp_px + 1: temp_py = temp_py - 1
      '
      ' Bounds check
      '
      If temp_px > max_x Then temp_px = 0
      If temp_py < 0 Then temp_py = max_y
      '
    Next
    '
    screeny = screeny + 8
    '
    If (y + 1) Mod 2 = 0 Then
      '
      px = px + 1: If px > max_x Then px = 0
      '
    Else
      '
      py = py + 1: If py > max_y Then py = 0
      '
    End If
    '
  Next
  '
  Call FlipBuffer(MainForm.Picture2, MainForm)
  '
End Sub
Private Sub Block_Draw(tile_to_draw As Byte, posx As Long, posy As Long, wid As Long, hgt As Long)
  '
  Dim fromx As Long, fromy As Long
  '
  Select Case tile_to_draw
    Case 0
      '
      fromx = 0: fromy = 0
      '
    Case 1
      '
      fromx = 32: fromy = 0
      '
    Case 2
      '
      fromx = 64: fromy = 0
      '
  End Select
  '
  Call PlaceSprite(MainForm.Picture2, MainForm.Picture1, fromx, fromy, posx, posy, wid, hgt)
  '
End Sub
