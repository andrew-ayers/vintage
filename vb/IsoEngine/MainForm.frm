VERSION 5.00
Begin VB.Form MainForm 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "IsoEngine - Use arrows or numpad to move"
   ClientHeight    =   4305
   ClientLeft      =   45
   ClientTop       =   615
   ClientWidth     =   5040
   ClipControls    =   0   'False
   Icon            =   "MainForm.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   287
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   336
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox Picture2 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   4335
      Left            =   0
      ScaleHeight     =   289
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   337
      TabIndex        =   1
      Top             =   0
      Visible         =   0   'False
      Width           =   5055
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   1035
      Left            =   5520
      ScaleHeight     =   69
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   105
      TabIndex        =   0
      Top             =   4560
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.Menu About 
      Caption         =   "About"
   End
End
Attribute VB_Name = "MainForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'***********************************************************
'*                                                         *
'*  T H E   P H O E N I X   G A R A G E   P R E S E N T S  *
'*                                                         *
'*              An Isometric Tile Engine Demo              *
'*                                                         *
'*            Source Code Release July 29, 1999            *
'*                                                         *
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
'*                                                         *
'* Here's an old one out of my collection - felt I would   *
'* go ahead and release it for others to see - yet another *
'* one of my projects that just got to the "demo" stage.   *
'*                                                         *
'* Once again, let me know what you think, ok?                      *
'*                                                         *
'* Thank you,                                              *
'*                                                         *
'* Andrew L. Ayers                                         *
'* andrewa@indirect.com                                    *
'*                                                         *
'***********************************************************
'
Dim px As Long, py As Long
Private Sub About_Click()
  '
  Call Form_Click
  '
End Sub
Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
  '
  Select Case KeyCode
    Case vbKeyEscape
      Unload Me
    Case vbKeyNumpad9
      py = py - 1: If py < 0 Then py = 30
      Call Form_Paint
    Case vbKeyNumpad8, vbKeyUp
      px = px - 1: If px < 0 Then px = 30
      py = py - 1: If py < 0 Then py = 30
      Call Form_Paint
    Case vbKeyNumpad7
      px = px - 1: If px < 0 Then px = 30
      Call Form_Paint
    Case vbKeyNumpad6, vbKeyRight
      px = px + 1: If px > 30 Then px = 0
      py = py - 1: If py < 0 Then py = 30
      Call Form_Paint
    Case vbKeyNumpad5
      px = 0: py = 0
      Call Form_Paint
    Case vbKeyNumpad4, vbKeyLeft
      px = px - 1: If px < 0 Then px = 30
      py = py + 1: If py > 30 Then py = 0
      Call Form_Paint
    Case vbKeyNumpad3
      px = px + 1: If px > 30 Then px = 0
      Call Form_Paint
    Case vbKeyNumpad2, vbKeyDown
      px = px + 1: If px > 30 Then px = 0
      py = py + 1: If py > 30 Then py = 0
      Call Form_Paint
    Case vbKeyNumpad1
      py = py + 1: If py > 30 Then py = 0
      Call Form_Paint
  End Select
  '
End Sub
Private Sub Form_Load()
  '
  Call LoadTiles
  Call LoadMap
  '
  px = 0: py = 0
  '
End Sub
Private Sub Form_Paint()
  '
  Dim hx As Long, hy As Long
  '
  hx = px: hy = py
  '
  Call DrawMap(hx, hy)
  '
End Sub
Private Sub Form_Click()
  '
  msg$ = "VB5 IsoEngine" + Chr$(10) + Chr$(10)
  msg$ = msg$ + "Copyright (C) 1998 by Andrew L. Ayers" + Chr$(10) + Chr$(10)
  msg$ = msg$ + "This program is protected under terms" + Chr$(10)
  msg$ = msg$ + "as set forth under the GPL - please see" + Chr$(10)
  msg$ = msg$ + "the file gpl.html for more details." + Chr$(10) + Chr$(10)
  msg$ = msg$ + "For more info email me: andrewa@indirect.com"
  '
  MsgBox msg$
  '
End Sub
