VERSION 5.00
Begin VB.Form MainForm 
   AutoRedraw      =   -1  'True
   BackColor       =   &H00000000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Mountain Demo by Andrew L. Ayers"
   ClientHeight    =   5970
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   7545
   Icon            =   "MainForm.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   398
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   503
   StartUpPosition =   3  'Windows Default
   Begin VB.Timer Timer1 
      Interval        =   50
      Left            =   1920
      Top             =   2280
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
'*                      Mountain Demo                      *
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
'* Why is this called Mountain? Well, the original intent  *
'* was to create a VB engine to do landscape generation -  *
'* kinda like VistaPro or Bryce, just on a simpler scale.  *
'* I never got the mountain generation code created, just  *
'* the grid generator and rotation code. Nothing fancy,    *
'* just wireframe. What you might try to do is add texture *
'* mapping in, or solid mode with shading. Another thing   *
'* to try would be to create a system so that the user can *
'* enter in a "Z" function (as a function of X and Y) and  *
'* have the engine plot that. Right now, you can change    *
'* the function in the CreateGrid() portion...             *
'*                                                         *
'* As always, let me know what you think, ok?              *
'*                                                         *
'* Thank you,                                              *
'*                                                         *
'* Andrew L. Ayers                                         *
'* andrewa@indirect.com                                    *
'*                                                         *
'***********************************************************
'
Dim all_stop As Boolean
Private Sub Form_Load()
  '
  all_stop = False
  '
  Call CreateGrid
  '
End Sub
Private Sub Form_Unload(Cancel As Integer)
  '
  all_stop = True
  '
End Sub
Private Sub Timer1_Timer()
  '
  Static yaw As Single, pit As Single
  '
  Do
    MainForm.Cls
    '
    yaw = yaw + 0.05: If yaw > 6.28 Then yaw = 0
    pit = pit + 0.03: If pit > 6.28 Then pit = 0
    '
    Call DrawMesh(yaw, pit, 0)
    '
    DoEvents
    '
  Loop Until all_stop
  '
End Sub
