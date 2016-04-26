VERSION 5.00
Begin VB.Form MainForm 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Affine Texturemapped Cube by Andrew L. Ayers"
   ClientHeight    =   3615
   ClientLeft      =   1320
   ClientTop       =   705
   ClientWidth     =   5055
   Icon            =   "MainForm.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   241
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   337
   Begin VB.Timer Timer2 
      Interval        =   1000
      Left            =   10320
      Top             =   3600
   End
   Begin VB.Timer Timer1 
      Interval        =   1
      Left            =   10920
      Top             =   3600
   End
   Begin VB.Image TextureImage 
      Height          =   2895
      Left            =   9960
      Top             =   360
      Visible         =   0   'False
      Width           =   3495
   End
   Begin VB.Label Label1 
      BackStyle       =   0  'Transparent
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   3240
      Width           =   3495
   End
   Begin VB.Image ViewportImage 
      Height          =   3000
      Left            =   120
      Stretch         =   -1  'True
      Top             =   120
      Width           =   4800
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
'*     A Visual Basic Affine Texturemapped Cube Engine     *
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
'* Well, after all this time - I have finally decided to   *
'* give what you all have been clamoring for - the code!   *
'* To be honest, I should have released this a long time   *
'* ago, but I became blinded by another thing - money.     *
'* I had it in my mind to develop this as a technology     *
'* to use in other projects - as an ActiveX control or     *
'* something, and possibly sell it. I never did anything   *
'* further, and it languished. I hope someone will benefit *
'* from it now, however.                                   *
'*                                                         *
'* So let me know what you think, ok?                      *
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
  ' Initialize the viewport (array pointer to bitmap)
  '
  Call PictArrayInit1D(ViewportImage, App.Path + "\blank.bmp", sa1, bmp1, Viewport())
  Call PictArrayInit1D(TextureImage, App.Path + "\textures.bmp", sa2, bmp2, Textures())
  '
  all_stop = False
  '
  Call LoadWorld
  '
End Sub
Private Sub Form_Unload(Cancel As Integer)
  '
  ' Destroy pointer to bitmap array and free up memory
  '
  all_stop = True
  '
  Call PictArrayKill(Viewport())
  Call PictArrayKill(Textures())
  '
End Sub
Private Sub Timer1_Timer()
  '
  Timer1.Enabled = False: FPS = 0
  '
  Do Until all_stop
    '
    Call ClearViewport
    '
    Call DrawWorld
    '
    ViewportImage.Refresh
    '
    FPS = FPS + 1
    '
    DoEvents
    '
  Loop
  '
End Sub
Private Sub Timer2_Timer()
  '
  Label1.Caption = "FPS = " + Format$(FPS)
  '
  FPS = 0
  '
End Sub
