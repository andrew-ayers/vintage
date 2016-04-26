VERSION 5.00
Begin VB.Form frmDemoWolfram 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Wolfram's 1D Cellular Automata"
   ClientHeight    =   8190
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   9630
   Icon            =   "frmDemoWolfram.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   8190
   ScaleWidth      =   9630
   StartUpPosition =   3  'Windows Default
   Begin VB.ComboBox cboMethod 
      Height          =   315
      Left            =   1920
      Style           =   2  'Dropdown List
      TabIndex        =   6
      Top             =   120
      Width           =   1935
   End
   Begin VB.CommandButton cmdStop 
      Caption         =   "Stop"
      Height          =   255
      Left            =   8520
      TabIndex        =   5
      Top             =   120
      Width           =   975
   End
   Begin VB.TextBox txtRule 
      Alignment       =   1  'Right Justify
      Height          =   285
      Left            =   4920
      TabIndex        =   4
      Top             =   120
      Width           =   735
   End
   Begin VB.CommandButton cmdNext 
      Caption         =   ">"
      Height          =   255
      Left            =   6600
      TabIndex        =   3
      Top             =   120
      Width           =   375
   End
   Begin VB.CommandButton cmdPrev 
      Caption         =   "<"
      Height          =   255
      Left            =   6120
      TabIndex        =   2
      Top             =   120
      Width           =   375
   End
   Begin VB.CommandButton cmdRun 
      Caption         =   "Run"
      Height          =   255
      Left            =   7320
      TabIndex        =   1
      Top             =   120
      Width           =   975
   End
   Begin VB.PictureBox Picture1 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      ForeColor       =   &H00C00000&
      Height          =   7455
      Left            =   120
      ScaleHeight     =   493
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   621
      TabIndex        =   0
      Top             =   600
      Width           =   9375
   End
   Begin VB.Label lblRule 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Rule #"
      Height          =   255
      Left            =   4350
      TabIndex        =   8
      Top             =   165
      Width           =   495
   End
   Begin VB.Label lblMethod 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "Automata Run Method :"
      Height          =   255
      Left            =   135
      TabIndex        =   7
      Top             =   165
      Width           =   1695
   End
End
Attribute VB_Name = "frmDemoWolfram"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim lngRuleNum As Long, lngRunMethod As Long
Dim isRunning As Boolean
Dim vbForeground As Long, vbBackground As Long
Private Sub cboMethod_Click()
  '
  lngRunMethod = CLng(Trim$(Right$(cboMethod.Text, 2)))
  '
  isRunning = False
  '
End Sub
Private Sub cmdNext_Click()
  '
  lngRuleNum = lngRuleNum + 1
  '
  If lngRuleNum > 255 Then lngRuleNum = 255
  '
  txtRule.Text = Format$(lngRuleNum)
  '
  isRunning = False
  '
End Sub
Private Sub cmdPrev_Click()
  '
  lngRuleNum = lngRuleNum - 1
  '
  If lngRuleNum < 0 Then lngRuleNum = 0
  '
  txtRule.Text = Format$(lngRuleNum)
  '
  isRunning = False
  '
End Sub
Private Sub cmdRun_Click()
  '
  Call RunAutomata
  '
End Sub
Private Sub cmdStop_Click()
  '
  isRunning = False
  '
End Sub
Private Sub Form_Load()
  '
  lngRuleNum = 110
  '
  txtRule.Text = Format$(lngRuleNum)
  '
  isRunning = False
  '
  vbForeground = RGB(255, 255, 255)
  vbBackground = RGB(0, 128, 192)
  '
  Picture1.BackColor = vbBackground
  '
  cboMethod.Clear
  '
  For t = 1 To 16
    '
    cboMethod.AddItem "Run Method " & Format$(t)
    '
  Next
  '
  cboMethod.Text = cboMethod.List(0)
  '
End Sub
Private Sub Form_Unload(Cancel As Integer)
  '
  isRunning = False
  '
End Sub
Private Sub txtRule_Change()
  '
  lngRuleNum = CLng(txtRule.Text)
  '
  isRunning = False
  '
End Sub
Private Sub RunAutomata()
  '
  Dim x As Long, y As Long
  Dim thisInput As Long
  '
  Picture1.Cls
  '
  Picture1.PSet (Picture1.ScaleWidth \ 2, 0), vbForeground
  '
  isRunning = True
  '
  For y = 1 To Picture1.ScaleHeight - 2
    '
    For x = 1 To Picture1.ScaleWidth - 2
      '
      Select Case lngRunMethod
        Case 1: If GetOutputA(GetInputA(x, y)) Then Picture1.PSet (x, y), vbForeground
        Case 2: If GetOutputA(GetInputB(x, y)) Then Picture1.PSet (x, y), vbForeground
        Case 3: If GetOutputA(GetInputC(x, y)) Then Picture1.PSet (x, y), vbForeground
        Case 4: If GetOutputA(GetInputD(x, y)) Then Picture1.PSet (x, y), vbForeground
        '
        Case 5: If GetOutputB(GetInputA(x, y)) Then Picture1.PSet (x, y), vbForeground
        Case 6: If GetOutputB(GetInputB(x, y)) Then Picture1.PSet (x, y), vbForeground
        Case 7: If GetOutputB(GetInputC(x, y)) Then Picture1.PSet (x, y), vbForeground
        Case 8: If GetOutputB(GetInputD(x, y)) Then Picture1.PSet (x, y), vbForeground
        '
        Case 9: If GetOutputC(GetInputA(x, y)) Then Picture1.PSet (x, y), vbForeground
        Case 10: If GetOutputC(GetInputB(x, y)) Then Picture1.PSet (x, y), vbForeground
        Case 11: If GetOutputC(GetInputC(x, y)) Then Picture1.PSet (x, y), vbForeground
        Case 12: If GetOutputC(GetInputD(x, y)) Then Picture1.PSet (x, y), vbForeground
        '
        Case 13: If GetOutputD(GetInputA(x, y)) Then Picture1.PSet (x, y), vbForeground
        Case 14: If GetOutputD(GetInputB(x, y)) Then Picture1.PSet (x, y), vbForeground
        Case 15: If GetOutputD(GetInputC(x, y)) Then Picture1.PSet (x, y), vbForeground
        Case 16: If GetOutputD(GetInputD(x, y)) Then Picture1.PSet (x, y), vbForeground
        '
      End Select
      '
    Next
    '
    If Not (isRunning) Then Exit For
    '
    DoEvents
    '
  Next
  '
End Sub
Private Function GetInputA(x As Long, y As Long) As Long
  '
  GetInputA = 0
  '
  If Picture1.Point(x - 1, y - 1) <> vbBackground Then GetInputA = GetInputA + 4
  If Picture1.Point(x, y - 1) <> vbBackground Then GetInputA = GetInputA + 2
  If Picture1.Point(x + 1, y - 1) <> vbBackground Then GetInputA = GetInputA + 1
  '
End Function
Private Function GetInputB(x As Long, y As Long) As Long
  '
  GetInputB = 0
  '
  If Picture1.Point(x - 1, y - 1) <> vbBackground Then GetInputB = GetInputB + 1
  If Picture1.Point(x, y - 1) <> vbBackground Then GetInputB = GetInputB + 2
  If Picture1.Point(x + 1, y - 1) <> vbBackground Then GetInputB = GetInputB + 4
  '
End Function
Private Function GetInputC(x As Long, y As Long) As Long
  '
  GetInputC = 7
  '
  If Picture1.Point(x - 1, y - 1) <> vbBackground Then GetInputC = GetInputC - 4
  If Picture1.Point(x, y - 1) <> vbBackground Then GetInputC = GetInputC - 2
  If Picture1.Point(x + 1, y - 1) <> vbBackground Then GetInputC = GetInputC - 1
  '
End Function
Private Function GetInputD(x As Long, y As Long) As Long
  '
  GetInputD = 7
  '
  If Picture1.Point(x - 1, y - 1) <> vbBackground Then GetInputD = GetInputD - 1
  If Picture1.Point(x, y - 1) <> vbBackground Then GetInputD = GetInputD - 2
  If Picture1.Point(x + 1, y - 1) <> vbBackground Then GetInputD = GetInputD - 4
  '
End Function
Private Function GetOutputA(thisInput As Long) As Boolean
  '
  GetOutputA = False
  '
  If (lngRuleNum And (2 ^ thisInput)) > 0 Then GetOutputA = True
  '
End Function
Private Function GetOutputB(thisInput As Long) As Boolean
  '
  ' Mirror of GetOutputA
  '
  GetOutputB = True
  '
  If (lngRuleNum And (2 ^ thisInput)) > 0 Then GetOutputB = False
  '
End Function
Private Function GetOutputC(thisInput As Long) As Boolean
  '
  ' Inverted bitmask of GetOutputA
  '
  GetOutputC = False
  '
  If (lngRuleNum And (2 ^ (7 - thisInput))) > 0 Then GetOutputC = True
  '
End Function
Private Function GetOutputD(thisInput As Long) As Boolean
  '
  ' Mirror of GetOutputC
  '
  GetOutputD = True
  '
  If (lngRuleNum And (2 ^ (7 - thisInput))) > 0 Then GetOutputD = False
  '
End Function
