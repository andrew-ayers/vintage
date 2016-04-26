VERSION 5.00
Begin VB.Form MainForm 
   AutoRedraw      =   -1  'True
   Caption         =   "VB/3D - Tutorial #1"
   ClientHeight    =   3255
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4635
   Icon            =   "MainForm.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   217
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   309
   StartUpPosition =   3  'Windows Default
   Begin VB.Timer Timer2 
      Interval        =   1000
      Left            =   120
      Top             =   600
   End
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   120
      Top             =   120
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
'*                     !!!  VB/3D  !!!                     *
'*                                                         *
'*             A quick and easy approach toward            *
'*      integrating 3D into a Visual Basic application     *
'*                                                         *
'*              Tutorial Lesson #1 - Basic 3D              *
'*                                                         *
'*                      March 1, 1999                      *
'*                                                         *
'*                   Updated July 7, 1999                  *
'*                                                         *
'***********************************************************
'* This tutorial and all code herein is Copyright (c) 1999 *
'* by Andrew L. Ayers. All Rights Reserved. Please feel    *
'* to use and learn from this code.                        *
'***********************************************************
'* In other words people, feel free to use this code in    *
'* whatever way you like, just don't go around claiming    *
'* this code is yours. If you use this code, and think it  *
'* is something worthwhile, please give me some credits. I *
'* would also appreciate it if you let me know about your  *
'* use of it, so I can see what my stuff is being used in  *
'* and who has benefitted from these tutorials. However,   *
'* you are not obligated to do so...                       *
'***********************************************************
'* These tutorials are meant for individuals who have a    *
'* pretty good grounding in VB and programming practices   *
'* and not for "newbies" to coding. They assume you have   *
'* coded at least a few programs in VB, beyond the "Hello  *
'* World" level. Also, the math contained inside the tut-  *
'* orials is advanced stuff - you would do well to know or *
'* have a good understanding of algebra, geometry, and     *
'* trigonometry. These tutorials do _not_ teach the math   *
'* behind the code - other tutorials on the Net do this    *
'* rather well, in far greater and more accurate detail    *
'* than I could go into.                                   *
'*                                                         *
'* These tutorials assume you have never coded nor seen    *
'* anything relating to 3D and Visual Basic, and are thus  *
'* needing a way to get started.                           *
'***********************************************************
'* I would appreciate any suggestions or comments for      *
'* these tutorials, such as improvements, or questions you *
'* would like to see answered. I can't guarantee an answer *
'* to all questions (though I will do my best), and I      *
'* can't guarantee that your suggestion will make it into  *
'* the tutorials, but I will consider all requests, and    *
'* will give credit to the individual(s) asking...         *
'*                                                         *
'* Thank you,                                              *
'*                                                         *
'* Andrew L. Ayers                                         *
'* andrewa@indirect.com                                    *
'***********************************************************
'* Special Form Setup Notes :                              *
'*                                                         *
'* AutoRedraw = True                                       *
'*                                                         *
'* This is set for one reason and one reason only - to     *
'* stop flicker. When this property is set, Windows sets   *
'* up a background buffer to redraw any changes from, such *
'* as changes caused by windows obscuring others, etc. In  *
'* effect this sets up a "free" double buffer. However, it *
'* comes at a cost - speed. Change this to false, and see  *
'* how the speed improves, but it flickers horribly. If    *
'* you are creating a real time 3D app, this would be un-  *
'* acceptable. For certain apps (like a CAD app), it would *
'* be OK. There may be another way, which I will leave up  *
'* to you to explore, and that would be to set up your own *
'* double buffer system using the BitBlt API call. I have  *
'* yet to actually try this, but may later in this series  *
'* of tutorials. Let me know if you do before I do, and if *
'* it works well for you (more speed, less flicker, etc).  *
'*                                                         *
'* ScaleMode = 3 (Pixel)                                   *
'*                                                         *
'* This is set merely as a convenience, since it is far    *
'* easier to work with pixels rather than any of the other *
'* notations. However, mayber by working with one of the   *
'* other systems, and converting between the two, you can  *
'* possibly set up a "real world" system, where, say, one  *
'* pixel equals one millimeter, or whatever...             *
'***********************************************************
'
Private FPS As Long         ' Frames-Per-Second counter
Private all_stop As Boolean ' Used to stop the engine when quitting
Private cube As Object3D    ' Define our object
Private Sub Form_Load()
  '
  Call LoadObject(cube)     ' Load our object
  '
  all_stop = False          ' Reset our runtime flag
  '
End Sub
Private Sub Form_Unload(Cancel As Integer)
  '
  all_stop = True           ' Unloading and quiting, so shut
  '                         ' the engine down...
  '
End Sub
Private Sub Timer1_Timer()
  '
  Dim temp As Object3D      ' Temporary object (because we don't want to modify our "pristine" original)
  Dim ang As Single         ' Rotation angle of the object
  '
  Timer1.Enabled = False    ' Shut down the timer on first tick
  '
  ' Enter infinite loop...
  '
  Do
    temp = cube             ' Get a copy of our object
    '
    ' The following line clears the form - for this tutorial,
    ' it is okay, but a faster way would be to use the "line"
    ' object of the form to "clear" it - however, even this may
    ' cause problems - I noticed that (on my machine at least)
    ' when using this method, the FPS would drop - and stay low,
    ' when something else (like a pop up menu or tooltip) was dis-
    ' played for any reason. I think it may be an AutoRedraw bug...
    '
    Me.Cls
    'Me.Line (0, 0)-(Me.ScaleWidth, Me.ScaleHeight), Me.BackColor, BF
    '
    Call ScaleObject(10, 10, 10, temp)      ' Scale the object
    Call RotateObject(ang, -0.5, 0, temp)  ' Rotate the object
    Call TranslateObject(0, 0, 50, temp)    ' Translate the object
    Call ProjectObject(temp, Me)            ' Project the object
    '
    Call DisplayObject(temp, Me)            ' Draw the object
    '
    FPS = FPS + 1   ' Add a frame to our FPS counter
    '
    ang = ang + 0.02: If ang > 6.282 Then ang = 0 ' Increase the angle
    '
    DoEvents ' Allow for background Windows processing
    '
  Loop Until all_stop   ' Loop until we quit
  '
End Sub
Private Sub Timer2_Timer()
  '
  ' Show how many frames-per-second occurred (in the form caption)
  '
  Me.Caption = "VB/3D - Tutorial #1 - FPS = " + Str$(FPS)
  '
  FPS = 0   ' Zero out counter
  '
End Sub
