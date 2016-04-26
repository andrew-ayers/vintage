Attribute VB_Name = "GeneralRoutines"
Public Function TrimExtraSpace(ByVal valu As String) As String
  '
  ' Remove leading and trailing spaces
  '
  Dim flag As Boolean
  '
  flag = False
  '
  valu = RTrim(LTrim(valu))
  '
  ' Loop through string and remove extra spaces between
  ' characters...
  '
  For t& = 1 To Len(valu)
    '
    this$ = Mid$(valu, t&, 1)
    '
    If this$ <> " " Then
      '
      If flag = True Then
        '
        TrimExtraSpace = TrimExtraSpace + " "
        '
        flag = False
        '
      End If
      '
      TrimExtraSpace = TrimExtraSpace + this$
      '
    Else
      '
      flag = True
      '
    End If
    '
  Next
  '
End Function
Public Function Field(strg As String, sep As String, cnt As Integer) As String
  '
  ' Finds occurance (as passed in "cnt" argument) inside of string "strg", in
  ' of fields delimited by "sep".
  '
  ' Example:
  '
  '   a$ = Field("XYZ|ZTX|ABC","|",2)
  '
  '   returns
  '
  '   a$ = "ZTX"
  '
  Field = ""
  '
  strg2$ = sep & strg & sep
  '
  If cnt < 1 Or Len(sep) <> 1 Then Exit Function
  '
  ls& = Len(strg2$)
  '
  For t& = 1 To ls&
    '
    If Mid$(strg2$, t&, 1) = sep Then
      '
      If cnt = 1 Then
        '
        BegStrg& = t& + 1
        '
      ElseIf cnt = 0 Then
        '
        EndStrg& = t& - 1
        '
        Field = Mid$(strg2$, BegStrg&, EndStrg& - BegStrg& + 1)
        '
        Exit For
        '
      End If
      '
      cnt = cnt - 1
      '
    End If
    '
  Next
  '
End Function




