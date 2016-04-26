DECLARE SUB ScanLine (line$, badflag!)
'
' Define "bad" phrases to look for
'
DIM SHARED bad$(1000)
'
bad$(0) = "FIVE NAMES"
bad$(1) = "LIFETIME OPPORTUNITY"
bad$(2) = "GET RICH QUICK"
'
' Set up mail folders/directories
'
maildir$ = "c:\myfiles\programs\ntscg201\mail\"
scanfile$ = maildir$ + "inbox"
tempfile$ = "c:\temp\tempmail"
goodfile$ = maildir$ + "goodmail"
badfile$ = maildir$ + "badmail"
'
' Open folders
'
OPEN scanfile$ FOR INPUT AS #1
OPEN tempfile$ FOR OUTPUT AS #2
OPEN goodfile$ FOR APPEND AS #3
OPEN badfile$ FOR APPEND AS #4
OPEN fromtemp$ FOR INPUT AS #5
'
' Clear "bad" flag
'
bad = 0
'
' Loop through our inbox messages, one line at a time...
'
WHILE NOT (EOF(1))
  '
  ' Get a mail line from our "in" box
  '
  LINE INPUT #1, a$
  '
  ' Is it a new message?
  '
  IF UCASE$(LEFT$(a$, 4)) = "FROM" THEN
    '
    ' Yes, so put last message where it belongs
    '
    IF bad = 1 THEN
      '
      ' Oh no! It's totally worthless! - Write to bad folder
      '
      WHILE NOT (EOF(5))
        LINE INPUT #5, b$
        PRINT #4, b$
      WEND
    ELSE
      '
      ' Yea! Helpful mail! - Write to good folder
      '
      WHILE NOT (EOF(5))
        LINE INPUT #5, b$
        PRINT #3, b$
      WEND
    END IF
    '
    ' Now reset our flag
    '
    bad = 0
    '
  END IF
  '
  ' Scan the message for "bad" words
  '
  CALL ScanLine(a$, bad)
  '
  ' Write it to the temp file
  '
  PRINT #2, a$
  '
WEND
'
' We are all done, so close up and exit
'
CLOSE
'
END

SUB ScanLine (line$, badflag)
  '
  badflag = 0
  '
  FOR t% = 0 TO 1000
    IF bad$(t%) = "" THEN EXIT FOR
    IF INSTR(line$, bad$(t%)) THEN badflag = 1: EXIT FOR
  NEXT
  '
END SUB

