DECLARE SUB lcd.driver.init ()
DECLARE SUB lcd.driver.clear ()
DECLARE SUB lcd.driver.home ()
DECLARE SUB lcd.driver.print (line1 AS STRING, line2 AS STRING)
DECLARE SUB lcd.driver.char (chr AS INTEGER)
DECLARE SUB lcd.driver.command (cmd AS INTEGER)
'
LCD.PORTADDRESS = &H378
LCD.DATA = &H378
LCD.STATUS = &H379
LCD.CONTROL = &H37A
'
shared LCD.PORTADDRESS,LCD.DATA,LCD.STATUS,LCD.CONTROL
'
CALL lcd.driver.init
CALL lcd.driver.clear
CALL lcd.driver.home
CALL lcd.driver.print("Hello           ", "There           ")

SUB lcd.driver.char (chr AS INTEGER)
  '
  OUT LCD.DATA, chr
  '
  OUT LCD.CONTROL, INP(LCD.CONTROL) OR &H01
  '
  delay .03
  '
  OUT LCD.CONTROL, INP(LCD.CONTROL) AND &HFE
  OUT LCD.CONTROL, INP(LCD.CONTROL) AND &HF7
  '
END SUB

SUB lcd.driver.clear
  '
  lcd.driver.command (0)
  lcd.driver.command (16)
  '
END SUB

SUB lcd.driver.command (cmd AS INTEGER)
  '
  OUT LCD.CONTROL, INP(LCD.CONTROL) OR &H8
  OUT LCD.DATA, cmd
  OUT LCD.CONTROL, INP(LCD.CONTROL) OR &H1
  OUT LCD.CONTROL, INP(LCD.CONTROL) AND &HFE
  OUT LCD.CONTROL, INP(LCD.CONTROL) AND &HF7
  delay .03
  '
END SUB

SUB lcd.driver.home
  '
  lcd.driver.command (0)
  lcd.driver.command (32)
  '
END SUB

SUB lcd.driver.init
  '
  DIM n AS INTEGER
  DIM command(3) AS INTEGER
  DIM b AS INTEGER
  DIM cmd AS INTEGER
  '
  PRINT "HD44780 16x2 Driver 4-bit (PowerBasic 3.2) V1.0"
  PRINT "by Andrew L. Ayers, from C source by Eduardo"
  PRINT "Kurita. The author may be reached at:"
  PRINT "andrewa@indirect.com"
  '
  ' Now we'll initialise the LCD
  '
  command(0) = 48
  command(1) = 32
  command(2) = 192
  command(3) = 16
  '
  b = INP(LCD.CONTROL)
  '
  OUT LCD.CONTROL, b AND &HDF
  '
  FOR n = 1 TO 3
    '
    lcd.driver.command (command(0))
    '
  NEXT
  '
  lcd.driver.command (command(1))
  '
  cmd = 0
  '
  FOR n = 2 TO 3
    '
    lcd.driver.command (cmd)
    lcd.driver.command (command(n))
    '
  NEXT
  '
END SUB

SUB lcd.driver.print (line1 AS STRING, line2 AS STRING)
  '
  DIM n AS INTEGER
  '
  ' Set position in display, first line
  '
  lcd.driver.command (128)
  lcd.driver.command (0)
  '
  ' send MSB nibble (4-bit) and LSB nibble (4-bit)
  '
  FOR n = 1 TO 16
    '
    lcd.driver.char (ASC(MID$(line1, n, 1)))
    lcd.driver.char (ASC(MID$(line1, n, 1)) * 16)
    '
  NEXT
  '
  ' Set position in display, second line
  '
  lcd.driver.command (192)
  lcd.driver.command (0)
  '
  ' send MSB nibble (4-bit) and LSB nibble (4-bit)
  '
  FOR n = 1 TO 16
    '
    lcd.driver.char (ASC(MID$(line2, n, 1)))
    lcd.driver.char (ASC(MID$(line2, n, 1)) * 16)
    '
  NEXT
  '
END SUB

