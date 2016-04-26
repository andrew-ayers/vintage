'****************************************************************************
'
' Description : Blast3D! - VGA Mode 13 3D Spinning Cube Demo for the
'               Blast Library - Demo Program - For PowerBasic 3.2 Only!
' Written by  : Copyright (c) 1997 by Andrew L. Ayers
' Date        : 07/21/97
' Comments    : This is just a little demo to show off the new BlastLine
'               routine as well as a little bit 3D that I am working on. I
'               want to make the actual calcs into assembler so that they
'               will be faster, at some point. I also may come out with a
'               tutorial or something. Until then, have phun!
'
'****************************************************************************
'
' PowerBasic compilation flag settings - *not* strictly needed...
'
$optimize speed
$cpu 80386
$lib all off
'
'****************************************************************************
'
' Declare our procedures
'
declare sub SetMode13()
declare sub SetMode3()
'
declare sub BlastGet(byval dseg%,byval doff%,byval sseg%,byval soff%,byval x1%,byval y1%,byval x2%,byval y2%)
declare sub BlastPut(byval dseg%,byval doff%,byval sseg%,byval soff%,byval x1%,byval y1%,byval c%)
declare sub BlastPset(byval tseg%,byval toff%,byval x%,byval y%,byval c%)
declare function BlastPoint%(byval tseg%,byval toff%,byval x%,byval y%)
declare sub BlastCopy(byval fseg%,byval foff%,byval tseg%,byval toff%)
declare sub BlastScroll(byval fseg%,byval foff%,byval tseg%,byval toff%,byval xoff%,byval yoff%)
declare sub BlastCLS(byval tseg%,byval toff%,byval c%)
declare sub BlastPrint(tseg%,toff%,x%,y%,text$,c%)
declare sub BlastLine(byval tseg%,byval toff%,byval x1%,byval y1%,byval x2%,byval y2%,byval c%)
'
declare sub RotateY3D(xp%,yp%,zp%,deg%)
declare sub RotateZ3D(xp%,yp%,zp%,deg%)
declare sub RotateX3D(xp%,yp%,zp%,deg%)
declare sub Scale3D(xp%,yp%,zp%,sx%,sy%,sz%)
declare sub Translate3D(xp%,yp%,zp%,tx%,ty%,tz%)
declare sub Project3D(xp%,yp%,zp%,sx%,sy%)
declare function TSIN!(deg%)
declare function TCOS!(deg%)
'
'****************************************************************************
'
' SIN/COS Table Arrays
'
dim stable(359) as shared single, ctable(359) as shared single
dim VIEWER.DIST as shared integer,VIEWPORT.CENTERX as shared integer,VIEWPORT.CENTERY as shared integer
'
VIEWER.DIST=250
VIEWPORT.CENTERX=159
VIEWPORT.CENTERY=99
'
redim buffer1%(31999) ' This is an off-screen buffer
'
' Build the SIN/COS Tables
'
for t%=0 to 359
  stable!(t%)=SIN(t%*(3.14159/180))
  ctable!(t%)=COS(t%*(3.14159/180))
next
'
dim ox%(7),oy%(7),oz%(7),px%(7),py%(7)
'
' Following is data for a cube
'
data 8
'
data -1,-1,1
data 1,-1,1
data 1,-1,-1
data -1,-1,-1
'
data -1,1,1
data 1,1,1
data 1,1,-1
data -1,1,-1
'
' Read in Cube Data
'
read NumPoints%
'
for t%=0 to NumPoints%-1
  read ox%(t%),oy%(t%),oz%(t%)
next
'
call SetMode13
'
do
  call BlastCLS(varseg(buffer1%(0)),varptr(buffer1%(0)),0)
  '
  for t%=0 to NumPoints%-1
    '
    xp%=ox%(t%):yp%=oy%(t%):zp%=oz%(t%)
    '
    call Scale3D(xp%,yp%,zp%,40,40,40)
    '
    call Translate3D(xp%,yp%,zp%,0,0,0)
    '
    call RotateX3D(xp%,yp%,zp%,ang%)
    call RotateY3D(xp%,yp%,zp%,ang%)
    call RotateZ3D(xp%,yp%,zp%,ang%)
    '
    call Project3D(xp%,yp%,zp%,px%(t%),py%(t%))
    '
  next
  '
  ang%=ang%+2:if ang%>359 then ang%=ang%-360
  '
  for t%=0 to 2
    '
    call BlastLine(varseg(buffer1%(0)),varptr(buffer1%(0)),px%(t%),py%(t%),px%(t%+1),py%(t%+1),15)
    '
  next
  '
  call BlastLine(varseg(buffer1%(0)),varptr(buffer1%(0)),px%(3),py%(3),px%(0),py%(0),15)
  '
  for t%=4 to 6
    '
    call BlastLine(varseg(buffer1%(0)),varptr(buffer1%(0)),px%(t%),py%(t%),px%(t%+1),py%(t%+1),15)
    '
  next
  '
  call BlastLine(varseg(buffer1%(0)),varptr(buffer1%(0)),px%(7),py%(7),px%(4),py%(4),15)
  '
  for t%=0 to 3
    '
    call BlastLine(varseg(buffer1%(0)),varptr(buffer1%(0)),px%(t%),py%(t%),px%(t%+4),py%(t%+4),15)
    '
  next
  '
  call BlastCopy(varseg(buffer1%(0)),varptr(buffer1%(0)),&ha000,0)
  '
loop until inkey$=chr$(27)
'
call SetMode3:cls
'
erase buffer1% ' Deallocate the off-screen buffer
'
end
'
'****************************************************************************
'
sub SetMode13
 '
 ! mov ax,&h13
 ! int &h10
 '
end sub
'
sub SetMode3
 '
 ! mov ax,&h03
 ! int &h10
 '
end sub
'
sub BlastCopy(byval fseg%,byval foff%,byval tseg%,byval toff%)
  '
  if tseg%=&ha000 then wait &h3da,8
  ' start
  ! push ds
  ' init
  ! mov ax,fseg%
  ! mov ds,ax
  ! mov si,foff%
  ! mov ax,tseg%
  ! mov es,ax
  ! mov di,toff%
  ' plot
  ! mov cx,32000
  ! rep movsw
  ' exit
  ! pop ds
  '
end sub
'
sub BlastPset(byval tseg%,byval toff%,byval x%,byval y%,byval c%)
  '
  ' start
  ! push ds
  ' init
  ! mov ax,tseg%
  ! mov ds,ax
  ! mov si,y%
  ' plot
  ! mov cl,6
  ! shl si,cl
  ! mov bx,si
  ! mov cl,2
  ! shl si,cl
  ! add si,bx
  ! mov bx,x%
  ! add si,bx
  ! mov al,c%
  ! mov [si],al
  ' exit
  ! pop ds
  '
end sub
'
sub BlastCLS(byval tseg%,byval toff%,byval c%)
  '
  ' start
  ! push ds
  ' init
  ! mov ax,tseg%
  ! mov ds,ax
  ! mov si,toff%
  ! mov ax,c%
  ! mov ah,al
  ! mov cx,&hfa00
  Plot1:
  ! mov [si],ax
  ! add si,2
  ! sub cx,2
  ! jnz Plot1
  ' exit
  ! pop ds
  '
end sub
'
function BlastPoint%(byval tseg%,byval toff%,byval x%,byval y%)
  '
  ' start
  ! push ds
  ' init
  ! mov ax,tseg%
  ! mov ds,ax
  ! mov si,y%
  ' pick
  ! mov cl,6
  ! shl si,cl
  ! mov bx,si
  ! mov cl,2
  ! shl si,cl
  ! add si,bx
  ! mov bx,x%
  ! add si,bx
  ! mov bx,toff%
  ! add si,bx
  ! mov bl,[si]
  ! mov function,bl
  ' exit
  ! pop ds
  '
end function
'
sub BlastLine(byval tseg%,byval toff%,byval x1%,byval y1%,byval x2%,byval y2%,byval c%)
  '
  xdiff%=x2%-x1%
  ydiff%=y2%-y1%
  '
  xstep%=1
  ystep%=320
  '
  if x1%>x2% then xstep%=-xstep%:xdiff%=-xdiff%
  if y1%>y2% then ystep%=-ystep%:ydiff%=-ydiff%
  '
  ' Init
  ! push ds
  ! mov ax,tseg%
  ! mov ds,ax
  ! mov si,y1%
  ! mov cl,6
  ! shl si,cl
  ! mov bx,si
  ! mov cl,2
  ! shl si,cl
  ! add si,bx
  ! mov bx,x1%
  ! add si,bx
  ! mov bx,toff%
  ! add si,bx
  '
  if xdiff%<=ydiff% then
    '
    if ydiff%<0 then ydiff=-(ydiff%)
    '
    ! mov bx,0
    ! mov cx,ydiff%
    LoopY:
    ! mov al,c%
    ! mov [si],al
    ! add si,ystep%
    ! add bx,xdiff%
    ! cmp bx,ydiff%
    ! jl NextY
    ! add si,xstep%
    ! sub bx,ydiff%
    NextY:
    ! dec cx
    ! cmp cx,0
    ! jge LoopY
    '
  else
    '
    if xdiff%<0	then xdiff%=-xdiff%
    '
    ! mov bx,0
    ! mov cx,xdiff%
    LoopX:
    ! mov al,c%
    ! mov [si],al
    ! add si,xstep%
    ! add bx,ydiff%
    ! cmp bx,xdiff%
    ! jl NextX
    ! add si,ystep%
    ! sub bx,xdiff%
    NextX:
    ! dec cx
    ! cmp cx,0
    ! jge LoopX
    '
  end if
  '
  ' Exit
  ! pop ds
  '
end sub
'
sub BlastPrint(tseg%,toff%,x%,y%,text$,c%)
  '
  ' CGA Character Set (8 x 8)
  '
  xx%=x%-1
  yy%=y%
  '
  def seg=&hffa6
  '
  for chr%=1 TO len(text$)
    xx%=xx%+8
    ptr%=8*ASC(MID$(text$,chr%,1))+&he
    '
    for l%=0 to 7
      BitPattern%=peek(ptr%+l%)
      if bit(BitPattern%,0) then call BlastPset(tseg%,toff%,xx%-0,yy%+l%,c%)
      if bit(BitPattern%,1) then call BlastPset(tseg%,toff%,xx%-1,yy%+l%,c%)
      if bit(BitPattern%,2) then call BlastPset(tseg%,toff%,xx%-2,yy%+l%,c%)
      if bit(BitPattern%,3) then call BlastPset(tseg%,toff%,xx%-3,yy%+l%,c%)
      if bit(BitPattern%,4) then call BlastPset(tseg%,toff%,xx%-4,yy%+l%,c%)
      if bit(BitPattern%,5) then call BlastPset(tseg%,toff%,xx%-5,yy%+l%,c%)
      if bit(BitPattern%,6) then call BlastPset(tseg%,toff%,xx%-6,yy%+l%,c%)
      if bit(BitPattern%,7) then call BlastPset(tseg%,toff%,xx%-7,yy%+l%,c%)
    next
  next
  '
  def seg
  '
end sub
'
sub BlastGet(byval dseg%,byval doff%,byval sseg%,byval soff%,byval x1%,byval y1%,byval x2%,byval y2%)
  '
  wid%=(x2%-x1%)+1
  hgt%=(y2%-y1%)+1
  '
  ! push ds
  '
  ! mov ax,sseg%
  ! mov ds,ax
  ! mov si,soff%
  ! mov ax,wid%
  ! mov bx,0008
  ! mul bx
  ! mov [si],ax
  ! inc si
  ! inc si
  ! mov ax,hgt%
  ! mov [si],ax
  ! inc si
  ! inc si
  ! push si
  '
  Gett:
  '
  ! mov bx,dseg%
  ! mov ds,bx
  ! mov si,y1%
  ! mov cl,06
  ! shl si,cl
  ! mov bx,si
  ! mov cl,02
  ! shl si,cl
  ! add si,bx
  ! mov bx,x1%
  ! add si,bx
  ! mov bx,doff%
  ! add si,bx
  ! mov al,[si]
  ! mov bx,sseg%
  ! mov ds,bx
  ! pop si
  ! mov [si],al
  ! inc si
  ! push si
  Done1:
  incr x1%
  if x1%<=x2% then goto Gett
  decr x1%,wid%
  incr y1%
  if y1%<=y2% then goto Gett
  ! pop si
  ! pop ds
  '
end sub
'
sub BlastPut(byval dseg%,byval doff%,byval sseg%,byval soff%,byval x1%,byval y1%,byval c%)
  '
  def seg=sseg%
  wid%=peeki(soff%)\8
  hgt%=peeki(soff%+2)
  ex%=x1%+wid%
  ey%=y1%+hgt%
  def seg
  '
  ! push ds
  '
  ! mov ax,sseg%
  ! mov ds,ax
  ! mov si,soff%
  ! add si,4
  ! push si
  Plot:
  ! mov ax,sseg%
  ! mov ds,ax
  ! pop si
  ! mov al,[si]
  ! inc si
  ! push si
  ! cmp al,c%
  ! jz Done2
  ! cmp x1%,0
  ! jl Done2
  ! cmp y1%,0
  ! jl Done2
  ! cmp x1%,319
  ! jg Done2
  ! cmp y1%,199
  ! jg Done2
  '
  ! mov bx,dseg%
  ! mov ds,bx
  ! mov si,y1%
  ! mov cl,6
  ! shl si,cl
  ! mov bx,si
  ! mov cl,2
  ! shl si,cl
  ! add si,bx
  ! mov bx,x1%
  ! add si,bx
  ! mov bx,doff%
  ! add si,bx
  ! mov [si],al
  '
  Done2:
  '
  incr x1%
  if x1%<ex% then goto Plot
  decr x1%,wid%
  incr y1%
  if y1%<ey% then goto Plot
  '
  ! pop si
  ! pop ds
  '
end sub
'
sub BlastScroll(byval fseg%,byval foff%,byval tseg%,byval toff%,byval xoff%,byval yoff%)
  '
  ' Scroll the screen!
  '
  NumBytes%=&hfa00 ' 64000 Bytes
  '
  OffsetBytes%=yoff%*320
  if OffsetBytes%<0 then OffsetBytes%=-OffsetBytes%
  NumBytes%=NumBytes%-OffsetBytes%
  '
  if yoff%>=0 then
    foff%=foff%+OffsetBytes%
  else
    toff%=toff%+OffsetBytes%
  end if
  '
  OffsetBytes%=xoff%
  if OffsetBytes%<0 then OffsetBytes%=-OffsetBytes%
  NumBytes%=NumBytes%-OffsetBytes%
  '
  if xoff%>=0 then
    foff%=foff%+OffsetBytes%
  else
    toff%=toff%+OffsetBytes%
  end if
  '
  ! push ds
  '
  ! mov ax,fseg%
  ! mov ds,ax
  ! mov si,foff%
  ! mov ax,tseg%
  ! mov es,ax
  ! mov di,toff%
  '
  ! mov cx,NumBytes%
  ! rep movsb
  '
  ! pop ds
  '
end sub
'
sub Project3D(xp%,yp%,zp%,sx%,sy%)
  '
  sx%=VIEWPORT.CENTERX+((VIEWER.DIST*xp%)/(zp%+VIEWER.DIST))
  sy%=VIEWPORT.CENTERY+((VIEWER.DIST*yp%)/(zp%+VIEWER.DIST))
  '
end sub
'
sub RotateX3D(xp%,yp%,zp%,deg%)
  '
  qxp%=xp%
  qyp%=yp%*TSIN!(deg%)+zp%*TCOS!(deg%)
  qzp%=yp%*TCOS!(deg%)-zp%*TSIN!(deg%)
  '
  xp%=qxp%
  yp%=qyp%
  zp%=qzp%
  '
end sub
'
sub RotateY3D(xp%,yp%,zp%,deg%)
  '
  qxp%=xp%*TSIN!(deg%)+zp%*TCOS!(deg%)
  qyp%=yp%
  qzp%=xp%*TCOS!(deg%)-zp%*TSIN!(deg%)
  '
  xp%=qxp%
  yp%=qyp%
  zp%=qzp%
  '
end sub
'
sub RotateZ3D(xp%,yp%,zp%,deg%)
  '
  qxp%=xp%*TSIN!(deg%)+yp%*TCOS!(deg%)
  qyp%=xp%*TCOS!(deg%)-yp%*TSIN!(deg%)
  qzp%=zp%
  '
  xp%=qxp%
  yp%=qyp%
  zp%=qzp%
  '
end sub
'
sub Scale3D(xp%,yp%,zp%,sx%,sy%,sz%)
  '
  xp%=xp%*sx%
  yp%=yp%*sy%
  zp%=zp%*sz%
  '
end sub
'
function TCOS!(deg%)
  '
  if deg%>359 then deg%=deg%-360
  if deg%<0 then deg%=360-deg%
  '
  TCOS!=ctable!(deg%)
  '
end function
'
sub Translate3D(xp%,yp%,zp%,tx%,ty%,tz%)
  '
  xp%=xp%+tx%
  yp%=yp%+ty%
  zp%=zp%+tz%
  '
end sub
'
function TSIN!(deg%)
  '
  if deg%>359 then deg%=deg%-360
  if deg%<0 then deg%=360-deg%
  '
  TSIN!=stable!(deg%)
  '
end function

