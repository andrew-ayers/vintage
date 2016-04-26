'****************************************************************************
'
' Description : Blast! Library Demo Program - For PowerBasic 3.2 Only!
' Written by  : Copyright (c) 1997 by Andrew L. Ayers
' Date        : 07/21/97
' Comments    : This demo shows how easy it is to achieve great looking
'               sprites easily.
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
'****************************************************************************
'
' Reserve memory for two off-screen buffers, sprites and demo variables
' Use REDIM so that we may de-allocate the memory at the end of the program
'
redim buffer1%(31999) ' This is an off-screen buffer
redim buffer2%(31999) ' This is another...
redim sbuffer%(2000)  ' This is our sprite buffer
'
dim bx%(40),by%(40),bt%(40),dx%(40),dy%(40) ' Variables for demo
'
'****************************************************************************
'
' Initialize Balls
'
for t%=0 to 39
  bx%(t%)=int(rnd*200)+50:by%(t%)=int(rnd*100)+50
  bt%(t%)=int(rnd*2)
  do
    dx%(t%)=INT(RND*11)-5:dy%(t%)=INT(RND*11)-5
  loop until dx%(t%)<>0 and dy%(t%)<>0
next
'
screen 0:width 40:cls
'
' Now, show some instructions
'
color 2
locate 2, 2:print "Blast! Library Demo by Andrew L. Ayers"
'
color 15
locate 4,1:print "The following demo illustrates a simple"
locate 5,1:print "use of the Blast! Library. Many 32 x 32"
locate 6,1:print "and 16 x 16 sprites (up to 40) bounce on"
locate 7,1:print "the screen in random directions. Use the"
locate 8,1:print "+/- keys to add or subtract sprites from"
locate 9,1:print "the screen. Notice how the sprites can"
locate 10,1:print "overlap each other, without showing a"
locate 11,1:print "black border. This masking effect is"
locate 12,1:print "done by simply specifying black (0) as"
locate 13,1:print "the invisible color. While in this demo"
locate 14,1:print "I am using 32 x 32 and 16 x 16 sprites,"
locate 15,1:print "any size sprite could be used. It all"
locate 16,1:print "depends on the level of detail your"
locate 17,1:print "sprites need as well as the number of"
locate 18,1:print "sprites you place on screen at one time."
'
' Draw a simple graphic for the background
'
color 4
locate 21,3:print "Please wait - Preparing Images.";
'
' Clear the background
'
call BlastCLS(varseg(buffer2%(0)),varptr(buffer2%(0)),0)
'
x1%=159:y1%=99:colr%=0
'
for t%=0 to 319 step 4
  call BlastLine(varseg(buffer2%(0)),varptr(buffer2%(0)),x1%,y1%,t%,0,colr%)
  colr%=colr%+1:if colr%>255 then colr%=0
next
'
print ".";
'
for t%=0 to 199 step 4
  call BlastLine(varseg(buffer2%(0)),varptr(buffer2%(0)),x1%,y1%,319,t%,colr%)
  colr%=colr%+1:if colr%>255 then colr%=0
next
'
print ".";
'
for t%=319 to 0 step -4
  call BlastLine(varseg(buffer2%(0)),varptr(buffer2%(0)),x1%,y1%,t%,199,colr%)
  colr%=colr%+1:if colr%>255 then colr%=0
next
'
print ".";
'
for t% = 199 to 0 step -4
  call BlastLine(varseg(buffer2%(0)),varptr(buffer2%(0)),x1%,y1%,0,t%,colr%)
  colr%=colr%+1:if colr%>255 then colr%=0
next
'
call BlastPrint(varseg(buffer2%(0)),varptr(buffer2%(0)),111,95,"Hello World!",32)
'
color 2
locate 21,1:print "Hit any key to begin demo - [ESC] exits"
color 15
'
do:loop until inkey$<>""
'
cls
'
call SetMode13
'
' Create a 32 x 32 sprite for us to use
'
o = 0
for t%=14 to 3 step -1
  c%=34-t%
  call NewCircle(15+o,15+o,t%,t%,c%)
  call NewPaint(15+o,15+o,c%,c%)
  o=o-.4
next
'
' Get our sprites
'
call BlastGet(&ha000,0,varseg(sbuffer%(0)),varptr(sbuffer%(0)),0,0,31,31)
'
for t%=11 to 18
  call BlastLine(&ha000,0,11,t%,18,t%,0) ' Clear out center
next
'
call BlastGet(&ha000,0,varseg(sbuffer%(0)),varptr(sbuffer%(514)),7,7,22,22)
'
' Initialize Demo Variables
'
numballs%=10:done%=0
'
do
  '
  ' Copy our background graphic to our off-screen buffer (which clears off
  ' old images of our sprites...) If you are using only a single buffer, just
  ' "get" the background behind each sprite before moving, move, then replace
  ' the background, or just erase with the "ERASE arrayname" (see HELP)
  ' command. I could done the "get, move, replace" on this demo, but I wanted
  ' to show how to use a background using the library...
  '
  call BlastCopy(varseg(buffer2%(0)),varptr(buffer2%(0)),varseg(buffer1%(0)),varptr(buffer1%(0)))
  '
  ' Put our sprites
  '
  for t%=0 to numballs%-1
    bx%(t%)=bx%(t%)+dx%(t%):by%(t%)=by%(t%)+dy%(t%)
    if bx%(t%)>319 then dx%(t%)=-dx%(t%):bx%(t%)=319
    if bx%(t%)<-31 then dx%(t%)=-dx%(t%):bx%(t%)=-31
    if by%(t%)>199 then dy%(t%)=-dy%(t%):by%(t%)=199
    if by%(t%)<-31 then dy%(t%)=-dy%(t%):by%(t%)=-31
    call BlastPut(varseg(buffer1%(0)),varptr(buffer1%(0)),varseg(sbuffer%(0)),varptr(sbuffer%(bt%(t%)*514)),bx%(t%),by%(t%),0)
  next
  '
  ' Copy the off-screen buffer to the visible screen
  '
  call BlastCopy(varseg(buffer1%(0)),varptr(buffer1%(0)),&ha000,0)
  '
  ' Get user input for number of balls, or exit
  '
  a$=inkey$
  select case a$
    case "-","_"
      numballs%=numballs%-1
      if numballs%<1 then numballs%=1
    case "+","="
      numballs%=numballs%+1
      if numballs%>40 then numballs%=40
    case chr$(27)
      done%=1
  end select
  '
loop until done%
'
call SetMode3
'
cls
'
' Deallocate our large buffers, using erase
'
erase buffer1% ' This is an off-screen buffer
erase buffer2% ' This is another...
erase sbuffer% ' This is our sprite buffer
'
'****************************************************************************
'
end
'
'**********************************************
'*            S U B R O U T I N E S           *
'**********************************************
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
  plot1:
  ! mov [si],ax
  ! add si,2
  ! sub cx,2
  ! jnz plot1
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
    if ydiff%<0 then ydiff%=-ydiff%
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
    if xdiff%<0 then xdiff%=-xdiff%
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
'****************************************************************************
'
sub NewCircle(xp%,yp%,xr%,yr%,c%)
  '
  ' Note: This circle routine is not part of the Blast! Library, and is only
  ' included in this demo to allow the creation of the "bouncing spheres"
  ' used in this demo. This routine is *not* perfect, and will "blow up" if
  ' used in certain situations. As such, I do not endorse use of this rou-
  ' tine in any way outside of this demo, and suggest for those in need of
  ' a general purpose circle routine to continue their search elsewhere...
  '
  ox%=xp%+sin(0)*xr%
  oy%=yp%+cos(0)*(yr%-1)
  '
  for t!=0 to 6.28 step .1
    x%=xp%+sin(t!)*xr%
    y%=yp%+cos(t!)*(yr%-1)
    call BlastLine(&ha000,0,ox%,oy%,x%,y%,c%)
    ox%=x%:oy%=y%
  next
  '
  x%=xp%+sin(0)*xr%
  y%=yp%+cos(0)*(yr%-1)
  '
  call BlastLine(&ha000,0,ox%,oy%,x%,y%,c%)
  '
end sub
'
sub NewPaint(xp%,yp%,pc%,bc%)
  '
  ' Note: This paint routine is not part of the Blast! Library, and is only
  ' included in this demo to allow the creation of the "bouncing spheres"
  ' used in this demo. This routine is *not* perfect, and will "blow up" if
  ' used in certain situations. As such, I do not endorse use of this rou-
  ' tine in any way outside of this demo, and suggest for those in need of
  ' a general purpose paint routine to continue their search elsewhere...
  '
  for t%=0 to 3
    select case t%
      case 0
        ox%=0:oy%=0
      case 1
        ox%=1:oy%=0
      case 2
        ox%=0:oy%=1
      case 3
        ox%=-1:oy%=1
    end select
    '
    call NewPaint2(xp%+ox%,yp%+oy%,pc%,bc%,t%)
  next
  '
end sub
'
sub NewPaint2(xp%,yp%,pc%,bc%,quad%)
  '
  if xp%<0 or xp%>319 or yp%<0 or yp%>199 then exit sub
  '
  if BlastPoint%(&ha000,0,xp%,yp%)=bc% or BlastPoint%(&ha000,0,xp%,yp%)=pc% then exit sub
  '
  call BlastPset(&ha000,0,xp%,yp%,pc%)
  '
  select case quad%
    case 0
      call NewPaint2(xp%-1,yp%,pc%,bc%,quad%)
      call NewPaint2(xp%,yp%-1,pc%,bc%,quad%)
    case 1
      call NewPaint2(xp%,yp%-1,pc%,bc%,quad%)
      call NewPaint2(xp%+1,yp%,pc%,bc%,quad%)
    case 2
      call NewPaint2(xp%+1,yp%,pc%,bc%,quad%)
      call NewPaint2(xp%,yp%+1,pc%,bc%,quad%)
    case 3
      call NewPaint2(xp%-1,yp%,pc%,bc%,quad%)
      call NewPaint2(xp%,yp%+1,pc%,bc%,quad%)
  end select
  '
end sub

