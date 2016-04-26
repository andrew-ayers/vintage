'****************************************************************************
'
' Description : BlastScroll! - VGA Mode 13 RPG Map Style Scrolling Demo for
'               the Blast Library - Demo Program - For PowerBasic 3.2 Only!
' Written by  : Copyright (c) 1997 by Andrew L. Ayers
' Date        : 07/21/97
' Comments    :
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
' Allocate buffer memory
'
redim workbuf(31999) as shared integer ' Offscreen work buffer
redim tempbuf(31999) as shared integer ' Offscreen temporary buffer
redim mapp(63, 63)    as shared integer ' Map array
'
'****************************************************************************
'
dim mx as shared integer, my as shared integer, scroll as shared integer
'
call SetMode13
'
call BuildMap
'
mx%=0:my%=0:scroll%=-1
'
do
  '
  call DrawMap
  '
  do:ky$=inkey$:loop until ky$<>""
  '
  select case ky$
    case "8"
      scroll%=0
      my%=my%-1:if my%<0 then my%=0:scroll%=-1
    case "2"
      scroll%=2
      my%=my%+1:if my%>51 then my%=51:scroll%=-1
    case "4"
      scroll%=3
      mx%=mx%-1:if mx%<0 then mx%=0:scroll%=-1
    case "6"
      scroll%=1
      mx%=mx%+1:if mx%>43 then mx%=43:scroll%=-1
    case chr$(27)
      exit do
    case else
      scroll%=-1
  end select
loop
'
call SetMode3:cls
'
'****************************************************************************
'
' Deallocate buffer memory
'
erase workbuf% ' Offscreen work buffer
erase tempbuf% ' Offscreen temporary buffer
erase mapp%     ' Map array
'
end
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
sub BuildMap
  '
  for y%=0 to 63
    for x%=0 to 63
      mapp%(x%,y%)=int(rnd*5)
    next
  next
  '
end sub
'
sub DrawMap
  '
  if scroll%>-1 then
    for tt%=0 to 15
      select case scroll%
        case 0
          xoff%=0:yoff%=-1
        case 1
          xoff%=1:yoff%=0
        case 2
          xoff%=0:yoff%=1
        case 3
          xoff%=-1:yoff%=0
      end select
      '
      ' Scroll the screen
      '
      call BlastScroll(varseg(workbuf%(0)),varptr(workbuf%(0)),varseg(tempbuf%(0)),varptr(tempbuf%(0)),xoff%,yoff%)
      '
      call BlastCopy(varseg(tempbuf%(0)),varptr(tempbuf%(0)),varseg(workbuf%(0)),varptr(workbuf%(0)))
      '
      ' Update the work buffer
      '
      select case scroll%
        case 0
          for x%=0 to 19
            tile%=mapp%(mx%+x%,my%)
            for t%=0 to tt%
              call BlastLine(varseg(workbuf%(0)),varptr(workbuf%(0)),x%*16,t%,x%*16+15,t%,tile%*2)
            next
          next
        case 1
          for y%=0 to 11
            tile%=mapp%(mx%+19,my%+y%)
            for t%=319-tt% to 319
              call BlastLine(varseg(workbuf%(0)),varptr(workbuf%(0)),t%,y%*16,t%,y%*16+15,tile%*2)
            next
          next
        case 2
          for x%=0 to 19
            tile%=mapp%(mx%+x%,my%+11)
            for t%=191-tt% to 191
              call BlastLine(varseg(workbuf%(0)),varptr(workbuf%(0)),x%*16,t%,x%*16+15,t%,tile%*2)
            next
          next
        case 3
          for y%=0 to 11
            tile%=mapp%(mx%,my%+y%)
            for t%=0 to tt%
              call BlastLine(varseg(workbuf%(0)),varptr(workbuf%(0)),t%,y%*16,t%,y%*16+16,tile%*2)
            next
          next
      end select
      '
      for t%=192 to 199
        call BlastLine(varseg(workbuf%(0)),varptr(workbuf%(0)),0,t%,319,t%,0)
      next
      '
      call BlastCopy(varseg(workbuf%(0)),varptr(workbuf%(0)),varseg(tempbuf%(0)),varptr(tempbuf%(0)))
      '
      call BlastPrint(varseg(tempbuf%(0)),varptr(tempbuf%(0)),10,10,"Coordinates are "+str$(mx%)+","+str$(my%),15)
      '
      ' Copy the work buffer to the screen
      '
      call BlastCopy(varseg(tempbuf%(0)),varptr(tempbuf%(0)),&ha000,0)
      '
    next
  else
    '
    v%=my%
    '
    for y%=0 to 11
      h%=mx%
      for x%=0 to 19
        tile%=mapp%(h%,v%)
        h%=h%+1
        '
        for t%=y%*16 to y%*16+15
          call BlastLine(varseg(workbuf%(0)),varptr(workbuf%(0)),x%*16,t%,x%*16+15,t%,tile%*2)
        next
        '
      next
      v%=v%+1
    next
    '
    call BlastCopy(varseg(workbuf%(0)),varptr(workbuf%(0)),&ha000,0)
    '
    call BlastPrint(&ha000,0,10,10,"Coordinates are "+STR$(mx%)+","+STR$(my%),15)
    '
  end if
  '
end sub
