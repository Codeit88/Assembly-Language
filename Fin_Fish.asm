;PROJECT 
;-------------------------------------------------------------------------------------------
;GROUP MEMBERS 
;1-> ABDULLAH DAR BSCS 3G 21-L-7512
;2-> SAMI KHOKHER BSCS 3G 21-L-1868
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
[org 0x0100]
jmp start
fishloc: dw 3240		; fish location 
red: dw 3110,0		; red loc and counter
green: dw 3480,0		; green loc and counter
score: dw 0		; score
random: dw 2720		; random number
clock: dw 0,0		; clock second and milli second
buffer: times 4000 db 0 


;--------------------------------------------------------------------------------------
; GLOBAL VARIABLES/STRINGS FOR INTRODUCTION PAGE
;--------------------------------------------------------------------------------------

; printimg messages
messagename:db 'Enter Your Name : $'
messagewelcome: db 'Welcome to ...... Galaxy $'

introname:db 'Hello Dear : $'
intronamesize: db 13
introwelcome: db 'Welcome to "THE CATCH MACHINE" $'
introwelcomesize: db 31
introInstructions: db '<-----INSTRUCTIONS-----> $'
introInstructionssize: db 24

intromove1: db '1- RIGHT, LEFT ,UP ,DOWN $'
intromovesize1: db 25
intromove2: db '-> Keys will be used for Movement $'
intromovesize2: db 34

introscore1: db '2- Each Green Food has +10 points $'
introscoresize1: db 34
introscore2: db '3- Each Red Food has +50 points $'
introscoresize2: db 32

intromessage: db '4- ENTER to Continue & ESC to Exit $'
intromessagesize: db 35

introdeveloper1: db 'Game Developers: 21L-7512 ABDULLAH DAR $'
introdeveloper1size: db 38
introdeveloper2: db '21L-1868 SAMI KHOKHER $'
introdeveloper2size: db 22

endexit: db 'Do You Want to Exit Game ?  $'
endexitsize: db 27

endyes: db 'YES -> [Y]  $'
endyessize: db 11

endno:db 'NO -> [N]  $'
endnosize: db 10

gamename: db 'THE CATCH MACHINE $'
gamenamesize: db 17

playername: db 'PLAYER : $'
playernamesize: db 9

currentscore: db 'SCORE : $'
currentscoresize: db 7

endterminate: db 'G_A_M_E  O_V_E_R  $'
endterminatesize: db 17

introstartsreset: dw 	0,	2,	324,		6,	168,	330,		172,	14,	336,		18,	180,	342,	184,			26,	348	,		158,	156,	446,		128,	290,	452,		294,	136,	458,		140,	302,	464,	306,			148,	470	
introstarts: dw 		1120,2402,644,	1606, 328,970,  2732,1134, 496, 		1618, 2580, 982, 2104, 		186, 1948 ,		1278,2556,766,	1728, 450,1092,  2854,1256, 618, 		1740, 2702, 1104, 2226, 		308, 2070

lastscore: times 20 db 0
; input buffers
buffername: db 80
db 0
times 80 db 0

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

;Global Variables for Multitasking

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------


a1:dw 220
a2:dw 440
a3:dw 880
a4: dw 1760

a5:dw 3520
a6:dw 7040

b1:dw 247
b2:dw 494
b3:dw 987
b4: dw 1975

b5:dw 3951
b6:dw 7902

c1:dw 131
c2:dw 261
c3:dw 524
c4: dw 1046

c5:dw 2093
c6:dw 4186

d1:dw 146
d2:dw 294
d3:dw 587
d4: dw 1174

d5:dw 2349
d6:dw 4698


e1:dw 83
e2:dw 164
e3:dw 329
e4: dw 659

e5:dw 1318
e6:dw 2637

f1:dw 87
f2:dw 174
f3:dw 349
f4: dw 699

f5:dw 1396
f6:dw 2793


;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR CLEARING SCREEN
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

clrscr: 			
		push es 		; pushing relevant registers
 		push ax 
 		push cx 
		push di 

 		mov ax, 0xb800 
 		mov es, ax 		; point es to video base 
 		xor di, di 		; point di to top left column 
 		mov ax, 0x0720 		; space char in normal attribute 
 		mov cx, 2000 		; number of screen locations 
 		cld 			; auto increment mode 
 		rep stosw 		; clear the whole screen 

 		pop di			; popping relevant used registers
 		pop cx 
 		pop ax 
 		pop es 
 		ret 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING SKY
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
sky
                push bp			; pushing relevant registers
                mov bp,sp
		push es 
 		push ax 
		push bx
 		push cx 
		push di  

		mov bx,[bp+4]		; assigning color code for sky by paramete
 		mov ax, 0xb800 
 		mov es, ax 		; point es to video base 
 		mov di,160 		; point di to top left column 
 		mov ax, bx		; printing color for sky
 		mov cx, 640 		; number of screen locations 
 		cld 			; auto increment mode 
 		rep stosw 		; print to geiven size

 		pop di			; popping relevant used registers
 		pop cx 
		pop bx
 		pop ax 
 		pop es 
   		pop bp
 		ret 2			; one parameter send i.e sky size
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING A MOUNTAIN
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
function_mountain:
		  push bp			; pushing relevant registers
	  	  mov bp,sp
                  push es
                  push ax			; following registers are used for
                  push bx  			; bx size of mountain
                  push cx 			; cx grass color
                  push dx  			; dx soil color
                  push di
						; fetching data from stack
		  mov bx, [bp+4]  		; size
		  mov dx, [bp+8]		; soil
      		  mov cx, [bp+10]		; grass
      
loopmountain:				; loop used for increment in size for printing mountains
                  mov di,bx				; giving di the starting point of mountain
                  add bx,160			; bx used for comparing di for each line
                  mov ax,0xb800
                  mov es,ax
                  mov word[es:di],cx			; pushing color to top of mountain
                  mov word[es:di-2],cx		; pushing color to top left of mountain
                  mov si,di				; assigning values for right side of mountain
                  add si,2
                  mov word[es:si],cx			; pushing color to top of mountain
                  mov word[es:si+2],cx 		; pushing color to top right of mountain
                  
mountain:					; loop used for printing single diagonal for a mountain

                  add di,156			; adding 156 so it will stay 2 blocks before the original point above
                  add si,164			; adding 164 so it will stay 2 blocks after  the original point above
                  mov word[es:di],cx			; placing color
                  mov word[es:si],cx            		; placing color
                  mov word[es:di-2],cx		; placing color
                  mov word[es:si+2],cx 		; placing color

                  cmp di,1280			; comparing till the ground reached
                  jna mountain
                  mov cx,dx				; chaning color to soil color
                  cmp bx,1280			; comparing till it reaches ground i,e last diagonal row
                  jna loopmountain
                  mov di,bx			
                  mov word[es:di],0x6720		; printing on bottom

                  pop di 				; popping relevant used registers
                  pop dx
                  pop cx
                  pop bx                
                  pop ax
                  pop es
                  pop bp
                  ret 2				; one parameter send i.e starting index
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING ALL THE MOUNTAINS
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
mountainss:
                  ;40
                  mov bx,498          		;starting index of mountain
                  push bx
                  call function_mountain
                  ;80
                  mov bx,386          		;starting index of mountain
                  push bx
                  call function_mountain
                  ;130
                  mov bx,592          		;starting index of mountain
                  push bx
                  call function_mountain
                  ;150
                  mov bx,784          		;starting index of mountain
                  push bx
                  call function_mountain
                  ret 4				; two parameters passed i.e colors of grass and soil
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING A STAR
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
stars:
	push ax			; pushing registers
	push es
	push di
	mov ax,0xb800		; setting video mode
	mov es,ax
	mov di,180		; starting index of stars
	mov word[es:di],0x0f2a
	mov di,218		; starting index of stars
	mov word[es:di],0x0f2a
	mov di,300		; starting index of stars
	mov word[es:di],0x0f2a
	pop di
	pop es
	pop ax			; popping all registers
ret				; return
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING A CLOUD
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

cloud:
		push bp			; pushing relevant registers
		mov bp,sp
                     	push es
                  	push ax
	  	push cx
                  	push di
                  mov cx,[bp+8]
	  mov di,[bp+4]			; starting index of cloud
                  mov ax,0xb800
                  mov es,ax
                  mov ax,3			; size of cloud
                  mov word[es:di], cx	; cloud color printing at top
                  add di,158
nextcloud:
                  mov word[es:di], cx	; printing cloud
                  add di,2
                  sub ax,1
                  jnz nextcloud

                  	pop di 			; popping relevant used registers
	  	pop cx
                  	pop ax
                  	pop es
	  	pop bp
                  ret 2				; one parameter sent i.e starting index
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING PLANET I.E EITHER SUN OR MOON FOR DAY NIGHT FUNCTIONALITY
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

planet:
		  push bp		; pushing relevant registers
		  mov bp,sp
                  	push es
                  	push ax
                  	push bx
                  	push cx
                  	push di

		  mov di,[bp+4]		; starting index of planet
		  mov bx,[bp+6]		; color of planet
                  mov cx,3		; size of planet
                  mov ax,0xb800
                  mov es,ax
pl1:    
                  mov word[es:di],bx	; printing first row of planet
                  add di,2
                  sub cx,1
                  jnz pl1
                  add di,152
                  mov cx,5            
pl2:    
                  mov word[es:di],bx	; printing second row of planet
                  add di,2
                  sub cx,1
                  jnz pl2
                  add di,152
                  mov cx,3
pl3:    
                  mov word[es:di],bx	; printing third row of planet
                  add di,2
                  sub cx,1
                  jnz pl3

                 		pop di 		; popping relevant used registers
                  	pop cx
                  	pop bx
                  	pop ax
                  	pop es
		  pop bp
                  ret 4			; two parameters passed i.e color and starting index
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR PRINTING ALL COLOUDS
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
clouds:         
                  mov bx,170		; starting index
	 	  push bx
                  call cloud
                  mov bx,260		; starting index
	 	  push bx
                  call cloud
                  mov bx,290		; starting index
	 	  push bx
                  call cloud
                  mov bx,210		; starting index
	 	  push bx
                  call cloud
                  ret 2
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR PRINTING SEA
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
sea: 
		push bp			; pushing relevant registers
		mov bp,sp
		push es 
 		push ax 
		push bx
 		push cx 
		push di 

		mov bx,[bp+4]		; sea color
 		mov ax, 0xb800 
 		mov es, ax 		; point es to video base 
 		mov di,1280		; point di to top left column 
 		mov ax, bx 		; printing sea 
 		mov cx, 720		; number of screen locations 
 		cld 			; auto increment mode 
 		rep stosw 		; print to given size
	
 		mov di,2720
 		mov ax, 0x01db 		; printing deep sea
 		mov cx, 640		; number of screen locations 
 		cld 			; auto increment mode 
 		rep stosw 		; print to end of screen as of givem size 

 		pop di 			; popping relevant used registers	
 		pop cx 
		pop bx
 		pop ax 
 		pop es 
		pop bp
 		ret 2			; one parameter send i.e color
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING GROUND
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
ground:
		push bp			; pushing relevant registers
		mov bp,sp
	    	push es 
 		push ax 
		push bx
 		push cx 
		push di 

		mov di,[bp+4]		; starting index of ground
		mov bx,[bp+6]		; color of ground
 		mov ax, 0xb800 
 		mov es, ax 		; point es to video base 
 		
		mov cx,80
 		mov ax, bx		; printing ground
 					
 		cld 			; auto increment mode 
 		rep stosw 		; print to whole line 

 		pop di 			; popping relevant used registers
 		pop cx 
		pop bx
 		pop ax 
 		pop es 
		pop bp
 		ret 4			; two parametrs send i.e starting index and color
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING SHIP HEAD/TOP
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
shiphead:
                  push es			; pushing relevant registers
                  push ax
                  push bx
                  push di
                  
                  sub di,160			; move di to top for printing head
                  mov ax,0xb800
                  mov es,ax
                  jmp r3
u1:
                  shr bx,1 			; multiplying size
                  add di,bx			
                  sub di,2			; getting half index from size
                  mov bx,di
                  add bx,4			; second end of top of ship i.e total 4 block to print
                  jmp r2

                  r3:     			; comparing ship size to make head of desired accurate size
                  cmp bx,30
                  jnae u1   

                  shr bx,1			; multiplying by two
                  add di,bx			; adding size to starting point
                  sub di,6			; decreasing by 6
                  mov bx,di			
                  add bx,12			; adding 6x2 for printing head of 6 blocks for bigger ships

                  r2:
shiptop:					; printing ship top
                  
                  mov word[es:di],0x08db	; printing color
                  add di,2
                  cmp di,bx
                  jne shiptop

                  pop di 			; popping relevant used registers
                  pop bx
                  pop ax
                  pop es
                  ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR CALCULATING  SHIP SIZE
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
calculatesize:			; calculating size of ship 
                  jmp start1	; jump to start

updateless:
           mov cx,3		; print ship of 3 levels
           mov si,3
           jnae r1
updatemore:
           mov cx,4		; print ship of 4 levels
           mov si,4
           jae r1
start1:
                  cmp bx,50
                  jnae updateless		; if size less than 50 
                  jae updatemore		; if size greater than 50 or above
                
                  r1:				; first printing ship head
                  call shiphead			; printing ship head
                  mov dx,0
                  ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR PRINTING SHIP
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
ship:
		push bp			; pushing relevant registers
		mov bp,sp
                  push es
                  push ax        	; ax color 
                  push bx        	; bx size
                  push cx        	; cx up/low
                  push dx        	
                  push di
                  push si    
		mov bx,[bp+4] 		; size of ship
		mov di,[bp+6]           ; index of ship
                  mov ax,0xb800
                  mov es,ax
                  mov ax,0x04db
                  call calculatesize	; calculting size function called
loops1:					; loop for forward prointing
                  mov word[es:di],ax 	; print ship
                  add dx,2		; size check
                  add di,2
                  cmp dx,bx
                  jne loops1
                  sub di,bx		; moving to second row of ship
                  add di,162
                  mov dx,0		; size count =0
                  sub bx,4		; comparing register lowered for next row
                  sub si,1
                  jz end		; if levels become zero end
                  mov ax,0x07db
loops2:					; loop for forward backward printing
                  mov word[es:di],ax	; printing ship
                  add dx,2
                  add di,2
                  cmp dx,bx		; comparing with bounds
                  jne loops2
                  sub di,bx
                  add di,162		; moving to next row
                  mov dx,0
                  sub bx,4
                  sub si,1
                  jz end		; if levels become zero end 
                ;  sub ax,0x0100
                  jnz loops1
end:
                  pop si 			; popping relevant used registers
                  pop di
                  pop dx
                  pop cx
                  pop bx
                  pop ax
                  pop es
                  pop bp
                  ret 4				; two parametrs size and starting index
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING ALL SHIPS
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
ships:
                  mov bx,1760		; staring index
                  push bx
                  mov bx,40		; size
                  push bx
                  call ship
                  mov bx,2016		; staring index
                  push bx
                  mov bx,52		; size
                  push bx
                  call ship
                  mov bx,2140		; staring index
                  push bx
                  mov bx,24		; size
                  push bx
                  call ship
                  ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR MOVING SCREEN OF GIVEN SIZE TO LEFT
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
movleft:
		push bp			; pushing relevant registers
		mov bp,sp
		  push ds
                  push es
                  push ax
                  push bx
                  push cx
                  push di
                  push si
			mov ax,0xb800			; video mode set
			mov es,ax
			mov ds,ax           

			mov ax,80      			; ax to 80   
			mul byte[bp+4]			; multiplying with row sent via parameter
			shl ax,1			; multiplying ax with two to get exact bytes

			mov di,ax			; moving to di
			add ax,2
			mov si,ax			; moving si 1 next to di

			mov cx,80			; comparing for single line
			push word[es:di]		; pushing first box
			cld                  		; auto inc
			rep movsw			; repeat

			sub di,2			; mov to last block of line
			pop word[es:di]			; print/pop to last block
       		  pop si 			; popping relevant used registers
                  pop di
                  pop cx
                  pop bx
                  pop ax
                  pop es
                  pop ds
		pop bp
                  ret 2
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FON MOVING SKY
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
movesky:
        push ax			; pushing relevant registers
	mov ax,1
skyloop:
        push ax
        call movleft
        inc ax			; ax ++
        cmp ax,8 		; moving till 8 line
	jne skyloop
	pop ax 			; popping relevant used registers
	ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR MOVING SCREEN TO RIGHT
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
movright:
		push bp			; pushing relevant registers
		mov bp,sp
		  push ds
                  push es
                  push ax
                  push bx
                  push cx
                  push di
                  push si
			mov ax,0xb800			; display mode
			mov es,ax
			mov ds,ax          

			mov ax,80   			; ax = 80        
			mul byte[bp+4]			; multiplying with row sent
			shl ax,1			; ax  x 2
			sub ax,2     	 		; moving ax to last block

			mov di,ax			; moving ax to di
			mov si,ax
			sub si,2			; placing si 1 before di

			mov cx,80			; count for single line
			push word[es:di]		; pushing in stack
			std                 		; decrementing 
			rep movsw			; repeat

			add si,4			; moving si to first block
			pop word[es:si]			; poping value
       		  pop si 			; popping relevant used registers
                  pop di
                  pop cx
                  pop bx
                  pop ax
                  pop es
                  pop ds
		pop bp
                  ret 2
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR MOVING SEA
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
movesea:
        push ax			; pushing relevant registers
	mov ax,10		; starting of sea
sealoop:

        push ax
        call movright
        inc ax			; inc
        cmp ax,18		; move to right till line 18 
	jne sealoop
	pop ax 			; popping relevant used registers
	ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR  MOVING DEEP SEA
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
movedeepsea:
        push ax			; pushing relevant registers
	mov ax,17	 	; ax =19 line

deepsealoop:

        push ax
        call movleft
        inc ax			; inc
        cmp ax,25 		; moving to left till last line
	jne deepsealoop
	pop ax 			; popping relevant used registers
	ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR DELAY
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
delay:
                  push cx			; pushing relevant registers
                  mov cx,0xFFFF
loop1:
                  loop loop1
                  mov cx,0xFFFF
loop2:
                  loop loop2
                  pop cx 			; popping relevant used registers
                  ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBRPUTINE FOR WEED 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
weed:
		push bp			; pushing relevant registers
		mov bp,sp
                  push es
                  push ax  		; gpr
                  push bx  		; size
                  push cx  		; color
                  push di

                  mov cx,[bp+10]		; placing color
		mov di,[bp+4]			; placing starting point
		mov bx,[bp+6]			; placing size of weed
                  mov ax,0xb800
                  mov es,ax
                  mov dx,cx
	mov dl,0xdd
	mov cl,0xde
                  mov word[es:di],cx		; printing at bottom
nextweed:
            
                  sub di,162			; printing till size user sent
                  mov word[es:di],cx
                  sub di,158
                  mov word[es:di],dx
                  sub bx,1
                  jnz nextweed

                  pop di  			; popping relevant used registers
                  pop cx
                  pop bx
                  pop ax
                  pop es
		pop bp
                  ret 4				; two parameters sent i.e size and starting index
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING IN DEEP SEA
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
deepsea: 
		push es 
 		push ax 
		push di 

 		mov ax, 0xb800 
 		mov es, ax 		; point es to video base 
	
 		mov di,2720
 		mov ax, 0x01db 		; printing deep sea
 		mov cx, 640		; number of screen locations 
 		cld 			; auto increment mode 
 		rep stosw 		; print to end of screen as of givem size 

     	        	pop di 			; popping relevant used registers	
 		pop ax 
 		pop es 	
	
                  mov bx,2			; size
                  push bx
                  mov bx,3850			; starting index
                  push bx
                  call weed

                  mov bx,1			; size
                  push bx
                  mov bx,3856			; starting index
                  push bx
                  call weed

                  mov bx,2			; size
                  push bx
                  mov bx,3862			; starting index
                  push bx
                  call weed

                  mov bx,1			; size
                  push bx
                  mov bx,3920			; starting index
                  push bx
                  call weed

                  mov bx,2			; size
                  push bx
                  mov bx,3926			; starting index
                  push bx
                  call weed

                  mov bx,1			; size
                  push bx
                  mov bx,3980			; starting index
                  push bx
                  call weed

                  mov bx,2			; size
                  push bx
                  mov bx,3988			; starting index
                  push bx
                  call weed
                  ret 2				; one parameter color

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR FISH 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
fish: 			
		push es 		; pushing relevant registers
 		push ax 
		push di 
		xor di,di
 		mov ax, 0xb800 		
 		mov es, ax 		; point es to video base 
		mov di,[cs:fishloc]		; getting fish loc in di
		mov word[es:di-2],0x18fe	; printing fish 
		mov word[es:di  ],0x18db
		mov word[es:di+2],0x18db
		mov word[es:di+4],0x18fe
 		pop di			; popping relevant used registers
 		pop ax 
 		pop es 
 		ret 			; return
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR FISH MOVE
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
checkright:
		push bx			; saving value in stack
		loopr:			; loop till value of fish become less than 160
			sub bx,160	; sub 160 each time
			cmp bx,160
			jnc loopr		; jmp if greater or equal to 160
		cmp bx,0			; cmp of bx==0
   		pop bx			; getting original value
		jne rright			; checking according to previous flags direction
		sub bx,160		; if equal than sub 160
		jmp rright			; return
		
checkleft:
		push bx			; saving original value
		loopl:			; loop till value of fish become less than 160
			sub bx,160	; sub 160 each time
			cmp bx,160
			jnc loopl		; jmp if greater or equal to 160
		cmp bx,0			; cmp of bx==0
   		pop bx			; getting original value
		jne rleft			; checking according to previous flags direction
		add bx,160		; if equal than add 160
		jmp rleft			; return
		
kbisr:
;call clrscr
		push ax
		push bx
		push es

		push cs
		pop ds

		mov ax,0xb800
		mov es,ax
		in al,0x60		; getting keyborad interupt
		cmp al,0x4d		; compare right key
		jne nextcompare1	; jump if not equal
		mov bx,[cs:fishloc]	; if equal add 2
		add bx,2
		jmp checkright		; check if on right col
		rright:			; return 
		mov [cs:fishloc],bx	; placing fish back
		jmp exit		; jump to exit
nextcompare1:
		cmp al,0x4b		; compare if left key is pressed
		jne nextcompare2	; jmp not equal
		mov bx,[cs:fishloc]	; get fish loc
		jmp checkleft		; check if left col
		rleft:			; return
		sub bx,2		; move left
		mov [cs:fishloc],bx	; place fish back
		jmp exit		; exit
nextcompare2:
		cmp al,0x48		; compare if up key
		jne nextcompare3	; jump if not equal
		mov bx,[cs:fishloc]	; get fish loc
                cmp bx,2880		; cmp with top of deep sea
		jc noup			; jump to skip 
		sub bx,160		; else go up one row
		mov [cs:fishloc],bx	; place fish back
		jmp exit		; exit
		noup:			; label
		call sound		; sound if hits top
		jmp exit		; exit
nextcompare3:
		cmp al,0x50		; check if down
		jne nextcompare4	; jump if not eqaul
		mov bx,[cs:fishloc]	; get fish loc
		cmp bx,3840		; check if bottom
		jnc nodown		; jump if not at bottom
		add bx,160		; move to bottom one row
		mov [cs:fishloc],bx	; place fish back
		jmp exit		; exit
		nodown:			; label
		call sound		; call sound if hits bottom
		jmp exit		; exit
nextcompare4:
		cmp al,0x01		; check if escape key to terminate
		jne nomatch		; jump next campare match
mov al,0x20		; ready to take next interrupt
		out 0x20,al
		pop es			; pop registers
                pop bx
		pop ax
                mov ax,10		; exit val
		iret

nomatch:				; no match key found
		pop es			; pop registers
		pop bx
		pop ax
		jmp far [cs:oldisr]	; call original isr if no key found
exit:					; label
		call collosioncheck
		mov al,0x20		; ready to take next interrupt
		out 0x20,al
		pop es			; pop registers
		pop bx
		pop ax
		iret			; return cs and ip
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR HOOKING
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
hook:
		pusha			; pushing all registers 
		xor ax,ax		; ax=0
		mov es,ax		; point es to 0
		mov ax, [es:9*4]	; placing keyboard isr function in old isr
		mov [oldisr], ax		
		mov ax, [es:9*4+2]	; placing cs too
		mov [oldisr + 2], ax

		mov ax,[es:8*4]	; placing old timer val
		mov [oldtimer],ax	; in global variable
		mov ax,[es:8*4+2]	; cs too
		mov [oldtimer+2],ax
		cli			; stopping all interrupts
		mov word[es:9*4], kbisr	; placing new keyborad function
		mov [es:9*4+2], cs	; cs too
		mov word[es:8*4], timer	; placing new keyborad function
		mov [es:8*4+2], cs	; cs too
		sti			; allowing all interrupts
		popa			; pop all registers
		ret			; return
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR UN HOOKING
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
unhook:
		pusha			; pushing all registers
		xor ax,ax		; ax=0
		mov es,ax		; es=0
		mov ax,[oldisr]		; getting old isr function value
		mov bx,[oldisr+2]	; getting cs too

		mov cx,[oldtimer]		; getting old isr function value
		mov dx,[oldtimer+2]	; getting cs too
		cli			; disabling all registers
		mov [es:9*4],ax		; placing all register in table
		mov [es:9*4+2],bx	; cs too
		mov [es:8*4],cx		; placing all register in table
		mov [es:8*4+2],dx	; cs too
		sti			; enabelling all interrupts
		popa			; popping all registers
		ret			; return
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR SOUND
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
delay1:
                  push cx			; pushing relevant registers
                  mov cx,0x0FFF
loop123:
                  loop loop123
		  pop cx
                  ret
sound:
		pusha
 		mov cx, 5
		loopsound1:  	

	       	mov al, 0b6h
		out 43h, al

		;load the counter 2 value for d3
		mov ax, 1fb4h
		out 42h, al
		mov al, ah
		out 42h, al

		;turn the speaker on
		in al, 61h
		mov ah,al
		or al, 3h
		out 61h, al
		call delay1
		mov al, ah
		out 61h, al

		call delay1

		;load the counter 2 value for a3
		mov ax, 152fh
		out 42h, al
		mov al, ah
		out 42h, al

		;turn the speaker on
		in al, 61h
		mov ah,al
		or al, 3h
		out 61h, al
		call delay1
		mov al, ah
		out 61h, al

		call delay1
	
		;load the counter 2 value for a4
		mov ax, 0A97h
		out 42h, al
		mov al, ah
		out 42h, al
	
		;turn the speaker on
		in al, 61h
		mov ah,al
		or al, 3h
		out 61h, al
		call delay1
		mov al, ah
		out 61h, al

		call delay1
 
 		loop loopsound1
		popa
		ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR TIMER 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
update1:
	call updateday			; update to day time
	mov word[cs:tickcount+2],0		; reset timer
	jmp returntimer			; return
update2:
	call updatenight			; update to night
	jmp returntimer			; return
update_random:
	mov word[cs:random],2720		; reset random to 2720 as it became greater than 3900
	jmp returnrandom			; return
updatesecond
	add word[cs:clock],1		; inc clock second
	mov word[cs:clock+2],0		; make mili sec to zero
	add word[cs:red+2],1		; inc red second counter
	cmp word[cs:red+2],5		; compare with upper limit
	je resetred			; if equal then reset
	returnresetred:			; return label
	add word[cs:green+2],1		; inc green second counter
	cmp word[cs:green+2],10		; compare with upper limit
	je resetgreen			; if equal then reset
	returnresetgreen:			; return label
	call displaypoints			; call diplay points to redisplay all points 
	jmp secondreturn			; end label
resetred:					; reset red label
	call updatered			; call update red to get new random value in red
	mov word[cs:red+2], 0		; counter to 0
	jmp returnresetred			; return
resetgreen:				; reset label for green
	call updategreen			; call update green to get new random value in green
	mov word[cs:green+2], 0		; counter to 0
	jmp returnresetgreen		; return

timer:					; timer subroutine

	pusha				; pushing all registers
	push cs
	pop ds

	add word[cs:random],10		; add random variable by 10
	cmp word[cs:random],3900		; compare it with 3900
	jnc update_random			; if greater than reset
	returnrandom:			; return from random label
	inc word[cs:clock+2]		; inc clock pulse
	cmp word[cs:clock+2],9		; comapre with 18 to make 1 sec
	je updatesecond			; if equal then update
	secondreturn:			; return label for second
	inc word[cs:tickcount]		; inc tick count
	cmp word[cs:tickcount],4		; compare with 2 to comtrol speed
	jne exittimer			; exit timer label to end
	mov word[cs:tickcount],0		; make it zero
	inc word[cs:tickcount+2]		; inc tick count to 1 as a second
	cmp word[cs:tickcount+2],160	; compare with 160 used for day and night updation
	je update1			; if equal to 160
	cmp word[cs:tickcount+2],80		; compare with 80
	je update2			; if equal to 80
returntimer:				
	;cli				; disable all interrupts   // These conditions were used before when movements were made with timer
	;call movesky			; move sky
	;call movesea			; move sea
	;sti				; enable all interrupts
exittimer:					; exit timer
	;mov al,0x20			; allow interrupts // placed after multitasking timer code // ends
	;out 0x20,al
	popa				; pop all regitser
;cli
;call Multitimer
;sti
;-------------------------------------Timer og multitasking -----------------------------------------------------
push ds
push bx
push cs
pop ds																		 ; initialize ds to data segment
mov bx, [current] ; read index of current in bx
shl bx, 1
shl bx, 1
shl bx, 1
shl bx, 1
shl bx, 1 
											; multiply by 32 for pcb start
mov [pcb+bx+0], ax 								; save ax in current pcb
mov [pcb+bx+4], cx 								; save cx in current pcb
mov [pcb+bx+6], dx 								; save dx in current pcb
mov [pcb+bx+8], si								 ; save si in current pcb
mov [pcb+bx+10], di 								; save di in current pcb
mov [pcb+bx+12], bp 								; save bp in current pcb
mov [pcb+bx+24], es 								; save es in current pcb
pop ax 										; read original bx from stack
mov [pcb+bx+2], ax	 							; save bx in current pcb
pop ax 										; read original ds from stack
mov [pcb+bx+20], ax	 							; save ds in current pcb
pop ax 										; read original ip from stack
mov [pcb+bx+16], ax								 ; save ip in current pcb
pop ax 										; read original cs from stack
mov [pcb+bx+18], ax								 ; save cs in current pcb
pop ax										 ; read original flags from stack
mov [pcb+bx+26], ax								 ; save cs in current pcb
mov [pcb+bx+22], ss 								; save ss in current pcb
mov [pcb+bx+14], sp 								; save sp in current pcb
mov bx, [pcb+bx+28]								 ; read next pcb of this pcb
mov [current], bx									 ; update current to new pcb
mov cl, 5
shl bx, cl 										; multiply by 32 for pcb start
mov cx, [pcb+bx+4] 								; read cx of new process
mov dx, [pcb+bx+6] 								; read dx of new process
mov si, [pcb+bx+8] 								; read si of new process
mov di, [pcb+bx+10] 								; read diof new process
mov bp, [pcb+bx+12] 								; read bp of new process
mov es, [pcb+bx+24]								 ; read es of new process
mov ss, [pcb+bx+22]								 ; read ss of new process
mov sp, [pcb+bx+14] 								; read sp of new process
push word [pcb+bx+26]							 ; push flags of new process
push word [pcb+bx+18]							 ; push cs of new process
push word [pcb+bx+16]							 ; push ip of new process
push word [pcb+bx+20]
 											; push ds of new process
mov al, 0x20
out 0x20, al ; send EOI to PIC

mov ax, [pcb+bx+0]								 ; read ax of new process
mov bx, [pcb+bx+2] 								; read bx of new process
pop ds 	

iret				; return 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR INIT PCB
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

; subroutine to register a new thread
; takes the segment, offset, of the thread routine and a parameter
; for the target thread subroutine

initpcb:

push bp
mov bp, sp
push ax
push bx
push cx
push si

mov bx, [bp+4] 			; read next available pcb index
cmp bx, 3				; are all PCBs used
je multiexit 				; yes, exit
mov cl, 5
shl bx, cl 				; multiply by 32 for pcb start
mov ax, [bp+8] 			; read segment parameter
mov [pcb+bx+18], ax	 	; save in pcb space for cs
mov ax, [bp+6] 			; read offset parameter

mov [pcb+bx+16], ax			 	; save in pcb space for ip
mov [pcb+bx+22], ds 			; set stack to our segment
mov si, [bp+4] 					; read this pcb index
mov cl, 9
shl si, cl 						; multiply by 512
add si, 256*2+stack 				; end of stack for this thread
;mov ax, [bp+4] 					; read parameter for subroutine
sub si, 2					 	; decrement thread stack pointer
;mov [si], ax 						; pushing param on thread stack
;sub si, 2 							; space for return address
mov [pcb+bx+14], si					 ;	 save si in pcb space for sp
mov word [pcb+bx+26], 0x0200				 ; initialize thread flags
mov ax, [pcb+28] 							; read next of 0th thread in ax
mov [pcb+bx+28], ax 						; set as next of new thread
mov ax, [bp+4] 								; read new thread index
mov [pcb+28], ax							 ; set as next of 0th thread
add word [bp+4], 1

multiexit:
pop si
pop cx
pop bx
pop ax
pop bp
ret 6
;--------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; MULTITASKING GLOBAL VARIABLES INITIALLIZED
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

; PCB layout:
; ax,bx,cx,dx,si,di,bp,sp,ip,cs,ds,ss,es,flags,next,dummy
; 0, 2, 4, 6, 8,10,12,14,16,18,20,22,24, 26 , 28 , 30

pcb: times 3*16 dw 0 ; space for 32 PCBs
stack: times 3*256 dw 0 ; space for 32 512 byte stacks
nextpcb: dw 1 ; index of next free pcb
current: dw 0 ; index of current pcb
lineno:  dw 1 ; line number for next thread

; TASK OF MULTITASKING // THREAD
mytaskone:
infiniteloopmultitimer:
	call Music
jmp infiniteloopmultitimer
ret

setmultitask:				; SUBROUTINE FOR SETTING MULTITASKING
push cs					 ; use current code segment;
mov ax, mytaskone
push ax					 ; use mytask as offset
push word 1 
call initpcb				 ; register the thread
inc word [lineno]			 ; update line number
ret
;----------------------------------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR CHECKING COLLOSION
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
redcollide:
	add word[cs:score],50			; add score 50 on red collision
	call updatered					; take next random number
	jmp redcollidereturn				; return
greencollide:
	add word[cs:score],10			; add score 10 on greem collision
	call updategreen					; take new random number to greeen
	jmp greencollidereturn			; return

collosioncheck:
	pusha							; pushing all registers
	mov ax,[cs:fishloc]				; checking fish address
	cmp ax,[cs:red]					; comparing with red point
	je redcollide						; if equal jump
	redcollidereturn:					; return
	cmp ax,[cs:green]				; compare fish with green index point
	je greencollide					; if equal jump
	greencollidereturn:				; return
	call displaypoints				; display points on screem
	popa							; popping all regiters
ret									; return
	
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR DISPLAY POINTS
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
displaypoints:
	pusha							; pushing alll registers
	mov ax,0xb800					; ax video move set
	mov es,ax
	mov di,[cs:red]					; displaying di
	mov word[es:di],0x14fe			; color and ascii
	mov di,[cs:green]				; displaying green color
	mov word[es:di],0x12fe			; color and ascii
	popa							; popping all registers
ret									; return
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR UPDATE RED
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
updatered:
	pusha							; pushing all registers
	mov word[cs:red+2],0			; mov counter to 0
	mov ax,[cs:random]				; getting random number
	mov [cs:red],ax					; mov value to red
	popa							; popping all registers
ret									; return

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR UPDATE GREEN
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
updategreen:
	pusha					; pushing all registers
	mov word[cs:green+2],0	; making counter 0
	mov ax,[cs:random]		; getting random value
	mov [cs:green],ax		; mov  to green
	popa					; popping all registers
ret							; return
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR SCORE
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
printnum: 
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax		 	; point es to video base
	mov ax, [bp+4]		 ; load number in ax
	mov bx, 10			 ; use base 10 for division
	mov cx, 0 			 ; initialize count of digits
	nextdigit: mov dx, 0		 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30				 ; convert digit into ascii value
	push dx					 ; save ascii value on stack
	inc cx			 		 ; increment count of values
	cmp ax, 0				 ; is the quotient zero
	jnz nextdigit		 		; if no divide it again
	mov di, 152		 		; point di to 70th column
	nextpos: pop dx		 ; remove a digit from the stack
	mov dh, 0x1f		 ; use normal attribute
	mov [es:di], dx		 ; print char on screen
	add di, 2			 ; move to next screen location
	loop nextpos		 ; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax 
	pop es
	pop bp
ret 2
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR LIVE 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
daynow1:					; day 
mov bx,0x1adb				; light green weed color
push bx						; stack in
jmp return_live				; return
nightnow1:					; night
mov bx,0x12db				; dark green weed color
push bx						; stack in
jmp return_live				; return

live:	
cli
	call movesky			; move sky
	call movesea			; move sea
sti
	cmp word[cs:tickcount+2],79		; check if tickcount less than 79
	jna daynow1			; jump if less
	cmp word[cs:tickcount+2],79		; check if tick count greater
	ja nightnow1			; jump if greater
return_live:
      	call deepsea			; print deep sea and weed
     	call fish				; print fish
	push word [cs:score]		; print score
	call printnum ; print score count	; print num
	call displaypoints			; display all points
	call delay				; delay 
        	cmp ax,10				; check is escape is pressed
        	je endlive				; if equal then end infinite loop
       	 jne live				; if not then infinite loop
endlive:					; label
		
		call unhook		; un hooking keyboard interrupt
		call saveScreen
		call Saving_Score
		call End_Page
; call Introduction_Page

repeatkeyexit:

		mov ah, 0 ; service 0 – get keystroke
		int 0x16 ; call BIOS keyboard service
		cmp al,0x6e
		je Againlive	
		cmp al,0x79
		je exitend
		jmp repeatkeyexit
	
      	
Againlive: 
	call restoreScreen
	call hook			; hooking fish move keyborad interrupt

jmp live
	
exitend:
         	ret				; return
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR RANDOM NUMBER GENERATION
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

RANDGEN:        		 	; generate a rand no using the system time
	RANDSTART:
	push bp
	mov bp,sp
	pusha
   	mov ah, 00h 		; interrupts to get system time        
   	int 1ah      		; CX:DX now hold number of clock ticks since midnight      
   	mov  ax, dx
   	xor  dx, dx
   	mov  cx, 10    
   	div  cx       		; here dx contains the remainder of the division - from 0 to 9
   	add  dl, '0'  		; to ascii from '0' to '9'
	mov bx,0x0f00
	mov bl,dl
	mov [bp+4],bx
	popa
	pop bp
ret   
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR UPDATION
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
updateday:
	pusha
                call clrscr
        	mov bx, 0x0bdb		; parameter sky color 
	push bx
	call sky			; sky printing function
        	mov bx,0x0edb		; parameter planet color
	push bx
        	mov bx,312		; parmeter starting index of planet
	push bx
        	call planet 		; calling planet fun
        	mov bx, 0x0adb		; parameter for  mountaings grass color
        	push bx
        	mov bx,0x06db		; parameter for mountains soil color
        	push bx
	call mountainss		; calling mountains fun
	mov bx,0x0fdb
	push bx
        	call clouds		; calling clouds fun
       	mov bx,0x09db		; parameter for sea color
	push bx
	call sea			; calling sea fun
        	mov bx,0x0adb		; parameter for ground color
	push bx
        	mov bx,1280		; parameter for ground starting index
	push bx
        	call ground		; calling ground fun
	call ships			; calling ship fun
	mov bx,0x1adb		; parameter for deep sea weed color
	push bx
	call deepsea		; calling deep sea fun
        	call fish
	mov bx,0x01db		; parameter for ground color
	push bx
        	mov bx,0		; parameter for ground starting index
	push bx
        	call ground		; calling ground fun
call Gameheader
	popa
	ret

updatenight:
           pusha
                call clrscr
        	mov bx, 0x00db		; parameter sky color 
	push bx
	call sky			; sky printing function
        	mov bx,0xffdb		; parameter planet color
	push bx
        	mov bx,312		; parmeter starting index of planet
	push bx
        	call planet 		; calling planet fun
        	mov bx, 0x02db		; parameter for  mountaings grass color
        	push bx
        	mov bx,0x04db		; parameter for mountains soil color
        	push bx
	call mountainss		; calling mountains fun
	mov bx,0x07db
	push bx
        	call clouds		; calling clouds fun
	call stars
       	mov bx,0x01db		; parameter for sea color
	push bx
	call sea			; calling sea fun
        	mov bx,0x02db		; parameter for ground color
	push bx
        	mov bx,1280		; parameter for ground starting index
	push bx
        	call ground		; calling ground fun
	call ships			; calling ship fun
	mov bx,0x12db		; parameter for deep sea weed color
	push bx
	call deepsea		; calling deep sea fun
        	call fish
mov bx,0x01db		; parameter for ground color
	push bx
        	mov bx,0		; parameter for ground starting index
	push bx
        	call ground		; calling ground fun
call Gameheader
	popa
	ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR INTRODUCTION PAGE STRING PRINTING
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

Printing_String:

        push di				; di used here in place of bp because bp used in printing 
        mov di, sp
        push ax
        push bx
        push es
        push bp
        push dx

        	mov ah, 0x13      		 ;bios print string service
	  	mov al, 0			; print on derired location
        	mov bh, 0          		;to print on page 0
        	mov cx, [di+12]      
		mov bl,cl			 ;attribute for string
 
	  	mov dh, [di+4]		; row
		mov dl, [di+6]		; col

        	push ds       
        	pop es         
        	mov bp, [di + 10] 		 ;putting string's ip in bp
        	mov cx, [di + 8]  		;putting size in cx
        	int 0x10

        pop dx					; popping all registers
        pop bp
        pop es
        pop bx
        pop ax
        pop di

        ret 10					; returning with parameters
;-------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
; SUBROUTINE FOR INTRODUCTION PAGE PRINTING
;--------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
Printing_Intro_page:
pusha
;----------------------------------------
; Printing Name
;----------------------------------------
xor bx,bx
mov bl,0x1b
push bx
mov ax,buffername+2	; string
push ax
xor ax,ax
mov al,[buffername+1]	; size of string
push ax
mov al,120	; coloumn
push ax
mov al,2			; row
push ax
call Printing_String
;----------------------------------------
; Printing Hello
;----------------------------------------
xor bx,bx
mov bl,0x1f
push bx
mov ax,introname		; string
push ax
xor ax,ax
mov al,[intronamesize]	; size of string
push ax
mov al,106			; coloumn
push ax
mov al,2			; row
push ax
call Printing_String
;----------------------------------------
; Printing Welcome Game
;----------------------------------------
xor bx,bx
mov bl,0x1f
push bx
mov ax,introwelcome		; string
push ax
xor ax,ax
mov al,[introwelcomesize]	; size of string
push ax
mov al,106			; coloumn
push ax
mov al,4			; row
push ax
call Printing_String
;----------------------------------------
; Printing Instructions Game
;----------------------------------------
xor bx,bx
mov bl,0x1e
push bx
mov ax,introInstructions		; string
push ax
xor ax,ax
mov al,[introInstructionssize]	; size of string
push ax
mov al,108			; coloumn
push ax
mov al,6				; row
push ax
call Printing_String
;----------------------------------------
; Printing Move Instructions 1 Game
;----------------------------------------
xor bx,bx
mov bl,0x1d
push bx
mov ax,intromove1		; string
push ax
xor ax,ax
mov al,[intromovesize1]	; size of string
push ax
mov al,102			; coloumn
push ax
mov al,8				; row
push ax
call Printing_String
;----------------------------------------
; Printing Move Instructions 2 Game
;----------------------------------------
xor bx,bx
mov bl,0x1b
push bx
mov ax,intromove2		; string
push ax
xor ax,ax
mov al,[intromovesize2]	; size of string
push ax
mov al,102 		; coloumn
push ax
mov al,10			; row
push ax
call Printing_String
;----------------------------------------
; Printing Score Instructions 1 Game
;----------------------------------------
xor bx,bx
mov bl,0x1a
push bx
mov ax,introscore1		; string
push ax
xor ax,ax
mov al,[introscoresize1]	; size of string
push ax
mov al,102			; coloumn
push ax
mov al,12			; row
push ax
call Printing_String
;----------------------------------------
; Printing Score Instructions 2 Game
;----------------------------------------
xor bx,bx
mov bl,0x14
push bx
mov ax,introscore2		; string
push ax
xor ax,ax
mov al,[introscoresize2]	; size of string
push ax
mov al,102			; coloumn
push ax
mov al,14			; row
push ax
call Printing_String
;----------------------------------------
; Printing Score Instructions 2 Game
;----------------------------------------
xor bx,bx
mov bl,0x14
push bx
mov ax,introscore2		; string
push ax
xor ax,ax
mov al,[introscoresize2]	; size of string
push ax
mov al,102			; coloumn
push ax
mov al,14			; row
push ax
call Printing_String
;----------------------------------------
; Printing Score Instructions 3 Game
;----------------------------------------
xor bx,bx
mov bl,0x9b
push bx
mov ax,intromessage		; string
push ax
xor ax,ax
mov al,[intromessagesize]	; size of string
push ax
mov al,102			; coloumn
push ax
mov al,16			; row
push ax
call Printing_String
;----------------------------------------
; Printing Developer 1
;----------------------------------------
xor bx,bx
mov bl,0x2f
push bx
mov ax,introdeveloper1		; string
push ax
xor ax,ax
mov al,[introdeveloper1size]	; size of string
push ax
mov al,120			; coloumn
push ax
mov al,21			; row
push ax
call Printing_String
;----------------------------------------
; Printing Developer 2
;----------------------------------------
xor bx,bx
mov bl,0x2f
push bx
mov ax,introdeveloper2		; string
push ax
xor ax,ax
mov al,[introdeveloper2size]	; size of string
push ax
mov al,137		; coloumn
push ax
mov al,22		; row
push ax
call Printing_String

popa
ret
;-------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
; SUBROUTINE FOR INTRODUCTION PAGE STARS
;--------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
updatestars:
mov di,[introstartsreset + si]
jmp returnupdatestars
updatestars1:
mov di,[introstartsreset + si]
jmp returnupdatestars1
updatestars2:
mov di,[introstartsreset + si]
jmp returnupdatestars2
; Starts_Intro
Intro_Stars:
pusha
mov ax,0xb800
mov es,ax
mov si,0
mov ax,0
starsloop:
xor bx,bx
		call delayintro
		mov di,[introstarts+si]
		mov word[es:di],0x0720
		mov word[es:di-480],0x0720
		mov word[es:di-640],0x0720
		mov word[es:di-960],0x0720
		mov word[es:di-1280],0x0720
		add di,320
		cmp di,3360
		jnc updatestars
		returnupdatestars:
	mov word[es:di],0x0f2a
	mov word[es:di-480],0x042a
	mov word[es:di-640],0x0e2a
	mov word[es:di-960],0x0a2a
	mov word[es:di-1280],0x0b2a
		mov [introstarts+si],di
		add si,2
		mov di,[introstarts+si]
		mov word[es:di-640],0x0720
		mov word[es:di-480],0x0720
		mov word[es:di-960],0x0720
		mov word[es:di-1280],0x0720
		mov word[es:di],0x0720
		add di,640
		cmp di,3360
		jnc updatestars1
		returnupdatestars1:
	mov word[es:di],0x0f2a
	mov word[es:di-480],0x042a
	mov word[es:di-640],0x0e2a
	mov word[es:di-960],0x0a2a
	mov word[es:di-1280],0x0b2a
		mov [introstarts+si],di
		add si,2

		mov di,[introstarts+si]
		mov word[es:di],0x0720
		mov word[es:di-480],0x0720
		mov word[es:di-640],0x0720
		mov word[es:di-960],0x0720
		mov word[es:di-1280],0x0720
		add di,1280
		cmp di,3360
		jnc updatestars2
		returnupdatestars2:
	mov word[es:di],0x0f2a
	mov word[es:di-480],0x042a
	mov word[es:di-640],0x0e2a
	mov word[es:di-960],0x0a2a
	mov word[es:di-1280],0x0b2a
		mov [introstarts+si],di
		add si,2
		cmp si,60
		jne starsloop
	mov si,0
inc ax
cmp ax,50
jne starsloop
popa
ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR DELAY INTRODUCTION
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
delayintro:
                  push cx			; pushing relevant registers
                  mov cx,0xFFFF
loopintro:
                  loop loopintro
                  mov cx,0xFFFF
                  pop cx				; popping all registers
                  ret
;--------------------------------------------------------------------------------------
; SUBROUTINE FOR INTRODUCTION PAGE LAYOUT
;--------------------------------------------------------------------------------------
Layout:
push bp
mov bp,sp
pusha 

mov ax,0xb800			; setting to video mode
mov es,ax
mov dx,0x01db			; color
mov di,124
mov cx,19

layoutouterloop:			; printing Square Box
add di,70
mov ax,di
add ax,90
layoutinnerloop:
mov [es:di],dx
add di,2
cmp di,ax
jne layoutinnerloop
loop layoutouterloop
							; PRINTING CORNERS & EDGES
mov di,194			; top-left		
mov word[es:di],0x1fc9
layouttop:
add di,2
mov word[es:di],0x1fcd
cmp di,282
jne layouttop
mov di,282			; top-right
mov word[es:di],0x1fbb
layoutright:
add di,160
mov word[es:di],0x1fba
cmp di,3162
jne layoutright
mov di,3162		; bottom-right
mov word[es:di],0x1fbc
layoutbottom:
sub di,2
mov word[es:di],0x1fcd
cmp di,3074
jne layoutbottom
mov di,3074		; bottom-left
mov word[es:di],0x1fc8
layoutleft:
sub di,160
mov word[es:di],0x1fba
cmp di,354
jne layoutleft
mov di,356		; top-left
mov word[es:di],0x9ac9
layouttop1:
add di,2
mov word[es:di],0x9acd
cmp di,440
jne layouttop1
mov di,440			; top-right
mov word[es:di],0x9abb
layoutright1:
add di,160
mov word[es:di],0x9aba
cmp di,3000
jne layoutright1
mov di,3000		; bottom-right
mov word[es:di],0x9abc
layoutbottom1:
sub di,2
mov word[es:di],0x9acd
cmp di,2916
jne layoutbottom1
mov di,2916		; bottom-left
mov word[es:di],0x9ac8
layoutleft1:
sub di,160
mov word[es:di],0x9aba
cmp di,516
jne layoutleft1
mov di,996
mov word[es:di],0x9ac8
layoutmid1:
add di,2
mov word[es:di],0x9acd
cmp di,1080
jne layoutmid1
mov word[es:di],0x9abc
mov di,1156
mov word[es:di],0x9ac9
layoutmid2:
add di,2
mov word[es:di],0x9acd
cmp di,1240
jne layoutmid2
mov word[es:di],0x9abb
;-------------------------DEVELOPER LAYOUT PRINTING-----------------------------------
mov dx,0x02db	; color
mov di,3360				; Printing Box of developer
mov cx,4
layoutouterloop2:
add di,76
mov ax,di
add ax,84
layoutinnerloop2:
mov [es:di],dx
add di,2
cmp di,ax
jne layoutinnerloop2
loop layoutouterloop2
;--------------------------------------------------; Printing Corners and edges

mov di,3436			; top-left
mov word[es:di],0xaec9
layouttop2:
add di,2
mov word[es:di],0xaecd
cmp di,3518
jne layouttop2
mov di,3518			; top-right
mov word[es:di],0xaebb
layoutright2:
add di,160
mov word[es:di],0xaeba
cmp di,3998
jne layoutright2
mov di,3998		; bottom-right
mov word[es:di],0xaebc
layoutbottom2:
sub di,2
mov word[es:di],0xaecd
cmp di,3916
jne layoutbottom2
mov di,3916		; bottom-left
mov word[es:di],0xaec8
layoutleft2:
sub di,160
mov word[es:di],0xaeba
cmp di,3596
jne layoutleft2
popa
pop bp
ret						; returning function intro layout
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR USER NAME
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
Username:
pusha
mov dx,messagename			; Display message
mov ah,09
int 0x21
mov dx,buffername				; take input name
mov ah,0x0a
int 0x21
mov bh,0
mov bl,[buffername+1]			; calculate size and store it in string
mov byte[buffername+2+bx],'$'
popa
ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR INTRODUCTION PAGE BOXES BACKGROUND
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
introstarsall:
pusha
mov ax,0xb800
mov es,ax
mov di,0
loopstarsall:
mov word[es:di],		0x01b0
mov word[es:di+4],	0x02b0
mov word[es:di+8],	0x03b0
mov word[es:di+12],	0x04b0
mov word[es:di+16],	0x05b0
mov word[es:di+20],	0x06b0
mov word[es:di+24],	0x07b0
mov word[es:di+28],	0x08b0
mov word[es:di+32],	0x09b0
mov word[es:di+36],	0x0ab0
mov word[es:di+40],	0x0bb0
mov word[es:di+44],	0x0cb0
mov word[es:di+48],	0x0db0
mov word[es:di+52],	0x0eb0
mov word[es:di+56],	0x0fb0
add di,60
cmp di,4000
jl loopstarsall
popa
ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR INTRODUCTION PAGE
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
Introduction_Page:
pusha
mov ah,0
mov al,13H
int 0x10
call Username
call clrscr
call introstarsall
call Layout
call Printing_Intro_page
;call Intro_Stars
repeatinput:
mov ah,0
int 0x16
cmp al,0x1B		; escape
je exitintro

cmp al,0x0D		; any other than enter
jne repeatinput
je endintrosafe

exitintro:
	call saveScreen
call End_Page

repeatkeyexitintro:

		mov ah, 0			 ; service 0 – get keystroke
		int 0x16				 ; call BIOS keyboard service
		cmp al,0x6e			; N key
		je Againliveintro	
		cmp al,0x79			; Y key
		je exitendintro
		jmp repeatkeyexit	; else take key agian
	
Againliveintro:
call restoreScreen
jmp repeatinput
exitendintro:
popa
add sp,2						; clearing stack
jmp TerminationProgram		; jumping to termination
endintrosafe:					; label normal program will run from here
popa
ret 
;--------------------------------------------------------------------------------------
; SUBROUTINE FOR END PAGE
;--------------------------------------------------------------------------------------

End_Page:
pusha

call End_PageLayout
call Endpagestring

popa
ret

;--------------------------------------------------------------------------------------
; SUBROUTINE FOR END PAGE LAYOUT
;--------------------------------------------------------------------------------------

End_PageLayout:

pusha
mov ax,0xb800
mov es,ax

mov dx,0x01db	; color
; starting from 34
mov di,1234
mov cx,7

layoutouterloopep:				; layout box printing
add di,90
mov ax,di
add ax,70
layoutinnerloopep:
mov [es:di],dx
add di,2
cmp di,ax
jne layoutinnerloopep
loop layoutouterloopep

mov di,1162		; top-left				; Layout corners & edges printing
mov word[es:di],0x1ec9
layouttopep:
add di,2
mov word[es:di],0x1ecd
cmp di,1234
jne layouttopep
mov di,1234			; top-right
mov word[es:di],0x1ebb
layoutrightep:
add di,160
mov word[es:di],0x1eba
cmp di,2514
jne layoutrightep
mov di,2514		; bottom-right
mov word[es:di],0x1ebc
layoutbottomep:
sub di,2
mov word[es:di],0x1ecd
cmp di,2442
jne layoutbottomep
mov di,2442	; bottom-left
mov word[es:di],0x1ec8
layoutleftep:
sub di,160
mov word[es:di],0x1eba
cmp di,1322
jne layoutleftep
;------------------------------------BLINKING GAME OVER WITH RED LAYOUT
mov di,1324		; top-left
mov word[es:di],0x94c9
layouttopep1:
add di,2
mov word[es:di],0x94cd
cmp di,1392
jne layouttopep1
mov di,1392			; top-right
mov word[es:di],0x94bb
layoutrightep1:
add di,160
mov word[es:di],0x94ba
cmp di,2352
jne layoutrightep1
mov di,2352		; bottom-right
mov word[es:di],0x94bc
layoutbottomep1:
sub di,2
mov word[es:di],0x94cd
cmp di,2284
jne layoutbottomep1
mov di,2284	; bottom-left
mov word[es:di],0x94c8
layoutleftep1:
sub di,160
mov word[es:di],0x94ba
cmp di,1484
jne layoutleftep1
popa
ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR END PAGE STRING
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
Endpagestring:
pusha
;----------------------------------------
; Printing EXIT Instructions Game
;----------------------------------------
xor bx,bx
mov bl,0x1e
push bx
mov ax,endexit		; string
push ax
xor ax,ax
mov al,[endexitsize]	; size of string
push ax
mov al,106			; coloumn
push ax
mov al,8		; row
push ax
call Printing_String
;----------------------------------------
; Printing EXIT Instructions Game
;----------------------------------------
xor bx,bx
mov bl,0x1a
push bx
mov ax,endyes		; string
push ax
xor ax,ax
mov al,[endyessize]	; size of string
push ax
mov al,108			; coloumn
push ax
mov al,11			; row
push ax
call Printing_String
;----------------------------------------
; Printing EXIT Instructions Game
;----------------------------------------
xor bx,bx
mov bl,0x1c
push bx
mov ax,endno		; string
push ax
xor ax,ax
mov al,[endnosize]	; size of string
push ax
mov al,122			; coloumn
push ax
mov al,11			; row
push ax
call Printing_String
popa
ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR SAVE SCREEN
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
saveScreen:	
			pusha						; push all general purpose registers
			mov cx, 2000 					; number of screen location
			mov ax, 0xb800
			mov es, ax					; ds = 0xb800
			mov si, 0
savescreenloop:
			mov ax,[es:di]
			mov [buffer+di],ax
			add di,2
			loop savescreenloop
			popa						; pop all general purpose registers
			ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR RESTORE SCREEN
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
restoreScreen:		
			pusha						; push all general purpose registers
			mov cx, 4000 					; number of screen locations
			mov ax, 0xb800
			mov es, ax 					; ds = 0xb800
			push cs
			pop ds		
			mov si, buffer
			mov di, 0
			cld 						; set auto increment mode
			rep movsb 					; save screen
			popa						; pop all general purpose registers
			ret	
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR GAME HEADER
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
Gameheader:
pusha
;----------------------------------------
; Printing Header GAME NAME
;----------------------------------------
xor bx,bx
mov bl,0x1f
push bx
mov ax,gamename		; string
push ax
xor ax,ax
mov al,[gamenamesize]	; size of string
push ax
mov al,32			; coloumn
push ax
mov al,0			; row
push ax
call Printing_String
;----------------------------------------
; Printing Header PLAYER NAME
;----------------------------------------
xor bx,bx
mov bl,0x1b
push bx
mov ax,playername		; string
push ax
xor ax,ax
mov al,[playernamesize]	; size of string
push ax
mov al,4		; coloumn
push ax
mov al,0			; row
push ax
call Printing_String
;----------------------------------------
; Printing Header PLAYER NAME
;----------------------------------------
xor bx,bx
mov bl,0x1e
push bx
mov ax,buffername+2		; string
push ax
xor ax,ax
mov al,[buffername+1]	; size of string
push ax
mov al,14	; coloumn
push ax
mov al,0			; row
push ax
call Printing_String
;----------------------------------------
; Printing Header GAME SCORE
;----------------------------------------
xor bx,bx
mov bl,0x1a
push bx
mov ax,currentscore		; string
push ax
xor ax,ax
mov al,[currentscoresize]	; size of string
push ax
mov al,66 		; coloumn
push ax
mov al,0			; row
push ax
call Printing_String
popa
ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR END DISPLAY
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
End_Display:
call End_PageLayout
pusha
;----------------------------------------
; Printing Header GAME NAME
;----------------------------------------
xor bx,bx
mov bl,0x1f
push bx
mov ax,gamename		; string
push ax
xor ax,ax
mov al,[gamenamesize]	; size of string
push ax
mov al,110			; coloumn
push ax
mov al,8			; row
push ax
call Printing_String
;----------------------------------------
; Printing Header EXIT GAME OVER
;----------------------------------------
xor bx,bx
mov bl,0x9c
push bx
mov ax,endterminate		; string
push ax
xor ax,ax
mov al,[endterminatesize]	; size of string
push ax
mov al,110		; coloumn
push ax
mov al,9			; row
push ax
call Printing_String
;----------------------------------------
; Printing Header PLAYER NAME
;----------------------------------------
xor bx,bx
mov bl,0x1e
push bx
mov ax,buffername+2		; string
push ax
xor ax,ax
mov al,[buffername+1]	; size of string
push ax
mov al,120	; coloumn
push ax
mov al,11			; row
push ax
call Printing_String
;----------------------------------------
; Printing Header PLAYER NAME
;----------------------------------------
xor bx,bx
mov bl,0x1b
push bx
mov ax,playername		; string
push ax
xor ax,ax
mov al,[playernamesize]	; size of string
push ax
mov al,110		; coloumn
push ax
mov al,11			; row
push ax
call Printing_String
;----------------------------------------
; Printing Header GAME SCORE
;----------------------------------------
xor bx,bx
mov bl,0x1a
push bx
mov ax,currentscore		; string
push ax
xor ax,ax
mov al,[currentscoresize]	; size of string
push ax
mov al,110		; coloumn
push ax
mov al,12			; row
push ax
call Printing_String
;----------------------------------------
; Printing Header GAME SCORE
;----------------------------------------
; 2156
mov ax,0xb800
mov es,ax
mov di,2156
mov si,0

loopprintscore:
mov ax,[lastscore+si]
mov [es:di],ax
add si,2
add di,2
cmp si,12
jne loopprintscore
popa
ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR SAVING SCORE
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
Saving_Score:
pusha
mov ax,0xb800
mov es,ax

mov di,148
mov si,0
savescoreloop:
mov ax,[es:di]
mov [lastscore+si],al
add si,1
add di,1
cmp si,16
jne savescoreloop

popa
ret

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR MUSIC
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

Music:

pusha

call MusicSound1
call MusicSound2
call MusicSound3
call MusicSound4
call MusicSound5
call MusicSound6
call MusicSound5
call MusicSound4
call MusicSound3
call MusicSound2

popa
ret

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR SOUND TONE & DELAY
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

delayTone:
push cx
mov cx,0xFFFF
tonedelayloop:
loop tonedelayloop
pop cx
ret

MusicTone:
cli
push bp
mov bp,sp
    pusha
    	
    	 out 42h, al
   	 mov al, ah
   	 out 42h, al		 ;turn the speaker on
   
 		mov ax,[bp+4]

	 in al, 61h
    	 mov ah,al
   	 or al, 3h
    	 out 61h, al

    	 call delayTone
	call delayTone
	call delayTone


    	mov al, ah		 ;turn the speaker off
    	out 61h, al
    popa
pop bp
sti
    ret 2
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR ALL SOUNDS
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

MusicSound1:
pusha
mov ax,[a1]
push ax
call MusicTone

mov ax,[b1]
push ax
call MusicTone

mov ax,[c1]
push ax
call MusicTone


mov ax,[d1]
push ax
call MusicTone

mov ax,[e1]
push ax
call MusicTone


mov ax,[f1]
push ax
call MusicTone

popa
ret

MusicSound2:
pusha

mov ax,[a2]
push ax
call MusicTone

mov ax,[b2]
push ax
call MusicTone

mov ax,[c2]
push ax
call MusicTone


mov ax,[d2]
push ax
call MusicTone

mov ax,[e2]
push ax
call MusicTone


mov ax,[f2]
push ax
call MusicTone
popa
ret

MusicSound3:
pusha


mov ax,[a3]
push ax
call MusicTone

mov ax,[b3]
push ax
call MusicTone

mov ax,[c3]
push ax
call MusicTone


mov ax,[d3]
push ax
call MusicTone

mov ax,[e3]
push ax
call MusicTone


mov ax,[f3]
push ax
call MusicTone

popa
ret


MusicSound4:
pusha

mov ax,[a4]
push ax
call MusicTone

mov ax,[b4]
push ax
call MusicTone

mov ax,[c4]
push ax
call MusicTone


mov ax,[d4]
push ax
call MusicTone

mov ax,[e4]
push ax
call MusicTone


mov ax,[f4]
push ax
call MusicTone


popa
ret


MusicSound5:
pusha

mov ax,[a5]
push ax
call MusicTone

mov ax,[b5]
push ax
call MusicTone

mov ax,[c5]
push ax
call MusicTone


mov ax,[d5]
push ax
call MusicTone

mov ax,[e5]
push ax
call MusicTone


mov ax,[f5]
push ax
call MusicTone

popa
ret


MusicSound6:
pusha


mov ax,[a6]
push ax
call MusicTone

mov ax,[b6]
push ax
call MusicTone

mov ax,[c6]
push ax
call MusicTone


mov ax,[d6]
push ax
call MusicTone

mov ax,[e6]
push ax
call MusicTone


mov ax,[f6]
push ax
call MusicTone

popa
ret


;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;MAIN PROGRAM START HERE
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
start:
call Introduction_Page
call clrscr		; clear screeen fun

        	mov bx, 0x0bdb		; parameter sky color 
	push bx
	call sky		; sky printing function
        
        	mov bx,0x0edb		; parameter planet color
	push bx
        	mov bx,312		; parmeter starting index of planet
	push bx
        	call planet 		; calling planet fun

        	mov bx, 0x0adb		; parameter for  mountaings grass color
        	push bx
        	mov bx,0x06db		; parameter for mountains soil color
        	push bx
	call mountainss		; calling mountains fun
	mov bx,0x0fdb
	push bx
        	call clouds		; calling clouds fun
        	mov bx,0x09db		; parameter for sea color
	push bx
	call sea			; calling sea fun
        	mov bx,0x0adb		; parameter for ground color
	push bx
        	mov bx,1280		; parameter for ground starting index
	push bx
        	call ground		; calling ground fun
	call ships			; calling ship fun
	mov bx,0x1adb		; parameter for deep sea weed color
	push bx
	call deepsea		; calling deep sea fun
        	call fish			; printing fish
	
call hook			; hooking fish move keyborad interrupt

mov bx,0x01db		; parameter for ground color
	push bx
        	mov bx,0		; parameter for ground starting index
	push bx
        	call ground		; calling ground fun
call Gameheader
call setmultitask

       	call live			; calling infinite live fun
;	call unhook		; un hooking keyboard interrupt
	call clrscr			; clearing screen
	call End_Display
	jmp gameisterminated
TerminationProgram:
	call clrscr			; clearing screen
gameisterminated:
        mov ax,0x4c00		; termination
	int 0x21

; Global Variables
oldisr:	dw 0,0			; space for saving old isr
tickcount: dw 0,0			; tickcount timer value
oldtimer:  dw 0,0 		; space for old timer function
;-------------------------------------------------------END-----------------------------------------------------------------
