[org 0x0100]

jmp start

num:db 0x2
num2:db 0x3
pow:db 0x3
result:db 0x0

power:
push bp
mov bp,sp
push bx
push cx
push ax

mov bl,[bp+8]
mov al,bl
mov cx,[bp+6]
sub cx,1
l1:
  mul bx
  dec cx
jnz l1

res: mov byte[result],al
pop ax
pop cx
pop bx
pop bp

ret 6

start:
xor ax,ax
mov al,[num]
push ax
mov al,[pow]
push ax
mov al,[result]
push ax
call power
pop ax

mov al,[num2]
push ax
mov al,[pow]
push ax
mov al,[result]
push ax
call power


mov ax,0x4c00
int 0x21