[org 0x0100]

jmp start

msg:dw 'Enter your Roll No : $'

start:
mov ah,09H
mov dx,msg
int 21H 

mov ax,0x4c00
int 0x21
