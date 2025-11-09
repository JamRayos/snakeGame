left equ 0
top equ 2
row equ 18
col equ 50
right equ left+col
bottom equ top+row

; Smaller box dimensions
small_left equ 8
small_top equ 7  
small_row equ 14
small_col equ 34  ; Reduced to ensure it fits within normal box
small_right equ small_left+small_col
small_bottom equ small_top+small_row

.model small
.data          
    msg db "Welcome to the snake game!!",0
    instructions db 0AH,0DH,"Use a, s, d and w to control your snake",0AH,0DH,"Use q anytime to quit",0DH,0AH, "Press any key to continue$"
    quitmsg db "Thanks for playing! hope you enjoyed",0
    gameovermsg db "OOPS!! your snake died! :P ", 0
    scoremsg db "Score: ",0
    head db '^',10,10
    body db '*',10,11, 3*15 DUP(0)
    segmentcount db 1
    fruitactive db 1
    fruitx db 8
    fruity db 8
    bombactive db 0
    bombx db 0
    bomby db 0
    gameover db 0
    quit db 0   
    delaytime db 5
    currentscore db 0
    boxsizechange db 0
    speedincrease db 0
    smallboxactive db 0  ; 0 = normal box, 1 = small box
 
.stack
    dw   128  dup(0)


.code

main proc far
    mov ax, @data
    mov ds, ax 
    
    mov ax, 0b800H
    mov es, ax
    
    mov ax, 0003H
    int 10H
    
    lea bx, msg
    mov dx,00
    call writestringat
    
    lea dx, instructions
    mov ah, 09H
    int 21h
    
    mov ah, 07h
    int 21h
    mov ax, 0003H
    int 10H
    call printbox      
    
mainloop:       
    call delay             
    lea bx, msg
    mov dx, 00
    call writestringat
    call shiftsnake
    cmp gameover,1
    je gameover_mainloop
    
    call keyboardfunctions
    cmp quit, 1
    je quitpressed_mainloop
    call fruitgeneration
    call bombgeneration
    call updatescoreeffects
    call draw
    
    jmp mainloop
    
gameover_mainloop: 
    mov ax, 0003H
    int 10H
    mov delaytime, 100
    mov dx, 0000H
    lea bx, gameovermsg
    call writestringat
    call delay    
    jmp quit_mainloop    
    
quitpressed_mainloop:
    mov ax, 0003H
    int 10H    
    mov delaytime, 100
    mov dx, 0000H
    lea bx, quitmsg
    call writestringat
    call delay    
    jmp quit_mainloop    

quit_mainloop:
;first clear screen
mov ax, 0003H
int 10h    
mov ax, 4c00h
int 21h  
main endp

delay proc 
    
    mov ah, 00
    int 1Ah
    mov bx, dx
    
jmp_delay:
    int 1Ah
    sub dx, bx

    cmp dl, delaytime                                                      
    jl jmp_delay    
    ret
    
delay endp

updatescoreeffects proc
    ; Speed increase every 10 points
    mov al, currentscore
    xor ah, ah
    mov bl, 10
    div bl
    cmp ah, 0
    jne checkboxsize
    
    mov al, speedincrease
    cmp al, currentscore
    je checkboxsize
    
    cmp delaytime, 2
    jle checkboxsize
    dec delaytime
    mov al, currentscore
    mov speedincrease, al

checkboxsize:
    ; Box size change every 12 points
    mov al, currentscore
    xor ah, ah
    mov bl, 12
    div bl
    cmp ah, 0
    jne ret_updatescoreeffects
    
    mov al, boxsizechange
    cmp al, currentscore
    je ret_updatescoreeffects
    
    ; Toggle box size
    call clearcurrentbox
    
    ; Reposition snake if it's outside the new box boundaries
    call reposition_snake_if_needed
    
    mov al, smallboxactive
    xor al, 1
    mov smallboxactive, al
    call printcurrentbox
    
    ; Clear and regenerate fruit and bomb for new box
    mov fruitactive, 0
    mov bombactive, 0
    
    mov al, currentscore
    mov boxsizechange, al

ret_updatescoreeffects:
    ret
updatescoreeffects endp

reposition_snake_if_needed proc
    ; Check if we're switching to small box
    cmp smallboxactive, 0
    je switch_to_small_box
    jmp switch_to_normal_box
    
switch_to_small_box:
    ; We're switching from normal to small box - reposition snake to center
    lea si, head
    add si, 1  ; Point to head coordinates
    
    ; Position head in center of small box
    mov dh, small_top + (small_row / 2)
    mov dl, small_left + (small_col / 2)
    ; Make sure x is even
    and dl, 0FEh
    mov [si], dx
    
    ; Clear all body segments except first one
    mov segmentcount, 1
    
    ; Initialize first body segment behind head
    lea si, body
    mov byte ptr [si], '*'
    mov ax, word ptr [head+1]  ; Fixed: use word ptr for 16-bit move
    sub al, 2  ; Position behind head
    mov word ptr [si+1], ax
    
    mov segmentcount, 2
    jmp reposition_done
    
switch_to_normal_box:
    ; We're switching from small to normal box - reposition snake to center
    lea si, head
    add si, 1  ; Point to head coordinates
    
    ; Position head in center of normal box
    mov dh, top + (row / 2)
    mov dl, left + (col / 2)
    ; Make sure x is even
    and dl, 0FEh
    mov [si], dx
    
    ; Clear all body segments except first one
    mov segmentcount, 1
    
    ; Initialize first body segment behind head
    lea si, body
    mov byte ptr [si], '*'
    mov ax, word ptr [head+1]  ; Fixed: use word ptr for 16-bit move
    sub al, 2  ; Position behind head
    mov word ptr [si+1], ax
    
    mov segmentcount, 2
    
reposition_done:
    ret
reposition_snake_if_needed endp

printcurrentbox proc
    cmp smallboxactive, 1
    jne print_normal_box
    call print_small_box
    ret
print_normal_box:
    call printbox
    ret
printcurrentbox endp

print_small_box proc
    mov dh, small_top
    mov dl, small_left
    mov cx, small_col
    mov bl, '*'
l1_small:                 
    call writecharat
    inc dl
    loop l1_small
    
    mov cx, small_row
l2_small:
    call writecharat
    inc dh
    loop l2_small
    
    mov cx, small_col
l3_small:
    call writecharat
    dec dl
    loop l3_small

    mov cx, small_row     
l4_small:
    call writecharat    
    dec dh 
    loop l4_small    
    ret
print_small_box endp

clearcurrentbox proc
    cmp smallboxactive, 1
    je clear_small_box
    
    ; Clear normal box
    mov dh, top
    mov dl, left
    mov cx, col
    mov bl, ' '
l1_clear:                 
    call writecharat
    inc dl
    loop l1_clear
    
    mov cx, row
l2_clear:
    call writecharat
    inc dh
    loop l2_clear
    
    mov cx, col
l3_clear:
    call writecharat
    dec dl
    loop l3_clear

    mov cx, row     
l4_clear:
    call writecharat    
    dec dh 
    loop l4_clear
    ret
    
clear_small_box:
    ; Clear small box
    mov dh, small_top
    mov dl, small_left
    mov cx, small_col
    mov bl, ' '
l1_clear_small:                 
    call writecharat
    inc dl
    loop l1_clear_small
    
    mov cx, small_row
l2_clear_small:
    call writecharat
    inc dh
    loop l2_clear_small
    
    mov cx, small_col
l3_clear_small:
    call writecharat
    dec dl
    loop l3_clear_small

    mov cx, small_row     
l4_clear_small:
    call writecharat    
    dec dh 
    loop l4_clear_small
    ret
clearcurrentbox endp

check_fruit_position proc
    ; Check if fruit position is valid
    mov dh, fruity
    mov dl, fruitx
    call readcharat
    cmp bl, '*'
    je invalid_fruit
    cmp bl, '^'
    je invalid_fruit
    cmp bl, '<'
    je invalid_fruit
    cmp bl, '>'
    je invalid_fruit
    cmp bl, 'v'
    je invalid_fruit
    cmp bl, 'B'
    je invalid_fruit
    cmp bl, 'X'
    je invalid_fruit
    clc
    ret
invalid_fruit:
    stc
    ret
check_fruit_position endp
   
fruitgeneration proc
    cmp fruitactive, 1
    jne start_fruit_gen
    ret
    
start_fruit_gen:
    mov ah, 00
    int 1Ah

    push dx
    mov ax, dx
    xor dx, dx
    xor bh, bh
    
    ; Choose appropriate bounds based on current box size
    cmp smallboxactive, 1
    je small_fruit_bounds
    
    ; Normal box bounds
    mov bl, row - 3      ; -3 to keep fruit inside the box boundaries
    jmp set_fruit_bounds
    
small_fruit_bounds:
    mov bl, small_row - 3 ; -3 to keep fruit inside the small box boundaries
    
set_fruit_bounds:
    div bx
    mov fruity, dl
    add fruity, 2        ; Start from inside the box (not on border)
    
    pop ax
    push ax
    cmp smallboxactive, 1
    jne normal_fruit_x
    mov bl, small_col - 4 ; -4 to keep fruit inside the small box
    jmp set_fruit_x
    
normal_fruit_x:
    mov bl, col - 4      ; -4 to keep fruit inside the normal box
    
set_fruit_x:
    xor bh, bh
    xor dx, dx
    div bx
    mov fruitx, dl
    add fruitx, 3        ; Start from inside the box (not on border)
    
    ; Make sure fruitx is even for proper display
    mov al, fruitx
    and al, 1
    jz fruit_x_even
    inc fruitx
fruit_x_even:
    
    pop ax
    
    ; Add appropriate offset based on current box
    cmp smallboxactive, 1
    je small_fruit_offset
    
    ; Normal box offset
    add fruity, top
    add fruitx, left
    
    ; Check normal box boundaries
    mov al, fruity
    cmp al, top + 1
    jl regenerate_fruit
    cmp al, bottom - 1
    jge regenerate_fruit
    mov al, fruitx
    cmp al, left + 2
    jl regenerate_fruit
    cmp al, right - 2
    jge regenerate_fruit
    jmp check_fruit_collision
    
small_fruit_offset:
    add fruity, small_top
    add fruitx, small_left
    
    ; Check small box boundaries
    mov al, fruity
    cmp al, small_top + 1
    jl regenerate_fruit
    cmp al, small_bottom - 1
    jge regenerate_fruit
    mov al, fruitx
    cmp al, small_left + 2
    jl regenerate_fruit
    cmp al, small_right - 2
    jge regenerate_fruit
    
check_fruit_collision:
    call check_fruit_position
    jc regenerate_fruit
    
    mov fruitactive, 1
    ret
    
regenerate_fruit:
    jmp start_fruit_gen
fruitgeneration endp

check_bomb_position proc
    ; Check if bomb position is valid
    mov dh, bomby
    mov dl, bombx
    call readcharat
    cmp bl, '*'
    je invalid_bomb
    cmp bl, '^'
    je invalid_bomb
    cmp bl, '<'
    je invalid_bomb
    cmp bl, '>'
    je invalid_bomb
    cmp bl, 'v'
    je invalid_bomb
    cmp bl, 'X'
    je invalid_bomb
    cmp bl, 'B'
    je invalid_bomb
    clc
    ret
invalid_bomb:
    stc
    ret
check_bomb_position endp

bombgeneration proc
    ; Generate bomb with 10% probability when no bomb is active
    cmp bombactive, 1
    jne check_bomb_chance
    ret
    
check_bomb_chance:
    ; Generate random number
    mov ah, 00
    int 1Ah
    
    ; Check if we should generate bomb (10% chance)
    test dx, 0007h  ; Roughly 1/8 chance
    jnz ret_bombactive
    
generate_bomb_now:
    mov ah, 00
    int 1Ah

    push dx
    mov ax, dx
    xor dx, dx
    xor bh, bh
    
    ; Choose appropriate bounds based on current box size
    cmp smallboxactive, 1
    je small_bomb_bounds
    mov bl, row - 3
    jmp set_bomb_bounds
small_bomb_bounds:
    mov bl, small_row - 3
set_bomb_bounds:
    div bx
    mov bomby, dl
    add bomby, 2
    
    pop ax
    push ax
    cmp smallboxactive, 1
    jne normal_bomb_x
    mov bl, small_col - 4
    jmp set_bomb_x
normal_bomb_x:
    mov bl, col - 4
set_bomb_x:
    xor bh, bh
    xor dx, dx
    div bx
    mov bombx, dl
    add bombx, 3
    
    ; Make sure bombx is even for proper display
    mov al, bombx
    and al, 1
    jz bomb_x_even
    inc bombx
bomb_x_even:
    pop ax
    
    ; Add appropriate offset based on current box
    cmp smallboxactive, 1
    je small_bomb_offset
    add bomby, top
    add bombx, left
    jmp verify_bomb_position
small_bomb_offset:
    add bomby, small_top
    add bombx, small_left
    
verify_bomb_position:
    call check_bomb_position
    jc bombregenerate
    
    mov bombactive, 1
    ret
    
bombregenerate:
    jmp generate_bomb_now
    
ret_bombactive:
    ret
bombgeneration endp

dispdigit proc
    add dl, '0'
    mov ah, 02H
    int 21H
    ret
dispdigit endp   
   
dispnum proc    
    test ax,ax
    jz retz
    xor dx, dx

    mov bx,10
    div bx

    push dx
    call dispnum  
    pop dx
    call dispdigit
    ret
retz:
    mov ah, 02  
    ret    
dispnum endp   

setcursorpos proc
    mov ah, 02H
    push bx
    mov bh,0
    int 10h
    pop bx
    ret
setcursorpos endp

draw proc
    lea bx, scoremsg
    mov dx, 0109
    call writestringat
    
    
    add dx, 7
    call setcursorpos
    mov al, currentscore
    xor ah, ah
    call dispnum
        
    lea si, head
draw_loop:
    mov bl, ds:[si]
    test bl, bl
    jz out_draw
    mov dx, ds:[si+1]
    call writecharat
    add si,3   
    jmp draw_loop 

out_draw:
    ; Draw fruit if active
    cmp fruitactive, 1
    jne skip_fruit_draw
    mov bl, 'X'
    mov dh, fruity
    mov dl, fruitx
    call writecharat
    
skip_fruit_draw:
    ; Draw bomb if active
    cmp bombactive, 1
    jne skip_bomb_draw
    mov bl, 'B'
    mov dh, bomby
    mov dl, bombx
    call writecharat
    
skip_bomb_draw:
    ret

draw endp

readchar proc
    mov ah, 01H
    int 16H
    jnz keybdpressed
    xor dl, dl
    ret
keybdpressed:
    mov ah, 00H
    int 16H
    mov dl,al
    ret
readchar endp                    

keyboardfunctions proc
    
    call readchar
    cmp dl, 0
    je next_14
    
    cmp dl, 'w'
    jne next_11
    cmp head, 'v'
    je next_14
    mov head, '^'
    ret
next_11:
    cmp dl, 's'
    jne next_12
    cmp head, '^'
    je next_14
    mov head, 'v'
    ret
next_12:
    cmp dl, 'a'
    jne next_13
    cmp head, '>'
    je next_14
    mov head, '<'
    ret
next_13:
    cmp dl, 'd'
    jne next_14
    cmp head, '<'
    je next_14
    mov head,'>'
next_14:    
    cmp dl, 'q'
    je quit_keyboardfunctions
    ret    
quit_keyboardfunctions:   
    inc quit
    ret
    
keyboardfunctions endp
                    
shiftsnake proc     
    mov bx, offset head
    
    xor ax, ax
    mov al, [bx]
    push ax
    inc bx
    mov ax, word ptr [bx]  ; Fixed: use word ptr for 16-bit move
    inc bx    
    inc bx
    xor cx, cx
snake_shift_loop:      
    mov si, [bx]
    test si, [bx]
    jz outside_shift
    inc cx     
    inc bx
    mov dx, word ptr [bx]  ; Fixed: use word ptr for 16-bit move
    mov word ptr [bx], ax
    mov ax,dx
    inc bx
    inc bx
    jmp snake_shift_loop
    
outside_shift:    
    pop ax
    push dx
    lea bx, head
    inc bx
    mov dx, word ptr [bx]  ; Fixed: use word ptr for 16-bit move
    
    ; Handle direction changes
    cmp al, '<'
    jne next_1
    dec dl
    dec dl
    jmp done_checking_the_head
next_1:
    cmp al, '>'
    jne next_2                
    inc dl 
    inc dl
    jmp done_checking_the_head
next_2:
    cmp al, '^'
    jne next_3 
    dec dh               
    jmp done_checking_the_head
next_3:
    inc dh
    
done_checking_the_head:    
    mov word ptr [bx], dx  ; Fixed: use word ptr for 16-bit move
    call readcharat 
    
    cmp bl, 'X'
    je i_ate_fruit
    
    cmp bl, 'B'
    je hit_bomb
    
    mov cx, dx
    pop dx 
    cmp bl, '*'   
    je game_over
    mov bl, 0
    call writecharat
    mov dx, cx
    
    ; Check collision using helper approach
    call check_collision
    jnc no_collision
    jmp game_over
    
no_collision:
    ret
    
hit_bomb:
    pop dx
    jmp game_over
    
i_ate_fruit:    
    mov al, segmentcount
    xor ah, ah
    lea bx, body
    mov cx, 3
    mul cx
    pop dx
    add bx, ax
    mov byte ptr ds:[bx], '*'
    mov word ptr [bx+1], dx  ; Fixed: use word ptr for 16-bit move
    inc segmentcount 
    
    inc currentscore
    
    ; Clear fruit
    mov dh, fruity
    mov dl, fruitx
    mov bl, 0
    call writecharat
    mov fruitactive, 0   
    
    ; Random chance to remove bomb
    cmp bombactive, 1
    jne skip_bomb_remove
    mov ah, 00
    int 1Ah
    test dx, 0001h
    jz skip_bomb_remove
    mov bombactive, 0
    mov dh, bomby
    mov dl, bombx
    mov bl, 0
    call writecharat
    
skip_bomb_remove:
    ret 
    
game_over:
    inc gameover
    ret
shiftsnake endp

check_collision proc
    ; Returns carry set if collision detected
    cmp smallboxactive, 1
    je check_small_collision
    
    ; Check normal box collision
    cmp dh, top
    je collision_detected
    cmp dh, bottom
    je collision_detected
    cmp dl, left
    je collision_detected
    cmp dl, right
    je collision_detected
    clc
    ret
    
check_small_collision:
    cmp dh, small_top
    je collision_detected
    cmp dh, small_bottom
    je collision_detected
    cmp dl, small_left
    je collision_detected
    cmp dl, small_right
    je collision_detected
    clc
    ret
    
collision_detected:
    stc
    ret
check_collision endp
   
printbox proc
    mov dh, top
    mov dl, left
    mov cx, col
    mov bl, '*'
l1:                 
    call writecharat
    inc dl
    loop l1
    
    mov cx, row
l2:
    call writecharat
    inc dh
    loop l2
    
    mov cx, col
l3:
    call writecharat
    dec dl
    loop l3

    mov cx, row     
l4:
    call writecharat    
    dec dh 
    loop l4    
    
    ret
printbox endp
              
writecharat proc
    ;80x25
    push dx
    mov ax, dx
    and ax, 0FF00H
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    
    
    push bx
    mov bh, 160
    mul bh 
    pop bx
    and dx, 0FFH
    shl dx,1
    add ax, dx
    mov di, ax
    mov es:[di], bl
    pop dx
    ret    
writecharat endp
            
readcharat proc
    push dx
    mov ax, dx
    and ax, 0FF00H
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1    
    push bx
    mov bh, 160
    mul bh 
    pop bx
    and dx, 0FFH
    shl dx,1
    add ax, dx
    mov di, ax
    mov bl,es:[di]
    pop dx
    ret
readcharat endp        

writestringat proc
    push dx
    mov ax, dx
    and ax, 0FF00H
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    
    push bx
    mov bh, 160
    mul bh
    
    pop bx
    and dx, 0FFH
    shl dx,1
    add ax, dx
    mov di, ax
loop_writestringat:
    
    mov al, [bx]
    test al, al
    jz exit_writestringat
    mov es:[di], al
    inc di
    inc di
    inc bx
    jmp loop_writestringat
    
    
exit_writestringat:
    pop dx
    ret
writestringat endp
     
end main