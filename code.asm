.286
.model small
.stack 1000h  

.data 

    screen_buf dw 2000 dup(4384);whitespace(32) + fill blue(00010001b)  
    buf_size equ 4000 
    pointer_x dw 1
    pointer_y dw 1           
    
    size_x equ 55
    size_y equ 25 
    mine_limit equ 150
    mines_counter dw 0
    flags_counter dw 0

    max_x equ size_x - 1
    max_y equ size_y - 1
    
    min_x equ 0
    min_y equ 0  

    can_move db 1

    maze_data_size equ size_x*size_y
    maze_data db maze_data_size dup(0)

    ;not used        10000000b
    flag_mask    equ 01000000b
    visible_mask equ 00100000b
    mine_mask    equ 00010000b
    counter_mask equ 00001111b

    ui_space equ (80 - size_x) * 2

    rand_a equ 1523
    rand_b equ 1264
    rand_c equ 1725
    rand_v dw  1487

    ;ui strings
    title_msg     db 'Minesweeper$'
    help_msg      db 'Controls:$'
    arrows_msg    db 'Arrows - move pointer$'
    open_msg      db 'Open cell - enter$'
    flag_msg      db 'Flag cell - space$'
    check_msg     db 'Check result - Y$'
    restart_msg   db 'Restart game - R$'
    exit_msg      db 'Exit game - esc$'
    mines_msg     db 'Mines count: $'
    flags_msg     db 'Flags count: $'
    number_buf    db 256 dup('$')

.code

main:         
   
    mov ax, @data
    mov ds, ax

    push 0B800h
    pop es  

    call clear_screen
    call create_maze_data 
    call render_all

    call game_loop

    mov ax, 4C00h         
    int 21h  
    
    game_loop: 
       
        game_loop_loop:   
            call fill_screen
            call render_pointer
            call check_input
        jmp game_loop  
        
    ret
    
    check_input:   
          
        mov ax, 0
        int 16h 
        
        cmp ax, 4D00h
        je input_right
        
        cmp ax, 5000h
        je input_down
        
        cmp ax, 4B00h
        je input_left
        
        cmp ax, 4800h
        je input_up    
                               
        cmp ax, 011Bh
        je input_exit   

        cmp ax, 1C0Dh
        je input_enter         

        cmp ax, 3920h
        je input_space

        cmp ax, 1372h
        je input_reset     

        cmp ax, 1579h
        je input_check              

        ret
        
        input_right: 
            mov cx, 1
            mov bx, 0
            call move_pointer
            ret
        
        input_down:
            mov cx, 0
            mov bx, 1
            call move_pointer
            ret
        
        input_left:
            mov cx, -1
            mov bx, 0
            call move_pointer
            ret
        
        input_up:
            mov cx, 0
            mov bx, -1
            call move_pointer
            ret  
            
        input_exit:     
            call clear_screen  
            
            mov ax, 4C00h         
            int 21h
            
            ret

        input_enter:
            mov ax, pointer_y
            mov bx, pointer_x
            call maze_dfs

            call render_all
            ret

        input_space:
            call place_flag
            ret

        input_reset:
            mov can_move, 1
            call create_maze_data 
            call render_all
            ret

        input_check:
            call game_end
            call render_all
            ret
        
    ret  

    place_flag:
        mov ax, pointer_y
        mov bx, size_x
        mul bx
        add ax, pointer_x
        mov di, ax

        mov al, [maze_data + di]
        and al, visible_mask
        cmp al, visible_mask
        je place_flag_exit

        mov ax, flags_counter
        cmp ax, mines_counter
        jge place_flag_exit

        mov al, [maze_data + di]
        and al, flag_mask
        cmp al, flag_mask
        jne place_flag_add
        dec flags_counter
        jmp place_flag_write
        place_flag_add:
        inc flags_counter

        place_flag_write:
        mov al, [maze_data + di]
        xor al, flag_mask
        mov [maze_data + di], al
        call render_all
        place_flag_exit:
    ret
    
    move_pointer:    
        ;cx - x, bx - y 

        push pointer_x
        push pointer_y

        cmp can_move, 1
        jne move_pointer_error
        
        mov ax, pointer_x
        add ax, cx
        
        cmp ax, min_x
        jl move_pointer_error  
        
        cmp ax, max_x
        jg move_pointer_error 
        
        mov pointer_x, ax  
        
        mov ax, pointer_y
        add ax, bx
        
        cmp ax, min_y
        jl move_pointer_error  
        
        cmp ax, max_y
        jg move_pointer_error 
        
        mov pointer_y, ax 
        jmp move_pointer_done

        move_pointer_error:
            pop pointer_y
            pop pointer_x
        ret
        move_pointer_done:
            pop bx
            pop cx
        ret
        
    ret

    create_maze_data:
        ;60*25
        ;cell = 1 byte
        ;0000 - mines counter
        ;0 - mine flag
        ;0 - mask flag
        ;0 - mark flag
        ;0 - not used

        ;mine mask - 16 (10000b)
        ;counter mask - 15 (1111b)

        ;clear_data
        mov mines_counter, 0
        mov flags_counter, 0
        xor di, di
        clear_maze_data_loop:
            mov [maze_data + di], 0
        inc di
        cmp di, maze_data_size
        jl clear_maze_data_loop

        ;generate random mines
        mov ah, 00h
        int 1ah
        mov rand_v, dx 

        xor cx, cx
        maze_data_mines_loop:

            call rand

            xor dx, dx
            mov ax, rand_v
            mov bx, maze_data_size
            div bx

            mov di, dx
            mov al, [maze_data + di]
            and al, mine_mask
            cmp al, mine_mask
            je maze_data_mines_loop_inc

            mov ax, mines_counter
            inc ax
            mov mines_counter, ax

            mov [maze_data + di], mine_mask

        maze_data_mines_loop_inc:
        inc cx
        cmp cx, mine_limit
        jl maze_data_mines_loop

        xor ax, ax;y
        xor bx, bx;x
        xor di, di;index in one-demension array
        create_maze_data_y_loop:
            create_maze_data_x_loop:
                xor cx, cx;mines counter   
                push di 
    
                ;create_maze_tl:
                    pop di
                    push di

                    cmp ax, min_y
                    jle create_maze_t

                    cmp bx, min_x
                    jle create_maze_t

                    sub di, size_x
                    dec di
                    ;di -= max_x + 1
        
                    mov dl, [maze_data + di]
                    and dl, mine_mask
        
                    cmp dl, mine_mask
                    jne create_maze_t
        
                    inc cx
    
                create_maze_t:
                    pop di
                    push di

                    cmp ax, min_y
                    jle create_maze_tr

                    sub di, size_x
                    ;di -= max_x
        
                    mov dl, [maze_data + di]
                    and dl, mine_mask
        
                    cmp dl, mine_mask
                    jne create_maze_tr
        
                    inc cx

                create_maze_tr:
                    pop di
                    push di

                    cmp ax, min_y
                    jle create_maze_l

                    cmp bx, max_x
                    jge create_maze_l

                    sub di, size_x
                    inc di
                    ;di -= max_x - 1
        
                    mov dl, [maze_data + di]
                    and dl, mine_mask
        
                    cmp dl, mine_mask
                    jne create_maze_l
        
                    inc cx

                create_maze_l:
                    pop di
                    push di

                    cmp bx, min_x
                    jle create_maze_r

                    dec di
                    ;di -= 1
        
                    mov dl, [maze_data + di]
                    and dl, mine_mask
        
                    cmp dl, mine_mask
                    jne create_maze_r
        
                    inc cx

                create_maze_r:
                    pop di
                    push di

                    cmp bx, max_x
                    jge create_maze_bl

                    inc di
                    ;di += 1
        
                    mov dl, [maze_data + di]
                    and dl, mine_mask
        
                    cmp dl, mine_mask
                    jne create_maze_bl
        
                    inc cx

                create_maze_bl:
                    pop di
                    push di

                    cmp ax, max_y
                    jge create_maze_b

                    cmp bx, min_x
                    jle create_maze_b

                    add di, size_x
                    dec di
                    ;di += max_x - 1
        
                    mov dl, [maze_data + di]
                    and dl, mine_mask
        
                    cmp dl, mine_mask
                    jne create_maze_b
        
                    inc cx
    
                create_maze_b:
                    pop di
                    push di

                    cmp ax, max_y
                    jge create_maze_br

                    add di, size_x
                    ;di += max_x
        
                    mov dl, [maze_data + di]
                    and dl, mine_mask
        
                    cmp dl, mine_mask
                    jne create_maze_br
        
                    inc cx

                create_maze_br:
                    pop di
                    push di

                    cmp ax, max_y
                    jge create_maze_set_mines_counter

                    cmp bx, max_x
                    jge create_maze_set_mines_counter

                    add di, size_x
                    inc di
                    ;di += max_x + 1
        
                    mov dl, [maze_data + di]
                    and dl, mine_mask
        
                    cmp dl, mine_mask
                    jne create_maze_set_mines_counter
        
                    inc cx

                create_maze_set_mines_counter:
                ;update cell mines counter
                pop di
                mov dl, counter_mask
                not dl
                and dl, [maze_data + di]
                or dl, cl
                mov [maze_data + di], dl

            inc bx
            inc di
            cmp bx, max_x
            jg create_maze_data_x_end
            jmp create_maze_data_x_loop
            create_maze_data_x_end:
        
        xor bx, bx
        inc ax
        cmp ax, max_y 
        jg create_maze_data_y_end
        jmp create_maze_data_y_loop
        create_maze_data_y_end:
    ret

    maze_dfs:
        ;ax = y
        ;bx = x
        push ax
        push bx  
        mov cx, size_x
        mul cx
        add ax, bx
        mov di, ax;index in one-demension array
        pop bx
        pop ax
        push di

        ;end if visible
        mov cl, [maze_data + di]
        and cl, visible_mask
        cmp cl, visible_mask
        jne check_maze_dfs_flag
        jmp maze_dfs_end

        check_maze_dfs_flag:
        mov cl, [maze_data + di]
        and cl, flag_mask
        cmp cl, flag_mask
        jne check_maze_dfs_bomb
        dec flags_counter

        check_maze_dfs_bomb:
        ;check for bomb
        mov cl, [maze_data + di]
        and cl, mine_mask
        cmp cl, mine_mask
        jne check_maze_dfs_counter
        call game_end
        jmp maze_dfs_end

        check_maze_dfs_counter:
        ;mark current as visible
        mov cl, [maze_data + di]
        or cl, visible_mask
        mov [maze_data + di], cl

        ;if mine counter more than zero - exit
        mov cl, [maze_data + di]
        and cl, counter_mask
        cmp cl, 0
        je start_maze_dfs
        jmp maze_dfs_end
        start_maze_dfs:

        ;maze_dfs_tl:
            pop di
            push di

            cmp ax, min_y
            jle maze_dfs_t

            cmp bx, min_x
            jle maze_dfs_t

            sub di, size_x
            dec di
            ;di -= max_x + 1

            mov dl, [maze_data + di]
            and dl, mine_mask

            cmp dl, mine_mask
            je maze_dfs_t

            sub ax, 1
            sub bx, 1
            call maze_dfs
            add ax, 1
            add bx, 1

        maze_dfs_t:
            pop di
            push di

            cmp ax, min_y
            jle maze_dfs_tr

            sub di, size_x
            ;di -= max_x

            mov dl, [maze_data + di]
            and dl, mine_mask

            cmp dl, mine_mask
            je maze_dfs_tr

            sub ax, 1
            call maze_dfs
            add ax, 1

        maze_dfs_tr:
            pop di
            push di

            cmp ax, min_y
            jle maze_dfs_l

            cmp bx, max_x
            jge maze_dfs_l

            sub di, size_x
            inc di
            ;di -= max_x - 1

            mov dl, [maze_data + di]
            and dl, mine_mask

            cmp dl, mine_mask
            je maze_dfs_l

            sub ax, 1
            add bx, 1
            call maze_dfs
            add ax, 1
            sub bx, 1

        maze_dfs_l:
            pop di
            push di

            cmp bx, min_x
            jle maze_dfs_r

            dec di
            ;di -= 1

            mov dl, [maze_data + di]
            and dl, mine_mask

            cmp dl, mine_mask
            je maze_dfs_r

            sub bx, 1
            call maze_dfs
            add bx, 1

        maze_dfs_r:
            pop di
            push di

            cmp bx, max_x
            jge maze_dfs_bl

            inc di
            ;di += 1

            mov dl, [maze_data + di]
            and dl, mine_mask

            cmp dl, mine_mask
            je maze_dfs_bl

            add bx, 1
            call maze_dfs
            sub bx, 1

        maze_dfs_bl:
            pop di
            push di

            cmp ax, max_y
            jge maze_dfs_b

            cmp bx, min_x
            jle maze_dfs_b

            add di, size_x
            dec di
            ;di += max_x - 1

            mov dl, [maze_data + di]
            and dl, mine_mask

            cmp dl, mine_mask
            je maze_dfs_b

            add ax, 1
            sub bx, 1
            call maze_dfs
            sub ax, 1
            add bx, 1

        maze_dfs_b:
            pop di
            push di

            cmp ax, max_y
            jge maze_dfs_br

            add di, size_x
            ;di += max_x

            mov dl, [maze_data + di]
            and dl, mine_mask

            cmp dl, mine_mask
            je maze_dfs_br

            add ax, 1
            call maze_dfs
            sub ax, 1

        maze_dfs_br:
            pop di
            push di

            cmp ax, max_y
            jge maze_dfs_end

            cmp bx, max_x
            jge maze_dfs_end

            add di, size_x
            inc di
            ;di += max_x + 1

            mov dl, [maze_data + di]
            and dl, mine_mask

            cmp dl, mine_mask
            je maze_dfs_end

            add ax, 1
            add bx, 1
            call maze_dfs
            sub ax, 1
            sub bx, 1

        maze_dfs_end:
        pop di
    ret

    push_maze_to_buffer:
        ;push maze to screen buf, shouldn't do it like this
        xor ax, ax;y
        xor cx, cx;x
        xor si, si;index in one-dimension array
        mov di, offset screen_buf
        render_maze_y_loop:
            render_maze_x_loop:
                mov BYTE PTR [di], ' '
                mov BYTE PTR [di + 1], 01100111b

                mov bl, [maze_data + si]
                and bl, visible_mask
                cmp bl, visible_mask
                je render_maze_visible

                mov bl, [maze_data + si]
                and bl, flag_mask
                cmp bl, flag_mask
                jne render_maze_loop_inc
                mov BYTE PTR [di], 'P'
                jmp render_maze_loop_inc

                render_maze_visible:
                mov bl, [maze_data + si]
                and bl, mine_mask
                cmp bl, mine_mask
                jne render_maze_counter

                mov BYTE PTR [di], '*'

                mov bl, [maze_data + si]
                and bl, flag_mask
                cmp bl, flag_mask
                je render_maze_mine_flag
                mov BYTE PTR [di + 1], 11110100b
                jmp render_maze_loop_inc

                render_maze_mine_flag:
                mov BYTE PTR [di + 1], 11110010b
                jmp render_maze_loop_inc

                render_maze_counter:

                mov bl, [maze_data + si]
                and bl, counter_mask
                add bl, '0'
                mov BYTE PTR [di], bl
                mov BYTE PTR [di + 1], 01110001b

                render_maze_loop_inc:
                inc si
                add di, 2

            inc cx
            cmp cx, max_x
            jle render_maze_x_loop
        add di, ui_space  
        xor cx, cx
        inc ax
        cmp ax, max_y 
        jle render_maze_y_loop
    ret

    game_end:
        mov can_move, 0
        mov flags_counter, 0
        xor di, di
        game_end_maze_data_loop:
            mov al, [maze_data + di]
            and al, mine_mask
            cmp al, mine_mask
            jne game_end_maze_data_loop_inc

            mov al, [maze_data + di]
            or al, visible_mask
            mov [maze_data + di], al

            mov al, [maze_data + di]
            and al, flag_mask
            cmp al, flag_mask
            jne game_end_maze_data_loop_inc

            dec mines_counter

        game_end_maze_data_loop_inc:
        inc di
        cmp di, maze_data_size
        jl game_end_maze_data_loop
    ret

    render_gui:
        mov si, offset title_msg
        mov di, 112;56 * 2
        push di
        call render_string
        pop di

        mov si, offset mines_msg
        add di, 320;+ 80 * 4
        push di
        call render_string

        mov ax, offset mines_counter
        push ax
        mov ax, offset number_buf
        push ax
        call itoa
        pop ax
        pop ax

        mov si, offset number_buf
        call render_string
        pop di

        mov si, offset flags_msg
        add di, 160;+ 80 * 2
        push di
        call render_string

        mov ax, offset flags_counter
        push ax
        mov ax, offset number_buf
        push ax
        call itoa
        pop ax
        pop ax

        mov si, offset number_buf
        call render_string
        pop di

        mov si, offset help_msg
        add di, 320;+ 80 * 4
        push di
        call render_string
        pop di

        mov si, offset arrows_msg
        add di, 160;+ 80 * 2
        push di
        call render_string
        pop di

        mov si, offset open_msg
        add di, 160;+ 80 * 2
        push di
        call render_string
        pop di

        mov si, offset flag_msg
        add di, 160;+ 80 * 2
        push di
        call render_string
        pop di

        mov si, offset check_msg
        add di, 160;+ 80 * 2
        push di
        call render_string
        pop di

        mov si, offset restart_msg
        add di, 160;+ 80 * 2
        push di
        call render_string
        pop di

        mov si, offset exit_msg
        add di, 160;+ 80 * 2
        push di
        call render_string
        pop di
    ret

    render_string:
        ;si - string
        ;di - offset
        mov al, [si]
        add di, offset screen_buf
        render_string_loop:
            
            mov BYTE PTR [di], al
            mov BYTE PTR [di + 1], 00010111b
            add di, 2

        inc si
        mov al, [si]
        cmp al, '$'
        jne render_string_loop
        
    ret

    render_all:
        call clear_buffer
        call push_maze_to_buffer
        call render_gui
    ret

    fill_screen:    
        
        xor di, di       

        fill_screen_loop:         
        
        mov bx, [screen_buf + di]
        mov es:[di], bx       
                
        add di, 2

        cmp di, buf_size
        jl fill_screen_loop   
            
    ret  
    
    render_pointer:  
        mov ax, pointer_y
        mov bx, 80 
        mul bx                 
        add ax, pointer_x   
        mov bx, 2
        mul bx
        
        mov di, ax
        inc di
        mov cl, es:di
        and cl, 10001111b
        or cl, 00110000b
        mov es:di, cl
    ret  

    clear_buffer:
        xor di, di       

        clear_buffer_loop:         
        
        mov [screen_buf + di], 4384       
                
        add di, 2

        cmp di, buf_size
        jl clear_buffer_loop 
    ret   

    clear_screen:
        mov ah, 00h
        mov al, 03h 
        int 10h
    ret

    rand proc
        pusha
        mov ax, rand_v
        mov bx, rand_a
        mul bx
        add ax, rand_b
        xor dx, dx
        mov bx, rand_c
        div bx
        mov rand_v, dx
        popa
        ret
    rand endp

    ;first - 16-bit number address, second - string start
    itoa:  
        push bp
        mov bp, sp   
        pusha        
        
        ;[ss:bp+4+0] - string address 
        ;[ss:bp+4+2] - number address       
        
        mov si, [ss:bp+4+2]   
        mov ax, [si]      
        
        mov di, [ss:bp+4+0] 
                
        xor cx, cx         
        cmp ax, 0
            jge itoa_loop 
            
        inc cx 
        neg ax
        
        itoa_loop: 
            xor dx, dx
            mov bx, 10  
            div bx
            add dl, '0'
            mov [di], dl
                
        inc di                   
        cmp ax, 0
        ja itoa_loop
        
        cmp cx, 1
        jne itoa_end
        mov BYTE PTR [di], '-' 
        inc di
        
        itoa_end:
        mov BYTE PTR[di], '$'
        
        push WORD PTR[ss:bp+4+0]
        push 0
        sub di, [ss:bp+4+0] 
        dec di
        push di
        call reverse_word
        pop ax
        pop ax
        pop ax     
        
        popa
        pop bp
    ret         

    ;first - buf, second - start, third - end
    reverse_word:
        push bp
        mov bp, sp   
        
        pusha        
        
        mov bx, [ss:bp+4+2]
        mov cx, [ss:bp+4+0]  
        
        cmp bx, cx
        je reverse_end 
        
        reverse_word_loop:
            
            mov si, [ss:bp+4+4]
            add si, bx
            mov al, [si]
            
            mov di, [ss:bp+4+4]
            add di, bx
            mov si, [ss:bp+4+4]
            add si, cx    
            mov dl, [si]
            mov [di], dl   
            
            mov [si], al
        
        inc bx
        dec cx
        
        cmp bx, cx
        jb reverse_word_loop
        
        reverse_end:    
        
        popa
        pop bp
    ret 

end main