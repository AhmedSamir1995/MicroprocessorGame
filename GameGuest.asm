;Ahmed Samir Hamed
;15-10-2016
;Problem 9) b)
;calculate the sum. of the numbers 1: N where N is stored in AL. Store the result in BX.
; WE ARE READING NUMBER FROM USER AS A CHARACTER
;---------------------------
.MODEL medium
.STACK 64
.DATA 
SCREENCELLSCOUNT EQU 1600
PONEPAINTCOL EQU 252 ;Player One Paint Col
PTWOPAINTCOL EQU 242 ;Player Two Paint Col
TIMERCOL EQU 0F9h
GAMETIME EQU 100

timerPos dw  3040                
pOnePos dw 1670 ;Player One Initial Pos (centre)
pTwoPos dw 1680 ;Player Two Initial Pos (centre)
pOneCenterVal dw ? ;The Value the centre of the brush will change after each move 
pTwoCenterVal dw ? ;The Value the centre of the brush will change after each move 

pOneScore dw 0
pTwoScore dw 0
winMsg db "YOU WIN!","$"
loseMsg db "YOU LOSE!","$"

youAreHost db 0

currentTimeTick db 0
currentSec db ? 

s1 dw 0
s2 dw 0 
.code
MAIN    PROC FAR
;'Moving the address of datasegment to the ds'
MOV AX,@DATA
MOV DS,AX
        
mov ax, 0b800h ; Adding the video memory address to the extra segment 
mov es, ax                                  



; To be managed later
; If you are the host -> Do nothing  
; If You are guest <- recieve Timer 

;call initTimer       
call initWhiteScreen ; Call initWhiteScreen 
call initPlayersPos ; Initialie players Positions
call initUart

Game:
 
;updateTime on Screen 
call drawTimer
mov ah, 1
int 16h 
    
JZ checka
; Check to transmit and transmit
	;checkT:
		mov dx, 3fdh
		in al, dx
		test al, 00100000b
	jz checka  
		mov dx, 3f8h
		mov al, ah
		out dx, al

		
check1:      
cmp ah, 48h
jne check2 
p1UP: ;moving player 1 UP
mov dx, -140h
;Check For Up possible move
call checkUp
add bx, 0
jnz approveUP1
approveUP1: call MovePlayerOne
call consumeLetter          
jmp checka 
 
check2:
cmp ah, 50h ;Compare with the scan code of down key
jne check3 
p1DOWN: ;moving player 1 DOWN  
mov dx, 140h
call MovePlayerOne 
call consumeLetter            
jmp checka     


check3:
cmp ah, 4Bh
jne check4
p1LEFT: ;moving player 1 LEFT
mov dx, -4h
call MovePlayerOne 
call consumeLetter         
jmp checka
           
           
check4: 
cmp ah, 4Dh
jne checka
p1RIGHT: ;moving player 1 RIGHT 
mov dx, 4h
call MovePlayerOne
call consumeLetter 
checka:

;Check to receive
		mov dx, 3fdh
		in al, dx
		test al, 00000001b
		jz continue
		; Move received scancode to apply changes
		mov dx, 3f8h; 
		in al, dx
		
		
cmp al, 48h
jne checkb 
p2UP:;moving player 2 UP
mov dx, -140h
call MovePlayerTwo  
 jz continue

checkb:
cmp al, 50h
jne checkc 
p2DOWN: ;moving player 2 DOWN      
mov dx, 140h
call MovePlayerTwo
    jz continue

checkc:
cmp al, 4bh
jne checkd
p2LEFT: ;moving player 2 LEFT
mov dx, -4h
call MovePlayerTwo           
     jz continue

checkd: 
cmp al,4Dh
jne continue
p2RIGHT: ;moving player 2 RIGHT 
mov dx, 4h
call MovePlayerTwo  

continue:
;Check time to exit
;cmp currentTimeTick, GAMETIME         
;je FinishGame

jmp Game

FinishGame:  
call calculateScore
call drawGameResult 

exit:   
        MOV AH, 4Ch ; Service 4Ch - Terminate with Error Code
        MOV AL, 0 ; Error code
        INT 21h ; Interrupt 21h - DOS General Interrupts

MAIN    ENDP
;-------------------------------------------------
initWhiteScreen                PROC    
                
                ;Hide Cursor
                mov AH, 01h
                mov cx, 2607h
                int 10h
                ;white Background
                mov ah, 9;display    
                mov bh, 0;page 0
                mov al, 219; Letter in al   
                mov cx, SCREENCELLSCOUNT; how many times    
                mov bl, 0ffh ;colour
                int 10h
                    RET
initWhiteScreen                ENDP     
;-------------------------------------------------  
initPlayersPos                PROC   
                mov bx,  pOnePos
                ;Draw Player 1
                call drawBrush
                mov bx, pTwoPos ;The position of player 2 
                ;Draw Player 2
                call drawBrush

                    RET
initPlayersPos                ENDP     
;-------------------------------------------------    
;This Function is responsilbe for player 1 movement and dx is the added vlaue to get the new centre
MovePlayerOne                PROC   
                ;Debug Code
                mov bx, s1
                inc bx
                mov s1, bx 
                mov bx, pOnePos 
                mov pOneCenterVal, dx
                mov dh, PONEPAINTCOL
                call drawColor    
                add bx, pOneCenterVal
                mov pOnePos, bx ; Save The New Centre
                call drawBrush 

                    RET
MovePlayerOne                ENDP     
;------------------------------------------------- 
;This Function is responsilbe for player 2 movement and dx is the added vlaue to get the new centre
MovePlayerTwo                PROC   
                mov bx, s2
                inc bx
                mov s2, bx
                mov bx, pTwoPos 
                mov pTwoCenterVal, dx
                mov dh, PTWOPAINTCOL
                call drawColor    
                add bx, pTwoCenterVal
                mov pTwoPos, bx ; Save The New Centre
                mov cx, 2                            
                call Drawbrush

                    RET
MovePlayerTwo                ENDP     
;------------------------------------------------- 
;drawBrush procedure takes the top left point of the brush (bx) 
drawBrush                PROC   
                    mov cx, 2                            
                    push bx
                    Brush:
                    mov es:[bx], 177
                    mov es:[bx+1], 0
                    
                    mov es:[bx+2], 177 
                    mov es:[bx+3], 0 
                    add bx, 160
                    loop Brush
                    pop bx
                    RET
drawBrush                ENDP     
;------------------------------------------------- 
;drawColor procedure takes the top left point of the brush (bx), color dx
drawColor                PROC   
                    mov cx, 2                                 
                    push bx
                    Color:
                    mov es:[bx], 219
                    mov es:[bx+1], dh
                    mov es:[bx+2], 219 
                    mov es:[bx+3], dh 
                    add bx, 160
                    loop Color        
                    pop bx
                    ;if you are the host, then you have to consume the letter
                    mov ch, 1
                    cmp ch, youAreHost 
                    jne returnDrawColour
                    call consumeLetter
                    returnDrawColour: RET
drawColor                ENDP     
;-------------------------------------------------   
;Consumes the pressed letter
consumeLetter                PROC   
                    ;consume the letter
                    mov ah,0
                    int 16h 
                    RET
consumeLetter                ENDP     
;-------------------------------------------------

;This Function check Left moves available or not saves result in bx (bx = 0 no more left moves available)
checkUp                PROC   
                mov bx, 0h
                ;cmp bx, 8h
                ;jae yes
                ;mov dh, 0h
                ;yes:
                    RET
checkUp                ENDP     
;-------------------------------------------------
;This Function Calculates the Score
calculateScore                PROC   
                    mov ax,0
                    mov bx, 0
                    mov di, 1
                    mov cx, 649h
                    countloop:
                    cmp es:[di], PONEPAINTCOL
                    je incP1
                    cmp es:[di], PTWOPAINTCOL
                    je incP2 
                    continueCount:
                    add di, 2
                    loop countloop
                    mov pOneScore, ax
                    mov pTwoScore, bx   
                    jmp exitCount 
                    incP1:
                        add ax, 1
                        jmp continueCount
                    incP2:
                        add bx,1
                        jmp continueCount 
                    exitCount: RET
calculateScore                ENDP                   
;-------------------------------------------------   
;This Function initializes the Timer 
initTimer                PROC   
                    sub ah,youarehost
                    jz youAreaHost
                    youAreGuest: ;Do nothing
                    youAreaHost:
                     mov ah,2Ch
                    int 21h 
                    mov currentSec, dh
                    mov currentTimeTick, 0h
                    RET
initTimer                ENDP     
;-------------------------------------------------    
;This Function used by the guest to recieve the current Timer Tick count
setTimer                PROC   
                    mov ah,youarehost
                    jnz SyouAreaHost
                    SyouAreGuest: ;Recieve the startTimer and set it in your variable 'timerStart' 
                   ;;SERIAL COMMUNICATION
                    SyouAreaHost: ;Do nothing
                    RET
setTimer                ENDP     
;-------------------------------------------------   
;This Function draws the Timer
drawTimer                PROC
                    ; Get the Current Time
                    mov ah,2Ch
                    int 21h
                    mov bh, dh
                    ;compare current sec (in dh from the proevious interupt)  to the current Sec
                    cmp dh, currentSec
                    je retDrawTimer                        
                    mov di, timerPos
                    mov es:[di], 219
                    mov es:[di+1], TIMERCOL
                    add di, 2
                    mov currentSec, dh
                    mov timerPos, di
                    mov bh, currentTimeTick
                    add bh, 1
                    mov currentTimeTick, bh
                    retDrawTimer:
                    RET
drawTimer                ENDP     
;-------------------------------------------------  
;This Function draws YOU WIN Message
drawGameResult            PROC  
                    mov ax, pOneScore
                    mov bx, pTwoScore
                    sub ax, bx
                    jz tie
                    cmp ax, bx
                    jb player2Wins
                    ;else player 1 wins
                    mov ah, 9
                    mov dx, offset winMsg
                    int 21h
                    tie:
                     mov ah, 9
                    ;mov dx, offset tieMsg
                    int 21h
                    jmp drawGameResultRet
                    player2Wins:
                     mov ah, 9
                    ;mov dx, offset loseMsg
                    int 21h
                    jmp drawGameResultRet
                    drawGameResultRet:
                    RET
drawGameResult                ENDP     
;------------------------------------------------- 

initUart proc
mov dx, 3fbh
mov al,10000000b
out dx, al

mov dx, 3f8h
mov al, 18h
out dx, al

mov dx, 3f9h
mov al, 0h
out dx, al

mov dx, 3fbh
mov al,00011011b
out dx, al
ret
initUart endp
END MAIN

