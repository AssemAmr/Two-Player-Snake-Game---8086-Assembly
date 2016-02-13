Data_segment_name segment para

;Game Object
Snake1 DW 12 DUP(?) ;X,Y coordinates  words
Snake1Size DB ? ;3->12
Snake1Direction DB 0 ;0->Right, 1->Up, 2->Left, 3->Down
Snake2Direction DB 2 ;0->Right, 1->Up, 2->Left, 3->Down ;;;;;;;;;;;;;;;;;;;;;;;;;New Wello segment to make it easer to check 
Snake2 DW 12 DUP(?) ;X,Y coordinates words
Snake2Size DB ? ;3->12
AppleLocation DW ? ;X,Y coordinates word
fileHandle DW ?
GameSpeed DB 35 ;;;; new wello 
LeaveGame DB 0 ; 0 no request to leave the game 
Game_Difficulty_Mode DB '0' ; 0 -> Increase the length, 1-> increase the speed

Game_GameEndStatus DB 0 ;0->Didn't End, 1->End

DetectCollision_CollisionType DB 3
;0->No Collision
;1->Snake1 head with Apple
;2->Snake2 head with Apple
;3->Snake1 & Snake2 heads
;4->Snake1 head & Snake2 body
;5->Snake1 head & map
;6->Snake1 head & Snake1 body
;7->Snake2 head & Snake1 body
;8->Snake2 head & map
;9->Snake2 head & Snake2 body
DetectAppleCollision_CollisionType DB ?
;0->No Collision
;1->Apple with Map


;Status
NotAdjustedScore DB 2 dup(?)
ScoreTemplate1 DB 'Score(Host/Guest):'
Score          DB  '00','/','00$'             ;left number --> Host Score / Right number --> Guest Score 
ScoreTemplate2 DB 'Host:Right'
PositionHost   DB  '(','10',',','20',')$'              ;rows & Columns for Host user
ScoreTemplate3 DB 'Guest:Left '
PositionGuest  DB  '(','10',',','60',')$'                ;rows & Columns for Guest user
Winner1        DB 'Host is the winner !$'
Winner2        DB 'Guest is the winner !$'
NoWinner       DB 'No Winner !$'

RightTemplate DB 'Right'
LeftTemplate DB  'Left '
UpTemplate DB    'Up   '
DownTemplate DB  'Down '   
  

Score1 DB ?
Score2 DB ?
HostGuestStatus DB 1 ;0->Host, 1->Guest ;;; NEW WOLLO SET 0 

;Game Map 23 rows x 80 columns
Map DB 22*80 DUP (30h) ;Each word -> space,attribute to define color (whether there is a wall or no)
MapName DB '1.txt' , 0
MapNameSave DB '1.txt' , 0 ;New Samanoudy
HandleSave DW ? ;New Samanoudy

;Bonus: Drawing  Assem 
PendingMap DB 22*80 DUP (30h)
CursorLocation DW 0A28h 
NumberofBarriers DB 0  ;---->maximum NumberofBarriers = 99
PaintingOption DW 0720h   ;  0270h to unpaint------4470 to paint
LoadMessage  db 'Please Choose the Map you want to load from (1 Or 2 or 3)$'
SaveMessage db 'Please Choose the Map you want to save in (1 Or 2 or 3)$'
LeaveDrawing db 0
ScanCode_Sent db 0
PaintingMode db 'D mode $'   ;D for delete mode  P for Paint mode 
BarrierNumbersMessage db 'Number of barriers:'
AsciiNumofBarriers     db ? ,?, '$'
ExceedMessage db 'Exceeded!$'


AXT dw ?
BXT dw ?
CXT dw ?
DXT dw ?
BPT dw ?
SIT dw ?
DIT dw ?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;new Wello 
;NAME
MMERROR DB 'First Character Must Be a letter$'
MMEnterName DB 'Please Enter your Name:$' 

MMUserName DB 15,0,16 DUP ('$')
GMMUserName DB 15,0,16 DUP ('$')

;MMUserName DB 3,3,'ali$'
;GMMUserName DB 5,5,'ahmed$'

MMPressEnter DB 'Press Enter Key to Continue$' 
;Main Menu 
CursorRows DB 11 
CursorColoumns DB 21 
MMChatMsg DB '*To start chatting, Press F1$'
MMGameMsg DB '*To start Snake game, Press F2$' 
MMCreateMapMsg DB '*To Create or edit Map, Press F3$' 
MMExitMsg DB '*To Exit, Press F4$' 

; selecting Map 
SelectMapERRORMsg DB 'You entered Wrong number, Number Must be(1 Or 2 or 3)$'
SelectMapMsg db 'Please Choose the map you want to play in (1 Or 2 or 3)$' 

;selecting Game Mode 
GameModeERRORMsg DB 'You entered Wrong Character, It Must be 0 Or 1)$'
GameModeMsg db 'Please Choose the mode of the Game ( 1 for Speed Mode , 0 for Normal Mode)$' 

; Sending and Receiveing
CharacterSend DB (?)
CharacterReceived DB 0FFh

;RequsetScreen
RequsetScreenMessage DW GameModeMsg

;Send chat invitation 
sendChatInvitationMsg1 db 'YOU sent a Chat Invitation to: $'
sendChatInvitationMsg2 db ' Refused your chat Request $ ' 
;ReceiveChatInvitaion 
RecChatInvitationErrorMsg db 'Error to Accept Press "y" to Refuse Press "n"$' ;
RecChatInvitationMsg1 db 'You Received An Invitation from: $'
RecChatInvitationMsg2 db ' to Accept Press "y" to Refuse Press "n"$'

;Send Game invitation 
sendGameInvitationMsg1 db 'YOU sent a Game Invitation to: $'
sendGameInvitationMsg2 db ' Refused your Game Request $ ' 


;ReceiveGameInvitaion
RecGameInvitationErrorMsg db 'Error to Accept Press "y" to Refuse Press "n"$' ;
RecGameInvitationMsg1 db 'You Received A Game Invitation from: $'
RecGameInvitationMsg2 db ' to Accept Press "y" to Refuse Press "n"$'
RecGameInvitationWaitMap db 'Please Wait For Loading Map...... $'
RecGameInvitationWaitMode db 'Please Wait For Loading Game Mode...... $'
ExitChatMsg db 'The other user exit chat......'

;Chat 

row    db    0
column db    0
temp1  db    ? 
temp2  db    ?
r1     db   12
c1     db    0

; TESTING 

ToPrint1 DB 30 ;;;;;;;;;;;;;;NEW WELLO 

namesok DB 0

Data_segment_name ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Stack_segment_name segment para stack
	db 32h dup(0) ;define your stack segment
Stack_segment_name ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Code_segment_name segment
Main_prog proc far
assume SS:Stack_segment_name,CS:Code_segment_name,DS:Data_segment_name


	  MOV AX,Data_segment_name ; load the starting address of the data
	  MOV DS,AX ; segment into DS reg.
	  
	  call initialization ;initilaize the serial port
	  Call EnterName
	  Call MainMenu
		
	  MOV AX,4C00h ; exit program
	  INT 21h

Main_prog endp

;;;;;;;;;;;
;Walid
;;;;;;;;;;;

SendData Proc ; sends the charachter stored in (CharacterSent)
	SendCheckBuffer: 
		mov dx,3fdh
		in al,dx
		test al,00100000b
		Jz SendCheckBuffer 
		mov al,CharacterSend
		mov dx,3f8h
		out dx,al
	ret 
SendData endp 

SendWaitConfermation proc 

SendAgain: 
CALL SendData 

MOV CX , 1000H 

WaitForReply3:
CALL ReceiveData 
MOV AL , CharacterReceived
CMP AL , 0EEh  ; to check if the user send the flag back 
JZ Safe 
CMP AL , 0FFH  ; To check if the user didn't send any thing 
LOOP WaitForReply3
jmp SendAgain
Safe: 

RET 

SendWaitConfermation endp 

ReceiveWithConfermation Proc 

Call ReceiveData 
MOV AL , CharacterReceived
CMP AL , 0FFH  ; To check if the user didn't send any thing 
jZ ExitFN 
 
; send a flag (0EE) to check that the other user is recieving 
MOV CharacterSend, 0EEh 
CALL SendData 

ExitFN: 

RET 

ReceiveWithConfermation endp

ReceiveData Proc ; Returns FF in (CharacterReceived) If No Character is Received. 
	MOV DX,3FDh
	IN AL,DX
	TEST AL,1
	JZ NoThingSent
	MOV DX,3F8h
	IN AL,DX
	MOV CharacterReceived,AL
	RET 
	
	NoThingSent: 
	MOV CharacterReceived , 0FFH 
	RET
ReceiveData endp 


SenderCheckConnection Proc 

Repeat9:
; send a flag (0EE) to check that the other user is recieving 
MOV CharacterSend, 0EEh 
CALL SendData 
MOV CX , 0FFFFH

WaitForReply9:
Call ReceiveData 
MOV AL , CharacterReceived
CMP AL , 0EEh  ; to check if the user send the flag back 
JZ ConnectionOpend
CMP AL , 0FFH  ; To check if the user didn't send any thing 
LOOP WaitForReply9
jmp Repeat9

ConnectionOpend: 
RET 

SenderCheckConnection endp 

ReceiverCheckConnection Proc 

WaitForData:
Call ReceiveData 
MOV AL , CharacterReceived
CMP AL , 0EEh  ; to check if the user send the flag  
JZ ConnectionOpend1
CMP AL , 0FFH  ; To check if the user didn't send any thing 
jmp WaitForData

ConnectionOpend1: 
; send a flag (0EE) to check that the other user is recieving 
MOV CharacterSend, 0EEh 
CALL SendData 

RET 

ReceiverCheckConnection endp 

MainMenu PROC
	;Save All Registers
	;CALL SaveRegisters
	;Save Flags
	; PUSHF 
	
	
	MainMenu1:
	
	;Clear Keyboard Buffer
 	 MOV AX,0C00h
	 INT 21h 
	
	 CALL PrintMainMenu
	 Waiting: 
	
	
     CALL GetMainMenuInput
	 mov al , ah 
	;MOV AL , 62 
	
	 CMP AL , 59 ; SCAN CODE OF (F1)
	 JZ F1pressd    
	
	 CMP AL , 60 ; SCAN CODE OF (F2)
	 JZ F2pressd
	
	 CMP AL , 61 ; SCAN CODE OF (F3)
	 JZ F3pressd  
	
	 Cmp AL , 62 ;SCAN CODE OF (F4)
	 JZ F4pressd 
	
	 CALL ReceiveWithConfermation
	;Call ReceiveData 
	 CMP CharacterReceived , 0FFH 
	 JZ Waiting
	
	 ;IF it received something
	MOV AL , CharacterReceived
	
	CMP AL , 59 ; SCAN CODE OF (F1)
	 JZ F1Received    
	
	 CMP AL , 60 ; SCAN CODE OF (F2)
	 JZ F2Received
	
	
	 JMP waiting
	 
	F1Received: 
			Call ReceiveChatInvitaion 
			JMP MainMenu1 
	F2Received: 
			Call ReceiveGameInvitaion 
			jmp MainMenu1 
	
	F1pressd: 
		 CALL SendChatInvitation
		 JMP MainMenu1
	F2pressd: 
		CALL SendGameInvitation
		JMP MainMenu1 
		
	F3pressd: 
		Call DrawMap 
		JMP MainMenu1
	F4pressd: 
		 
	
	;Return Flags
	;POPF
	;Load All Registers
	;CALL LoadRegisters
	;Return
	RET
MainMenu ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SendRecNames proc 
 MOV CX , 16
 MOV SI , 2
; sending our name to the other player ; he will be waiting for it 
 Sending_MM:
 MOV AL , MMUserName[SI]
 MOV CharacterSend , AL 
 CALL SendData
 inc SI
 Loop Sending_MM
 
 MOV CX , 16
 Mov DI,2
; Reciving the other player name  

 Recieving_MM:
  Retry_MM:
  CALL ReceiveData
  Cmp CharacterReceived, 0FFh
  JZ Retry_MM
  mov al, CharacterReceived
  MOV GMMUserName[DI], Al
  inc DI
 
 
 Loop Recieving_MM
; CALL ReceiveData 

MOV namesok,1

ret
SendRecNames endp 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RecSendNames proc 

; Reciving the other player name  
  MOV CX , 16
  Mov DI,2

 Recieving_GMM:
  Retry_GMM:
  CALL ReceiveData
  Cmp CharacterReceived, 0FFh
  JZ Retry_GMM
  mov al, CharacterReceived
  MOV GMMUserName[DI], Al
  inc DI
 
 Loop Recieving_GMM

  MOV CX , 16
  MOV SI , 2
; sending our name to the other player ; he will be waiting for it 
 Sending_GMM:
  MOV AL , MMUserName[SI]
  MOV CharacterSend , AL
  CALL SendData
  inc SI
  Loop Sending_GMM
MOV namesok,1 
ret
RecSendNames endp 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SendMap proc

  MOV CX , 80*22
  MOV SI , 0
; sending our name to the other player ; he will be waiting for it 
 Sending_Map:
  MOV AL ,Map[SI]
  MOV CharacterSend , AL 
  CALL SendData
  inc SI
  Loop Sending_Map

ret
SendMap endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ReceiveMap proc

  MOV CX , 80*22
  Mov DI,0

 Recieving_Map:
 
  Retry_Map:
  CALL ReceiveData
  Cmp CharacterReceived, 0FFh
  JZ Retry_Map
  mov al, CharacterReceived
  MOV Map[DI], Al
  inc DI
  
 Loop Recieving_Map

ret
ReceiveMap endp

SelectGameMode PROC 

	Call Clear_Screen 	
	JMP GameModeNOError9
	
	GameModeError9:
	Call Clear_Screen 	

	;Print Error Msg 
	MOV DH , 6 
	MOV DL , 10 
	Lea CX , GameModeERRORMsg
	CALL PrintMMText
	
	GameModeNOError9:
	;next code is to print select map message  
	MOV DH , 8 
	MOV DL , 10
	Lea CX , GameModeMsg
	CALL PrintMMText
	
	; TO Print 'Press Enter' 
	MOV DH , 10 
	MOV DL , 10 
	Lea CX , MMPressEnter
	CALL PrintMMText
	;MOVE  the cursor to the  next line 
	MOV DL , 28 
	MOV DH , 9 
	MOV AH , 2 
	INT 10H 
	;take Number of Game Mode  
	MOV AH , 1 
	INT 21H 
	
	
	; to check that the user entered a correct number 
	;0
	SUB al,30h
	CMP al , 0 
	Jz CorrectGameMode
	;1 
	CMP al , 1
	Jz CorrectGameMode
	JMP GameModeError9
	
	CorrectGameMode:
	MOV Game_Difficulty_Mode , AL 
	
	GameModeExit:
	
Ret 

SelectGameMode endp 

SelectMap Proc 

Call Clear_Screen 	
	JMP SelectMapNOError9
	
	SelectMapError9:
	Call Clear_Screen 	

	;Print Error Msg 
	MOV DH , 6 
	MOV DL , 22 
	Lea CX , SelectMapERRORMsg
	CALL PrintMMText
	
	SelectMapNOError9:
	;next code is to print select map message  
	MOV DH , 8 
	MOV DL , 22 
	Lea CX , SelectMapMsg
	CALL PrintMMText
	
	; TO Print 'Press Enter' 
	MOV DH , 10 
	MOV DL , 22 
	Lea CX , MMPressEnter
	CALL PrintMMText
	;MOVE  the cursor to the  next line 
	MOV DL , 28 
	MOV DH , 9 
	MOV AH , 2 
	INT 10H 
	;take Number of Map  
	MOV AH , 1 
	INT 21H 
	
	
	; to check that the user entered a correct number 
	;Smaller than 1 
	CMP al , 31H 
	JB SelectMapError9
	;Greater than 3 
	CMP al , 33H
	JA SelectMapError9
	
	MOV MapName , AL 
	SelectMapExit:
	
Ret 

SelectMap endp 


PrintMessageRequsetScreen proc 

	CALL ClearRequestScreen 
	MOV CX , RequsetScreenMessage
	MOV DH, 23 
	MOV DL , 0 
	CALL PrintMMText 
	Ret 
PrintMessageRequsetScreen endp 

ClearRequestScreen proc 

    push es
	mov ax,0b800h
	mov es,ax
	mov di,23*160 
	mov ax,0720h
	cld
	mov cx,2*80
	rep stosw
	pop es
	ret

ClearRequestScreen endp 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


SendChatInvitation proc 

; to send the request of the chat 
 MOV CharacterSend , 59 
 CALL SendWaitConfermation
 
 

 ; to exchange names with the other player 
CALL SenderCheckConnection

cmp namesok,1
JZ SendChatInvitation_namesok
call SendRecNames 

SendChatInvitation_namesok:
; print msg 
LEA DX , sendChatInvitationMsg1 
 MOV RequsetScreenMessage , DX 
 CALL PrintMessageRequsetScreen 
 
 ; Print other player name 
 LEA DX , GMMUserName[2] 
 MOV AH , 09 
 INT 21H 
 
 
 WaitForReply:
 CALL ReceiveData 
 CMP CharacterReceived , 0FFH 
 JZ WaitForReply
 Cmp CharacterReceived , 'y' 
 JZ ACCEPT9
 Cmp CharacterReceived , 'n' 
 JZ Refuse9
 JmP WaitForReply
 
 ACCEPT9: 
 CALL Chat
 JMP ChatInvExit
 
 Refuse9: 
 ;Print the other Player Name 
 
 LEA DX , GMMUserName[2]
 MOV RequsetScreenMessage , DX 
 CALL PrintMessageRequsetScreen 
 
 mov ah , 02 
 LEA DX ,sendChatInvitationMsg2 
 int 21h 
 

 
 ChatInvExit: 
 RET 
SendChatInvitation endp



ReceiveChatInvitaion proc 

CALL ReceiverCheckConnection 
cmp namesok,1
JZ ReceiveChatInvitation_namesok
call RecSendNames

ReceiveChatInvitation_namesok:
Jmp Print_Invitation

RetryEnterAgain_Chat:

 LEA DX , RecChatInvitationErrorMsg 
 MOV RequsetScreenMessage , DX 
 CALL PrintMessageRequsetScreen 
 jmp WaitRecChatInvAnswer
Print_Invitation:

LEA DX , RecChatInvitationMsg1 
 MOV RequsetScreenMessage , DX 
 CALL PrintMessageRequsetScreen 
 
 ;Print the other Player name 
 MOV AH , 09 
 LEA DX ,GMMUserName[2]
 INT 21H  
 
LEA DX , RecChatInvitationMsg2 
mov ah , 09 
int 21h  

WaitRecChatInvAnswer:
;Waiting for the answer from user
mov ah, 01
int 21h
cmp al, 'y'

Jz AcceptChat

cmp al, 'n'

Jz RefuseChat


Jmp RetryEnterAgain_Chat

AcceptChat:
mov LeaveDrawing,2  ; leave drawing but save last map
mov CharacterSend , 'y'
call SendData
call Chat 
jmp EndChatInvitation

RefuseChat:
mov LeaveDrawing,0  ;dont leave drawing
mov CharacterSend , 'n'
call SendData
call ClearRequestScreen


EndChatInvitation :

Ret 
ReceiveChatInvitaion endp 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReceiveGameInvitaion proc 

CALL ReceiverCheckConnection 
cmp namesok,1
JZ ReceiveGameInvitation_namesok
call RecSendNames

ReceiveGameInvitation_namesok:



Jmp Print_GameInvitation

RetryEnterAgain_Game:

 LEA DX , RecGameInvitationErrorMsg
 MOV RequsetScreenMessage , DX 
 CALL PrintMessageRequsetScreen 
 
 jmp WaitRecGameInvAnswer
 
 
Print_GameInvitation:
;pRINT MSG "you Received an invitation from: "
LEA DX , RecGameInvitationMsg1
 MOV RequsetScreenMessage , DX 
 CALL PrintMessageRequsetScreen 
 
 ;Print the other Player Name 
 MOV AH , 09 
 LEA DX ,GMMUserName[2]
 INT 21H  
 ;Print MsG "to accept Press..."
LEA DX , RecGameInvitationMsg2 
mov ah , 09 
int 21h  

WaitRecGameInvAnswer:
;Waiting for the answer from user
mov ah, 01
int 21h
cmp al, 'y'

Jz AcceptGame1

cmp al, 'n'

Jz RefuseGame


Jmp RetryEnterAgain_Game

AcceptGame1:


;Send to host Pc accept
mov LeaveDrawing,2
mov CharacterSend , 'y'
call SendData


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Printing message WAITING MAP 
LEA DX , RecGameInvitationWaitMap
MOV RequsetScreenMessage , DX 
CALL PrintMessageRequsetScreen 
 ;Receive the Map from Host
Call ReceiverCheckConnection
Call ReceiveMap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Printing message WAITING GAME MODE 
LEA DX , RecGameInvitationWaitMode
MOV RequsetScreenMessage , DX 
CALL PrintMessageRequsetScreen
;Receive the Mode from Host
Call ReceiverCheckConnection
still: 
Call ReceiveData 
Cmp CharacterReceived , 0FFh 
jz still 
Mov Al, CharacterReceived
Mov  Game_Difficulty_Mode,Al



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Nowwwwww starting the game
MOV HostGuestStatus , 1 
call Game
JMP EndGameInvitation 


RefuseGame:
mov LeaveDrawing,0
mov CharacterSend , 'n'
call SendData
call ClearRequestScreen
 
 
EndGameInvitation:

Ret 
ReceiveGameInvitaion endp
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SendGameInvitation proc 

;CALL SenderCheckConnection
MOV CharacterSend , 60 
CALL SendWaitConfermation 
 
CALL SenderCheckConnection 

cmp namesok,1
JZ SendGameInvitation_namesok
call SendRecNames 

SendGameInvitation_namesok:
 
 ;PRINT WAITING MSG 
LEA DX , sendGameInvitationMsg1
MOV RequsetScreenMessage , DX 
CALL PrintMessageRequsetScreen
 ;PRINT PLAYER NAME 
MOV AH , 09  
LEA DX , GMMUserName[2] 
INT 21H  
 
 WaitForReplyGame:
 ;Printing Message
 CALL ReceiveData 
 CMP CharacterReceived , 0FFH 
 JZ WaitForReplyGame
 Cmp CharacterReceived , 'y' 
 JZ ACCEPTGame
 CMP CharacterReceived , 'n'
 JZ RefuseGame1
 JMP WaitForReplyGame
 
 ACCEPTGame:
 
 ;Selecting Map & Sending the map
 Call SelectMap
 Call StoreMap
 call SenderCheckConnection 
 Call SendMap
 
 ;Selecting Mode & Sending Mode
 Call SelectGameMode
 call SenderCheckConnection 
 mov Al, Game_Difficulty_Mode
 mov CharacterSend, Al
 Call SendData
 mov al ,CharacterSend 

 
 ;Starting the Game
 MOV HostGuestStatus , 0 
 Call Game
 
 JMP GameInvExit
 
 RefuseGame1: 
 
 ;PRINT PLAYER NAME 
 LEA DX , GMMUserName
 MOV RequsetScreenMessage , DX 
 CALL PrintMessageRequsetScreen 
 ;PRINT Refuse MSG
 MOV AH , 09  
 LEA DX ,  sendGameInvitationMsg2
 INT 21H  
 
 GameInvExit: 
 
 RET 
SendGameInvitation endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



EnterName proc 
;Save All Registers
	;CALL SaveRegisters
	;Save Flags
	PUSHF
	Call Clear_Screen 	
	JMP EnterNameNOError9
	
	EnterNameError9:
	Call Clear_Screen 	

	;Print Error Msg 
	MOV DH , 6  ; rows 
	MOV DL , 22 ; columns 
	Lea CX , MMERROR
	CALL PrintMMText
	
	EnterNameNOError9:
	;next code is to print enter your name 
	MOV DH , 8 
	MOV DL , 22 
	Lea CX , MMEnterName
	CALL PrintMMText
	
	; TO Print 'Press Enter' 
	MOV DH , 10 
	MOV DL , 22 
	Lea CX , MMPressEnter
	CALL PrintMMText
	;MOVE  the cursor to the  next line 
	MOV DL , 28 
	MOV DH , 9 
	MOV AH , 2 
	INT 10H 
	;take user name 
	LEA DX , MMUserName
	MOV AH , 0AH 
	INT 21H 
	
	;Smaller than A 
	CMP MMUserName[2] , 41H 
	JB EnterNameError9
	;Greater than z 
	CMP MMUserName[2] , 7AH
	JA EnterNameError9
	;Greater than Z 
	CMP MMUserName[2] , 5AH
	JA GreaterThan_Z
	
	JMP EnterNameExit
	GreaterThan_Z: 
	; Smaller than a 
	CMP MMUserName[2] , 61h
	JB EnterNameError9
	

	
	
	
	EnterNameExit:
	MOV BL , MMUserName[1]
	MOV BH , 0 
	MOV MMUserName[2+BX], '$' 
	;Return Flags
	POPF
	;Load All Registers
	;CALL LoadRegisters
	;Return
	RET
EnterName endp 

GetMainMenuInput PROC ; Reterns in Al (59->F1 , 60->F2, 61->F3 , 62->F4) 
	;Save All Registers
	; CALL SaveRegisters
	;Save Flags
	PUSHF
	mov ah , 1
	int 16h 
	jz Empty99 
	mov ah,0
	int 16h
	jmp Exit99
	Empty99 :
	mov ah , 0
	Exit99:
	
	;Return Flags
	POPF
	;Load All Registers
	;CALL LoadRegisters
	;Return
	RET
GetMainMenuInput ENDP

PrintMainMenu proc 

MOV BX, 0 
CALL Clear_Screen 
call ClearRequestScreen 

; TO Print first message
	MOV DH , 8 
	MOV DL , 22 
	Lea CX , MMChatMsg
	CALL PrintMMText
; TO Print Game message	

	MOV DH , 9 
	MOV DL , 22 
	Lea CX , MMGameMsg
	CALL PrintMMText
; TO Print mAP message	

	MOV DH , 10 
	MOV DL , 22 
	Lea CX , MMCreateMapMsg
	CALL PrintMMText
; TO Print Exit message	

	MOV DH , 11 
	MOV DL , 22 
	Lea CX , MMExitMsg
	CALL PrintMMText

	
;TO Print the line in the bottom 
	MOV DH , 22 
	MOV DL , 0 
	MOV AH , 02H
	INT 10H 
	MOV DL , '_' 
	MOV CX , 80 
	MOV AH , 2
	MMP___: 
	INT 21H 
	LOOP MMP___
	RET 
PrintMainMenu endp ; Prints a message from the data segment with offset equal to (BX) to Vram Starting from location (Dl= Rows , Dh = Coulomns) 


PrintMMText proc 

CALL SaveRegisters
	MOV AH , 02 
	INT 10H 
	MOV AH , 09
	MOV DX , cX 
	INT 21H
CALL LoadRegisters	
	RET

PrintMMText endp 



GenerateApple PROC
	;Save All Registers
	GenerateApple_Generate:
	mov ah , 2Ch ; Get System Current time (hunderads of seconds) 
	INT 21H 
	MOV Bl , 80 ;to make the Number from 0 to 79 Divide by 80 and take the remainder
	MOV AL , DL 
	MOV AH , 0 
	DIV Bl
	MOV BYTE PTR AppleLocation[1] , AH ; move it in the X Location 
	;CALL Printing

	MOV AH , 2CH 
	INT 21H 
	MOV AH , 0 
	MOV AL , DL 
	MOV BL , 22   ; to make the Number from 0 to 21 Divide by 22 and take the remainder
	DIV BL 
	MOV BYTE PTR AppleLocation[0] , AH  ; move it in the Y Location 
	;CALL Printing
	
	CALL DetectAppleCollision
	CMP DetectAppleCollision_CollisionType,1
	JZ GenerateApple_Generate
	
	;Return
	RET
GenerateApple ENDP


UpdateApple PROC
	;Save All Registers
	CALL SaveRegisters
	;Save Flags
	PUSHF
	
	
	
	;Return Flags
	POPF
	;Load All Registers
	CALL LoadRegisters
	;Return
	RET
UpdateApple ENDP




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SaveRegisters PROC
	MOV AXT,AX
	MOV BXT,BX
	MOV CXT,CX
	MOV DXT,DX
	MOV BPT,BP
	MOV SIT,SI
	MOV DIT,DI
	RET
SaveRegisters ENDP

LoadRegisters PROC
	MOV AX,AXT
	MOV BX,BXT
	MOV CX,CXT
	MOV DX,DXT
	MOV BP,BPT
	MOV SI,SIT
	MOV DI,DIT
	RET
LoadRegisters ENDP

;;;;;;;;;;;
;Samanoudy
;;;;;;;;;;;





;;;;;;;;;;;
;Assem
;;;;;;;;;;;

Chat PROC
	;Save All Registers
	;CALL SaveRegisters
	;Save Flags
	PUSHF
	
    mov row,0
    mov column,0
    mov r1 , 12
    mov c1, 0	

;call initialization ;initilaize the serial port 
call cls            ;clear screen
call split          ;splitting the screen

Mn:

call setcur         ;update the cursor position
call checkkeypress  ;check if key pressed 
					;if key is present in buffer read it and display
call Receive        ;then check the revice buffer  
                    ;if not empty -read the charcter and displays it
jmp Mn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
finish:
	
	;Return Flags
	POPF
	;Load All Registers
	;CALL LoadRegisters
	;Return
	;Call EnterPress1
  	;Call EnterPress
	Call SendExitMSG
   
	RET
Chat ENDP



SendExitMSG proc

	Call EnterPress
  	Call EnterPress1

	; sending Exit Msg
	Sending_Chat_MSG:
	MOV AL , 27
	MOV CharacterSend , AL 
	CALL SendData
	
	RET
SendExitMSG ENDP



;;;;;;;;;;;
;Serial Communication
;;;;;;;;;;;



;!!!!!!!!!!!!!!!!!!!!!!!!TESTING FUNCTION !!!!!!!!!!!!!!!!!!!!!!!!!!!!

Printing proc ; Print Number of Two Digits, input(ToPrint1 ~HEX) 

CALL SaveRegisters
MOV AL , ToPrint1
MOV AH , 0 
MOV BL , 10 
DIV BL 
ADD AL , 30H 
ADD AH , 30h 
MOV BL ,  AL
MOV BH , AH  
MOV DL ,BL   
MOV AH , 2 
INT 21H  
MOV DL , BH  
INT 21H
CALL LoadRegisters
RET 
Printing endp 

; CAHT FNS 


cls proc near
mov ax,0b800h
mov es,ax
mov ax,0720h
mov di,0
mov cx,2000
rep stosw 
ret
cls endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

split proc near
;upper
mov ah,06
mov al,0   ;NULL Character
mov bh,7   ;Attribute of upper screen

mov ch,0   ;upper left coordinates of upper screen(0,0)
mov cl,0

mov dh,11  ;lower right coordinates of upper screen(79,11)
mov dl,79
;lower
mov ah,06
mov al,0
mov bh,70h ;Attribute of lower screen
mov ch,12  ;upper left coordinates of lower screen(0,12)
mov cl,0
mov dh,24  ;lower right coordinates of lower screen(79,24)
mov dl,79
int 10h
ret
split endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

setcur proc near
mov ah,03
mov bh,0
int 10h
mov ah,02
mov dh,DS:[row]
mov dl,DS:[column]
int 10h
ret
setcur endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

setcur1 proc near
mov ah,03
mov bh,0
int 10h
mov ah,02
mov dh,DS:[r1]
mov dl,DS:[c1]
int 10h
ret
setcur1 endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
EnterPress proc near

Jmp DEnter

BEnter:
mov DS:[column],0
inc DS:[row]
jmp exitEnter

DEnter:
cmp DS:[row],11
jne BEnter

mov DS:[row],11
mov DS:[column],0
call scrllup
exitEnter:
ret
EnterPress endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnterPress1 proc near

jmp DDEnter

BBEnter:
mov DS:[c1],0
inc DS:[r1]
jmp exittEnter

DDEnter:
cmp DS:[r1],24
jne BBEnter

mov DS:[r1],24
mov DS:[c1],0
call scrllup1
exittEnter:
ret
EnterPress1 endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

prnt proc near
mov ah,09           ;Function 9 =write char and attrib at [preset] cursor position
mov bl,07           ;attribute =7 [as we wrote in the upper screen ]
mov bh,0            ;page zero = current page display
mov al,DS:[temp1]   ;ACII of character 
mov cx,1            ;single char to be displayed
int 10h             
ret
prnt endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

prnt1 proc near
mov ah,09
mov bl,70h
mov bh,0
mov al,DS:[temp2]
mov cx,1
int 10h
ret
prnt1 endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

scrllup proc near
mov ah,06      
mov al,1      ;scroll one line up
mov bh,7      ;attribute of upper screen
mov ch,0      ;upper left coordinates of upper screen(0,0)
mov cl,0
mov dh,11     ;lower right coordinates of upper screen(79,11)
mov dl,79
int 10h
ret
scrllup endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

scrllup1 proc near
mov ah,06
mov al,1         ;scroll one line up
mov bh,70h       ;attribute of lower screen
mov ch,12        ;upper left coordinates of lower screen(0,12)
mov cl,0         
mov dh,24        ;lower right coordinates of lower screen(79,24)
mov dl,79
int 10h
ret
scrllup1 endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

initialization proc near

mov dx,3FBh            ;Address of line control register
mov al,10000000b       ;Data to be outputted on LCR =1000 0000 [To make DLAB=1]
Out dx,al              ;OUT DX,AL   Put Data on AL on the port of address DX

mov dx,3F8h            ;Divisor Latch Low [DLAB=1]
mov al,0Ch              
Out dx,al

mov dx,3F9h            ;Divisor Latch High [DLAB=0]
mov al,0
Out dx,al
;Divisor = 00 0C ; The Baud Rate =9600

mov dx,3FBh              ;Return to LCR
mov al,00011011b         ;Data =8 bit - 1 stop bit - even parity
Out dx,al

ret
initialization endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
movcur proc near
A:
inc DS:[column]
cmp DS:[column],80    
je D
jne exit

B:
mov DS:[column],0
inc DS:[row]
jmp exit

D:
cmp DS:[row],11
jne B

mov DS:[row],11
mov DS:[column],0
call scrllup
exit:
ret
movcur endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

movcur1 proc near
AA:
inc DS:[c1] 
cmp DS:[c1],80
je DE
jne exitt
BB:
mov DS:[c1],0
inc DS:[r1]
jmp exitt
DE:
cmp DS:[r1],24
jne BB
mov DS:[r1],24
mov DS:[c1],0
call scrllup1
exitt:
ret
movcur1 endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checkkeypress proc near
mov ah,01      ;check the status of the keyboard buffer 
int 16h       
  
jz e           ;if ZF=1 No waiting characters in the buffer 

mov ah,0       ;read the character from the keyboard buffer 
int 16h
mov DS:[temp1],al
cmp al,27
je e1
cmp al,0Dh
je Enter_Press

call prnt      ;take temp1 and print it into the current cursor position 
call movcur    ;update the cursor position
call Send      
jmp e
e1:
pop Bp 
pop bp 
JMP finish
; mov ax,4c00h ; exit program
; int 21h

jmp e
Enter_Press:
Call EnterPress
call Send      


e:
ret
checkkeypress endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Receive proc near
mov dx,3FDh        ;Line status register
In al,dx

test al,00000001b ; Test if DR=1
jz ee             ;Jump if zero [DR=0] No data is ready to be picked up 

mov dx,3F8h       ;Read data from Receive buffer [3F8] into AL    
In al,dx

CMP AL,27
JNZ Receive_Continue
pop Bp 
pop bp 
JMP finish

Receive_Continue:
mov DS:[temp2],al ;Then move it to temp2

cmp al,0Dh
je Enter_Press1

call setcur1
call prnt1
call movcur1

jmp ee
Enter_Press1:
Call EnterPress1



ee:
ret
Receive endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Send proc near
CheckSend:
mov dx,3FDh   ;Read the line status register`
In al,dx
test al,20h   ;test the THRE [Transmit hold register empty] 
jz CheckSend  ; if THRE=0 then loop until it = 1 [until the old data is sent
mov dx,3F8h     
mov al,DS:[temp1] ;mov temp1 [the data read from the user to THR]
Out dx,al
eee:
ret
Send endp



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GameSpeed_Init PROC
	
	CMP Game_Difficulty_Mode,0 
	JZ GameSpeed_Init_NormalMode ;;If 0 then normal mode, initialize speed to 10 "fastest"
	JMP GameSpeed_Init_SpeedMode ;;Else then 1 then speed mode, initialize speed to 35 "slowest"
	
	GameSpeed_Init_NormalMode:
	MOV GameSpeed,10
	RET
	GameSpeed_Init_SpeedMode:
	MOV GameSpeed,30
	RET
GameSpeed_Init ENDP

;;;;;;;;;;;;;;;;;;;;;;;;; Game

Game PROC
;Initialize Game Data
;;Initialize Snakes Positions
	
	;Move Cursor to end of screen
	MOV dx,184Fh
	MOV bx,0
	MOV ah,02h
	INT 10h
		
	
	;;;Snake1 Position
	MOV Snake1,1409h
	MOV Snake1+2,1309h
	MOV Snake1+4,1209h
	MOV Snake1+6,1109h
	MOV Snake1+8,1009h
	MOV Snake1+10,0A09h
	MOV Snake1+12,0909h
	MOV Snake1+14,0809h
	MOV Snake1+16,0709h
	MOV Snake1+18,0609h
	MOV Snake1+20,0509h
	MOV Snake1+22,0409h
	;;;Snake2 Position
	MOV Snake2,3C09h
	MOV Snake2+2,3D09h
	MOV Snake2+4,3E09h
	MOV Snake2+6,3F09h
	MOV Snake2+8,4009h
	MOV Snake2+10,4109h
	MOV Snake2+12,4209h
	MOV Snake2+14,4309h
	MOV Snake2+16,4409h
	MOV Snake2+18,4509h
	MOV Snake2+20,4609h
	MOV Snake2+22,4709h
	
;;Initialize Snakes Sizes
	MOV Snake1Size,3
	MOV Snake2Size,3
;;Initialize Snakes Directions
	MOV Snake1Direction,0
	MOV Snake2Direction,2
;;Initialize Scores
	MOV NotAdjustedScore,00h
	MOV NotAdjustedScore+1,00h
;;Generate the first Apple
	CALL GenerateApple
;;Store the Map from the File into the DataSegment and Print it
	;CALL StoreMap
	CALL Clear_Screen
	LEA SI,Map
	CALL PrintMap
	
;;Initialize Game End Status 0->Didn't End, 1->End
	MOV Game_GameEndStatus,0

;;Initialize Game Speed
	CALL GameSpeed_Init
	
GameCycle: ;Infinite Loop
	;;Get Input for the Snake Direction from the user
	CALL GetInput
	
	;Header Part,, Share data between Host and Guest
	CMP HostGuestStatus,0 ;IF it is 0 then Host, else Guest
	JNZ GuestHeader
	;;If Host:
	CALL GameHeaderHost
	JMP GameLogic
	
	GuestHeader:
	;;If Guest
	CALL GameHeaderGuest
	
	GameLogic:
	;Game Logic1,, The same regardless of Host or Guest
	
	;;Clear the Tails of Snakes and Apple
	CALL ClearSnakesApple
	
	;;Update Snake Positions based on the Snakes Directions
	CALL UpdateSnakes
	;;Check if Collision happened due to the current Snakes Positions
	CALL DetectCollision
	
	;;Print the Apple
	CALL PrintApple
	;Print the Snake
	CALL PrintSnakes
	
	;;Update Scores, Snake Lengths or Game Speed according to the collisions
	CALL UpdateStatus
	
	;;Prepare Status for Printing and Print it
	CALL AdjustPositionScore
	CALL UpdateStatusDirection
	CALL PrintStatus
        
	;;If Game Ends Jump out
	CMP Game_GameEndStatus,1
	JZ Game_GameEnd
	
	;;Game Time
	CALL LoopDelay
	
JMP GameCycle

	Game_GameEnd:
	;;Press any key to continue
	;MOV AH,1
	;INT 21h
	
	RET
Game ENDP


StoreMap PROC

;to open the file	
mov ah, 3Dh
;Read only file
mov al, 0
LEA Dx, MapName	
int 21h


;Mov from Ax to  file handle
mov fileHandle, Ax

;Reading from file
mov Bx , fileHandle
mov ah, 3Fh
;Set no of bites to read
mov Cx,22*80
;Setting Dx to the address location
LEA Dx, Map
int 21h


;Closing the file
mov ah, 3Eh
mov Bx , fileHandle
int 21h

	
	RET
StoreMap ENDP

PrintApple PROC

	
	;Getting apple location  Y
	Mov al, byte ptr AppleLocation[0]
	mov ah, 0
	mov bl,160
	mul bl
	Mov dx,ax
	
	;Getting apple location  X
	Mov al, byte ptr AppleLocation[1]
	mov ah, 0
	mov bl,2
	mul bl
	
	;Getting the offset by adding y*160+x*2
	Add Ax, DX
		
	;Moving Ds to The VRAM
	    mov Dx,0b800h
		mov ES,Dx 
		
    ;Moving offset to Di
		mov di, Ax
		
	;Printing sapace with red colour	
		Mov Ax, 2420h
		cld
		Stosw

	
	RET
PrintApple ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintSnakes PROC
	Call PrintSnake1
	Call PrintSnake2
	;Return
	RET
PrintSnakes ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PrintSnake1 PROC

	
;Moving Ds to The VRAM
	    mov Dx,0b800h
		mov ES,Dx 
		
	;Setting si to 0
	Mov Si, 0	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Now Printing A face For the snake 
    ;Getting Snake Head location  Y
	Mov al, byte ptr Snake1[si]
	mov ah, 0
	mov bl,160
	mul bl
	Mov dx,ax
	
	inc Si
	;Getting Snake Head location  X
	Mov al, byte ptr Snake1[Si]
	mov ah, 0
	mov bl,2
	mul bl
	
	;Getting the offset by adding y*160+x*2
	Add Ax, DX

    ;Moving offset to Di
		mov di, Ax
		
	;Printing Face of the snake	
		Mov Ax, 4201h
		cld
		Stosw
      inc si
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;Printing remain the snake so I need to -1 from the size of
	;the snake as I already prited its head
	
	

	MOV Ch, 0
	
	Mov Cl, Snake1Size
	
	Dec Cx

	PrintingSnake1 :
		;Getting Snake location  Y
	Mov al, byte ptr Snake1[si]
	mov ah, 0
	mov bl,160
	mul bl
	Mov dx,ax
	
	inc Si
	;Getting snake location  X
	Mov al, byte ptr Snake1[Si]
	mov ah, 0
	mov bl,2
	mul bl
	
	;Getting the offset by adding y*160+x*2
	Add Ax, DX
		
	
		
    ;Moving offset to Di
		mov di, Ax
		
	;Printing sapace with red colour	
		Mov Ax, 4420h
		cld
		Stosw
      inc si
	  

loop PrintingSnake1		
	
	RET
PrintSnake1 ENDP

PrintSnake2 PROC

	;Moving Ds to The VRAM

	    mov Dx,0b800h
	    mov ES,Dx 
		
	;Setting si to 0
	Mov Si, 0	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Now Printing A face For the snake 
    ;Getting Snake Head location  Y
	Mov al, byte ptr Snake2[si]
	mov ah, 0
	mov bl,160
	mul bl
	Mov dx,ax
	
	inc Si
	;Getting Snake Head location  X
	Mov al, byte ptr Snake2[Si]
	mov ah, 0
	mov bl,2
	mul bl
	
	;Getting the offset by adding y*160+x*2
	Add Ax, DX

    ;Moving offset to Di
		mov di, Ax
		
	;Printing Face of the snake	
		Mov Ax, 1401h
		cld
		Stosw
      inc si
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;Printing remain the snake so I need to -1 from the size of
	;the snake as I already prited its head
	
	MOV Ch, 0
	
	Mov Cl, Snake2Size
	
	Dec Cx

	PrintingSnake2 :
		;Getting Snake location  Y
	Mov al, byte ptr Snake2[si]
	mov ah, 0
	mov bl,160
	mul bl
	Mov dx,ax
	
	inc Si
	;Getting snake location  X
	Mov al, byte ptr Snake2[Si]
	mov ah, 0
	mov bl,2
	mul bl
	
	;Getting the offset by adding y*160+x*2
	Add Ax, DX
		
	
		
    ;Moving offset to Di
		mov di, Ax
		
	;Printing sapace with blue colour	
		Mov Ax, 1120h
		cld
		Stosw
      inc si
	  

loop PrintingSnake2		
	

	;Return
	RET
PrintSnake2 ENDP


PrintStatus PROC
	;Save All Registers
	;CALL SaveRegisters
	
	
	; print isolate line
	mov ax,0b800h
	mov es,ax
	mov ax,072Dh
	mov di,22*160
	mov cx,80
	cld
	rep stosw
	
	; print score
	lea si,ScoreTemplate1
	mov al,07h
	mov di,23*160
	mov cx,23
	do1:
		movsb
		stosb
	loop do1
		
	
	; print Host Position
	add di,22
	mov al,20h
	mov ah,01000000b
	stosw
	lea si,ScoreTemplate2
	mov al,07h
	mov cx,17
	do2:
		movsb
		stosb
	loop do2
	
	
	; print Guest Position
	add di,18
	mov al,20h
	mov ah,00010000b 
	stosw
	lea si,ScoreTemplate3
	mov al,07h
	mov cx,18
	do3:
		movsb
		stosb
	loop do3 
	
	

	;Return
	RET
PrintStatus ENDP



PrintMap PROC
	mov ax,0b800h
	mov es,ax
	mov al,20h
	;lea si,Map
	mov di,0
	mov cx,22*80
	ForLoopDraw:
		mov dl, byte ptr [si]
		cmp dl,30h
		JE DrawGap
		JNE DrawBarrier
		DrawGap:
			mov ah,07h
			stosw
			jmp EndloopCycle
		DrawBarrier:
			mov ah,70h
			stosw
		EndLoopCycle:
			inc si
	loop ForLoopDraw
			
	;Return
	RET
PrintMap ENDP



UpdateStatus PROC
	;push registers
	;CALL SaveRegisters
	
	
	
	;Knowing the Type of the collision that occurred during
	cmp DetectCollision_CollisionType,0
	JE UpdateStatus_Exit
	cmp DetectCollision_CollisionType,1
	JE IncrementSnake1
	cmp DetectCollision_CollisionType,2
	JE IncrementSnake2
	cmp DetectCollision_CollisionType,3
	JE HeadsCollision ;both dead 
	cmp DetectCollision_CollisionType,4
	JE LabelGW  ;Snake1Die
	cmp DetectCollision_CollisionType,5
	JE LabelGW;Snake1Die
	cmp DetectCollision_CollisionType,6
	JE LabelGW ;Snake1Die
	cmp DetectCollision_CollisionType,7
	JE labelHW ;Snake2Die
	cmp DetectCollision_CollisionType,8
	JE LabelHW ;Snake2Die
	cmp DetectCollision_CollisionType,9
	JE LabelHW ;Snake2Die
		
	IncrementSnake1:  ;Host Snake ate an apple
		CALL UpdateStatus_Increment1
		RET
		
	IncrementSnake2:  ;Guest Snake ate an apple
		CALL UpdateStatus_Increment2
		RET
		
	HeadsCollision:   ; collision from heads
		CALL UpdateStatus_HeadsCollision
		RET
		
	LabelHW:
		CALL HostWin
		RET
		
	LabelGW:
		CALL GuestWin
		RET
		
	LabelDS:
		CALL DrawScore
	
	UpdateStatus_Exit:
		RET
		
UpdateStatus ENDP


UpdateStatus_Increment1 PROC
	;Generate New Apple
	CALL GenerateApple
	
	INC NotAdjustedScore
	CMP NotAdjustedScore,30
	JNE UpdateStatus_INC1_Update
	CALL HostWin
	RET
	
	UpdateStatus_INC1_Update:
	CMP Game_Difficulty_Mode,0 ;;If 0, increase Snake1 Size
	JNZ UpdateStatus_INC1_GameSpeed
	INC Snake1Size
	CMP Snake1Size,13 ;;If Snake1 Size became 13, set it to 12
	JNZ UpdateStatus_INC1_Exit
	MOV Snake1Size,12
	RET
	
	UpdateStatus_INC1_GameSpeed:
	SUB GameSpeed,5
	CMP GameSpeed,5 ;;If 0, set it to 10
	JNZ UpdateStatus_INC1_Exit
	MOV GameSpeed,10
	
	UpdateStatus_INC1_Exit:
	RET
UpdateStatus_Increment1 ENDP

UpdateStatus_Increment2 PROC
	;Generate New Apple
	CALL GenerateApple
	
	INC NotAdjustedScore+1
	CMP NotAdjustedScore+1,30
	JNE UpdateStatus_INC2_Update
	CALL GuestWin
	RET
	
	UpdateStatus_INC2_Update:	
	CMP Game_Difficulty_Mode,0 ;;If 0, increase Snake2 Size
	JNZ UpdateStatus_INC2_GameSpeed
	INC Snake2Size
	CMP Snake2Size,13 ;;If Snake2 Size became 13, set it to 12
	JNZ UpdateStatus_INC2_Exit
	MOV Snake2Size,12
	RET
	
	UpdateStatus_INC2_GameSpeed:
	SUB GameSpeed,5
	CMP GameSpeed,5 ;;If 0, set it to 10
	JNZ UpdateStatus_INC2_Exit
	MOV GameSpeed,10
	
	UpdateStatus_INC2_Exit:
	RET
UpdateStatus_Increment2 ENDP

UpdateStatus_HeadsCollision PROC
	MOV ah,NotAdjustedScore
	MOV al,NotAdjustedScore+1
	CMP ah,al
	JA UpdateStatus_HC_LabelHW
	JB UpdateStatus_HC_LabelGW
	JE UpdateStatus_HC_LabelDS
	
	UpdateStatus_HC_LabelHW:
	CALL HostWin
	RET
	UpdateStatus_HC_LabelGW:
	CALL GuestWin
	RET
	UpdateStatus_HC_LabelDS:
	CALL DrawScore
	RET
UpdateStatus_HeadsCollision ENDP

Clear_Screen proc
	mov ax,0b800h
	mov es,ax
	mov di,0
	mov ax,0720h
	cld
	mov cx,24*80
	rep stosw
	ret
Clear_Screen endp

AdjustPositionScore proc
	;ASCII Adjustment: rows Position of Snake 1 to be printed
	mov al,byte ptr Snake1
	mov ah,00h
	mov bl,0Ah
	div bl
	add ah,30h
	add al,30h
	mov PositionHost+1,al
	mov PositionHost+2,ah
	
	;ASCII Adjustment: Columns Position of Snake 1 to be printed
	mov al, byte ptr Snake1+1
	mov ah,00h
	mov bl,0Ah
	div bl
	add ah,30h
	add al,30h
	mov PositionHost+4,al
	mov PositionHost+5,ah

	
	;ASCII Adjustment: rows Position of Snake 2 to be printed
	mov al,byte ptr Snake2
	mov ah,00h
	mov bl,0Ah
	div bl
	add ah,30h
	add al,30h
	mov PositionGuest+1,al
	mov PositionGuest+2,ah
	
	;ASCII Adjustment: Columns Position of Snake 1 to be printed
	mov al, byte ptr Snake2+1
	mov ah,00h
	mov bl,0Ah
	div bl
	add ah,30h
	add al,30h
	mov PositionGuest+4,al
	mov PositionGuest+5,ah
	
	;Adjust Snake1 Score
	mov al, NotAdjustedScore
	mov ah,00h
	mov bl,0Ah
	div bl
	add ah,30h
	add al,30h
	mov Score,al
	mov Score+1,ah
	
	;Adjust Snake2 Score
	mov al, NotAdjustedScore+1
	mov ah,00h
	mov bl,0Ah
	div bl
	add ah,30h
	add al,30h
	mov Score+3,al
	mov Score+4,ah
	
	ret
	
AdjustPositionScore endp


HostWin proc
	;Print the Host is The Winner
	Call Clear_Screen 
	Mov Bx,0	 
    MOV DH , 6  ; rows 
	MOV DL , 22 ; columns 
	LEA CX , Winner1
	CALL PrintMMText
	MOV Bx,0
	MOV DH , 7  ; rows 
	MOV DL , 22 ; columns
	 
	CMP HostGuestStatus,0
	JZ HostWin_IAmHost
	Lea CX , GMMUserName[2]
	JMP HostWin_Exit
	
	HostWin_IAmHost:
	Lea CX , MMUserName[2]
	 
	HostWin_Exit:
	CALL PrintMMText
	MOV Game_GameEndStatus,1
	Mov GameSpeed,250
	call LoopDelay
	Mov GameSpeed,250
	call LoopDelay
	RET
HostWin endp

GuestWin proc  
	;Print the Guest is The Winner 
	Call Clear_Screen 	
	Mov Bx,0
    MOV DH , 6  ; rows 
	MOV DL , 22 ; columns 
	Lea CX , Winner2
	CALL PrintMMText
	Mov Bx,0
	MOV DH , 7  ; rows 
	MOV DL , 22 ; columns 
	
	CMP HostGuestStatus,1
	JZ GuestWin_IAmGuest
	Lea CX , GMMUserName[2]
	JMP GuestWin_Exit
	
	GuestWin_IAmGuest:
	Lea CX , MMUserName[2]
	 
	GuestWin_Exit:
	CALL PrintMMText
	MOV Game_GameEndStatus,1
	Mov GameSpeed,250
	call LoopDelay
	Mov GameSpeed,250
	call LoopDelay
	RET
GuestWin endp

DrawScore proc  
	;Print No Winners
	Call Clear_Screen 	
    Mov Bx,0
    MOV DH , 6  ; rows 
	MOV DL , 22 ; columns 
	Lea CX , NoWinner
    CALL PrintMMText

	MOV Game_GameEndStatus,1
	Mov GameSpeed,250
	call LoopDelay
	Mov GameSpeed,250
	call LoopDelay
	RET
DrawScore endp


GetInput PROC
	
	Call Get_KeyBoard_Buffer 
	MOV AL , ScanCode_Sent
	
	MOV BL , HostGuestStatus ; To update the snake direction according to type of user 
	MOV BH , 0 
	MOV DL , Snake1Direction[BX]
	;MOV AL ,72 
	
	CMP AL , 75 ; SCAN CODE OF (Left arrow)
	JZ LEFT9   
	
	CMP AL , 72 ; SCAN CODE OF (Up arrow)
	JZ UP9
	
	CMP AL , 77 ; SCAN CODE OF (Right arrow)
	JZ RIGHT9 
	
	CMP AL , 80 ; SCAN CODE OF (Down arrow)
	JZ DOWN9 
	
	Cmp AL , 62 ;SCAN CODE OF (F4)
	JZ F49 
	
	JMP Exit9
	;;;;;;;;;;;;;;;;;;;;;
	; Next Block of code is to check the previous Direction to make sure the snake don't move in the opposite direction 
	;;;;;;;;;;;;;;;;;;;;;
	
	
	
	RIGHT9:
	CMP DL , 2 ; To check if the Previous Direction was Left Then it won't change the Direction 
	JZ Exit9
	MOV Snake1Direction[BX] , 0 ; if user BX is 0 -> host if bp is 1 -> Guest (it will change the direction)
	JMP Exit9

	
	UP9: 
	CMP DL , 3 ; To check if the Previous Direction was Down Then it won't change the Direction 
	JZ Exit9
	MOV Snake1Direction[BX] , 1 
	JMP Exit9 
	
	
	LEFT9: 
	CMP DL , 0 ; To check if the Previous Direction was right Then it won't change the Direction 
	JZ Exit9
	MOV Snake1Direction[BX] , 2 
	JMP Exit9 
	
	DOWN9:
	CMP DL , 1 ; To check if the Previous Direction was UP Then it won't change the Direction 
	JZ Exit9
	MOV Snake1Direction[BX] , 3 
	JMP Exit9 
	
	F49:
	MOV Game_GameEndStatus , 1
	Exit9: 
	
	;Return
	RET
GetInput ENDP

LoopDelay PROC 		; this Function takes input (GameSpeed)and perform a delay of .01s * GameSpeed 
	;Save All Registers
	;CALL SaveRegisters
	;Save Flags
	PUSHF
	MOV BL , GameSpeed
	MOV BH , 0 
	MOV AX , 2710H ; Time base = 10k us = .01s 
	MUL BX 
	MOV CX , DX 	;most seg bits  ===
	MOV DX , AX 	;least seg bits === total delay time in Micro Seconds 
	MOV AH , 86H 
	INT 15H
	
	;Return Flags
	POPF
	;Load All Registers
	;CALL LoadRegisters
	;Return
	RET
LoopDelay ENDP

DetectCollision PROC
	
	;This is called every game frame to check for the collisions
	;This is called on both PCs regardless of who is the host/guest
	;Return 
	;0->No Collision
	;1->Snake1 head with Apple
	;2->Snake2 head with Apple
	;3->Snake1 & Snake2 heads
	;4->Snake1 head & Snake2 body
	;5->Snake1 head & map
	;6->Snake1 head * Snake1 body
	;7->Snake2 head & Snake1 body
	;8->Snake2 head & map
	;9->Snake2 head * Snake2 body
	
	;I. Check collision of Snake1 & Snake2 with Apple
	;;1- Check collision of Snake1 head with Apple
	MOV AX,AppleLocation
	CMP Snake1,AX
	JNZ DetectCollision_Step2
	MOV DetectCollision_CollisionType,1
	JMP DetectCollision_Exit
	
	;;2- Check collision of Snake2 head with Apple
	DetectCollision_Step2:
	MOV AX,AppleLocation
	CMP Snake2,AX
	JNZ DetectCollision_Step3
	MOV DetectCollision_CollisionType,2
	JMP DetectCollision_Exit
	
	;II. Check Collision of Snake1 & Snake2 heads
	DetectCollision_Step3:
	MOV AX,Snake1
	MOV BX,Snake2
	CMP AX,BX
	JNZ DetectCollision_Step3_2
	MOV DetectCollision_CollisionType,3
	JMP DetectCollision_Exit
	;;There is a case where both heads exchange places
	;;We have to consider it as Collision of heads
	;;It happens if Head1 position = Snake2 second node position
	;;AND
	;;Head2 position = Snake1 second node position
	DetectCollision_Step3_2:
	CMP AX,Snake2+2 ;;CMP Head1 with Snake2 Second Node
	JNZ DetectCollision_Step4
	CMP BX,Snake1+2
	JNZ DetectCollision_Step4
	MOV DetectCollision_CollisionType,3
	JMP DetectCollision_Exit
	
	;III. Check Collisions of Snake1
	;;1- Check collision of Snake1 head with Snake 2 body
	DetectCollision_Step4:
	MOV BP,2
	MOV CH,00h
	MOV CL,Snake2Size
	DEC CL
	DetectCollision_Step4_Loop:
	MOV AX,Snake2[BP]
	MOV BX,Snake1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;For debugging only
	CMP Snake1,AX ;If 0->they collided, else no collision
	JNZ DetectCollision_Step4_NoCollision
	;;;If we arrive here it means this type of collision happened
	MOV DetectCollision_CollisionType,4
	JMP DetectCollision_Exit
	DetectCollision_Step4_NoCollision:
	ADD BP,2
	LOOP DetectCollision_Step4_Loop
	
	
	;;2- Check collision of Snake1 head with map
	DetectCollision_Step5:
	MOV BX,Snake1 ;X-Coordinate at BH, Y-Coordinate at BL
	MOV AL,BL
	MOV CL,80
	MUL CL ;Now AX has 80*row,, we have to add the column
	MOV DX,AX; Now DX has 80*row
	MOV AH,00h
	MOV AL,BH
	ADD AX,DX
	MOV SI,AX
	CMP Map[SI],'1'
	JNZ DetectCollision_Step6
	MOV DetectCollision_CollisionType,5
	JMP DetectCollision_Exit
	
	;;3- Check collision of Snake1 head with Snake 1 body
	DetectCollision_Step6:
	MOV BP,2
	MOV CH,00h
	MOV CL,Snake1Size
	DEC CL
	DetectCollision_Step6_Loop:
	MOV AX,Snake1[BP]
	CMP Snake1,AX ;If 0->they collided, else no collision
	JNZ DetectCollision_Step6_NoCollision
	;;;If we arrive here it means this type of collision happened
	MOV DetectCollision_CollisionType,6
	JMP DetectCollision_Exit
	DetectCollision_Step6_NoCollision:
	ADD BP,2
	LOOP DetectCollision_Step6_Loop
	
	;IV. Check Collisions of Snake2
	;;1- Check collision of Snake2 head with Snake1 body
	DetectCollision_Step7:
	MOV BP,2
	MOV CH,00h
	MOV CL,Snake1Size
	DEC CL
	DetectCollision_Step7_Loop:
	MOV AX,Snake1[BP]
	CMP Snake2,AX ;If 0->they collided, else no collision
	JNZ DetectCollision_Step7_NoCollision
	;;;If we arrive here it means this type of collision happened
	MOV DetectCollision_CollisionType,7
	JMP DetectCollision_Exit
	DetectCollision_Step7_NoCollision:
	ADD BP,2
	LOOP DetectCollision_Step7_Loop
	;;2- Check collision of Snake2 head with map
	DetectCollision_Step8:
	MOV BX,Snake2 ;X-Coordinate at BH, Y-Coordinate at BL
	MOV AL,BL
	MOV CL,80
	MUL CL ;Now AX has 80*row,, we have to add the column
	MOV DX,AX; Now DX has 80*row
	MOV AH,0
	MOV AL,BH
	ADD AX,DX
	MOV SI,AX
	CMP Map[SI],'1'
	JNZ DetectCollision_Step9
	MOV DetectCollision_CollisionType,8
	JMP DetectCollision_Exit
	;;3- Check collision of Snake 2 head with Snake2 body
	DetectCollision_Step9:
	MOV BP,2
	MOV CH,00h
	MOV CL,Snake2Size
	DEC CL
	DetectCollision_Step9_Loop:
	MOV AX,Snake2[BP]
	CMP Snake2,AX ;If 0->they collided, else no collision
	JNZ DetectCollision_Step9_NoCollision
	;;;If we arrive here it means this type of collision happened
	MOV DetectCollision_CollisionType,9
	JMP DetectCollision_Exit
	DetectCollision_Step9_NoCollision:
	ADD BP,2
	LOOP DetectCollision_Step9_Loop
	
	;V. No Collision happened
	MOV DetectCollision_CollisionType,0
	
	DetectCollision_Exit:
	;Return
	RET
DetectCollision ENDP

DetectAppleCollision PROC
	MOV DetectAppleCollision_CollisionType,0
	MOV BX,AppleLocation ;X-Coordinate at BH, Y-Coordinate at BL
	MOV AL,BL
	MOV CL,80
	MUL CL ;Now AX has 80*row,, we have to add the column
	MOV DX,AX; Now DX has 80*row
	MOV AH,00h
	MOV AL,BH
	ADD AX,DX
	MOV SI,AX
	CMP Map[SI],'1'
	JNZ DetectAppleCollision_Snake1
	MOV DetectAppleCollision_CollisionType,1
	RET
	
	 DetectAppleCollision_Snake1:
	 MOV BX,AppleLocation ;Apple Location is in BX
	 MOV BP,0
   	 
	 DetectAppleCollision_Snake1_Loop:
	 CMP Snake1[BP],BX
	 JNZ DetectAppleCollision_Snake1_NoCollision ;;If no collision jump to next node in the snake
	 MOV DetectAppleCollision_CollisionType,1
	 RET
	
	 DetectAppleCollision_Snake1_NoCollision:
	 ADD BP,2
	 CMP BP,24
	 JNZ DetectAppleCollision_Snake1_Loop
	
	
	 DetectAppleCollision_Snake2:
	 MOV BX,AppleLocation ;Apple Location is in BX
	 MOV BP,0
   	 
	 DetectAppleCollision_Snake2_Loop:
	 CMP Snake2[BP],BX
	 JNZ DetectAppleCollision_Snake2_NoCollision ;;If no collision jump to next node in the snake
	 MOV DetectAppleCollision_CollisionType,1
	 RET
	
	 DetectAppleCollision_Snake2_NoCollision:
	 ADD BP,2
	 CMP BP,24
	 JNZ DetectAppleCollision_Snake2_Loop
	
	RET
DetectAppleCollision ENDP

UpdateSnakes PROC
	
	
	;This is called every game frame to update Snake1 & Snake2 positions
	;This is called on both PCs regardless of who is the host/guest
	
	;I. Update Snake1
	;1- Update the remaining Snake1 body from tail to second node, each node takes position of the other node
	MOV BP,20
	MOV CX,11
	UpdateSnakes_Step1_Loop:
	MOV AX,Snake1[BP]
	MOV Snake1[BP+2],AX
	SUB BP,2
	LOOP UpdateSnakes_Step1_Loop
	
	;2- Update Snake1 head according to Snake1 direction "0->Right, 1->Up, 2->Left, 3->Down"
	MOV AX,Snake1 ;Move Snake1 head location to AX, modify AX according to direction, then Move AX to Snake1
	;;Right
	CMP Snake1Direction,0
	JNZ UpdateSnakes_Step1_Up ;Not Right Direction, test for next Direction
	INC AH
	CMP AH,80
	MOV Snake1,AX
	JNZ UpdateSnakes_Step2 ;If no, Finish Updating Snake1
	MOV AH,0
	MOV Snake1,AX
	JMP UpdateSnakes_Step2 ;Finish Updating Snake1
	;;Up
	UpdateSnakes_Step1_Up:
	CMP Snake1Direction,1
	JNZ UpdateSnakes_Step1_Left ;Not Up Direction, test for next Direction
	CMP AL,0 ;Test if at first row
	JNZ UpdateSnakes_Step1_Up_NotFirstRow ;If not at first Row
	MOV AL,21
	MOV Snake1,AX
	JMP UpdateSnakes_Step2 ;Finish Updating Snake1
	UpdateSnakes_Step1_Up_NotFirstRow:
	DEC AL
	MOV Snake1,AX
	JMP UpdateSnakes_Step2 ;Finish Updating Snake1
	
	;;Left
	UpdateSnakes_Step1_Left:
	CMP Snake1Direction,2
	JNZ UpdateSnakes_Step1_Down ;Not Up Direction, test for next Direction
	CMP AH,0 ;Test if at first row
	JNZ UpdateSnakes_Step1_Up_NotFirstCol ;If not at first Col
	MOV AH,79
	MOV Snake1,AX
	JMP UpdateSnakes_Step2 ;Finish Updating Snake1
	UpdateSnakes_Step1_Up_NotFirstCol:
	DEC AH
	MOV Snake1,AX
	JMP UpdateSnakes_Step2 ;Finish Updating Snake1
	
	;;Down
	UpdateSnakes_Step1_Down:
	INC AL
	CMP AL,22
	MOV Snake1,AX
	JNZ UpdateSnakes_Step2 ;If no, Finish Updating Snake1
	MOV AL,0
	MOV Snake1,AX ;End of Updating Snake1
	
	
	UpdateSnakes_Step2:
	;II. Update Snake2
	;1- Update the remaining Snake2 body from tail to second node, each node takes position of the other node
	MOV BP,20
	MOV CX,11
	UpdateSnakes_Step2_Loop:
	MOV AX,Snake2[BP]
	MOV Snake2[BP+2],AX
	SUB BP,2
	LOOP UpdateSnakes_Step2_Loop
	;2- Update Snake2 head according to Snake2 direction
	MOV AX,Snake2 ;Move Snake2 head location to AX, modify AX according to direction, then Move AX to Snake2
	;;Right
	CMP Snake2Direction,0
	JNZ UpdateSnakes_Step2_Up ;Not Right Direction, test for next Direction
	INC AH
	CMP AH,80
	MOV Snake2,AX
	JNZ UpdateSnakes_Exit ;If no, Finish Updating Snake2
	MOV AH,0
	MOV Snake2,AX
	JMP UpdateSnakes_Exit ;Finish Updating Snake2
	;;Up
	UpdateSnakes_Step2_Up:
	CMP Snake2Direction,1
	JNZ UpdateSnakes_Step2_Left ;Not Up Direction, test for next Direction
	CMP AL,0 ;Test if at first row
	JNZ UpdateSnakes_Step2_Up_NotFirstRow ;If not at first Row
	MOV AL,21
	MOV Snake2,AX
	JMP UpdateSnakes_Exit ;Finish Updating Snake2
	UpdateSnakes_Step2_Up_NotFirstRow:
	DEC AL
	MOV Snake2,AX
	JMP UpdateSnakes_Exit ;Finish Updating Snake2
	
	;;Left
	UpdateSnakes_Step2_Left:
	CMP Snake2Direction,2
	JNZ UpdateSnakes_Step2_Down ;Not Up Direction, test for next Direction
	CMP AH,0 ;Test if at first row
	JNZ UpdateSnakes_Step2_Up_NotFirstCol ;If not at first Col
	MOV AH,79
	MOV Snake2,AX
	JMP UpdateSnakes_Exit ;Finish Updating Snake2
	UpdateSnakes_Step2_Up_NotFirstCol:
	DEC AH
	MOV Snake2,AX
	JMP UpdateSnakes_Exit ;Finish Updating Snake2
	
	;;Down
	UpdateSnakes_Step2_Down:
	INC AL
	CMP AL,22
	MOV Snake2,AX
	JNZ UpdateSnakes_Exit ;If no, Finish Updating Snake2
	MOV AL,0
	MOV Snake2,AX ;End of Updating Snake2
	
	
	UpdateSnakes_Exit:
	;Return
	RET
UpdateSnakes ENDP

ClearSnakesApple Proc ;;;;;Clear snakes tales and apples of the previous time cycle

	;get the tale position of Snake1 in the screen and move it in AL
	mov al,Snake1Size
	dec al
	mov ah,0
	mov dl,2
	mul dl
	lea bx,Snake1
	mov cl,al ;recovering the pointer in the array of Snake1
	XLAT ; now we got the Y position 
	xchg cl,al 
	inc al ; increment to X position
	lea bx,Snake1
	XLAT ; now we got the X position
	
	mov dl,al
	mov dh,cl
	;dx = X position & Y position
	
	;multiply Y by 160 
	mov al,dh
	mov ah,0
	mov cl,160
	mul cl
	mov bx,ax
    
	;multiply X by 2
	mov al,dl
	mov ah,0
	mov cl,2
	mul cl
	add bx,ax
	
	mov di,bx
	mov ax,0720h
	stosw
	
	
	;get the tale position of Snake2 in the screen and move it in AL
	mov al,Snake2Size
	dec al
	mov ah,0
	mov dl,2
	mul dl
	lea bx,Snake2
	mov cl,al ;recovering the pointer in the array of Snake1
	XLAT ; now we got the Y position 
	xchg cl,al 
	inc al ; increment to X position
	lea bx,Snake2
	XLAT ; now we got the X position
	
	mov dl,al
	mov dh,cl
	;dx = X position & Y position
	
	;multiply Y by 160 
	mov al,dh
	mov ah,0
	mov cl,160
	mul cl
	mov bx,ax
    
	;multiply X by 2
	mov al,dl
	mov ah,0
	mov cl,2
	mul cl
	add bx,ax
	
	mov di,bx
	mov ax,0720h
	stosw
	
	;clearing apple location
	mov al, byte ptr [AppleLocation]
	mov ah,0
	mov cl,160
	mul cl
	mov bx,ax
	
	mov al,byte ptr [AppleLocation+1]
	mov ah,0
	mov cl,2
	mul cl
	add bx,ax
	
	mov di,bx
	mov ax,0720h
	stosw
	
	RET
ClearSnakesApple endp


GameHeaderHost PROC
	;This is called only if I am the Host
	;The Host is always considered Snake1 on both PCs
	;The Guest is always considered Snake2 on both PCs
	
	;1- Send the Apple's Position
	MOV AL,byte ptr AppleLocation[0]
	MOV CharacterSend,AL
	CALL SendData
	;;Wait for confirmation
	CALL GameHeader_WaitConfirmation
	
	
	MOV AL,byte ptr AppleLocation[1]
	MOV CharacterSend,AL
	CALL SendData
	;;Wait for confirmation
	CALL GameHeader_WaitConfirmation
	
	
	
	;3- Receive Snake2's Direction
	GameHeaderHost_Receive1:
	CALL ReceiveData
	CMP CharacterReceived,0FFh
	JZ GameHeaderHost_Receive1
	MOV AL,CharacterReceived
	MOV Snake2Direction,AL
	
	MOV AL,'a'
	MOV CharacterSend,AL
	CALL SendData
	
	
	;2- Send Snake1's Direction
	 MOV AL,Snake1Direction
	 MOV CharacterSend,AL
	 CALL SendData
	;Wait for confirmation
	 CALL GameHeader_WaitConfirmation
	 
	;4- Send Game End or not to Guest
	 MOV AL,Game_GameEndStatus
	 MOV CharacterSend,AL
	 CALL SendData
	;Wait for confirmation
	 CALL GameHeader_WaitConfirmation
	
	;5- Receive Game_GameEndStatus from Guest
	 GameHeaderHost_Receive2:
	 CALL ReceiveData
	 CMP CharacterReceived,0FFh
	 JZ GameHeaderHost_Receive2
	 MOV AL,CharacterReceived
	 PUSH AX
	 
	 MOV AL,'a'
	 MOV CharacterSend,AL
	 CALL SendData
	
	 POP AX
	 OR Game_GameEndStatus,AL
	
	;Return
	RET
GameHeaderHost ENDP

GameHeaderGuest PROC
	;This is called only if I am the Guest
	;The Host is always considered Snake1 on both PCs
	;The Guest is always considered Snake2 on both PCs
	
	;1- Receive the Apple's Position
	GameHeaderGuest_Receive1:
	CALL ReceiveData
	CMP CharacterReceived,0FFh
	JZ GameHeaderGuest_Receive1
	MOV AL,CharacterReceived
	MOV byte ptr AppleLocation[0],AL
	
	MOV AL,'a'
	MOV CharacterSend,AL
	CALL SendData
	
	
	GameHeaderGuest_Receive2:
	CALL ReceiveData
	CMP CharacterReceived,0FFh
	JZ GameHeaderGuest_Receive2
	MOV AL,CharacterReceived
	MOV byte ptr AppleLocation[1],AL
	
	MOV AL,'a'
	MOV CharacterSend,AL
	CALL SendData
	
	
	;3- Send Snake2's Direction
	MOV AL,Snake2Direction
	MOV CharacterSend,AL
	CALL SendData
	CALL GameHeader_WaitConfirmation
	
	
	;2- Receive Snake1's Direction
	 GameHeaderGuest_Receive3:
	 CALL ReceiveData
	 CMP CharacterReceived,0FFh
	 JZ GameHeaderGuest_Receive3
	 MOV AL,CharacterReceived
	 MOV Snake1Direction,AL
	
	 MOV AL,'a'
	 MOV CharacterSend,AL
	 CALL SendData
	
	
	;4- Receive Game_GameEndStatus from Host
	 GameHeaderGuest_Receive4:
	 CALL ReceiveData
	 CMP CharacterReceived,0FFh
	 JZ GameHeaderGuest_Receive4
	 MOV AL,CharacterReceived
	 PUSH AX
	
	 MOV AL,'a'
	 MOV CharacterSend,AL
	 CALL SendData
	
	;5- Send Game End or not to Host
	 MOV AL,Game_GameEndStatus
	 MOV CharacterSend,AL
	 CALL SendData
	;Wait for confirmation
	 CALL GameHeader_WaitConfirmation
	
	 POP AX
	 OR Game_GameEndStatus,AL
	
	;Return
	RET
GameHeaderGuest ENDP

GameHeader_WaitConfirmation PROC
	GameHeader_Confirm:
	CALL ReceiveData
	CMP CharacterReceived,'a'
	JNZ GameHeader_Confirm
	RET
GameHeader_WaitConfirmation ENDP


UpdateStatusDirection1 proc

	mov ax,ds
	mov es,ax
	cmp Snake1Direction,0
	JE UpdateStatusDirection1_Change1Right
	cmp Snake1Direction,1
	JE UpdateStatusDirection1_Change1Up
	cmp Snake1Direction,2
	JE UpdateStatusDirection1_Change1Left
	cmp Snake1Direction,3
	JE UpdateStatusDirection1_Change1Down
		
	UpdateStatusDirection1_Change1Right:
		lea di,ScoreTemplate2+5
		lea si,RightTemplate
		mov cx,5
		rep movsb
		jmp UpdateStatusDirection1_EndFunction
		
	UpdateStatusDirection1_Change1Up:
		lea di,ScoreTemplate2+5
		lea si,UpTemplate
		mov cx,5
		rep movsb
		jmp UpdateStatusDirection1_EndFunction
	
	UpdateStatusDirection1_Change1Left:
		lea di,ScoreTemplate2+5
		lea si,LeftTemplate
		mov cx,5
		rep movsb
		jmp UpdateStatusDirection1_EndFunction
		
	UpdateStatusDirection1_Change1Down:
		lea di,ScoreTemplate2+5
		lea si,DownTemplate
		mov cx,5
		rep movsb
		jmp UpdateStatusDirection1_EndFunction
		
	UpdateStatusDirection1_EndFunction:
	ret
		
UpdateStatusDirection1 endp


UpdateStatusDirection2 proc
	
	mov ax,ds
	mov es,ax
	cmp Snake2Direction,0
	JE UpdateStatusDirection2_Change2Right
	cmp Snake2Direction,1
	JE UpdateStatusDirection2_Change2Up
	cmp Snake2Direction,2
	JE UpdateStatusDirection2_Change2Left
	cmp Snake2Direction,3
	JE UpdateStatusDirection2_Change2Down
	
			
	UpdateStatusDirection2_Change2Right:
		lea di,ScoreTemplate3+6
		lea si,RightTemplate
		mov cx,5
		rep movsb
		jmp UpdateStatusDirection2_EndFunction
	
	UpdateStatusDirection2_Change2Up:
		lea di,ScoreTemplate3+6
		lea si, UpTemplate
		mov cx,5
		rep movsb
		jmp UpdateStatusDirection2_EndFunction
	
	UpdateStatusDirection2_Change2Left:
		lea di,ScoreTemplate3+6
		lea si,LeftTemplate
		mov cx,5
		rep movsb
		jmp UpdateStatusDirection2_EndFunction
	
	UpdateStatusDirection2_Change2Down:
		lea di,ScoreTemplate3+6
		lea si,DownTemplate
		mov cx,5
		rep movsb
		
		
	UpdateStatusDirection2_EndFunction:
	ret
		
UpdateStatusDirection2 endp


UpdateStatusDirection proc
	
	CALL UpdateStatusDirection1
	CALL UpdateStatusDirection2
	ret
UpdateStatusDirection endp

Get_KeyBoard_Buffer proc ; return the character in AL , ZF = 0 ; if no character return al = 0 
	mov ah , 1 
	int 16h 
	jz Empty 
	mov ah,0
	int 16h
	mov ScanCode_Sent, ah
	jmp Exit3
	Empty :
	mov al , 0
	mov ScanCode_Sent , al
	Exit3:
	ret 
Get_KeyBoard_Buffer endp

DrawMap proc
	;initial location
	CALL Clear_Screen
	CALL UpdateCursor
	CALL PrintIsolateLine
	; mov dl,LeaveDrawing
	; add dl,30h
	; mov ah,2h
	; int 21h
	; mov ah,01h
	; int 21h
	cmp LeaveDrawing,0
	JNE DrawMap_CALLPendingMap
	jmp DrawMap_ContinueDrawing
	DrawMap_CALLPendingMap:
	mov LeaveDrawing,0
	lea si,PendingMap
	CALL PrintMap
	DrawMap_ContinueDrawing:
		CALL GetInputToDraw
		Call CheckReceivedInvitations
		cmp LeaveDrawing,0
		JZ DrawMap_ContinueDrawing
		CMP LeaveDrawing,1
		JE DrawMap_Label
		jmp DrawMap_EndFunction
		
		DrawMap_Label:
		; push es
		mov LeaveDrawing,0
		MOV CursorLocation,0A28h
		; mov ax,ds
		; mov es,ax
		; lea di,PendingMap
		; mov ax,0030h
		; mov cx,22*80
		; rep stosb
		; pop es
	DrawMap_EndFunction:
		RET
DrawMap endp



CheckReceivedInvitations proc 

	CALL ReceiveWithConfermation
	 CMP CharacterReceived , 0FFH 
	 JZ ExitCheckInvitation
	
	 ;IF it received something
	MOV AL , CharacterReceived
	
	CMP AL , 59 ; SCAN CODE OF (F1)
	 JZ F1Received1    
	
	 CMP AL , 60 ; SCAN CODE OF (F2)
	 JZ F2Received1
	
	
	 JMP ExitCheckInvitation
	 
	F1Received1:
			lea si,PendingMap
			CALL SaveMap
			Call ReceiveChatInvitaion
			Jmp ExitCheckInvitation 
	F2Received1:
			lea si,PendingMap
			Call SaveMap
			Call ReceiveGameInvitaion 
			jmp ExitCheckInvitation 
	ExitCheckInvitation: 
 RET 
CheckReceivedInvitations endp 

PaintOrUnPaint PROC

	;Check if already the user wants to paint so he we need to check if he exceeded the number of barriers or not:
	cmp PaintingOption,4420h
	JE PaintOrUnPaint_CheckNumberOfBarriers
	jmp PaintOrUnPaint_ContinueToDraw
	PaintOrUnPaint_CheckNumberOfBarriers:
		cmp NumberofBarriers,99
		JAE PaintOrUnPaint_ExitFunctionPaint
			
		
	PaintOrUnPaint_ContinueToDraw:
		;Getting CursorLocation rows
		Mov al, byte ptr CursorLocation[1]
		mov ah, 0
		mov bl,160
		mul bl
		Mov dx,ax
		
		;Getting CursorLocation columns
		Mov al, byte ptr CursorLocation[0]
		mov ah, 0
		mov bl,2
		mul bl
		
		;Getting the offset by adding y*160+x*2
		Add Ax, DX
			
		;Moving Ds to The VRAMs
		mov Dx,0b800h
		mov ES,Dx 
			
		;Moving offset to Di
		mov di, Ax
		
		;Printing barrier with red colour	
		Mov Ax, PaintingOption
		cld
		Stosw		
		
	PaintOrUnPaint_ExitFunctionPaint:
	RET
PaintorUnPaint ENDP

PrintCursor PROC

	push ax
	;Getting CursorLocation rows
	mov cx, CursorLocation
	dec cl
	dec ch
	Mov al, cl
	mov ah, 0
	mov bl,160
	mul bl
	Mov dx,ax
	
	;Getting CursorLocation columns
	Mov al, ch
	mov ah, 0
	mov bl,2
	mul bl
	
	;Getting the offset by adding y*160+x*2
	Add Ax, DX
		
	;Moving Ds to The VRAM
	    mov Dx,0b800h
		mov ES,Dx 
		
    ;Moving offset to Di
		mov di, Ax
		
	;Printing barrier with red colour	
		mov Ax,0f020h
		cld
		Stosw

	pop ax
	RET
PrintCursor ENDP

GetInputToDraw PROC
	
	CALL CountBarriers
	
	;Printing if the user in the painting mode or in the delete mode 
	CALL PrintExceedMessage
	mov dh,23 ;rows
	mov dl,70;columns
	lea cx,Paintingmode
	CALL PrintMMText
	CALL Get_KeyBoard_Buffer
	
	CMP ScanCode_Sent,0 ; no input
	JZ GetInputToDraw_ExitToExitDrawing
	
	CMP ScanCode_Sent , 75 ; SCAN CODE OF (Left arrow)
	JZ GetInputToDraw_LEFTCursor   
	
	CMP ScanCode_Sent , 72 ; SCAN CODE OF (Up arrow)
	JZ GetInputToDraw_UPCursor
	
	CMP ScanCode_Sent , 77 ; SCAN CODE OF (Right arrow)
	JZ GetInputToDraw_RIGHTCursor
	
	CMP ScanCode_Sent , 80 ; SCAN CODE OF (Down arrow)
	JZ GetInputToDraw_DOWNCursor

	CMP ScanCode_Sent, 25  ; SCAN CODE OF P
	JZ GetInputToDraw_PaintMode
	
	CMP ScanCode_Sent, 32   ; SCAN CODE OF D
	JZ GetInputToDraw_UnPaintMode
	
	CMP ScanCode_Sent, 38 ;SCAN CODE OF L
	JZ GetInputToDraw_LabelLoadingMap
	
	CMP ScanCode_Sent, 31 ;SCAN CODE OF S
	JZ GetInputToDraw_LabelSavingMap
	
	CMP ScanCode_Sent , 3Ch ;SCAN CODE OF (F12)
	JZ GetInputToDraw_ExitDrawing2
	
	GetInputToDraw_ExitToExitDrawing:
		jmp GetInputToDraw_ExitDrawing
	
	GetInputToDraw_RIGHTCursor:
		CALL PaintOrUnPaint
		inc byte ptr CursorLocation
		jmp GetInputToDraw_ExitDrawing
	
	GetInputToDraw_UPCursor:
		CALL PaintOrUnPaint
		dec byte ptr CursorLocation+1
		jmp GetInputToDraw_ExitDrawing

	GetInputToDraw_LEFTCursor:
		CALL PaintOrUnPaint
		dec byte ptr CursorLocation
		jmp GetInputToDraw_ExitDrawing
	
	GetInputToDraw_DOWNCursor:
		CALL PaintOrUnPaint
		inc byte ptr CursorLocation+1
		jmp GetInputToDraw_ExitDrawing
		
	GetInputToDraw_PaintMode:
		mov PaintingOption,4420h
		mov PaintingMode, 'P'
		jmp GetInputToDraw_ExitDrawing
	GetInputToDraw_UnPaintMode:
		mov PaintingOption,0720h
		mov PaintingMode, 'D'
		jmp GetInputToDraw_ExitDrawing
		
	GetInputToDraw_LabelLoadingMap:
		CALL LoadingMap
		jmp GetInputToDraw_ExitDrawing
		
	GetInputToDraw_LabelSavingMap:
		CALL SavingMap
		jmp GetInputToDraw_ExitDrawing
		
	GetInputToDraw_ExitDrawing2:
		mov LeaveDrawing,1
	GetInputToDraw_ExitDrawing:
		CALL UpdateCursor
	
	;Return
	RET
GetInputToDraw ENDP


LoadingMap proc
	lea si,Map
	CALL SaveMap
	CALL SelectMapLoad
	CALL StoreMap
	lea si,Map
	CALL PrintMap
	CALL PrintIsolateLine
	ret
LoadingMap endp


SavingMap proc	
	lea si,Map
	CALL SaveMap
	CALL SelectMapSave
	CALL SaveMapToFile
	lea si,Map
	CALL PrintMap
	CALL PrintIsolateLine
	ret
SavingMap endp


UpdateCursor proc

	;check if the Cursor is exceeded the boundaries of the screen
	cmp byte ptr CursorLocation+1,0ffh
	JE UpdateCursor_RowTo21
	cmp byte ptr CursorLocation+1,21
	JA UpdateCursor_RowToZero
	cmp byte ptr CursorLocation,0ffh
	JE UpdateCursor_ColumnTo79
	cmp byte ptr CursorLocation,79
	JA UpdateCursor_ColummnToZero
	
	
	jmp UpdateCursor_UpdateCursorlabel
	UpdateCursor_RowToZero:
		mov byte ptr CursorLocation+1,0
		jmp UpdateCursor_UpdateCursorlabel
	UpdateCursor_RowTo21:
		mov byte ptr CursorLocation+1,15h
		jmp UpdateCursor_UpdateCursorlabel
	UpdateCursor_ColummnToZero:
		mov byte ptr CursorLocation,0
		jmp UpdateCursor_UpdateCursorlabel
	UpdateCursor_ColumnTo79:
		mov byte ptr CursorLocation,4fh
		
	UpdateCursor_UpdateCursorlabel:
		mov dx,CursorLocation
		mov bx,0
		mov ah,02h
		int 10h
		;CALL PrintCursor
	ret
UpdateCursor endp

SaveMapToFile proc 

;CREATE A FILE WITH HANDLE

Mov Cx,0
LEA Dx, MapNameSave
Mov Ah, 3Ch
int 21h


;TO OPEN THE FILE	
mov ah, 3Dh
;READ WRITE FILE
mov al, 2
LEA Dx, MapNameSave	
int 21h
Mov HandleSave,Ax


;WRITE TO FILE WITH HANDLE
Mov Bx, HandleSave
Mov Cx, 22*80
LEA Dx, Map
mov ah,40h
int 21h


;CLOSE A FILE WITH HANDLE
Mov Bx, HandleSave
Mov Ah, 3Eh
int 21h



Ret
SaveMapToFile ENDP


SaveMap PROC

	mov ax,0b800h
	mov es,ax
	mov al,20h
	;lea si,Map
	mov di,1
	mov cx,22*80
	SaveMap_ForLoopWrite:
		mov dl, byte ptr es:[di]
		cmp dl,07h  ; it's a gap  so write 0
		JE SaveMap_WriteChar0
		mov byte ptr ds: [si],31h  ;it's a barrier so write 1
		jmp SaveMap_EndLoopWrite
		SaveMap_WriteChar0:
			mov byte ptr ds:[si],30h
		SaveMap_EndLoopWrite:
			inc si
			add di,2
	loop SaveMap_ForLoopWrite
	

	;Return
	RET
SaveMap ENDP


CountBarriers proc
	mov bl,0
	push ds
	mov ax,0b800h
	mov ds,ax
	mov di,1
	mov cx,22*80
	CountBarriers_ForLoopCheck:
		mov dl, byte ptr [di]
		cmp dl,07h
		JNE CountBarriers_IncrementBarriers
		jmp CountBarriers_EndLoopCheck
		CountBarriers_IncrementBarriers:
			inc bx
		CountBarriers_EndLoopCheck:
			add di,2
	loop CountBarriers_ForLoopCheck
			
	;Return
	
	pop ds
	mov NumberofBarriers,bl
	RET
CountBarriers endp



PrintIsolateLine proc
; print isolate line
	mov ax,0b800h
	mov es,ax
	mov ax,072Dh
	mov di,22*160
	mov cx,80
	cld
	rep stosw
	ret

PrintIsolateLine endp

PrintingBarriersN proc  

	MOV AL , NumberofBarriers
	MOV AH , 0 
	MOV BL , 10 
	DIV BL 
	ADD AL , 30H 
	ADD AH , 30h 
	MOV AsciiNumofBarriers , AL
	MOV AsciiNumofBarriers+1 , AH  
	mov dh,23 ;rows
	mov dl,0 ;columns
	mov bx,0
	lea cx,BarrierNumbersMessage
	CALL PrintMMText

RET 
PrintingBarriersN endp 

PrintExceedMessage proc
	CALL PrintingBarriersN
	CMP NumberofBarriers,99
	JE PrintExceedMessage_Exceeded
	; Remove Exceed Message
		mov ax,0b800h
		mov es,ax
		mov ax,0720h
		mov di,23*160+23*2
		mov cx,9
		cld
		rep stosw
		jmp PrintExceedMessage_ExitFunction
		
	PrintExceedMessage_Exceeded:
	    mov dh,23 ;rows
		mov dl, 23 ;columns
		lea cx,ExceedMessage
		CALL PrintMMText
		
	PrintExceedMessage_ExitFunction:
	ret
PrintExceedMessage endp


SelectMapLoad Proc 

Call Clear_Screen 	
	JMP SelectMapLoad_SelectMapNOError9
	
	SelectMapLoad_SelectMapError9:
	Call Clear_Screen 	

	;Print Error Msg 
	MOV DH , 6 
	MOV DL , 22 
	Lea CX , SelectMapERRORMsg
	CALL PrintMMText
	
	SelectMapLoad_SelectMapNOError9:
	;next code is to print select map message  
	MOV DH , 8 
	MOV DL , 22 
	Lea CX , LoadMessage
	CALL PrintMMText
	
	;MOVE  the cursor to the  next line 
	MOV DL , 28 
	MOV DH , 9 
	MOV AH , 2 
	INT 10H 
	;take Number of Map  
	MOV AH , 1 
	INT 21H 
	
	
	; to check that the user entered a correct number 
	;Smaller than 1 
	CMP al , 31H 
	JB SelectMapLoad_SelectMapError9
	;Greater than 3 
	CMP al , 33H
	JA SelectMapLoad_SelectMapError9
	
	MOV MapName , AL 
	SelectMapLoad_SelectMapExit:
	
Ret 

SelectMapLoad endp

SelectMapSave Proc 

Call Clear_Screen 	
	JMP SelectMapSave_SelectMapNOError9
	
	SelectMapSave_SelectMapError9:
	Call Clear_Screen 	

	;Print Error Msg 
	MOV DH , 6 
	MOV DL , 22 
	Lea CX , SelectMapERRORMsg
	CALL PrintMMText
	
	SelectMapSave_SelectMapNOError9:
	;next code is to print select map message  
	MOV DH , 8 
	MOV DL , 22 
	Lea CX , SaveMessage
	CALL PrintMMText
	
	;MOVE  the cursor to the  next line 
	MOV DL , 28 
	MOV DH , 9 
	MOV AH , 2 
	INT 10H 
	;take Number of Map  
	MOV AH , 1 
	INT 21H 
	
	
	; to check that the user entered a correct number 
	;Smaller than 1 
	CMP al , 31H 
	JB SelectMapSave_SelectMapError9
	;Greater than 3 
	CMP al , 33H
	JA SelectMapSave_SelectMapError9
	
	MOV MapNameSave , AL 
	SelectMapSave_SelectMapExit:
	
Ret 

SelectMapSave endp

Code_segment_name ends
end Main_prog
