;main comment block here 
;  Comment block below must be filled out completely for each assignment
;  ************************************************************* 
;  Student Name: Nathaniel Lee
;  COMSC-260 Spring 2020
;  Date: 03/29/2020
;  Assignment #6
;  Version of Visual Studio used (2019):  
;  Did program compile? Yes
;  Did program produce correct results? Yes 
;  Is code formatted correctly including indentation, spacing and vertical alignment? Yes
;  Is every line of code commented? Yes
;
;  Estimate of time in hours to complete assignment: 6 hours
;
;  In a few words describe the main challenge in writing this program: This program had a lot of conditions and was very long to write
;  
;  Short description of what program does:
;  Simulate 32 bit left shifter 
;  Using shift instructions to display binary digits
;  *************************************************************
;  Reminder: each assignment should be the result of your
;  individual effort with no collaboration with other students.
;
;  Reminder: every line of code must be commented and formatted  
;  per the ProgramExpectations.pdf file on the class web site
; *************************************************************

.386                    ;identifies minimum CPU for this program

.MODEL flat,stdcall     ;flat - protected mode program
                        ;stdcall - enables calling of MS_windows programs

;allocate memory for stack
;(default stack size for 32 bit implementation is 1MB without .STACK directive 
;  - default works for most situations)

.STACK 4096              ;allocate 4096 bytes (1000h) for stack

mPrtChar  MACRO  arg1    ;arg1 is replaced by the name of character to be displayed
         push eax        ;save eax
         mov al, arg1    ;character to display should be in al
         call WriteChar  ;display character in al
         pop eax         ;restore eax
ENDM


mPrtStr macro   arg1          ;arg1 is replaced by the name of character to be displayed
         push edx             ;save eax
         mov edx, offset arg1 ;character to display should be in al
         call WriteString     ;display character in al
         pop edx              ;restore eax
ENDM

;*************************PROTOTYPES*****************************

ExitProcess PROTO,
    dwExitCode:DWORD    ;from Win32 api not Irvine to exit to dos with exit code

ReadChar PROTO          ;Irvine code for getting a single char from keyboard
				        ;Character is stored in the al register.
			            ;Can be used to pause program execution until key is hit.

WriteChar PROTO         ;Irvine code to write character stored in al to console

WriteString PROTO		; write null-terminated string to output
                        ;EDX points to string

WriteDec PROTO                       

;************************  Constants  ***************************

LF       equ     0Ah                   ; ASCII Line Feed

;************************DATA SEGMENT***************************

.data

    ;inputs for testing the Shifter function
    inputA  byte 0,1,0,1,0,1,0,1
    inputB  byte 0,0,1,1,0,0,1,1
    inputC  byte 1,1,1,1,0,0,0,0
    ARRAY_SIZE equ $ - inputC         

    ;numbers for testing DoLeftShift
    nums   dword 10101010101010101010101010101010b
           dword 01010101010101010101010101010101b
           dword 11010101011101011101010101010111b
    NUM_SIZE EQU $-nums               ;total bytes in the nums array

    NUM_OF_BITS EQU SIZEOF(DWORD) * 8 ;Total bits for a dword

    ;You can add LFs to the strings below for proper output line spacing
    ;but do not change anything between the quotes "do not change".
    ;You can also combine messages where appropriate.

    ;I will be using a comparison program to compare your output to mine and
    ;the spacing must match exactly.

    endingMsg           byte LF,"Hit any key to exit!",0    ; ending message

    ;Change my name to your name
    titleMsg            byte "Program 6 by Nathaniel Lee",LF,LF,0   ; title message

    testingShifterMsg   byte "Testing Shifter",LF,0         ; testing shifter message
    testingBin          byte LF,"Testing DspBin",LF,0       ; testing dspbin message
    enabledMsg          byte "(Shifting enabled C = 1, Disabled C = 0)",LF,0    ; shifting message

    header       byte  "A B C | Output",LF,0     ; header label
    spaceBar            byte " | ",0

    dashes byte "------------------------------------",LF,0 ; dashes



;************************CODE SEGMENT****************************

.code

Main PROC

;start student code here
;See the pdf file for the pseudo code for the main function
   

    mPrtStr  titleMsg               ; print title message with MY name
    mPrtStr  testingShifterMsg      ; print testing shifting function message
    mPrtStr  enabledMsg             ; print shifting instruction message
    mPrtStr  dashes                 ; print dashes
    mPrtStr  header                 ; print head labels

  
    mov      esi, 0                 ; clear esi
    
loopTop:                            ; first while loop
    
    cmp      esi, ARRAY_SIZE        ; compare esi and the size of the array
    jae      DONEHERE               ; if exceed bounds jump to DONEHERE
       
    movzx    eax, inputA[esi]       ; al = inputA[esi]
    call     WriteDec               ; display decimal number
    mPrtChar ' '                    ; display space
    movzx    ebx, inputB[esi]       ; bl = inputB[esi]
    mov      al, bl                 ; al = bl
    call     WriteDec               ; display decimal number
    mPrtChar ' '                    ; print space
    movzx    ecx, inputC[esi]       ; cl = inputC[esi]
    mov      al, cl                 ; al = cl
    call     WriteDec               ; print decimal number
    mov      al, inputA[esi]        ; al = inputA[esi]
    mPrtStr  spaceBar               ; print space
  
        
    call     Shifter                ; call Shifter function
    call     WriteDec               ; display dec number in al
    mPrtChar LF                     ; print line feed
    inc      esi                    ; increment esi by 1 (byte array)
    jmp      loopTop                ; unconditional jump to first while loop

DONEHERE:                           ; exit first while loop

    mPrtStr  testingBin             ; print testing binary message
    mPrtStr  dashes                 ; print dashes
    mov      esi, 0                 ; esi = 0
    
loop2Top:                           ; beginning of second while loop

    cmp      esi, NUM_SIZE          ; compare esi and size of array
    jae      FINALDONE              ; if exceed bounds jump to FINALDONE
    mov      eax, nums[esi]         ; eax = nums[esi]
    call     DspBin                 ; DspBin dispays binary digits
    mPrtChar LF                     ; print line feed

    add      esi, 4                 ; esi = esi + 4 (dword array)
    jmp      loop2Top               ; unconditional jump to second while loop

FINALDONE:                          ; finished with program

    mPrtStr  endingMsg              ; display ending message
    call     ReadChar               ; pause execution
    INVOKE   ExitProcess, 0         ; exit to dos: like C++ exit(0)


Main ENDP



;************** Shifter – Simulate a partial shifter circuit per the circuit diagram in the pdf file.  
;  Shifter will simulate part of a shifter circuit that will input 
;  3 bits and output a shifted or non-shifted bit.
;
;
;   CL--------------------------
;              |               |
;              |               |
;             NOT    BL        |     AL
;              |     |         |     |
;              --AND--         --AND--
;                 |                |
;                 --------OR--------
;                          |
;                          AL
;
; NOTE: To implement the NOT gate use XOR to flip a single bit.
;
; Each input and output represents one bit.
;
;  Note: do not access the arrays in main directly in the Adder function. 
;        The data must be passed into this function via the required registers below.
;
;       ENTRY - AL = input bit A 
;               BL = input bit B
;               CL = enable (1) or disable (0) shift
;       EXIT  - AL = shifted or non-shifted bit
;       REGS  -  (list registers you use)
;
;       For the inputs in the input columns you should get the 
;       output in the output column below.
;
;The chart below shows the output for 
;the given inputs if shifting is enabled (cl = 1)
;If shift is enabled (cl = 1) then output should be the shifted bit (al).
;In the table below shifting is enabled (cl = 1)
;
;        input      output
;     al   bl  cl |   al 
;--------------------------
;      0   0   1  |   0 
;      1   0   1  |   1 
;      0   1   1  |   0 
;      1   1   1  |   1   
;
;The chart below shows the output for 
;the given inputs if shifting is disabled (cl = 0)
;If shift is disabled (cl = 0) then the output should be the non-shifted bit (B).

;        input      output
;     al   bl  cl |   al 
;--------------------------
;      0   0   0  |   0 
;      1   0   0  |   0 
;      0   1   0  |   1 
;      1   1   0  |   1   

;
;Note: the Shifter function does not do any output to the console.All the output is done in the main function
;
;Do not access the arrays in main directly in the shifter function. 
;The data must be passed into this function via the required registers.
;
;Do not change the name of the Shifter function.
;
;See additional specifications for the Shifter function on the 
;class web site.
;
;You should use AND, OR and XOR to simulate the shifter circuit.
;
;Note: to flip a single bit use XOR do not use NOT.
;
;You should save any registers whose values change in this function 
;using push and restore them with pop.
;
;The saving of the registers should
;be done at the top of the function and the restoring should be done at
;the bottom of the function.
;
;Note: do not save any registers that return a value (eax).
;
;Each line of this function must be commented and you must use the 
;usual indentation and formating like in the main function.
;
;Don't forget the "ret" instruction at the end of the function
;
;Do not delete this comment block. Every function should have 
;a comment block before it describing the function. FA17


Shifter proc

;start student code here

    push ecx         ; push ecx to stack to store original data

    ;starting from right of the circuit for efficiency

    and  al, cl      ; and the bits of al and cl 
    xor  cl, 1       ; flip the bit in cl
    and  cl, bl      ; and the bits in cl and bl
    or   al, cl      ; or the bits in al and cl

    pop  ecx         ; pop ecx off stack and restore data

    ret              ; ret to main function

Shifter endp




;************** DspBin - display a Dword in binary including leading zeros
;
;       ENTRY – EAX contains operand1, the number to print in binary
;
;       For Example if parm1 contained contained AC123h the following would print:
;                00000000000010101100000100100011
;       For Example if parm1 contained 0005h the following would print:
;                00000000000000000000000000000101
;
;       EXIT  - None
;       REGS  - List registers you use
;
; to call DspBin:
;               mov eax, 1111000110100b    ;number to print in binary is in eax
;               call DspBin            ; 00000000000000000001111000110100 should print
;     
;       Note: leading zeros do print
;       Note; at the end of this function use ret 4 (instead of just ret) to remove the parameter from the stack
;                 Do not use add esp, 4 in the main function.
;--------------

    ;You should have a loop that will do the following:

    ;The loop should execute NUM_OF_BITS times(32 times) times so that all binary digits will print including leading 0s.

    ;You should use the NUM_OF_BITS constant as the terminating loop condition and not hard code it.
    
    ;You should start at bit 31 down to and including bit 0 so that the digits will 
    ;   print in the correct order, left to right.
    ;Each iteration of the loop will print one binary digit.

    ;Each time through the loop you should do the following:
    
    ;clear al to 0
    ;You should use a shift instruction to shift the bit starting at position 31 to the carry flag 
    ;   then use a rotate command to copy the carry flag to the right end of al.

    ;Then Use the OR instruction to convert the 1 or 0 to a character ('1' or '0').
    
    ;then print it with WriteChar.

    ;You should keep processing the number until all 32 bits have been printed from bit 31 to bit 0. 
    
    ;Efficiency counts.

    ;DspBin just prints the raw binary number.

    ;No credit will be given for a solution that uses mul, imul, div or idiv. 
    ;
    ;You should save any registers whose values change in this function 
    ;using push and restore them with pop.
    ;
    ;The saving of the registers should
    ;be done at the top of the function and the restoring should be done at
    ;the bottom of the function.
    ;
    ;Each line of this function must be commented and you must use the 
    ;usual indentation and formating like in the main function.
    ;
    ;
    ;Do not delete this comment block. Every function should have 
    ;a comment block before it describing the function. FA17


DspBin proc

;start student code here

    push edi                  ; push edi to stack to store orignial data
    push ebx                  ; push ebx to stack to store original data
    push eax                  ; push eax to stack to store original data

    mov  edi, 0               ; edi = 0
    mov  ebx, eax             ; ebx = eax (temp register)

topLoop:                      ; beginning of first while loop

    cmp  edi, NUM_OF_BITS     ; compare edi and the number of bits
    jae  DONE                 ; if edi exceeds bounds then exit loop

    xor  al, al               ; clear al to 0
    shl  ebx, 1               ; shift bit in ebx to the left
    rcl  al, 1                ; rotate bit in CF to the end of al
    or   al, 00110000b        ; convert digit to character
    call WriteChar            ; display character

    inc  edi                  ; increment edi by 1
    jmp  topLoop              ; uncondtional jump to first while loop

DONE:                         ; end of function

    pop eax                   ; pop restore eax to original value
    pop ebx                   ; pop restore ebx to original value
    pop edi                   ; pop restore edi to original value

    ret                       ; return to main function

DspBin endp

END Main