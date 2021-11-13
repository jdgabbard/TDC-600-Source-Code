;MSX-DOS version 1.03 disassembly                               Jun 1,2006
;Disassembled and commented by Luis C.Grosso
;Basically the program is a BDOS function dispatcher plus an error handler
;Garbage removed, Source code made a bit shorter

        ORG     0100H
L0100:  JP      L0200           ;skip over the relocation table

;DATA RELOCATION TABLE ENDED WITH 0000H
;Each table element is formed as follows
;       DEFW    LXXXX-L0300+100H+2
;                 |     |    |   +--- offset to the instruction argument (high)
;                 |     |    +------- offset to compensate the INC B at L0262
;                 |     +------------ start of code to be relocated
;                 +------------------ label of the instruction to relocate
;
L0103:  DEFW    L0243-L0300+100H+2      ;the first four table elements relocate
        DEFW    L024A-L0300+100H+2      ;data that is used once before reloc.
        DEFW    L024E-L0300+100H+2      ;and is never used any more, and not
        DEFW    L0251-L0300+100H+2      ;moved to any location after reloc.
        DEFW    L0300-L0300+100H+2
        DEFW    L0306-L0300+100H+2
        DEFW    L0311-L0300+100H+2
        DEFW    L0314-L0300+100H+2
        DEFW    L0406-L0300+100H+2
        DEFW    L0409-L0300+100H+1
        DEFW    L040B-L0300+100H+1
        DEFW    L040D-L0300+100H+1
        DEFW    L040F-L0300+100H+1
        DEFW    L0411-L0300+100H+1
        DEFW    L0413-L0300+100H+1
        DEFW    L0506-L0300+100H+2
        DEFW    L0509-L0300+100H+2
        DEFW    L050C-L0300+100H+2
        DEFW    L0517-L0300+100H+2
        DEFW    L051E-L0300+100H+2
        DEFW    L0526-L0300+100H+2
        DEFW    L052D-L0300+100H+2
        DEFW    L0532-L0300+100H+2
        DEFW    L0538-L0300+100H+2
        DEFW    L0542-L0300+100H+2
        DEFW    L0561-L0300+100H+2
        DEFW    L0564-L0300+100H+2
        DEFW    L0571-L0300+100H+2
        DEFW    L058C-L0300+100H+2
        DEFW    L0592-L0300+100H+2
        DEFW    L05A6-L0300+100H+2
        DEFW    L05B0-L0300+100H+2
        DEFW    L05B7-L0300+100H+2
        DEFW    L05BD-L0300+100H+2
        DEFW    L05CE-L0300+100H+2
        DEFW    L05D1-L0300+100H+3
        DEFW    L05E5-L0300+100H+2
        DEFW    L05EC-L0300+100H+2
        DEFW    L05F4-L0300+100H+2
        DEFW    L05FD-L0300+100H+2
        DEFW    L060B-L0300+100H+2
        DEFW    L060E-L0300+100H+2
        DEFW    L0612-L0300+100H+2
        DEFW    L061B-L0300+100H+2
        DEFW    L061E-L0300+100H+2
        DEFW    L063F-L0300+100H+2
        DEFW    L0653-L0300+100H+2
        DEFW    L0658-L0300+100H+2
        DEFW    L0667-L0300+100H+2
        DEFW    L066F-L0300+100H+2
        DEFW    L0672-L0300+100H+2
        DEFW    L0682-L0300+100H+2
        DEFW    L068F-L0300+100H+2
        DEFW    L0699-L0300+100H+3
        DEFW    L069D-L0300+100H+2
        DEFW    L0713-L0300+100H+1
        DEFW    L0716-L0300+100H+1
        DEFW    L0719-L0300+100H+1
        DEFW    L071C-L0300+100H+1
        DEFW    L071F-L0300+100H+1
        DEFW    L072E-L0300+100H+1
        DEFW    L073A-L0300+100H+1
        DEFW    L0A00-L0300+100H+2
        DEFW    L0A03-L0300+100H+2
        DEFW    L0A06-L0300+100H+2
        DEFW    L0A09-L0300+100H+2
        DEFW    L0A0C-L0300+100H+2
        DEFW    L0A13-L0300+100H+2
        DEFW    L0A30-L0300+100H+2
        DEFW    L0A48-L0300+100H+2
        DEFW    0000H                               ;end of table

        ORG     $+256-LOW $     ;continue at next 256 byte page start
L0200:  JR      L0238           ;skip over the copyright message

L0202:  DEFB    0DH,0AH,'MSX-DOS version 1.03'
        DEFB    0DH,0AH,'Copyright 1984 by Microsoft',0DH,0AH,'$'

L0238:  LD      SP,9000H        ;put SP in safe place
        LD      HL,LF1C9        ;STROUT but lost in next instr
        CALL    L4030           ;get HIMEM in HL
        PUSH    HL              ;push himem
        PUSH    DE              ;and DE (content unknown)
L0243:  LD      DE,L0202        ;the copyright message
        LD      A,(LF340)       ;get the cold boot flag
        AND     A               ;is it coldbooting?
L024A:  CALL    Z,L0317         ;if yes print the message
        POP     HL              ;pop HL (unknown value)
L024E:  LD      HL,L0A80        ;last DOS resident byte to reloc
L0251:  LD      BC,L0300        ;first DOS resident byte to reloc
        XOR     A               ;clear carry and A=0
        SBC     HL,BC           ;calculate the block length
        EX      DE,HL           ;pass it to DE
        POP     HL              ;pop himem
        SBC     HL,DE           ;calculate the destination address
        LD      L,A             ;low byte = 0
        PUSH    HL              ;push destination address
        PUSH    DE              ;push block length
        PUSH    BC              ;and source address
        LD      A,H             ;calc the offset between blocks
        SUB     B               ;by substracting dest - source
        LD      C,A             ;pass the result to C (hi byte)
L0262:  DEC     B               ;decrement high byte, why?
L0263:  LD      HL,L0103        ;start of the relocation table
L0266:  LD      E,(HL)          ;get a value (lo)
        INC     HL              ;from the relocation table
        LD      D,(HL)          ;and also the high byte
        INC     HL              ;increment the table pointer
        LD      A,D             ;check if the end
        OR      E               ;marker was reached
        JR      Z,L0278         ;jump if yes
        EX      DE,HL           ;pass the table value to HL
        LD      A,H             ;pass the table value (hi) to A
        ADD     A,B             ;add source address
        LD      H,A             ;pass the result back to H
        LD      A,(HL)          ;get a byte from source
        ADD     A,C             ;add the offset
        LD      (HL),A          ;relocate it
        EX      DE,HL           ;pass the table pointer to HL
        JR      L0266           ;process the next table item
L0278:  POP     HL              ;source
        POP     BC              ;length
        POP     DE              ;destination
        PUSH    DE              ;push destination
        LDIR                    ;copy the block
        RET                     ;and jump to it

        ORG     $+256-LOW $     ;continue at next 256 byte page start
;THE CODE BEYOND THIS POINT IS COPIED TO HIGH MEMORY AND RELOCATED
L0300:  LD      HL,L0409        ;new disk error hook
        LD      (LF323),HL      ;addr of disk error routine pointer
L0306:  LD      HL,L0411        ;new ctrl-C error hook
        LD      (LF325),HL      ;addr of ctrl-C handling routine
        LD      HL,LF340        ;HL=coldboot flag address
        LD      A,(HL)          ;get the flag
        LD      (HL),H          ;signal 'not colbooting'
L0311:  LD      (L041A),A       ;save the previous flag state
L0314:  JP      L05B0           ;jump to L05B0

;PRINT THE MESSAGE POINTED BY 'DE' AND TERMINATED WITH '$'
L0317:  LD      A,(DE)          ;get one byte
        CP      24H              ;'$' is it the end marker?
        RET     Z               ;ret if yes
        PUSH    DE              ;push the pointer
        LD      E,A             ;pass the chr to E
        CALL    L5454           ;direct console IO
        POP     DE              ;pop the pointer
        INC     DE              ;increment it
        JR      L0317           ;process next character

        ORG     $+256-LOW $     ;continue at next 256 byte page start
L0400:  DEFB    00H,00H,00H,00H,00H,00H

L0406:  JP      L0631           ;BDOS entry point

;THIS DATA AREA IS ALSO USED BY COMMAND.COM, BE CARFUL NOT TO MODIFY LABELS
L0409:  DEFW    L0500           ;disk error routine address
L040B:  DEFW    L0500           ;extra pointers
L040D:  DEFW    L0500           ;maybe reserved
L040F:  DEFW    L0500           ;for future expansion
L0411:  DEFW    L056D           ;ctrl-C handling routine address
L0413:  DEFW    L0406           ;start address of COMMAND.COM at high memory
L0415:  DEFW    0001H           ;number of bytes to check
L0417:  DEFW    0FFFFH          ;checksum to be compared with the calculated
L0419:  DEFB    00H             ;batch file busy flag
L041A:  DEFB    00H             ;previous coldboot flag state
L041B:  DEFW    0000H,0000H,0000H,0000H,0000H ;buffer for % pointers used
        DEFW    0000H,0000H,0000H,0000H,0000H ;when processing batch files

;TWO BATCH FCBs
L042F:  DEFB    00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H
L0454:  DEFB    00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H

        ORG     $+256-LOW $     ;continue at next 256 byte page start
;ERROR HANDLING ROUTINE
;Error codes are returned this way by the disk rom
;A - Drive
;C - Error code:
;X x x x X X X X
;|       | | | |
;|       | | | +------------ 1 - Writing / 0 - Reading
;|       0 0 0 ------------- Write protect
;|       0 0 1 ------------- Not ready
;|       1 0 1 ------------- Unsupported media
;|         +---------------- 1 - Disk error (other)
;+-------------------------- 1 - Bad FAT
;
L0500:  ADD     A,41H           ;add code of 'A' to drive number
        BIT     7,C             ;is it 'bad fat' ?
        JR      NZ,L0561        ;jump if yes, else..
L0506:  LD      (L0822),A       ;update drive name in the message
L0509:  LD      HL,L07C9        ;point to 'read' string
L050C:  LD      DE,L0814        ;message with r/w action
        LD      A,C             ;pass error code to A
        BIT     0,A             ;reading or writing?
        LD      BC,0004H        ;string length is 4 bytes
        JR      Z,L051A         ;jump if error when reading, else..
L0517:  LD      HL,L07CD        ;point to 'writ' string
L051A:  LDIR                    ;update the message
        CP      0AH             ;is it 'unsupported media type' ?
L051E:  LD      DE,L07ED        ;'unsupported..'message
        PUSH    AF              ;save the error code and flags
        JR      Z,L0535         ;jump if unsupp... error, else..
        AND     0FEH            ;mask off the r/w bit
L0526:  LD      DE,L07D1        ;the 'write protect' message
        JR      Z,L0535         ;jump if write protect error
        CP      02H             ;is it 'not ready' error?
L052D:  LD      DE,L07E1        ;'not ready' message
        JR      Z,L0535         ;jump if 'not ready' error, else..
L0532:  LD      DE,L0806        ;any other will report 'disk error'
L0535:  CALL    LF1C9           ;STROUT display the error message
L0538:  LD      DE,L080D        ;during read or write operation
        CALL    LF1C9           ;STROUT display the message
        POP     AF              ;pop the flags
        LD      C,02H           ;signal 'abort'
        RET     Z               ;ret if 'unsuppotred media' error
L0542:  LD      DE,L0826        ;'abort, retry, ignore' message
        CALL    LF1C9           ;STROUT display the message
        CALL    L5445           ;CONSOLE INPUT
        PUSH    AF              ;save the character
        CALL    L5183           ;display CR/LF
        POP     AF              ;pop the character
        AND     5FH             ;convert to upper case
        LD      C,00H           ;signal 'ignore'
        CP      49H             ;is it 'I' (ignore)?
        RET     Z               ;ret if yes
        INC     C               ;signal 'retry'
        CP      52H             ;is it 'R' (retry)?
        RET     Z               ;ret if yes
        INC     C               ;signal 'abort'
        CP      41H             ;is it 'A' (abort)?
        RET     Z               ;ret if yes
        JR      L0542           ;loop back if none of the above

;PROCESS THE BAD FAT ERROR
L0561:  LD      (L07C5),A       ;update drive name in the message
L0564:  LD      DE,L07B4        ;the 'bad fat' message
        CALL    LF1C9           ;display the message
        LD      C,02H           ;signal 'abort'
        RET                     ;and ret

;CONTROL-C HANDLING ROUTINE
L056D:  CALL    LF368           ;enable disk rom in page 1
        EI                      ;enable ints
L0571:  LD      SP,L0A00        ;put the stack in a safe place
        LD      A,(LF23B)       ;printer echo flag
        OR      A               ;is the printer echo active?
        JR      Z,L0587         ;jump if not, else..
L057A:  LD      BC,04B0H        ;check it 1200 times
L057D:  PUSH    BC              ;push the counter
        CALL    L543C           ;CONSOLE STATUS (check ^c,^p,^n)
        POP     BC              ;pop the couunter
        DEC     BC              ;decrement the counter
        LD      A,C             ;has the counter
        OR      B               ;reached zero?
        JR      NZ,L057D        ;loop back if not
L0587:  LD      A,03H           ;ctrl-c character
        CALL    L535D           ;print the control character
L058C:  LD      A,(L0419)       ;get the batch file busy flag
        OR      A               ;batch file in process?
        JR      Z,L05A9         ;jump if not, else..
L0592:  LD      DE,L0751        ;'terminate batch...' message
        CALL    LF1C9           ;display it
        CALL    L5445           ;console input
        AND     5FH             ;convert to upper case
        CP      4EH             ;is it 'N'?
        JR      Z,L05A9         ;yes, return to the system
        CP      59H             ;is it 'Y'?
        JR      NZ,L0592        ;loop back if none of the above
        XOR     A               ;signal 'no batch in process'
L05A6:  LD      (L0419),A       ;update the variable
L05A9:  CALL    LF36B           ;enable RAM in page 1
        EI                      ;enable ints
        JP      0000H           ;return to the system

;CONTINUE WITH THE INITIALIZATION PROCESS (COLD OR WARM BOOT)
L05B0:  LD      SP,L0A00        ;put the stack at L0A00
        CALL    LF368           ;enable disk rom in page 1
        EI                      ;enable ints
L05B7:  LD      HL,L0A03        ;the second bios entry point
        LD      (0001H),HL      ;prepare the jmp at 0000H
L05BD:  LD      HL,L0406        ;the bdos entry point
        LD      (0006H),HL      ;prepare the jmp at 0005H
        LD      A,0C3H          ;JP opcode
        LD      (0000H),A       ;place the jump instructions
        LD      (0005H),A       ;at 0000H and 0005H
        CALL    L50A9           ;perform a disk reset

;CHECK IF THE COMMAND.COM IS INTACT, IF NOT, THEN RELOAD IT
L05CE:  LD      HL,(L0413)      ;start addr of COMMAND.COM
L05D1:  LD      BC,(L0415)      ;number of words to check
        LD      DE,0000H        ;initial checksum
L05D8:  LD      A,(HL)          ;get a byte
        INC     HL              ;increment pointer
        ADD     A,E             ;add the checksum low byte
        LD      E,A             ;store in E
        LD      A,(HL)          ;get another byte
        INC     HL              ;increment pointer
        ADC     A,D             ;add the chksum high with carry
        LD      D,A             ;store in D
        DEC     BC              ;decrement counter
        LD      A,B             ;check if it
        OR      C               ;is zero
        JR      NZ,L05D8        ;loop back if not
L05E5:  LD      HL,(L0417)      ;get the stored checksum
        SBC     HL,DE           ;compare with the calculated chksm
        JR      NZ,L05F4        ;jump if they don't match, else..
L05EC:  LD      HL,(L0413)      ;get the address of COMMAND.COM
        CALL    LF36B           ;enable RAM in page 1
        EI                      ;enable ints
        JP      (HL)            ;exec COMMAND.COM at high mem

;OPEN COMMAND.COM FILE, ASK FOR A DOS DISK IF CAN'T OPEN THE FILE
L05F4:  LD      DE,L0A5B        ;FCB for COMMAND.COM
        CALL    L4462           ;OPEN FILE
        OR      A               ;opened ok?
        JR      Z,L0608         ;jump if yes, else..
L05FD:  LD      DE,L0770        ;'Insert DOS disk..' message
        CALL    LF1C9           ;STROUT print it
        CALL    L544E           ;wait for a key
        JR      L05F4           ;loop back

;LOAD COMMAND.COM AT 0100H AND JUMP TO IT
L0608:  LD      HL,0000H        ;the file start
L060B:  LD      (L0A7C),HL      ;is pointed by
L060E:  LD      (L0A7E),HL      ;the file pointer
        INC     HL              ;set to one byte
L0612:  LD      (L0A69),HL      ;the record size
        LD      HL,L0100        ;start of any .COM file
        LD      (LF23D),HL      ;set the transfer address
L061B:  LD      DE,L0A5B        ;FCB for 'COMMAND.COM'
L061E:  LD      HL,L0300        ;max length
        CALL    L47B2           ;random block read
        OR      A               ;any error detected?
        JR      Z,L05FD         ;jump back if yes
        CALL    LF36B           ;enable RAM in page 1
        EI                      ;enable ints
        JP      L0100           ;jump to start of COMMAND.COM

;BDOS FUNCTION NOT PRESENT
L062E:  XOR     A               ;signal 'function not present'
        LD      B,A             ;pass it to B
        RET                     ;and ret

;BDOS FUNCTION PROCESSOR
L0631:  LD      A,01H           ;signal 'CPM compatible' in the flag
        LD      (LF306),A       ;(take parameters back to caller)
        LD      A,C             ;pass the function number to A
        CP      31H             ;is the function number too big?
        JR      NC,L062E        ;yes, signal function not present
        LD      (LF304),SP      ;save the actual stack pointer
L063F:  LD      SP,L0A00        ;define a new stack in a safe place
        CP      11H             ;is it 'search first' function?
        JR      NZ,L064A        ;jump if not, else..
        LD      (LF307),DE      ;save pointer to FCB with dir mask
L064A:  CP      12H             ;is it 'search next' function?
        JR      NZ,L0652        ;jump if not, else..
        LD      DE,(LF307)      ;get pointer to FCB with dir mask
L0652:  PUSH    HL              ;save HL
L0653:  LD      HL,L068A        ;the return address
        EX      (SP),HL         ;put it into the stack
        PUSH    HL              ;and save HL again
L0658:  LD      HL,L06BE        ;table with BDOS function addresses
        LD      B,00H           ;high byte = 0
        ADD     HL,BC           ;three byte elements, so
        ADD     HL,BC           ;HL=function number * 3 + offset
        ADD     HL,BC           ;table pointer in HL
        LD      A,(HL)          ;get length of data area
        INC     HL              ;for this function
        LD      B,(HL)          ;get function address lo
        INC     HL              ;increment pointer
        LD      H,(HL)          ;get function address hi
        LD      L,B             ;pass lo byte to L
        EX      (SP),HL         ;put addr in the stack, get 1st HL
L0667:  LD      (L083D),A       ;save data length
        OR      A               ;length = 0? (no data)
        JR      Z,L0685         ;jump if no data at all
        PUSH    HL              ;else save HL
        EX      DE,HL           ;pass caller's data address to HL
L066F:  LD      (L083E),HL      ;save caller's data address
L0672:  LD      DE,L0840        ;the start of data buffer
        LD      B,00H           ;data length can be 255 bytes max
        LD      C,A             ;pass data length to A
        INC     A               ;is it FF?, (buffered input)
        JR      NZ,L067F        ;jump if not, else..
        LD      A,(HL)          ;get data length from caller
        LD      C,A             ;pass it to C
        INC     BC              ;allow space for the two
        INC     BC              ;bytes buffer header
L067F:  LDIR                    ;copy data to buffer
        POP     HL              ;restore HL
L0682:  LD      DE,L0840        ;DE points now to the data buffer
L0685:  CALL    LF368           ;enable disk rom in page 1
        EI                      ;enable ints
        RET                     ;execute the function routne

;BDOS FUNCTION RETURN ADDRESS
L068A:  PUSH    AF              ;save dsk rom return code
        CALL    LF36B           ;enable RAM in page 1
        EI                      ;enable ints
L068F:  LD      A,(L083D)       ;get the length of data to take back
        OR      A               ;some data to take back?
        JR      Z,L06AA         ;jump if not, else..
        LD      C,A             ;pass the length to C
        LD      B,00H           ;high byte = 0
        PUSH    HL              ;save HL
L0699:  LD      DE,(L083E)      ;address of caller's data
L069D:  LD      HL,L0840        ;address of data from disk rom
        INC     A               ;is it FF?, (buffered input)
        JR      NZ,L06A7        ;jump if not, else..
        LD      A,(DE)          ;get data length from caller
        LD      C,A             ;pass it to C
        INC     BC              ;allow space for the two
        INC     BC              ;bytes buffer header
L06A7:  LDIR                    ;copy back to caller's buffer
        POP     HL              ;restore argument HL from disk rom
L06AA:  LD      A,(LF306)       ;get the CPM compatibility flag
        OR      A               ;is it pure MSXDOS?
        JR      Z,L06B8         ;yes, do not return parameters to caller
        POP     AF              ;else retrieve return code and
        LD      L,A             ;parameters
        LD      H,B             ;in HL
        LD      SP,(LF304)      ;retrieve the original value of SP
        RET                     ;ret
L06B8:  POP     AF              ;retrieve return code
        LD      SP,(LF304)      ;retrieve the original value of SP
        RET                     ;ret

;TABLE WITH BDOS FUNCTION ADDRESSES
L06BE:  DEFB    00H     ;00               -  # of bytes to copy to hi ram buff
        DEFW    0000H   ;SYSTEM RESET     -  address of the function routine
        DEFB    00H     ;01
        DEFW    L5445   ;CONSOLE INPUT
        DEFB    00H     ;02
        DEFW    L53A7   ;CONSOLE OUTPUT
        DEFB    00H     ;03
        DEFW    L546E   ;AUX INPUT
        DEFB    00H     ;04
        DEFW    L5474   ;AUX OUTPUT
        DEFB    00H     ;05
        DEFW    L5465   ;LST OUTPUT
        DEFB    00H     ;06
        DEFW    L5454   ;DIRECT CONSOLE I/O
        DEFB    00H     ;07
        DEFW    L5462   ;DIRECT INPUT
        DEFB    00H     ;08
        DEFW    L544E   ;DIRECT INPUT WITH CTRL_C CHECK
        DEFB    00H     ;09
        DEFW    LF1C9   ;STRING OUTPUT
        DEFB    0FFH    ;0A
        DEFW    L50E0   ;BUFFERED INPUT
        DEFB    00H     ;0B
        DEFW    L543C   ;CONSOLE STATUS
        DEFB    00H     ;0C
        DEFW    L41EF   ;GET VERSION NUMBER
        DEFB    00H     ;0D
        DEFW    L509F   ;DISK RESET
        DEFB    00H     ;0E
        DEFW    L50D5   ;SELECT DISK
        DEFB    20H     ;0F
        DEFW    L4462   ;OPEN FILE
        DEFB    20H     ;10
        DEFW    L456F   ;CLOSE FILE
        DEFB    0FH     ;11
        DEFW    L4FB8   ;SEARCH FIRST
        DEFB    0FH     ;12
        DEFW    L5006   ;SEARCH NEXT
        DEFB    20H     ;13
        DEFW    L436C   ;DELETE FILE
        DEFB    21H     ;14
        DEFW    L4775   ;SEQUENCIAL READ
        DEFB    21H     ;15
        DEFW    L477D   ;SEQUENCIAL WRITE
        DEFB    20H     ;16
        DEFW    L461D   ;CREATE FILE
        DEFB    20H     ;17
        DEFW    L4392   ;RENAME FILE
        DEFB    00H     ;18
        DEFW    L504E   ;GET LOGIN VECTOR
        DEFB    00H     ;19
        DEFW    L50C4   ;GET DEFAULT DRIVE NAME
        DEFB    00H     ;1A
        DEFW    L5058   ;SET DMA ADDRESS
        DEFB    00H     ;1B
        DEFW    L505D   ;GET ALLOCATION
        DEFB    00H     ;1C
L0713:  DEFW    L062E   ;NO FUNCTION
        DEFB    00H     ;1D
L0716:  DEFW    L062E   ;NO FUNCTION
        DEFB    00H     ;1E
L0719:  DEFW    L062E   ;NO FUNCTION
        DEFB    00H     ;1F
L071C:  DEFW    L062E   ;NO FUNCTION
        DEFB    00H     ;20
L071F:  DEFW    L062E   ;NO FUNCTION
        DEFB    24H     ;21
        DEFW    L4788   ;RANDOM READ
        DEFB    24H     ;22
        DEFW    L4793   ;RANDOM WRITE
        DEFB    24H     ;23
        DEFW    L501E   ;GET FILE SIZE
        DEFB    24H     ;24
        DEFW    L50C8   ;SET RANDOM RECORD
        DEFB    00H     ;25
L072E:  DEFW    L062E   ;NO FUNCTION
        DEFB    25H     ;26
        DEFW    L47BE   ;RANDOM BLOCK WRITE
        DEFB    25H     ;27
        DEFW    L47B2   ;RANDOM BLOCK READ
        DEFB    24H     ;28
        DEFW    L47D1   ;RANDOM WRITE WITH ZERO FILE
        DEFB    00H     ;29
L073A:  DEFW    L062E   ;NO FUNCTION
        DEFB    00H     ;2A
        DEFW    L553C   ;GET DATE
        DEFB    00H     ;2B
        DEFW    L5552   ;SET DATE
        DEFB    00H     ;2C
        DEFW    L55DB   ;GET TIME
        DEFB    00H     ;2D
        DEFW    L55E6   ;SET TIME
        DEFB    00H     ;2E
        DEFW    L55FF   ;SET/RESET VERIFY FLAG
        DEFB    00H     ;2F
        DEFW    L46BA   ;ABSOLUTE DISK READ
        DEFB    00H     ;30
        DEFW    L4720   ;ABSOLUTE DISK WRITE


L0751:  DEFB    0DH,0AH,'Terminate batch file (Y/N)? $'
L0770:  DEFB    0DH,0AH,'Insert DOS disk in default drive'
        DEFB    0DH,0AH,'and strike any key when ready',0DH,0AH,'$'
L07B4:  DEFB    0DH,0AH,'Bad FAT, drive '
L07C5:  DEFB    'A'             ;modified to reflect the drive name
        DEFB    0DH,0AH,'$'
L07C9:  DEFB    'read'
L07CD:  DEFB    'writ'
L07D1:  DEFB    0DH,0AH,'Write protect$'
L07E1:  DEFB    0DH,0AH,'Not ready$'
L07ED:  DEFB    0DH,0AH,'Unsupported media type$'
L0806:  DEFB    0DH,0AH,'Disk$'
L080D:  DEFB    ' error '
L0814:  DEFB    'read'          ;modified to reflect 'read/write' operation
        DEFB    'ing drive '
L0822:  DEFB    'A'             ;modified to reflect the drive name
        DEFB    0DH,0AH,'$'
L0826:  DEFB    'Abort, Retry, Ignore? $'

L083D:  DEFB    00H             ;data length
L083E:  DEFW    0000H           ;caller's data address

;BUFFER FOR BDOS FUNCTIONS DATA (132 BYTES LONG)
L0840:  DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
        DEFB    00H,00H,00H,00H

;L08C4:  DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
;        DEFB    00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H

        ORG     $+256-LOW $ +100H  ;continue at next 256 byte page start
;CPM COMPATIBLE BIOS (PARTIAL)
L0A00:  JP      L05B0           ;cold boot
L0A03:  JP      L05B0           ;warm boot
L0A06:  JP      L0A0F           ;console status
L0A09:  JP      L0A2C           ;conin
L0A0C:  JP      L0A43           ;conout

;CONSOLE STATUS -- A = 0FFH if there is a console keyboard character waiting
L0A0F:  LD      (LF304),SP      ;save the SP in a variable
L0A13:  LD      SP,L0A00        ;put the stack at L0A00
        CALL    LF368           ;enable disk rom in page 1
        EI                      ;enable ints
        CALL    L4034           ;test console status
        PUSH    AF              ;push the status
        CALL    LF36B           ;enable RAM in page 1
        EI                      ;enable ints
        POP     AF              ;pop the status
        LD      SP,(LF304)      ;retrieve the original SP
        LD      A,00H           ;signal 'no key available'
        RET     Z               ;ret if no key, else..
        DEC     A               ;signal 'a key available' A=FF
        RET                     ;and ret

;CONSOLE INPUT -- returns the next console keyboard character in A
L0A2C:  LD      (LF304),SP      ;save the SP in a variable
L0A30:  LD      SP,L0A00        ;put the stack at L0A00
        CALL    LF368           ;enable disk rom in page 1
        EI                      ;enable ints
        CALL    L4078           ;direct console input
        CALL    LF36B           ;enable RAM in page 1
        EI                      ;enable ints
        LD      SP,(LF304)      ;retrieve the original SP
        RET                     ;and ret

;CONSOLE OUTPUT -- outputs the character in C to the console device
L0A43:  LD      A,C             ;pass the character to A
        LD      (LF304),SP      ;save the SP in a variable
L0A48:  LD      SP,L0A00        ;put the stack at L0A00
        CALL    LF368           ;enable disk rom in page 1
        EI                      ;enable ints
        CALL    L408F           ;equivalent to CHPUT
        CALL    LF36B           ;enable RAM in page 1
        EI                      ;enable ints
        LD      SP,(LF304)      ;retrieve the original SP
        RET                     ;and ret

;FCB for the COMMAND.COM file  (37 BYTES)
L0A5B:  DEFB    00H
        DEFB    'COMMAND COM'
        DEFW    0000H
L0A69:  DEFW    0000H           ;logical record size
        DEFW    0000H           ;file length low word
        DEFW    0000H           ;file length high word
        DEFW    0000H           ;file date
        DEFS    10              ;reserved bytes
        DEFB    00H             ;current record
L0A7C:  DEFW    0000H           ;current relative record number low word
L0A7E:  DEFW    0000H           ;current relative record number high word
L0A80:  EQU     $               ;last byte to be relocated

;DISK ROM BIOS CALLS
L4030:  EQU     4030H           ;get HIMEM in HL
L4034:  EQU     4034H           ;test console status
L4078:  EQU     4078H           ;direct console input
L408F:  EQU     408FH           ;equivalent to CHPUT
L41EF:  EQU     41EFH           ;0C - get version number
L436C:  EQU     436CH           ;13 - delete file
L4392:  EQU     4392H           ;17 - rename file
L4462:  EQU     4462H           ;0F - open file
L456F:  EQU     456FH           ;10 - close file
L461D   EQU     461DH           ;16 - create file
L46BA:  EQU     46BAH           ;2F - absolute disk read
L4720:  EQU     4720H           ;30 - absolute disk write
L4775:  EQU     4775H           ;14 - sequencial read
L477D:  EQU     477DH           ;15 - sequencial write
L4788:  EQU     4788H           ;21 - random read
L4793:  EQU     4793H           ;22 - random write
L47B2:  EQU     47B2H           ;27 - random block read
L47BE:  EQU     47BEH           ;26 - random block write
L47D1:  EQU     47D1H           ;28 - random write with zero file
L4FB8:  EQU     4FB8H           ;11 - search first
L5006:  EQU     5006H           ;12 - search next
L501E:  EQU     501EH           ;23 - get file size
L504E:  EQU     504EH           ;18 - get login vector
L5058:  EQU     5058H           ;1A - set dma address
L505D:  EQU     505DH           ;1B - get allocation
L509F:  EQU     509FH           ;0D - disk reset, DMA addr and default unit
L50A9:  EQU     50A9H           ;perform a disk reset only
L50C4:  EQU     50C4H           ;19 - get default drive name
L50C8:  EQU     50C8H           ;24 - set random record
L50D5:  EQU     50D5H           ;0E - select disk
L50E0:  EQU     50E0H           ;0A - buffered input
L5183:  EQU     5183H           ;display CR/LF
L535D:  EQU     535DH           ;print a control character
L53A7:  EQU     53A7H           ;02 - console output
L543C:  EQU     543CH           ;0B - console status
L5445:  EQU     5445H           ;01 - console input
L544E   EQU     544EH           ;08 - direct input with ctrlC check
L5454:  EQU     5454H           ;06 - direct console i/o
L5462:  EQU     5462H           ;07 - direct input
L5465:  EQU     5465H           ;05 - lst output
L546E:  EQU     546EH           ;03 - aux input
L5474:  EQU     5474H           ;04 - aux output
L553C:  EQU     553CH           ;2A - get date
L5552:  EQU     5552H           ;2B - set date
L55DB:  EQU     55DBH           ;2C - get time
L55E6:  EQU     55E6H           ;2D - set time
L55FF:  EQU     55FFH           ;2E - set/reset verify flag

;SYSTEM VARIABLES
LF1C9:  EQU     0F1C9H          ;STROUT print a '$' term'd string pointed by HL
LF23B:  EQU     0F23BH          ;printer echo flag 0=not active 1=active
LF23D:  EQU     0F23DH          ;data transfer address
LF304:  EQU     0F304H          ;used to store SP temporarily
LF306:  EQU     0F306H          ;cp/m compatib flag (ret param back to caller)
LF307:  EQU     0F307H          ;pointer to FCB with dir mask
LF323:  EQU     0F323H          ;pointer to the disk error routine pointer
LF325:  EQU     0F325H          ;pointer to the ctrl-c error handling routine
LF340:  EQU     0F340H          ;cold boot flag 0=coldboot 1=warmboot
LF368:  EQU     0F368H          ;enable disk rom in page 1
LF36B:  EQU     0F36BH          ;enable ram in page 1
        END
