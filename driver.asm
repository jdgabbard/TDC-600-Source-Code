; Diskdriver Talent TDC600
;
; FDC	WD37C65
;
; Documentation work by Doug Gabbard, RetroDepot.net
; Original disassembly taken from: https://sourceforge.net/p/msxsyssrc/git/ci/master/tree/

; Main Status Register A0=0+RD
; DATA A0=1+RD
; DATA A0=1+WR
; Operations Register LDOR+WR
; Control Register Mapped: High

;       EXTRN   INIHRD	=	78ABH
;       EXTRN   DRIVES	=	7910H
;       EXTRN   INIENV	=	794CH
;       EXTRN   DSKIO	=	752EH
;       EXTRN   DSKCHG	=	7996H
;       EXTRN   GETDPB	=	79D5H
;       EXTRN   CHOICE	=	7C39H
;       EXTRN   DSKFMT	=	7C9FH
;       EXTRN   MTOFF	=	78F8H
;       EXTRN   OEMSTA	=	7F6DH
;       EXTRN   MYSIZE	=	25
;       EXTRN   SECLEN	=	Should be 512 - Size of biggest sector
;       EXTRN   DEFDPB	=	74AF - DEFAULT MEDIA F9

; symbols of routines which can be used by the diskdriver

;       PUBLIC  PROMPT
;       PUBLIC  SETINT
;       PUBLIC  PRVINT
;       PUBLIC  GETSLT
;       PUBLIC  GETWRK
;       PUBLIC  DIV16
;       PUBLIC  ENASLT
;       PUBLIC  XFER

        

;
FDCSTA	EQU	8000H	; ---LI		;WD37C65 PORT - STATUS REGISTER
FDCDAT	EQU	8001H	; --SL-		;WD37C65 PORT - DATA REGISTER
FDCLDO	EQU	9000H	; --S--		;WD37C65 PORT - LOAD OPERATION REGISTER
FDCSTAL	EQU	0000H				;WD37C65 PORT - STATUS REGISTER MIRROR
FDCDATL	EQU	0001H				;WD37C65 PORT - DATA REGISTER MIRROR
FDCLDOL	EQU	1000H				;WD37C65 PORT - LOAD OPERATION REGISTER MIRROR

WRSLT	EQU	0014H	; -C---
ENASLT	EQU	0024H	; -C---

RAMAD1	EQU	0F342H	; ---L-		;SLOT ADDR OF RAM IN PAGE 1
SECBUF	EQU	0F34DH	; --SL-		;TEMPORARY STORAGE FOR FAT SECTOR POINTER
SETROM	EQU	0F368H	; -C---		;SWITCH DISK-ROM TO PAGE 1
ROMBDOS	EQU	0F37DH	; -C---		;INTER-SLOT CALL TO BDOS ROUTINES
EXPTBL	EQU	0FCC1H	; ----I
DISINT	EQU	0FFCFH	; -C---		;HOOK TO EXTENDED BIOS - DISABLING INTERRUPTS - BEFORE INTERACT W/ DISK
ENAINT	EQU	0FFD4H	; -C---		;HOOK TO EXTENDED BIOS - ENABLING INTERRUPTS - AFTER INTERACT W/ DISK
SSLTRG	EQU	0FFFFH	; --SL-		SECONDARY SLOT REGISTER

DEFDPB	EQU	DSK7203-1

MYSIZE	EQU	25
SECLEN	EQU	512


		        ORG	7405H

;---------------------------------------------------
;	  Subroutine Get current slotid on page
;	     Inputs  B = page
;	     Outputs Slot ID in 'A'
;	     Registers: AF,HL affected
;---------------------------------------------------

GTCSLT:
		PUSH BC					;Save the values
        PUSH DE
        LD A,B					;Get input for Page. B=Page (0,1,2,3)
        OR A					;If 0, set Z-flag
        IN A,(0A8H)				;Read Primary Slot Register into 'A'
        JR Z,GTCSLT3			;If Page 0 is Slot 0, skip ahead
        PUSH BC					;Save again, as we'll be decrementing
GTCSLT2:	
		RRCA					;Rotate twice with carry, appears to calculate
        RRCA					;page slot assignment
        DJNZ GTCSLT2			;Decrement 'B' and Jump if not 0
        POP BC					;Once Zero, A is calculated, get back 'B'

GTCSLT3:
		AND 03H					;Primary Slot Bitmask for 'Page 0'
        LD E,A					;Used as an offset: 00XX + HL
        LD D,00H
        LD HL,EXPTBL			;Load HL with Expanded Slot Table
        ADD HL,DE				;Now we have the offset memory location
        LD E,A					;Store that offset.
        LD A,(HL)				;Get the Expanded Slot Table Page Value
        AND 80H					;Test if is expanded
        OR E					;Now combine
        LD E,A					;And store - Now we have Primary Slot ID???
        INC HL					;Now get Secondary Slot Register for Page
        INC HL
        INC HL
        INC HL
        LD A,B					;Get inpute for Page. B=Page (0,1,2,3)
        OR A					;If 0, set Z-Flag
        LD A,(HL)				;Get Secondary Slot Register Value
        RLCA					;Rotate Page 3 into Page 0 position
        RLCA	
        JR Z,GTCSLT5			;But if Page 0, Jump Ahead
GTCSLT4:
		RRCA					;If not Page 0, Rotate Page 3 back to Page 3
        RRCA					;Decrement Page Input until 0, then go on.	
        DJNZ GTCSLT4
GTCSLT5:
		AND 0CH					;Logical And for Page 1
        OR E					;Combine with Primary Slot ID
        POP DE					;Restore and Return with Slot ID in 'A'
        POP BC
        RET

;---------------------------------------------------
;	  Subroutine Set slotid on page 0
;	     Inputs  B = slotid
;	     Outputs ________________________
;---------------------------------------------------

SSLTID:
		DI
        PUSH BC
        LD B,A
        AND 03H
        LD C,A
        LD A,B
        BIT 7,A
        JR NZ,SSLTID2
        IN A,(0A8H)
        AND 0FCH
        OR C
        OUT (0A8H),A
        POP BC
        RET

SSLTID2:
		PUSH DE
        IN A,(0A8H)
        AND 0FCH
        OR C
        LD D,A
        RRCA
        RRCA
        AND 0C0H
        LD E,A
        LD A,D
        AND 3FH
        OR E
        OUT (0A8H),A
        LD A,B
        AND 0CH
        RRCA
        RRCA
        LD C,A
        LD A,(SSLTRG)
        CPL
        AND 0FCH
        OR C
        LD (SSLTRG),A
        LD A,D
        OUT (0A8H),A
        POP DE
        POP BC
        RET
		
;	  Subroutine Enable FDC on page 0
;	     Inputs  ________________________
;	     Outputs ________________________

ENAFDC:
		PUSH AF
        PUSH BC
        PUSH DE
        PUSH HL
        CALL GETWRK
        LD B,0			; page 0
        CALL GTCSLT			; Get current slotid on page
        LD (IX+24),A
        LD B,1			; page 1
        CALL GTCSLT			; Get current slotid on page
        LD (IX+23),A
        LD B,2			; page 2
        CALL GTCSLT			; Get current slotid on page
        LD (IX+22),A
        DI 
        LD A,(IX+23)
        CALL SSLTID			; Set slotid on page 0
        POP HL
        POP DE
        POP BC
        POP AF
        RET

;---------------------------------------------------
;
;		DISK PARAMETERS IN BLOCK DATA
;
;---------------------------------------------------

DSKPMT:						;DISK PARAMETERS
DSK3603:					;BLOCK DATA?
							;Confirmed by Arjen
        DEFB	0F8H		;F8 MEDIA
        DEFW	512			;80 TRACKS
        DEFB	0FH			;9 SECTORS
        DEFB	04H			;SINGLE SIDED
        DEFB	01H			;3.5 360KB
        DEFB	02H
        DEFW	1
        DEFB	2
        DEFB	112
        DEFW	12
        DEFW	355
        DEFB	2
        DEFW	5

DSK7203:						;BLOCK DATA
		DEFB	0F9H		;F9 MEDIA
        DEFW	512			;80 TRACKS
        DEFB	0FH			;9 SECTORS
        DEFB	04H			;DOUBLE SIDED
        DEFB	01H			;3.5 720KB
        DEFB	02H
        DEFW	1
        DEFB	2
        DEFB	112
        DEFW	14
        DEFW	714
        DEFB	3
        DEFW	7
DSK3203:		
		DEFB	0FAH		;FA MEDIA
        DEFW	512			;80 TRACKS
        DEFB	0FH			;8 SECTORS
        DEFB	04H			;SINGLE SIDED
        DEFB	01H			;3.5 320KB
        DEFB	02H
        DEFW	1
        DEFB	2
        DEFB	112
        DEFW	10
        DEFW	316
        DEFB	1
        DEFW	3
DSK6403:
        DEFB	0FBH		;FB MEDIA
        DEFW	512			;80 TRACKS
        DEFB	0FH			;8 SECTORS
        DEFB	04H			;DOUBLE SIDED
        DEFB	01H			;3.5 640KB
        DEFB	02H
        DEFW	1
        DEFB	2
        DEFB	112
        DEFW	12
        DEFW	635
        DEFB	2
        DEFW	5
DSK1805:
        DEFB	0FCH		;FC MEDIA
        DEFW	512			;40 TRACKS
        DEFB	0FH			;9 SECTORS
        DEFB	04H			;SINGLE SIDED
        DEFB	00H			;5.25 180KB
        DEFB	01H
        DEFW	1
        DEFB	2
        DEFB	64
        DEFW	9
        DEFW	352
        DEFB	2
        DEFW	5
DSK3605:
        DEFB	0FDH		;FD MEDIA
        DEFW	512			;40 TRACKS
        DEFB	0FH			;9 SECTORS
        DEFB	04H			;DOUBLE SIDED
        DEFB	01H			;5.25 360KB
        DEFB	02H
        DEFW	1
        DEFB	2
        DEFB	112
        DEFW	12
        DEFW	355
        DEFB	2
        DEFW	5
DSK1605:
        DEFB	0FEH		;FE MEDIA
        DEFW	512			;40 TRACKS
        DEFB	0FH			;8 SECTORS
        DEFB	04H			;SINGLE SIDED
        DEFB	00H			;5.25 160KB
        DEFB	01H
        DEFW	1
        DEFB	2
        DEFB	64
        DEFW	7
        DEFW	314
        DEFB	1
        DEFW	3
DSK3205:
        DEFB	0FFH		;FF MEDIA
        DEFW	512			;40 TRACKS
        DEFB	0FH			;8 SECTORS
        DEFB	04H			;DOUBLE SIDED
        DEFB	01H			;5.25 320KB
        DEFB	02H
        DEFW	1
        DEFB	2
        DEFB	112
        DEFW	10
        DEFW	316
        DEFB	1
        DEFW	3


;	  Subroutine DSKIO
;	     Inputs  ________________________
;	     Outputs ________________________

DSKIO:
		JP NC,J$761F
;
        CALL DISINT
        DI
        CALL ENAFDC			; Enable FDC on page 0
        CALL C$7563
J.753B:
		PUSH AF
        LD C,100
        JR NC,J$7542
        LD C,0
J$7542:
		CALL C.781E
        LD (IX+0),200
        LD A,(IX+12)
        AND A
        JR NZ,J$7554
        LD (IX+1),C
        JR J$7557
;
;	-----------------
J$7554:
		LD (IX+2),C
J$7557:
		LD A,(IX+24)		; saved slotid on page 0
        CALL SSLTID			; Set slotid on page 0
        POP AF
        EI
        CALL ENAINT
        RET
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C$7563:
		CALL C.76E4
;
        RET C
;
        LD A,H
        AND A
        JP M,J.759E
;
        CALL C.79F0
;
        CALL C.7A48
;
        RET C
;
        INC B
        DEC B
        RET Z
;
        LD A,H
        AND A
        JP M,J.759E
;
        PUSH HL
        PUSH DE
        PUSH BC
        LD A,(IX+24)		; saved slotid on page 0
        CALL SSLTID			; Set slotid on page 0
        LD DE,(SECBUF)
        PUSH DE
        LD BC,512
        CALL XFER
        LD A,(IX+23)
        CALL SSLTID			; Set slotid on page 0
;
        POP HL
        POP BC
        POP DE
        CALL C.75A9
;
        POP HL
        JR J$75A1
;
;	-----------------
J.759E:	
		CALL C.75A9
J$75A1:
		RET C
;
        DEC B
        RET Z
;
        CALL C.77A0
;
        JR J.759E
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C.75A9:
		LD E,07H	; 7 
J$75AB:
		CALL C.781E
;
        PUSH HL
        PUSH DE
        PUSH BC
        LD BC,0
        LD DE,0
        LD A,45H	; "E"
        CALL C.783C
;
J.75BC:
		LD A,(DE)
        RLA
        JR C,J.75D0
;
        DJNZ J.75BC
;
        DEC C
        LD A,(DE)
        RLA
        JR C,J.75D0
;
        JR NZ,J.75BC
;
        SCF
        JR J$75DE
;
;	-----------------
J.75CC:
		LD A,(DE)
        RLA
        JR NC,J.75CC
;
J.75D0:
		AND 40H	; "@"
        JR Z,J$75DB
;
        INC E
        LD A,(HL)
        LD (DE),A
        DEC E
        INC HL
        JR J.75CC
;
;	-----------------
J$75DB:
		CALL C.786A
;
J$75DE:
		POP BC
        POP DE
        POP HL
        JP C,J$7618
;
        LD A,(IX+15)
        AND 7FH
        RET Z
;
        BIT 1,A
        JR NZ,J$761C
;
        PUSH AF
        LD A,(IX+6)
        AND 01H	; 1 
        INC A
        CPL
        AND (IX+11)
        LD (IX+11),A
        CALL C.77E2
;
        POP AF
        DEC E
        JP NZ,J$75AB
;
        SCF
        LD E,A
        BIT 4,E
        LD A,0AH	; 10 
        RET NZ
;
        BIT 2,E
        LD A,08H	; 8 
        RET NZ
;
        BIT 5,E
        LD A,04H	; 4 
        RET NZ
;
        LD A,0CH	; 12 
        RET
;
;	-----------------
J$7618:	
		LD A,02H	; 2 
        SCF
        RET
;
;	-----------------
J$761C:
		XOR A
        SCF
        RET
;
;	-----------------
J$761F:
		CALL DISINT
        DI
        CALL ENAFDC			; Enable FDC on page 0
        CALL C$762C
        JP J.753B
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C$762C:
		CALL C.76E4
;
        RET C
;
        LD A,H
        AND A
        JP M,J.766F
;
        CALL C.79F0
;
        CALL C$7A23
;
        CALL C.7A48
;
        RET C
;
        INC B
        DEC B
        RET Z
;
        LD A,H
        AND A
        JP M,J.766F
;
        PUSH HL
        LD HL,(SECBUF)
        CALL C.767A
;
        POP HL
        RET C
;
        PUSH BC
        PUSH DE
        PUSH HL
        LD A,(IX+24)		; saved slotid on page 0
        CALL SSLTID			; Set slotid on page 0
;
        EX DE,HL
        LD HL,(SECBUF)
        LD BC,512
        CALL XFER
;
        LD A,(IX+23)
        CALL SSLTID			; Set slotid on page 0
;
        POP HL
        POP DE
        POP BC
        AND A
        JR J$7673
;
;	-----------------
J.766F:
		CALL C.767A
;
        RET C
;
J$7673:
		DEC B
        RET Z
;
        CALL C.77A0
;
        JR J.766F
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C.767A:
		LD E,07H	; 7 
J$767C:
		CALL C.781E
;
        PUSH HL
        PUSH DE
        PUSH BC
        LD BC,0
        LD DE,0
        LD A,66H	; "f"
        CALL C.783C
;
J.768D:
		LD A,(DE)
        RLA
        JR C,J.76A1
;
        DJNZ J.768D
;
        DEC C
        LD A,(DE)
        RLA
        JR C,J.76A1
;
        JR NZ,J.768D
;
        SCF
        JR J$76AF
;
;	-----------------
J.769D:
		LD A,(DE)
        RLA
        JR NC,J.769D
;
J.76A1:
		AND 40H	; "@"
        JR Z,J$76AC
;
        INC E
        LD A,(DE)
        LD (HL),A
        INC HL
        DEC E
        JR J.769D
;
;	-----------------
J$76AC:
		CALL C.786A
;
J$76AF:
		POP BC
        POP DE
        POP HL
        JP C,J$76E0
;
        LD A,(IX+15)
        AND 7FH
        RET Z
;
		PUSH AF
        LD A,(IX+6)
        AND 01H	; 1 
        INC A
        CPL
        AND (IX+11)
        LD (IX+11),A
        CALL C.77E2
;
        POP AF
        DEC E
        JP NZ,J$767C
;
        SCF
        LD E,A
        BIT 2,E
        LD A,08H	; 8 
        RET NZ
;
        BIT 5,E
        LD A,04H	; 4 
        RET NZ
;
        LD A,0CH	; 12 
        RET	
;
;	-----------------
J$76E0:
		LD A,02H	; 2 
        SCF
        RET
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C.76E4:
		PUSH AF
        PUSH BC
        PUSH HL
        CALL GETWRK
;
        POP HL
        POP BC
        POP AF
        CP 02H	; 2 
        JR C,J$76F5
;
J$76F1:
		LD A,0CH	; 12 
        SCF
        RET
;
;	-----------------
J$76F5:
		PUSH AF
        LD A,C
        CP 0F8H
        JR NC,J$76FE
;
        POP AF
        JR J$76F1
;
;	-----------------
J$76FE:
		EX (SP),HL
        PUSH HL
        PUSH BC
        BIT 1,C
        LD C,E
        LD B,D
        LD DE,8
        JR NZ,J$770B
        INC DE
J$770B:
		CALL DIV16
        LD A,L
        INC A
        LD (IX+9),A
        LD L,C
        POP BC
        POP AF
        LD H,A
        LD A,(IX+5)
        DEC A
        JR Z,J.7723
        LD A,H
        OR A
        JR Z,J.7723
        LD A,11H
J.7723:
		ADD A,1CH
        PUSH AF
        AND 01H	; 1 
        BIT 0,C
        JR Z,J.7732
        SRL L
        JR NC,J.7732
        OR 04H	; 4 
J.7732:
		LD (IX+6),A
        PUSH AF
        SRL A
        SRL A
        AND 01H	; 1 
        LD (IX+8),A
        POP AF
        LD D,A
        LD A,C
        RRCA
        RRCA
        AND 0C0H
        OR D
        LD D,A
        POP AF
        LD (FDCLDOL),A		; PC AT mode, motor on, dma enabled, select drive
        LD (IX+12),A
        LD A,(IX)
        AND A
        LD (IX),0FFH
        JR NZ,J$7763
;
        PUSH HL
        LD HL,0
J$775D:
		DEC HL
        LD A,L
        OR H
        JR NZ,J$775D
;
        POP HL
J$7763:
		LD C,L
        LD (IX+7),L
        LD A,(IX+5)
        DEC A
        JR Z,J$777A
;
        LD A,(IX+3)
        CP H
        JR Z,J.779B
;
        XOR 01H	; 1 
        LD (IX+3),A
        JR J.779B
;
;	-----------------
J$777A:
		LD A,H
        CP (IX+4)
        LD (IX+4),A
        JR Z,J.779B
;
        PUSH IX
        PUSH DE
        PUSH BC
        LD A,(IX+24)		; saved slotid on page 0
        CALL SSLTID			; Set slotid on page 0
        CALL PROMPT
        POP BC
        POP DE
        POP IX
        DI
        LD A,(IX+23)
        CALL SSLTID			; Set slotid on page 0
;
J.779B:
		CALL C.77E2
;
        POP HL
        RET
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C.77A0:
		INC H
        INC H
        LD A,(IX+9)
        INC A
        LD (IX+9),A
        BIT 7,D
        JR NZ,J$77B0
;
        CP 0AH	; 10 
        RET C
;
J$77B0:
		CP 09H	; 9 
        RET C
;
        LD A,01H	; 1 
        LD (IX+9),A
        BIT 6,D
        JR Z,J.77CE
;
        BIT 2,D
        JR NZ,J.77CE
;
        SET 2,D
        LD A,D
        AND 0FH	; 15 
        LD (IX+6),A
        LD A,01H	; 1 
        LD (IX+8),A
        RET
;
;	-----------------
J.77CE:
		RES 2,D
        LD A,D
        AND 0FH	; 15 
        LD (IX+6),A
        XOR A
        LD (IX+8),A
        INC C
        LD (IX+7),C
        CALL C.77E2
;
        RET
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C.77E2:
		CALL C.781E
;
        LD A,(IX+6)
        AND 01H	; 1 
        INC A
        AND (IX+11)
        JR NZ,J$780A
;
        LD A,(IX+6)
        AND 01H	; 1 
        INC A
        OR (IX+11)
        LD (IX+11),A
        LD A,07H	; 7 
        CALL C.785B
;
        LD A,(IX+6)
        CALL C.785B
;
        CALL C.788C
;
J$780A:
		LD A,0FH	; 15 
        CALL C.785B
;
        LD A,(IX+6)
        CALL C.785B
;
        LD A,(IX+7)
        CALL C.785B
;
        JP C.788C
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C.781E:
		LD	A,(FDCSTAL)
        AND 0D0H			; RQM,DIO,CB
        XOR 80H
        RET Z
        XOR A
        LD (IX+11),A
        LD (FDCLDOL),A		; PC AT mode, motor 2 off, motor 1 off, dma disabled, reset, select drive 1
        CALL C.7837
        LD A,(IX+12)
        LD (FDCLDOL),A
        RET
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C.7837:
		EX (SP),HL
        EX (SP),HL
        EX (SP),HL
        EX (SP),HL
        RET
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C.783C:
		PUSH BC
        LD B,06H	; 6 
        PUSH IX
J$7841:
		CALL C.785B
;
        LD A,(IX+6)
        INC IX
        DJNZ J$7841
;
        POP IX
        POP BC
        LD A,(IX+9)
        CALL C.785B
;
        LD A,1BH
        CALL C.785B
;
        LD A,0FFH
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C.785B:
		PUSH AF
J$785C:
		LD A,(FDCSTAL)
        AND 0E0H			; RQM,DIO,EXM
        CP 80H			; DR ready, CPU->FDC, Execution finshed ?
        JR NZ,J$785C		; nope, wait
        POP AF
        LD (FDCDATL),A
        RET

;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________

C.786A:
		PUSH IX
J.786C:
		LD A,(FDCSTAL)
        AND 0C0H			; RQM, DIO
        CP 0C0H
        JR NZ,J.786C
        LD A,(FDCDATL)
        LD (IX+14),A
        INC IX
        CALL C.7837
        LD A,(FDCSTAL)
        AND 0C0H			; RQM, DIO
        CP 80H
        JR NZ,J.786C
        POP IX
        RET
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C.788C:
		PUSH BC
        LD B,20H	; " "
J$788F:
		LD A,08H	; 8 
        CALL C.785B
;
        CALL C.786A
;
        LD A,(IX+14)
        AND 0F0H
        CP 20H	; " "
        JR Z,J$78A9
;
        CALL C.7837
;
        DEC BC
        LD A,B
        OR C
        JR NZ,J$788F
;
        SCF
J$78A9:
		POP BC
        RET

;	  Subroutine INIHRD
;	     Inputs  ________________________
;	     Outputs ________________________

INIHRD:
        LD B,0			; page 0
        CALL GTCSLT			; Get current slotid on page
        PUSH AF
        LD B,1			; page 1
        CALL GTCSLT			; Get current slotid on page
        CALL SSLTID			; Set slotid on page 0
        XOR A
        LD (FDCLDOL),A		; PC AT mode, motor 2 off, motor 1 off, dma disabled, reset, select drive 1
        CALL C.7837
        LD A,0CH
        LD (FDCLDOL),A		; PC AT mode, motor 2 off, motor 1 off, dma enabled, select drive 1
        LD A,03H
        CALL C.785B
        LD A,9FH
        CALL C.785B
        LD A,03H	; 3 
        CALL C.785B
        LD A,1CH
        LD (FDCLDOL),A		; PC AT mode, motor 2 off, motor 1 on, dma enabled, select drive 1
        LD A,07H	; 7 
        CALL C.785B
        LD A,00H
        CALL C.785B
        LD HL,0
INIHRD2:
		CALL C.7837
        DEC HL
        LD A,H
        OR L
        JR NZ,INIHRD2
        LD A,0CH
        LD (FDCLDOL),A		; PC AT mode, motor 2 off, motor 1 off, dma enabled, select drive 1
        POP AF
        CALL SSLTID			; Set slotid on page 0
        RET

;	  Subroutine MTOFF
;	     Inputs  ________________________
;	     Outputs ________________________

MTOFF:
        PUSH BC
        PUSH DE
        PUSH HL
        LD A,0CH			; PC AT mode, motor 2 off, motor 1 off, dma enabled, select drive 1
        LD (IX+12),A
        LD E,A
        LD B,1			; page 1
        CALL GTCSLT			; Get current slotid on page
        LD HL,FDCLDOL
        CALL WRSLT
        POP HL
        POP DE
        POP BC
        RET

;	  Subroutine DRIVES
;	     Inputs  ________________________
;	     Outputs ________________________

DRIVES:
        CALL ENAFDC			; Enable FDC on page 0
        PUSH BC
        PUSH AF
        LD A,2DH			; PC AT mode, motor 2 on, motor 1 off, dma enabled, select drive 2
        LD (FDCLDOL),A
        CALL C.788C
        LD A,07H	; 7 
        CALL C.785B
        LD A,01H	; 1 
        CALL C.785B
        CALL C.788C
        LD L,1
        JR C,DRIVES2
        LD L,2
DRIVES2:
		LD (IX+5),L
        LD A,0CH			; PC AT mode, motor 2 off, motor 1 off, dma enabled, select drive 1
        LD (FDCLDOL),A
        POP AF
        JR Z,DRIVES3
        LD L,2
DRIVES3:	
		PUSH HL
        PUSH AF
        CALL GETWRK
        LD A,(IX+24)		; saved slotid on page 0
        CALL SSLTID			; Set slotid on page 0
        POP AF
        POP HL
        POP BC
        RET

;	  Subroutine INIENV
;	     Inputs  ________________________
;	     Outputs ________________________

INIENV:
        CALL GETWRK
        LD D,(IX+5)
        XOR A
        LD B,19H
INIENV2:
		LD (HL),A
        INC HL
        DJNZ INIENV2
        LD (IX+5),D
        LD (IX+10),02H	; 2 
        LD HL,I$7966
        JP SETINT

I$7966:						;Disk drive interrupt handler?
		PUSH AF
        CALL GETWRK
        LD A,(HL)
        AND A
        JR Z,J.7986
        CP 0FFH
        JR Z,J.7986
        DEC A
        LD (HL),A
        JR NZ,J.7986
        LD A,0CH			; PC AT mode, motor 2 off, motor 1 off, dma enabled, select drive 1
        LD E,A
        LD B,1			; page 1
        CALL GTCSLT			; Get current slotid on page
        PUSH HL
        LD HL,FDCLDOL
        CALL WRSLT
        POP HL
J.7986:
		INC HL
        LD A,(HL)
        AND A
        JR Z,J$798C
        DEC (HL)
J$798C:
		INC HL
        LD A,(HL)
        AND A
        JR Z,J$7992
        DEC (HL)
J$7992:
		POP AF
        JP PRVINT

;	  Subroutine DSKCHG
;	     Inputs  ________________________
;	     Outputs ________________________

DSKCHG:
        EI
        PUSH HL
        PUSH BC
        PUSH AF
        CALL GETWRK
        POP AF
        POP BC
        POP HL
        AND A
        LD B,(IX+2)
        JR NZ,DSKCHG2
        LD B,(IX+1)
DSKCHG2:
		INC B
        DEC B
        LD B,01H	; 1 
        RET NZ
        PUSH BC
        PUSH HL
        LD DE,1
        LD HL,(SECBUF)
        CALL DSKIO
        JR C,DSKCHG3
        LD HL,(SECBUF)
        LD B,(HL)
        POP HL
        PUSH BC
        CALL GETDPB
        LD A,0CH	; 12 
        JR C,DSKCHG3
        POP AF
        POP BC
        CP C
        SCF
        CCF
        LD B,0FFH
        RET NZ
        INC B
        RET
;
;	-----------------
DSKCHG3:
		POP DE			;Not sure why we're throwing away BC register saves
        POP DE
        RET

;	  Subroutine GETDPB
;	     Inputs  ________________________
;	     Outputs ________________________


GETDPB:	EI 
        EX DE,HL
        INC DE
        LD A,B
        SUB 0F8H
        RET C
;
        LD L,A
        LD H,00H
        ADD HL,HL
        LD C,L
        LD B,H
        ADD HL,HL
        ADD HL,HL
        ADD HL,HL
        ADD HL,BC
        LD BC,DSKPMT
        ADD HL,BC
        LD BC,18
        LDIR
        RET
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C.79F0:
		PUSH HL
        PUSH DE
        PUSH BC
        LD HL,I$7A84
        LD DE,(SECBUF)
        LD BC,01B5H
        LDIR
        LD HL,I$7A4E
J$7A02:
		LD E,(HL)
        INC HL
        LD D,(HL)
        INC HL
        LD A,E
        OR D
        JR Z,J$7A1F
;
        PUSH HL
        LD HL,(SECBUF)
        ADD HL,DE
        INC HL
        LD C,(HL)
        INC HL
        LD B,(HL)
        EX DE,HL
        LD HL,(SECBUF)
        ADD HL,BC
        EX DE,HL
        LD (HL),D
        DEC HL
        LD (HL),E
        POP HL
        JR J$7A02
;
;	-----------------
J$7A1F:
		POP BC
        POP DE
        POP HL
        RET
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C$7A23:
		PUSH HL
        PUSH DE
        PUSH BC
        LD HL,I$7A6A
J$7A29:	
		LD E,(HL)
        INC HL
        LD D,(HL)
        LD A,E
        OR D
        JR Z,J$7A44
;
        PUSH HL
        LD HL,(SECBUF)
        ADD HL,DE
        EX DE,HL
        POP HL
        INC HL
        LD C,(HL)
        INC HL
        LD B,(HL)
        INC HL
        PUSH HL
        EX DE,HL
        LD (HL),C
        INC HL
        LD (HL),B
        POP HL
        JR J$7A29
;
;	-----------------
J$7A44:
		POP BC
        POP DE
        POP HL
        RET
;
;	-----------------
;
;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________
;
C.7A48:
		PUSH HL
        LD HL,(SECBUF)
        EX (SP),HL
        RET
;
;	-----------------
I$7A4E:
		ADD HL,HL
        NOP
        LD L,00H
        INC A
        NOP
        LD E,(HL)
        NOP
        LD H,H
        NOP 
        LD (HL),C
        NOP
        ADC A,B
        NOP
        DEC DE
        LD BC,0156H
        LD L,D
        LD BC,017AH
        LD A,A
        LD BC,01A6H
        DEFB 0,0
I$7A6A:	
		LD A,(3E00H)
        LD H,(HL)
        LD E,B
        NOP 
        LD A,(DE)
        LD (HL),A
        HALT
;
;	-----------------
?.7A73:
		DEFB 0,0,0
        LD A,B
        DEFB 0,0,0
        SUB C
        DEFB 0,0,0
        SUB E
        DEFB 0,0,0,0,0
I$7A84:
		PUSH IX
        PUSH HL
        PUSH DE
        PUSH BC
        LD A,(IX+24)		; saved slotid on page 0
        CALL SSLTID			; Set slotid on page 0
;
        PUSH IX
        LD A,(RAMAD1)
        LD H,40H	; "@"
        CALL ENASLT
;
        POP IX
        LD A,(IX+23)
        LD H,80H
        CALL ENASLT
;
        POP BC
        POP DE
        POP HL
        POP IX
J$7AA8:
		DEC HL
        LD A,H
        ADD A,02H	; 2 
        INC HL
        JP M,00AFH
;
        LD E,07H	; 7 
J$7AB2:
		CALL 0147H			;CALL FORMAT from MSX-BIOS ?
;
        PUSH HL
        PUSH DE
        PUSH BC
        LD BC,0
        LD DE,8000H
        LD A,45H	; "E"
        CALL 0165H			;CHKNEW from MSX-BIOS ?
;
J.7AC3:
		LD A,(DE)
        RLA
        JR C,J.7AD7
;
        DJNZ J.7AC3
;
        DEC C
        LD A,(DE)
        RLA
        JR C,J.7AD7
;
        JR NZ,J.7AC3
;
        SCF
        JR J$7AE5
;
;	-----------------
J.7AD3:
		LD A,(DE)
        RLA
        JR NC,J.7AD3
;
J.7AD7:
		AND 40H	; "@"
        JR Z,J$7AE2
;
        INC E
        LD A,(HL)
        LD (DE),A
        DEC E
        INC HL
        JR J.7AD3
;
;	-----------------
J$7AE2:
		CALL 0193H			;Call to bios - where?
;
J$7AE5:
		POP BC
        POP DE
        POP HL
        JP C,00A7H
;
        LD A,(IX+15)
        AND 7FH
        JR NZ,J$7AFA
;
        DEC B
        JR Z,J$7B33
;
        CALL 00DDH			;Cant be call to bios, this is middle of paddle routine
;
        JR J$7AA8
;
;	-----------------
J$7AFA:
		BIT 1,A
        JR NZ,J$7B30
;
        PUSH AF
        LD A,(IX+6)
        AND 01H	; 1 
        INC A
        CPL
        AND (IX+11)
        LD (IX+11),A
        CALL 011FH
;
        POP AF
        DEC E
        JR NZ,J$7AB2
;
        SCF
        LD E,A
        BIT 4,E
        LD A,0AH	; 10 
        JR NZ,J.7B32
;
        BIT 2,E
        LD A,08H	; 8 
        JR NZ,J.7B32
;
        BIT 5,E
        LD A,04H	; 4 
        JR NZ,J.7B32
;
        LD A,0CH	; 12 
        JR J.7B32
;
;	-----------------
?.7B2B:
		LD A,02H	; 2 
        SCF
        JR J.7B32
;
;	-----------------
J$7B30:
		XOR A
        SCF
J.7B32:
		SCF
J$7B33:
		PUSH IX
        PUSH HL
        PUSH DE
        PUSH BC
        PUSH AF
        PUSH IX
        LD A,(IX+22)
        LD H,80H
        CALL ENASLT
;
        CALL SETROM
;
        POP IX
        PUSH IX
        LD A,(IX+23)
        LD H,40H	; "@"
        CALL ENASLT
;
        POP IX
        LD A,(IX+23)
        CALL SSLTID			; Set slotid on page 0
;
        POP AF
        POP BC
        POP DE
        POP HL
        POP IX
        RET
;
;	-----------------
?.7B61:
		INC H
        INC H
        LD A,(IX+9)
        INC A
        LD (IX+9),A
        BIT 7,D
        JR NZ,J$7B71
;
        CP 0AH	; 10 
        RET C
;
J$7B71:
		CP 09H	; 9 
        RET C
;
        LD A,01H	; 1 
        LD (IX+9),A
        BIT 6,D
        JR Z,J.7B8F
;
        BIT 2,D
        JR NZ,J.7B8F
;
        SET 2,D
        LD A,D
        AND 0FH	; 15 
        LD (IX+6),A
        LD A,01H	; 1 
        LD (IX+8),A
        RET
;
;	-----------------
J.7B8F:
		RES 2,D
        LD A,D
        AND 0FH	; 15 
        LD (IX+6),A
        XOR A
        LD (IX+8),A
        INC C
        LD (IX+7),C
        CALL 011FH
;
        RET
;
;	-----------------
?.7BA3:
		PUSH BC
        PUSH DE
        PUSH HL
        LD A,(IX+23)
        LD H,40H	; "@"
        CALL ENASLT
        LD A,(IX+23)
        CALL SSLTID			; Set slotid on page 0
        POP HL
        CALL C.77E2
        PUSH HL
        LD A,(IX+24)		; saved slotid on page 0
        CALL SSLTID			; Set slotid on page 0
        LD A,(RAMAD1)
        LD H,40H	; "@"
        CALL ENASLT
        POP HL
        POP DE
        POP BC
        RET
;
;	-----------------
?.7BCB:
		LD A,(FDCSTA)				;ANOTHER ROUTINE TO WRITE DATA TO THE FDC
        AND 0D0H
        XOR 80H
        RET Z
;
        XOR A
        LD (IX+11),A
        LD (FDCLDO),A				;WRITE TO LOAD OPERATION REGISTER
        CALL 0160H
;
        LD A,(IX+12)
        LD (FDCLDO),A				;WRITE TO LOAD OPERATION REGISTER
        RET
;
;	-----------------
?.7BE4:
		EX (SP),HL
        EX (SP),HL
        EX (SP),HL
        EX (SP),HL
        RET
;
;	-----------------
?.7BE9:
		PUSH BC
        LD B,06H	; 6 
        PUSH IX
J$7BEE:
		CALL 0184H
;
        LD A,(IX+6)
        INC IX
        DJNZ J$7BEE
;
        POP IX
        POP BC
        LD A,(IX+9)
        CALL 0184H
;
        LD A,1BH
        CALL 0184H
;
        LD A,0FFH
        PUSH AF				;Routine for reading FDC status
J$7C09:						;Read Status Register
		LD A,(FDCSTA)
        AND 0E0H			;Compare if Ready
        CP 80H
        JR NZ,J$7C09		;If not, wait until it is
;
        POP AF				;When ready, write data and return
        LD (FDCDAT),A
        RET
;
;	-----------------
?.7C17:						;Routine for reading data from FDC
		PUSH IX
J.7C19:
		LD A,(FDCSTA)		;Read Status Register
        AND 0C0H			;Compare if data is available
        CP 0C0H
        JR NZ,J.7C19		;If not, wait until it is
;
        LD A,(FDCDAT)		;When ready, write data
        LD (IX+14),A		;Get something........
        INC IX				;Increment Pointer
        CALL 0160H			;And go do what??????
;
        LD A,(FDCSTA)		;Now get status again
        AND 0C0H			;Assuming checking if done
        CP 80H
        JR NZ,J.7C19		;If not done, do it again
;
        POP IX				;Otherwise, restore and return
        RET

;	  Subroutine CHOICE
;	     Inputs  ________________________
;	     Outputs ________________________

CHOICE:
        LD HL,I$7C3D
        RET
;


;TEXT DATA ----------------------------------
I$7C3D:

	;Text Data terminates with 00H.
	;DEFB 0DH,0AH,31H,29H,20H, 33H,2EH,35H,22H,20H, 20H,53H,69H,6DH,70H,6CH
	;DEFB 65H,20H,6CH,61H,64H,6FH,20H,28H,33H,36H,30H,20H,4BH,62H,29H,0DH
	;DEFB 0AH,32H,29H,20H,33H,2EH,35H,22H,20H,20H,44H,6FH,62H,6CH,65H,20H
	;DEFB 6CH,61H,64H,6FH,20H,20H,28H,37H,32H,30H,20H,4BH,62H,29H,0DH,0AH
	;DEFB 33H,29H,20H,35H,2EH,32H,35H,22H,20H,44H,6FH,62H,6CH,65H,20H,6CH
	;DEFB 61H,64H,6FH,20H,20H,28H,33H,36H,30H,20H,4BH,62H,29H,0DH,0AH,0DH
	;DEFB 0AH,00H
	
	;Orignal Spanish Text
	;DEFB 0DH,0AH,'1) 3.5"  Simple lado (360 Kb)'
	;DEFB 0DH,0AH,'2) 3.5"  Doble lado  (720 Kb)'
	;DEFB 0DH,0AH,'3) 5.25" Doble lado  (360 Kb)',0DH,0AH,0DH,0AH,00H
	
	;Suggest English Modification
	DEFB 0DH,0AH,'1) 3.5"  Single Sided(360 Kb)'
	DEFB 0DH,0AH,'2) 3.5"  Double Sided(720 Kb)'
	DEFB 0DH,0AH,'3) 5.25" Double Sided(360 Kb)',0DH,0AH,0DH,0AH,00H

;--------------------------------------------






;	  Subroutine DSKFMT
;	     Inputs  ________________________
;	     Outputs ________________________

DSKFMT:
        CALL DISINT
        DI
        CALL ENAFDC			; Enable FDC on page 0
J$7CA5	EQU $-1				; Equates way down here?!?!
        DEC A
        JR Z,J.7CAF
        BIT 0,A
        JR NZ,J.7CAF
J$7CAC	EQU $-1
        LD A,05H	; 5 
J.7CAF:
		ADD A,0F8H
        LD	C,A
J$7CB2:
		PUSH AF
        LD A,D
        CP 02H	; 2 
J$7CB6:
		JR C,J$7CBE
;
        POP AF
        LD A,0CH	; 12 
        JP J.7D8D
		
J$7CBC	EQU $-2

;
;	-----------------
J$7CBE:
		LD DE,0
        CALL C.76E4
;
        POP BC
        LD A,06H	; 6 
J$7CC7:
		JP C,J.7D8D
;
        LD A,B
        LD (IX+13),A
        LD A,01H	; 1 
        EX AF,AF'
J.7CD1:
		PUSH DE
        PUSH IX
        POP HL
        LD BC,0007H
        ADD HL,BC
        PUSH HL
J$7CDA:
		LD C,01H	; 1 
        LD A,4DH	; "M"
        CALL C.785B
;
        LD A,D
        CALL C.785B
J$7CE3	EQU	$-2
;
        LD A,02H	; 2 
        CALL C.785B
;
        LD A,09H	; 9 
        CALL C.785B
;
        LD A,52H	; "R"
        CALL C.785B
;
        LD A,0E5H
        LD DE,0
J$7CF9:
		CALL C.785B
;
J$7CFC:
		LD B,04H	; 4 
        EX AF,AF'
        LD (IX+9),A
        INC A
        EX AF,AF'
        POP HL
        PUSH HL
J.7D06:
		LD A,(DE)
        RLA
        JR NC,J.7D06
;
        AND 40H	; "@"
        JR Z,J$7D17
;
        LD A,(HL)
        INC E
        LD (DE),A
        DEC E
        INC HL
        DJNZ J.7D06
;
        JR J$7CFC
;
;	-----------------
J$7D17:
		POP HL
        POP DE
        CALL C.786A
;
        BIT 1,(IX+15)
        JR NZ,J$7D8B
;
        BIT 2,D
        JR NZ,J.7D3F
;
        LD A,(IX+13)
        CP 0F8H
        JR Z,J.7D3F
;
        SET 2,D
        LD (IX+6),D
        LD (IX+8),01H	; 1 
        LD (IX+9),01H	; 1 
        LD A,01H	; 1 
        EX AF,AF'
        JR J.7CD1
;
;	-----------------
J.7D3F:
		LD C,27H	; "'"
        LD A,(IX+13)
        CP 0FDH
        JR Z,J$7D4A
;
        LD C,4FH	; "O"
J$7D4A:
		LD A,(IX+7)
        CP C
        JR Z,J$7D91
;
        INC A
        LD (IX+7),A
        IN A,(0AAH)
        AND 0F0H
        ADD A,07H	; 7 
        OUT (0AAH),A
        IN A,(0A9H)
        AND 10H	; 16 
        JR NZ,J$7D6D
;
        IN A,(0AAH)
        DEC A
        OUT (0AAH),A
        IN A,(0A9H)
        AND 02H	; 2 
        JR Z,J$7D87
;
J$7D6D:
		RES 2,D
        LD (IX+6),D
        LD (IX+8),00H
        LD (IX+9),01H	; 1 
        CALL C.77E2
;
        LD A,01H	; 1 
        EX AF,AF'
        JP NC,J.7CD1
;
J$7D83:
		LD A,06H	; 6 
        JR J.7D8D
;
;	-----------------
J$7D87:
		LD A,0AH	; 10 
        JR J.7D8D
;
;	-----------------
J$7D8B:
		LD A,00H
J.7D8D:
		SCF
        JP J.753B
;
;	-----------------
J$7D91:
		XOR A
        LD (IX+7),A
        RES 2,D
        LD (IX+6),D
        LD (IX+8),00H
        LD (IX+9),01H	; 1 
        CALL C.77E2
        JR C,J$7D83
        CALL GETWRK
        LD A,(IX+24)		; saved slotid on page 0
        CALL SSLTID			; Set slotid on page 0
        LD HL,(SECBUF)
        PUSH HL
        PUSH HL
        POP DE
        INC DE
        LD (HL),00H
        LD BC,0200H
        LDIR
        LD HL,I$7E92
        POP DE
        LD BC,00DBH
        LDIR
        LD A,(IX+13)
        LD IY,(SECBUF)
        LD (IY+21),A
        CP 0F8H
        JR NZ,J$7DDB
        LD (IY+26),01H	; 1 
        JR J.7DEB
;
;	-----------------
J$7DDB:
		CP 0F9H
        JR NZ,J.7DEB
        LD (IY+22),03H	; 3 
        LD (IY+19),0A0H
        LD (IY+20),05H	; 5 
J.7DEB:
		LD HL,(SECBUF)
        LD DE,0
        LD B,01H	; 1 
        LD C,A
        LD A,(IX+6)
        AND 03H	; 3 
        SCF
        PUSH IX
        CALL DSKIO
;
        POP IX
        JP C,J.7E7B
;
        LD HL,(SECBUF)
        PUSH HL
        POP IY
        LD A,(IY+16)
        LD B,00H
J$7E0F:
		LD (HL),00H
        INC HL
        DJNZ J$7E0F
;
        ADD A,A
        ADD A,07H	; 7 
        LD B,A
        LD DE,1
J$7E1B:
		LD A,(IX+13)
        PUSH BC
        PUSH DE
        LD HL,(SECBUF)
        LD B,01H	; 1 
        LD C,A
        LD A,(IX+6)
        AND 03H	; 3 
        SCF
        PUSH IX
        CALL DSKIO
;
        POP IX
        POP DE
        POP BC
        JP C,J.7E7B
;
        INC DE
        DJNZ J$7E1B
;
        LD HL,(SECBUF)
        LD A,(IX+13)
        LD (HL),A
        INC HL
        LD (HL),0FFH
        INC HL
        LD (HL),0FFH
        LD HL,(SECBUF)
        LD B,01H	; 1 
        LD C,A
        LD DE,1
        LD A,(IX+6)
        AND 03H	; 3 
        SCF
        PUSH IX
        CALL DSKIO
;
        POP IX
        JP C,J.7E7B
;
        LD HL,(SECBUF)
        LD A,(IX+13)
        LD B,01H	; 1 
        LD C,A
        LD DE,2
        CP 0F9H
        JR NZ,J$7E72
;
        INC DE
J$7E72:
		SCF
        LD A,(IX+6)
        AND 03H	; 3 
        JP DSKIO
;
;	-----------------
J.7E7B:
		PUSH AF
        LD C,00H
        LD (IX),0C8H
        LD A,(IX+13)
        AND A
        JR NZ,J$7E8D
;
        LD (IX+1),C
        POP AF
        RET
;
;	-----------------
J$7E8D:
		LD (IX+2),C
        POP AF
        RET
;
;	-----------------
I$7E92:
		EX DE,HL
        CP 90H

;Data String = TALENT
        
		DEFB "TALENT.1",00H

;ID STRING??? = 00H,02H,02H,01H,00H,02H,70H,00H,D0H,02H,FDH,02H,00H,09H,00H,02H,00H,00H,00H
;-----------------------
        LD (BC),A
        LD (BC),A
        LD BC,0200H
        LD (HL),B
        NOP
        RET NC
;
        LD (BC),A
        DEFB 0FDH		; << Illegal Op Code Byte >>

        LD (BC),A
        NOP
        ADD HL,BC
        NOP
        LD (BC),A
        DEFB 0,0,0
;-----------------------
		

        RET NC
;
        LD (0C059H),DE
        LD (0C0DAH),A
        LD (HL),56H	; "V"
        INC HL
        LD (HL),0C0H
J$7EBD:
		LD SP,F51FH
        LD DE,C0B5H
        LD C,0FH				;BDOS CALL TO OPEN FILE (FCB)
        CALL ROMBDOS
        INC A
        JP Z,0C063H
        LD DE,0100H
        LD C,1AH				;BDOS CALL TO SET DISK TRANSFER ADDRESS
        CALL ROMBDOS
        LD HL,1
        LD (0C0C3H),HL
        LD HL,04000H-0100H
        LD DE,C0B5H
        LD C,27H				;BDOS CALL TO RANDOM BLOCK READ (FCB)
        CALL ROMBDOS
        JP 0100H
;
;	-----------------
?.7EE8:
		LD E,B
        RET NZ
        CALL 0
        LD A,C
        AND 0FEH
        CP 02H	; 2 
        JP NZ,J$C06A
;
        LD A,(0C0DAH)
        AND A
        JP Z,4022H
;
        LD DE,C08FH
        CALL 0C081H
;
        LD C,07H				;BDOS CALL TO DIRECT CONSOLD INPUT
        CALL ROMBDOS
;
        JR J$7EBD
;
;	-----------------
?.7F09:
		CP C
        RES 6,A
        CALL 0F2F6H
;
        CALL PE,0F8FDH
;
        CP C
J$7F13:
		LD A,(DE)
        OR A
        RET Z
;
        PUSH DE
        LD E,A
        LD C,06H				;BDOS CALL TO DIRECT CONSOLE IO
        CALL ROMBDOS
;
        POP DE
        INC DE
        JR J$7F13
;
;	-----------------
?.7F21:	

;Text data
;------------------------------------------------------
        DEFB 'Boot error',0DH,0AH
		DEFB 'Press any key for retry',0DH,0AH,00H,00H
		DEFB 'MSXDOS  SYS',00H
;------------------------------------------------------

;ROM PADDING
;------------------------------------------------------
        DEFB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DEFB	0,0,0,0,0
;------------------------------------------------------

;	  Subroutine OEMSTA
;	     Inputs  ________________________
;	     Outputs ________________________

		ORG 7F6DH
OEMSTA:
        SCF
        RET
