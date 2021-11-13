;--------------------------------------------------------------------
;
;						SYMBOLS DEFINED BY DRIVER
;
;  AS DEFINED BY ROMINFO.BAS FROM THE ORIGINAL PROJECT:
;  https://sourceforge.net/p/msxsyssrc/git/ci/master/tree/disk100/
;
;--------------------------------------------------------------------

DSKIO		EQU			752EH
DSKCHG		EQU			7996H
GETDPB		EQU			79D5H
CHOICE		EQU			7C39H
DSKFMT		EQU			7C9FH
MTOFF		EQU			78F8H
INIHRD		EQU			78ABH
MYSIZE		EQU			25
DRIVES		EQU			7910H
DEFDPB		EQU			74AFH	;DEFAULT MEDIA F9
INIENV		EQU			794CH
OMESTA		EQU			7F6DH
SECLEN		EQU			0000H	;512 - ASSUMPTION THIS IS FAT12 SECTOR SIZE
								;OR A BUFFER POINTER?