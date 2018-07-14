\************************************************
\* Prepare the Big Code File
\* ELTcode
\************************************************

C%=&F40
L%=&1128
D%=&563A
ZP=&70

ORG &1100

.LBL
EQUB &6C
LDX #&60
LDA #&B
STA ZP+1
LDY #0
STY ZP
TYA
INY
.CHK3
CLC
ADC (ZP),Y
INY
BNE CHK3
INC ZP+1
.CHK4
CLC
ADC (ZP),Y
INY
BPL CHK4
CMP &B00
BEQ LBL+2
LDA #&7F
STA &FE4E
JMP (&FFFC)

.elitea
PRINT "elitea=",~P%
INCBIN "output/ELTA.bin"
.eliteb
PRINT "eliteb=",~P%
INCBIN "output/ELTB.bin"
.elitec
PRINT "elitec=",~P%
INCBIN "output/ELTC.bin"
.elited
PRINT "elited=",~P%
INCBIN "output/ELTD.bin"
.elitee
PRINT "elitee=",~P%
INCBIN "output/ELTE.bin"
.elitef
PRINT "elitef=",~P%
INCBIN "output/ELTF.bin"
.eliteg
PRINT "eliteg=",~P%
INCBIN "output/ELTG.bin"
.checksum0
PRINT "checksum0=",~P%
SKIP 1      ; byte skipped for checksum later
.ships
PRINT "ships=",~P%
INCBIN "data/SHIPS.bin"
.end

PRINT "P%=",~P%

\\ CHECKSUM PERFORMED IN BCFS.PY SCRIPT

PRINT "S.ELTcode 1100 ", ~(L%+&6000-C%), " ", ~L%, ~L%

\\ SAVE UNPROTECTED CODE

SAVE "output/ELTcode.unprot.bin", &1100, (L%+&6000-C%), L%

\\ SAVE JUST THE HEADER

SAVE "output/ELThead.bin", &1100, elitea, &1100
