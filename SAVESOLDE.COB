IDENTIFICATION DIVISION.
PROGRAM-ID. SaveSolde.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT SoldeFile ASSIGN TO 'solde.txt'
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS SoldeStatus.

DATA DIVISION.
FILE SECTION.
FD SoldeFile.
01 SoldeRecord USAGE IS DISPLAY.
   05 Sol        PIC 9(4).9(2).
   05 Delimite   PIC X VALUE "/".
   05 SolSec      PIC 9(4).9(2).

WORKING-STORAGE SECTION.
01 Solde PIC 9(4)V99. 
01 SoldeSec PIC 9(4)V99. 
01 SoldeStatus PIC X(2).

PROCEDURE DIVISION.
    OPEN OUTPUT SoldeFile
    MOVE Solde TO Sol
    MOVE SoldeSec TO SolSec
    WRITE SoldeRecord 
    CLOSE SoldeFile.
