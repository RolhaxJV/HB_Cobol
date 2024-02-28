IDENTIFICATION DIVISION.
PROGRAM-ID. ExistSolde.
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
    OPEN INPUT SoldeFile
    READ SoldeFile INTO SoldeRecord
    IF SoldeStatus = '35'
        DISPLAY "Solde.txt does not exist ", SoldeStatus
        CLOSE SoldeFile
        OPEN OUTPUT SoldeFile
        MOVE 1000 TO Solde
        MOVE 500 TO SoldeSec
    ELSE
        CLOSE SoldeFile
    END-IF.
