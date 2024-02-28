IDENTIFICATION DIVISION.
PROGRAM-ID. ExistHistorique.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT HistoriqueFile ASSIGN TO 'historique.txt'
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS HistStatus.

DATA DIVISION.
FILE SECTION.
FD HistoriqueFile.
01 TransactionRecord USAGE IS DISPLAY.
   05 Act        PIC X(10).
   05 Mont   PIC 9(4).9(2).

WORKING-STORAGE SECTION.
01 HistStatus PIC X(2).

PROCEDURE DIVISION.
    OPEN INPUT HistoriqueFile
    IF HistStatus = '35'
        DISPLAY "Historique.txt does not exist ", HistStatus
        CLOSE HistoriqueFile
        OPEN OUTPUT HistoriqueFile
    ELSE
        CLOSE HistoriqueFile
    END-IF.
