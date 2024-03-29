       IDENTIFICATION DIVISION.
       PROGRAM-ID. SaveTransaction.
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
       LINKAGE SECTION.
       01 Montant PIC 9(4)V99.
       01 Action PIC X(10).

       PROCEDURE DIVISION USING Montant,Action.
           OPEN EXTEND HistoriqueFile
           MOVE Action TO Act
           MOVE Montant TO Mont
           WRITE TransactionRecord AFTER ADVANCING PAGE
           CLOSE HistoriqueFile.
