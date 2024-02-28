IDENTIFICATION DIVISION.
PROGRAM-ID. GestionBancaire.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT SoldeFile ASSIGN TO 'solde.txt'
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS SoldeStatus.
    SELECT HistoriqueFile ASSIGN TO 'historique.txt'
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS HistStatus.

DATA DIVISION.
FILE SECTION.
FD SoldeFile.
01 SoldeRecord USAGE IS DISPLAY.
   05 Sol        PIC 9(4).9(2).
   05 Delimite   PIC X VALUE "/".
   05 SolSec      PIC 9(4).9(2).

FD HistoriqueFile.
01 TransactionRecord USAGE IS DISPLAY.
   05 Act        PIC X(10).
   05 Mont   PIC 9(4).9(2).

WORKING-STORAGE SECTION.
01 Solde PIC 9(4)V99. 
01 Montant PIC 9(4)V99.
01 SoldeSec PIC 9(4)V99. 
01 CHOIX PIC 9.
01 Action PIC X(10).
01 HistStatus PIC X(2).
01 SoldeStatus PIC X(2).

PROCEDURE DIVISION.
INCLUDE ExistSolde.cob.
INCLUDE ExistHistorique.cob.
INCLUDE ReadSolde.cob.
INCLUDE SaveTransaction.cob.
INCLUDE SaveSolde.cob.

    CALL 'ExistSolde'
    CALL 'ExistHistorique'
    CALL 'ReadSolde'
    DISPLAY "Gestion bancaire"
    PERFORM MENU-BANQUE UNTIL CHOIX = 5
    CALL 'SaveSolde'
    DISPLAY "Exit "
    STOP RUN.

MENU-BANQUE.
    DISPLAY "Rentrez : "
    DISPLAY "1. Dépôt"
    DISPLAY "2. Retrait"
    DISPLAY "3. Virement"
    DISPLAY "4. Solde"
    DISPLAY "5. Quitter"
    ACCEPT CHOIX
    EVALUATE CHOIX
        WHEN 1
            PERFORM DEPOT
        WHEN 2
            PERFORM RETRAIT
        WHEN 3
            PERFORM VIREMENT
        WHEN 4
            PERFORM AFF-SOLDE
        WHEN 5
            EXIT 
        WHEN GREATER THAN 5
            DISPLAY "Invalide, Saisir un numéro entre 1 et 5 "
    END-EVALUATE.

DEPOT.
    DISPLAY "Entrez le montant à déposer :"
    ACCEPT Montant
    COMPUTE Solde = Solde + Montant
    MOVE "Dépot" TO Action
    CALL 'SaveTransaction'
    DISPLAY "Dépôt effectué".

RETRAIT.
    DISPLAY "Entrez le montant :"
    ACCEPT Montant
    IF Montant > Solde THEN
        DISPLAY "Insuffisant, Opération annulée"
    ELSE
        COMPUTE Solde = Solde - Montant
        MOVE "Retrait" TO Action
        CALL 'SaveTransaction'
        DISPLAY "Retrait effectué"
    END-IF.

VIREMENT.
    DISPLAY "Entrez le montant :"
    ACCEPT Montant
    IF Montant > Solde THEN
        DISPLAY "Insuffisant, Opération annulée"
    ELSE
        COMPUTE Solde = Solde - Montant
        COMPUTE SoldeSec = SoldeSec + Montant
        MOVE "Virement" TO Action
        CALL 'SaveTransaction'
        DISPLAY "Virement effectué"
    END-IF.

AFF-SOLDE.
    DISPLAY "Solde actuel : ", Solde, " €".

*> READ-SOLDE.
*>     READ SoldeFile INTO SoldeRecord
*>     CLOSE SoldeFile
*>     MOVE Sol TO Solde
*>     MOVE SolSec TO SoldeSec.

*> SAVE-TRANSACTION.
*>     OPEN EXTEND HistoriqueFile
*>     MOVE Action TO Act
*>     MOVE Montant TO Mont
*>     WRITE TransactionRecord AFTER ADVANCING PAGE
*>     CLOSE HistoriqueFile.

*> SAVE-SOLDE.
*>     OPEN OUTPUT SoldeFile
*>     MOVE Solde TO Sol
*>     MOVE SoldeSec TO SolSec
*>     WRITE SoldeRecord 
*>     CLOSE SoldeFile.

*> EXIST-SOLDE.
*>     OPEN INPUT SoldeFile
*>     display SoldeStatus
*>     IF SoldeStatus = '35'
*>         DISPLAY "Solde.txt does not exist ", SoldeStatus
*>         CLOSE SoldeFile
*>         OPEN OUTPUT SoldeFile
*>         MOVE 1000 TO Solde
*>         MOVE 500 TO SoldeSec
*>     ELSE
*>         CLOSE SoldeFile
*>     END-IF.

*> EXIST-HISTORIQUE.
*>     OPEN INPUT HistoriqueFile
*>     display HistStatus
*>     IF HistStatus = '35'
*>         DISPLAY "Historique.txt does not exist ", HistStatus
*>         CLOSE HistoriqueFile
*>         OPEN OUTPUT HistoriqueFile
*>     ELSE
*>         CLOSE HistoriqueFile
*>     END-IF.

END PROGRAM GestionBancaire.
