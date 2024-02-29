       IDENTIFICATION DIVISION.
       PROGRAM-ID. GestionBancaire.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Solde PIC 9(4)V99. 
       01 Montant PIC 9(4)V99.
       01 SoldeSec PIC 9(4)V99. 
       01 CHOIX PIC 9.
       01 Action PIC X(10).

       PROCEDURE DIVISION.

           CALL 'ExistSolde' USING Solde, SoldeSec
           CALL 'ExistHistorique'
           CALL 'ReadSolde' USING Solde,SoldeSec
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
           CALL 'SaveTransaction' USING Montant,Action
           DISPLAY "Dépôt effectué".

       RETRAIT.
           DISPLAY "Entrez le montant :"
           ACCEPT Montant
           IF Montant > Solde THEN
               DISPLAY "Insuffisant, Opération annulée"
           ELSE
               COMPUTE Solde = Solde - Montant
               MOVE "Retrait" TO Action
               CALL 'SaveTransaction' USING Montant,Action
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
               CALL 'SaveTransaction' USING Montant,Action
               DISPLAY "Virement effectué"
           END-IF.

       AFF-SOLDE.
           DISPLAY "Solde actuel : ", Solde, " €".
       END PROGRAM GestionBancaire.
