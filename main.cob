>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. BANKPROCESSOR.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT INPUT-FILE ASSIGN TO "input.csv"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 INPUT-LINE                     PIC X(200).

WORKING-STORAGE SECTION.
01 EOF-FLAG                       PIC X VALUE "N".
01 IS-FIRST-LINE                  PIC X VALUE "Y".

01 WS-ACCOUNT-ID-TEXT             PIC X(20).
01 WS-AMOUNT-TEXT                 PIC X(20).
01 WS-TYPE                        PIC X(20).

01 WS-ACCOUNT-ID                  PIC 9(5) VALUE 0.
01 WS-AMOUNT                      PIC 9(7) VALUE 0.

01 WS-ACCOUNT-COUNT               PIC 9(3) VALUE 0.
01 WS-SEARCH-INDEX                PIC 9(3) VALUE 0.
01 WS-FOUND-INDEX                 PIC 9(3) VALUE 0.
01 WS-FOUND-FLAG                  PIC X VALUE "N".

01 WS-LINE-COUNT                  PIC 9(5) VALUE 0.
01 WS-PROCESSED-COUNT             PIC 9(5) VALUE 0.
01 WS-ERROR-COUNT                 PIC 9(5) VALUE 0.
01 WS-IS-VALID                    PIC X VALUE "Y".
01 WS-ERROR-MESSAGE               PIC X(100).

01 WS-DEPOSIT-COUNT               PIC 9(5) VALUE 0.
01 WS-WITHDRAW-COUNT              PIC 9(5) VALUE 0.
01 WS-TOTAL-DEPOSIT               PIC 9(9) VALUE 0.
01 WS-TOTAL-WITHDRAW              PIC 9(9) VALUE 0.

01 WS-ACCOUNTS.
   05 WS-ACCOUNT-ENTRY OCCURS 100 TIMES.
      10 WS-STORED-ACCOUNT-ID     PIC 9(5).
      10 WS-STORED-BALANCE        PIC S9(9) VALUE 0.

PROCEDURE DIVISION.
    OPEN INPUT INPUT-FILE

    PERFORM UNTIL EOF-FLAG = "Y"
        READ INPUT-FILE
            AT END
                MOVE "Y" TO EOF-FLAG
            NOT AT END
                IF IS-FIRST-LINE = "Y"
                    MOVE "N" TO IS-FIRST-LINE
                ELSE
                    ADD 1 TO WS-LINE-COUNT
                    PERFORM PROCESS-LINE
                END-IF
        END-READ
    END-PERFORM

    CLOSE INPUT-FILE

    PERFORM DISPLAY-REPORT

    STOP RUN.

PROCESS-LINE.
    MOVE "Y" TO WS-IS-VALID
    MOVE SPACES TO WS-ERROR-MESSAGE

    MOVE SPACES TO WS-ACCOUNT-ID-TEXT
    MOVE SPACES TO WS-AMOUNT-TEXT
    MOVE SPACES TO WS-TYPE
    MOVE 0 TO WS-ACCOUNT-ID
    MOVE 0 TO WS-AMOUNT

    IF WS-IS-VALID = "Y"
        UNSTRING INPUT-LINE
            DELIMITED BY ","
            INTO WS-ACCOUNT-ID-TEXT
                 WS-AMOUNT-TEXT
                 WS-TYPE
        END-UNSTRING
    END-IF

    IF WS-IS-VALID = "Y"
        IF FUNCTION TRIM(WS-ACCOUNT-ID-TEXT) = ""
            MOVE "N" TO WS-IS-VALID
            MOVE "account_id manquant" TO WS-ERROR-MESSAGE
        END-IF
    END-IF

    IF WS-IS-VALID = "Y"
        IF FUNCTION TRIM(WS-AMOUNT-TEXT) = ""
            MOVE "N" TO WS-IS-VALID
            MOVE "amount manquant" TO WS-ERROR-MESSAGE
        END-IF
    END-IF

    IF WS-IS-VALID = "Y"
        IF FUNCTION TRIM(WS-TYPE) = ""
            MOVE "N" TO WS-IS-VALID
            MOVE "type manquant" TO WS-ERROR-MESSAGE
        END-IF
    END-IF

    IF WS-IS-VALID = "Y"
        IF FUNCTION TRIM(WS-ACCOUNT-ID-TEXT) IS NUMERIC
            MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-ACCOUNT-ID-TEXT))
                TO WS-ACCOUNT-ID
        ELSE
            MOVE "N" TO WS-IS-VALID
            MOVE "account_id invalide" TO WS-ERROR-MESSAGE
        END-IF
    END-IF

    IF WS-IS-VALID = "Y"
        IF FUNCTION TRIM(WS-AMOUNT-TEXT) IS NUMERIC
            MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-AMOUNT-TEXT))
                TO WS-AMOUNT
        ELSE
            MOVE "N" TO WS-IS-VALID
            MOVE "amount invalide" TO WS-ERROR-MESSAGE
        END-IF
    END-IF

    IF WS-IS-VALID = "Y"
        IF FUNCTION TRIM(WS-TYPE) NOT = "deposit"
           AND FUNCTION TRIM(WS-TYPE) NOT = "withdraw"
            MOVE "N" TO WS-IS-VALID
            MOVE "type invalide" TO WS-ERROR-MESSAGE
        END-IF
    END-IF

    IF WS-IS-VALID = "Y"
        PERFORM FIND-OR-CREATE-ACCOUNT
    END-IF

    IF WS-IS-VALID = "Y"
        IF FUNCTION TRIM(WS-TYPE) = "deposit"
            ADD WS-AMOUNT TO WS-STORED-BALANCE(WS-FOUND-INDEX)
            ADD 1 TO WS-DEPOSIT-COUNT
            ADD WS-AMOUNT TO WS-TOTAL-DEPOSIT
        ELSE
            SUBTRACT WS-AMOUNT FROM WS-STORED-BALANCE(WS-FOUND-INDEX)
            ADD 1 TO WS-WITHDRAW-COUNT
            ADD WS-AMOUNT TO WS-TOTAL-WITHDRAW
        END-IF

        ADD 1 TO WS-PROCESSED-COUNT

        DISPLAY "OK   | Compte: " WS-ACCOUNT-ID
                " | Montant: " WS-AMOUNT
                " | Type: " FUNCTION TRIM(WS-TYPE)
    ELSE
        ADD 1 TO WS-ERROR-COUNT
        DISPLAY "ERR  | Ligne " WS-LINE-COUNT
                " | " FUNCTION TRIM(WS-ERROR-MESSAGE)
                " | Contenu: " FUNCTION TRIM(INPUT-LINE)
    END-IF.

FIND-OR-CREATE-ACCOUNT.
    MOVE "N" TO WS-FOUND-FLAG
    MOVE 0 TO WS-FOUND-INDEX

    PERFORM VARYING WS-SEARCH-INDEX FROM 1 BY 1
        UNTIL WS-SEARCH-INDEX > WS-ACCOUNT-COUNT OR WS-FOUND-FLAG = "Y"
        IF WS-STORED-ACCOUNT-ID(WS-SEARCH-INDEX) = WS-ACCOUNT-ID
            MOVE "Y" TO WS-FOUND-FLAG
            MOVE WS-SEARCH-INDEX TO WS-FOUND-INDEX
        END-IF
    END-PERFORM

    IF WS-FOUND-FLAG = "N"
        IF WS-ACCOUNT-COUNT >= 100
            MOVE "N" TO WS-IS-VALID
            MOVE "limite de comptes atteinte" TO WS-ERROR-MESSAGE
        ELSE
            ADD 1 TO WS-ACCOUNT-COUNT
            MOVE WS-ACCOUNT-ID TO WS-STORED-ACCOUNT-ID(WS-ACCOUNT-COUNT)
            MOVE 0 TO WS-STORED-BALANCE(WS-ACCOUNT-COUNT)
            MOVE WS-ACCOUNT-COUNT TO WS-FOUND-INDEX
        END-IF
    END-IF.

DISPLAY-REPORT.
    DISPLAY "========================================"
    DISPLAY "           RAPPORT FINAL"
    DISPLAY "========================================"
    DISPLAY "Lignes lues         : " WS-LINE-COUNT
    DISPLAY "Transactions OK     : " WS-PROCESSED-COUNT
    DISPLAY "Erreurs             : " WS-ERROR-COUNT
    DISPLAY "Comptes distincts   : " WS-ACCOUNT-COUNT
    DISPLAY "Depots              : " WS-DEPOSIT-COUNT
    DISPLAY "Retraits            : " WS-WITHDRAW-COUNT
    DISPLAY "Total depose        : " WS-TOTAL-DEPOSIT
    DISPLAY "Total retire        : " WS-TOTAL-WITHDRAW
    DISPLAY "----------------------------------------"
    DISPLAY "SOLDES PAR COMPTE"
    DISPLAY "----------------------------------------"

    PERFORM VARYING WS-SEARCH-INDEX FROM 1 BY 1
        UNTIL WS-SEARCH-INDEX > WS-ACCOUNT-COUNT
        DISPLAY "Compte "
                WS-STORED-ACCOUNT-ID(WS-SEARCH-INDEX)
                " | Solde final : "
                WS-STORED-BALANCE(WS-SEARCH-INDEX)
    END-PERFORM

    DISPLAY "========================================".