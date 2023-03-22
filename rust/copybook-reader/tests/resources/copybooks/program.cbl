       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT TRANSACTIONS ASSIGN TO 'output.txt'
                  *> Record Sequential is fixed length with no delimiter.
                  ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       *> create a file specification
       FD TRANSACTIONS.
       01 TRANSACTION-RECORD.
           02 UID PIC 9(5).
           02 DESC PIC X(25).
           02 DETAILS.
            03 AMOUNT PIC 9(6)V9(2).
            03 START-BALANCE PIC 9(6)V9(2).
            03 END-BALANCE PIC 9(6)V9(2).
           02 ACCOUNT-ID PIC 9(7).
           02 ACCOUNT-HOLDER PIC X(50).

       *> create a single record for insertion
       *> this has the same structure as the
       *> record above but with actual values
       WORKING-STORAGE SECTION.
       01 TRANSACTION-RECORD-VAL.
           02 UID PIC 9(5) VALUE 12345.
           02 DESC PIC X(25) VALUE 'TEST TRANSACTION'.
           02 DETAILS.
                03 AMOUNT PIC 9(6)V9(2) VALUE 000124.34.
                03 START-BALANCE PIC 9(6)V9(2) VALUE 000177.54.
                03 END-BALANCE PIC 9(6)V9(2) VALUE 53.2.
           02 ACCOUNT-ID PIC 9(7).
           02 ACCOUNT-HOLDER PIC X(50).

       PROCEDURE DIVISION.
           *> print the record we are writing
           DISPLAY 'WRITING RECORD: 'TRANSACTION-RECORD-VAL.
           *> open the file in output mode
           *> this will re-create the file
           OPEN OUTPUT TRANSACTIONS
            *> write 2 records to the file (they have the same values)
            WRITE TRANSACTION-RECORD FROM TRANSACTION-RECORD-VAL
            WRITE TRANSACTION-RECORD FROM TRANSACTION-RECORD-VAL
           *> close the file
           CLOSE TRANSACTIONS
           STOP RUN.
