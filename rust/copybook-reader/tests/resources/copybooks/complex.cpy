      *************************************************************************
      ***** This copybook has a mix of features, comment styles, data types****
      *************************************************************************
       01  USER-RECORD.
           03  USER-KEY.
               05  USER-ID                 PIC 9(06)  VALUE ZERO.
               05  USER-RECORD-TYPE        PIC 9(02)  VALUE ZERO.
                   88  USER-CONTROL        VALUE 0.
                   88  USER-DELIVERIES     VALUE 3.
                   88  USER-PAYMENTS       VALUE 4.
               05  USER-RECORD-NUMBER      PIC 9(04)  VALUE ZERO.
           03  USER-DATA                   PIC X(500) VALUE SPACE.
      *
       01  USER-CONTROL-RECORD.
           03  U-KEY.
               *> some comment describing the user id
               05  USER-ID             PIC 9(06)  VALUE ZERO.
               05  U-RECORD-TYPE            PIC 9(02)  VALUE ZERO.
               05  U-RECORD-NUMBER          PIC 9(04)  VALUE ZERO.
           03  U-DATA                       PIC X(500) VALUE SPACE.
           03  FILLER                         REDEFINES U-DATA.
               05  U-MIN-USER-ID         PIC 9(04).
               05  U-MAX-USER-ID         PIC 9(04).
      * Here is a descriptive comment
       01  USER-DELIVERY-RECORD.
           03  U-U-KEY.
               05  U-U-USER-ID             PIC 9(06)  VALUE ZERO.
               05  U-U-RECORD-TYPE            PIC 9(02)  VALUE 3.
               05  U-U-RECORD-NUMBER          PIC 9(04)  VALUE ZERO.
           03  U-U-DATA                       PIC X(500) VALUE SPACE.
           03  FILLER                         REDEFINES U-U-DATA.
               05  U-U-ENTRY                  OCCURS 20.
                   07  U-U-DATE-OF-DELIVERY   PIC 9(06).
                   07  U-U-TECHNICIAN         PIC 9(04).
                   07  U-U-GALLONS            PIC 9(04).
                   07  U-U-UNIT-PRICE         PIC 9(03). *> inline field comment.
      *> differently formatted comment
       01  USER-PAYMENT-RECORD.
           03  U-P-KEY.
               05  U-P-USER-ID             PIC 9(06)  VALUE ZERO.
               05  U-P-RECORD-TYPE            PIC 9(02)  VALUE 4.
               05  U-P-RECORD-NUMBER          PIC 9(04)  VALUE ZERO.
           03  U-P-DATA                       PIC X(500) VALUE SPACE.
           03  FILLER                         REDEFINES U-P-DATA.
               05  U-P-ENTRY                  OCCURS 20.
                   07  U-P-DATE-OF-PAYMENT    PIC 9(06).
                   07  U-P-AMOUNT             PIC 9(06).
      *
       01  ACCTFILE-LENGTH                    PIC 9(04) COMP VALUE 512.
       01  ACCTFILE-KEYLENGTH                 PIC 9(04) COMP VALUE 12.
       01  ACCTFILE-RESP                      PIC 9(08) COMP VALUE ZERO.
       01  ACCTFILE-RESP2                     PIC 9(08) COMP VALUE ZERO.
      *