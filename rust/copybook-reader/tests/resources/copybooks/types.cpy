       01 RECORD-R.
           05 NINE-NUMBER PIC 99.
           05 TWO-NINE-NUMBER PIC 9(2).
           05 ALPHA PIC aaa.
           05 ALPHA-PAREN PIC a(3).
           05 ALPHA-NUM PIC xxxx.
           05 ALPHA-NUM-PAREN PIC x(4).
           05 DECIMAL PIC 9(2)v9(3). *> can store .175
           05 SIGNED-NUM PIC s9(2). *> examples -76
           05 ASSUMED-DECIMAL PIC p9. *> .6
           05 PICTURE-ALPHA PIC X(10).
           05 PICTURE-CHAR PIC X.
           05 PICTURE-NUM PIC 9(2).
           05 PIC-NUM-COMP PIC 9(2) COMP.
           05 PIC-NUM-COMP-3 PIC 9(2) COMP-3.
           05 PIC-DECIMAL PIC 9(6)V9(2).
           05 EXPLICIT-DECIMAL PIC 9(6).9(2).
           05 PIC-DECIMAL PIC S9(6)V9(2).