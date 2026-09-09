       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUN-TESTS.
      *> Test runner for the InCollege suite, written in COBOL.
      *> For each inputs/<name>-Input.txt it runs the program on that input
      *> and compares the output to expected/<name>.txt, printing PASS/FAIL.
      *> Trailing spaces and trailing blank lines are ignored in the compare.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LIST-FILE ASSIGN TO "testlist.tmp"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS LIST-ST.
           SELECT CMP-FILE ASSIGN TO DYNAMIC CMP-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS CMP-ST.

       DATA DIVISION.
       FILE SECTION.
       FD LIST-FILE.
       01 LIST-REC PIC X(200).
       FD CMP-FILE.
       01 CMP-REC PIC X(200).

       WORKING-STORAGE SECTION.
       01 CMP-NAME        PIC X(200).
       01 LIST-ST         PIC XX.
       01 CMP-ST          PIC XX.
       01 CMD             PIC X(400).
       01 FULL-PATH       PIC X(200).
       01 NAME-PART       PIC X(200).
       01 EXP-NAME        PIC X(200).
       01 PLEN            PIC 9(4).
       01 NLEN            PIC 9(4).
       01 PH-CNT          PIC 9(4).
       01 PHA-CNT         PIC 9(4).
       01 EOF-LIST        PIC 9 VALUE 0.
       01 EOF-CMP         PIC 9 VALUE 0.
       01 MISS-FLAG       PIC 9 VALUE 0.
       01 I               PIC 9(4).
       01 PASS-CNT        PIC 9(4) VALUE 0.
       01 FAIL-CNT        PIC 9(4) VALUE 0.
       01 MISS-CNT        PIC 9(4) VALUE 0.
       01 GOT-N           PIC 9(4).
       01 EXP-N           PIC 9(4).
       01 MATCH-FLAG      PIC 9.
       01 PASS-DSP        PIC ZZZ9.
       01 FAIL-DSP        PIC ZZZ9.
       01 MISS-DSP        PIC ZZZ9.
       01 GOT-TABLE.
          05 GOT-LINE OCCURS 600 TIMES PIC X(200).
       01 EXP-TABLE.
          05 EXP-LINE OCCURS 600 TIMES PIC X(200).

       PROCEDURE DIVISION.
       MAIN.
      *> build the program under test (ignore errors; a bad build just
      *> makes every test fail, which is the correct signal)
           MOVE "cobc -x InCollege.cbl -o incollege 2>/dev/null" TO CMD
           CALL "SYSTEM" USING CMD
           CALL "SYSTEM" USING "rm -rf results; mkdir -p results"
           MOVE "ls -1 inputs/*-Input.txt | sort > testlist.tmp" TO CMD
           CALL "SYSTEM" USING CMD

           OPEN INPUT LIST-FILE
           PERFORM UNTIL EOF-LIST = 1
               READ LIST-FILE
                   AT END MOVE 1 TO EOF-LIST
                   NOT AT END
                       MOVE LIST-REC TO FULL-PATH
                       PERFORM RUN-ONE
               END-READ
           END-PERFORM
           CLOSE LIST-FILE

           CALL "SYSTEM" USING "rm -f testlist.tmp .got.tmp"
           MOVE PASS-CNT TO PASS-DSP
           MOVE FAIL-CNT TO FAIL-DSP
           MOVE MISS-CNT TO MISS-DSP
           DISPLAY " "
           DISPLAY "SCORE: " FUNCTION TRIM(PASS-DSP) " passed, "
               FUNCTION TRIM(FAIL-DSP) " failed, "
               FUNCTION TRIM(MISS-DSP) " missing expected"
           DISPLAY "Diffs of failures are in ./results/ (<name>.diff)."
           STOP RUN.

       RUN-ONE.
           MOVE FUNCTION TRIM(FULL-PATH) TO FULL-PATH
           COMPUTE PLEN = FUNCTION LENGTH(FUNCTION TRIM(FULL-PATH))
      *> name = path without leading "inputs/" (7) and trailing
      *> "-Input.txt" (10)
           COMPUTE NLEN = PLEN - 17
           MOVE SPACES TO NAME-PART
           MOVE FULL-PATH(8:NLEN) TO NAME-PART
           MOVE SPACES TO EXP-NAME
           STRING "expected/" DELIMITED SIZE
                  FUNCTION TRIM(NAME-PART) DELIMITED SIZE
                  ".txt" DELIMITED SIZE
               INTO EXP-NAME

      *> reset the account store before a normal test or a PhaseA
           MOVE 0 TO PH-CNT
           MOVE 0 TO PHA-CNT
           INSPECT NAME-PART TALLYING PH-CNT FOR ALL "-Phase"
           INSPECT NAME-PART TALLYING PHA-CNT FOR ALL "-PhaseA"
           IF PH-CNT = 0 OR PHA-CNT > 0
               CALL "SYSTEM" USING "rm -f accounts.txt"
           END-IF

      *> load this test's input, clear old output, run the program
           MOVE SPACES TO CMD
           STRING "cp " DELIMITED SIZE
                  FUNCTION TRIM(FULL-PATH) DELIMITED SIZE
                  " InCollege-Input.txt" DELIMITED SIZE
               INTO CMD
           CALL "SYSTEM" USING CMD
           CALL "SYSTEM" USING "rm -f InCollege-Output.txt"
           CALL "SYSTEM" USING "timeout 10 ./incollege > .got.tmp 2>&1"

           PERFORM LOAD-GOT
           PERFORM LOAD-EXP
           PERFORM COMPARE.

       LOAD-GOT.
           MOVE ".got.tmp" TO CMP-NAME
           MOVE 0 TO GOT-N
           MOVE 0 TO EOF-CMP
           OPEN INPUT CMP-FILE
           PERFORM UNTIL EOF-CMP = 1
               READ CMP-FILE
                   AT END MOVE 1 TO EOF-CMP
                   NOT AT END
      *> cap at table size so a runaway/looping program can't
      *> overwrite past GOT-LINE(600); a frozen count vs the
      *> expected count is still the correct FAIL signal
                       IF GOT-N < 600
                           ADD 1 TO GOT-N
                           MOVE CMP-REC TO GOT-LINE(GOT-N)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CMP-FILE
           PERFORM UNTIL GOT-N <= 0 OR GOT-LINE(GOT-N) NOT = SPACES
               SUBTRACT 1 FROM GOT-N
           END-PERFORM.

       LOAD-EXP.
           MOVE EXP-NAME TO CMP-NAME
           MOVE 0 TO EXP-N
           MOVE 0 TO EOF-CMP
           MOVE 0 TO MISS-FLAG
           OPEN INPUT CMP-FILE
           IF CMP-ST NOT = "00"
               MOVE 1 TO MISS-FLAG
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL EOF-CMP = 1
               READ CMP-FILE
                   AT END MOVE 1 TO EOF-CMP
                   NOT AT END
                       IF EXP-N < 600
                           ADD 1 TO EXP-N
                           MOVE CMP-REC TO EXP-LINE(EXP-N)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CMP-FILE
           PERFORM UNTIL EXP-N <= 0 OR EXP-LINE(EXP-N) NOT = SPACES
               SUBTRACT 1 FROM EXP-N
           END-PERFORM.

       COMPARE.
           IF MISS-FLAG = 1
               ADD 1 TO MISS-CNT
               DISPLAY "NO EXPECTED  " FUNCTION TRIM(NAME-PART)
               EXIT PARAGRAPH
           END-IF
           MOVE 1 TO MATCH-FLAG
           IF GOT-N NOT = EXP-N
               MOVE 0 TO MATCH-FLAG
           ELSE
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > GOT-N
                   IF GOT-LINE(I) NOT = EXP-LINE(I)
                       MOVE 0 TO MATCH-FLAG
                   END-IF
               END-PERFORM
           END-IF
           IF MATCH-FLAG = 1
               ADD 1 TO PASS-CNT
               DISPLAY "PASS  " FUNCTION TRIM(NAME-PART)
           ELSE
               ADD 1 TO FAIL-CNT
               DISPLAY "FAIL  " FUNCTION TRIM(NAME-PART)
               MOVE SPACES TO CMD
               STRING "diff '" DELIMITED SIZE
                      "expected/" DELIMITED SIZE
                      FUNCTION TRIM(NAME-PART) DELIMITED SIZE
                      ".txt' .got.tmp > 'results/" DELIMITED SIZE
                      FUNCTION TRIM(NAME-PART) DELIMITED SIZE
                      ".diff'" DELIMITED SIZE
                   INTO CMD
               CALL "SYSTEM" USING CMD
           END-IF.
