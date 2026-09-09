       IDENTIFICATION DIVISION.
       PROGRAM-ID. IN-COLLEGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNTS-FILE ASSIGN TO "accounts.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(100).

       FD ACCOUNTS-FILE.
       01 ACCOUNT-RECORD.
           05 ACCOUNT-USERNAME PIC X(20).
           05 ACCOUNT-PASSWORD PIC X(20).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 OUT-LINE PIC X(100).
       01 IN-LINE PIC X(100).
       01 USERNAME PIC X(20).
       01 PASSWORD PIC X(20).
       01 CHOICE PIC X(1).
       01 FOUND-FLAG PIC 9 VALUE 0.
       01 FOUND-PASSWORD PIC X(20).
       01 ACCOUNT-COUNT PIC 99 VALUE 0.
       01 VALID-FLAG PIC 9 VALUE 0.
       01 SESSION-FLAG PIC 9 VALUE 1.
       01 SKILL-FLAG PIC 9 VALUE 0.
       01 LOGIN-FLAG PIC 9 VALUE 0.
       01 EOF-FLAG PIC 9 VALUE 0.
       01 PASS-LENGTH PIC 99.
       01 HAS-CAPITAL PIC 9 VALUE 0.
       01 HAS-DIGIT PIC 9 VALUE 0.
       01 HAS-SPECIAL PIC 9 VALUE 0.
       01 I PIC 999.
       01 CHAR-CODE PIC 999.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           PERFORM MENU-CYCLE.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.

       MENU-CYCLE.
           PERFORM UNTIL SESSION-FLAG = 0
               STRING "Welcome to InCollege!" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "1) Log In" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "2) Create New Account" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "Enter your choice:" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               PERFORM READ-INPUT
               MOVE IN-LINE TO CHOICE
               
               EVALUATE CHOICE
                   WHEN "1"
                       PERFORM LOGIN
                   WHEN "2"
                       PERFORM CREATE-ACCOUNT
               END-EVALUATE
           END-PERFORM.

       CREATE-ACCOUNT.
           PERFORM COUNT-ACCOUNTS.
           IF ACCOUNT-COUNT >= 5
               STRING "All permitted accounts have been created."
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF.

           STRING "Please enter your username:" 
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM WRITE-OUTPUT.
           PERFORM READ-INPUT.
           MOVE IN-LINE TO USERNAME.

           STRING "Please enter your password:" 
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM WRITE-OUTPUT.
           PERFORM READ-INPUT.
           MOVE IN-LINE TO PASSWORD.

           PERFORM FIND-USERNAME.
           IF FOUND-FLAG = 1
               STRING "Username is already taken." 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
           ELSE
               PERFORM VALIDATE-PASSWORD
               IF VALID-FLAG = 1
                   PERFORM SAVE-ACCOUNT
                   STRING "Account created successfully." 
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM WRITE-OUTPUT
               ELSE
                   STRING "Password rejected - must be 8-12 chars,"
                       " uppercase, digit, and special char."
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM WRITE-OUTPUT
               END-IF
           END-IF.

       LOGIN.
           MOVE 0 TO LOGIN-FLAG.
           PERFORM UNTIL LOGIN-FLAG = 1
               STRING "Please enter your username:" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               PERFORM READ-INPUT
               MOVE IN-LINE TO USERNAME

               IF USERNAME = "logout"
                   MOVE 0 TO SESSION-FLAG
                   EXIT PARAGRAPH
               END-IF

               STRING "Please enter your password:" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               PERFORM READ-INPUT
               MOVE IN-LINE TO PASSWORD

               PERFORM FIND-USERNAME
               IF FOUND-FLAG = 1 AND FOUND-PASSWORD = PASSWORD
                   STRING "You have successfully logged in."
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM WRITE-OUTPUT
                   
                   STRING "Welcome, " FUNCTION TRIM(USERNAME) "!"
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM WRITE-OUTPUT
                   
                   MOVE 1 TO LOGIN-FLAG
                   PERFORM POST-LOGIN-MENU
               ELSE
                   STRING "Incorrect username/password, try again."
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM WRITE-OUTPUT
               END-IF
           END-PERFORM.

       POST-LOGIN-MENU.
           MOVE 1 TO SESSION-FLAG.
           PERFORM UNTIL SESSION-FLAG = 0
               STRING "1) Jobs" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "2) Find" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "3) Skills" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "Enter your choice (or logout):" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               PERFORM READ-INPUT
               MOVE IN-LINE TO CHOICE
               
               EVALUATE CHOICE
                   WHEN "1"
                       STRING "Jobs feature is under construction."
                           DELIMITED BY SIZE INTO OUT-LINE
                       PERFORM WRITE-OUTPUT
                   WHEN "2"
                       STRING "Find feature is under construction."
                           DELIMITED BY SIZE INTO OUT-LINE
                       PERFORM WRITE-OUTPUT
                   WHEN "3"
                       PERFORM SKILL-MENU
                   WHEN OTHER
                       IF IN-LINE = "logout"
                           MOVE 0 TO SESSION-FLAG
                       END-IF
               END-EVALUATE
           END-PERFORM.

       SKILL-MENU.
           MOVE 1 TO SKILL-FLAG.
           PERFORM UNTIL SKILL-FLAG = 0
               STRING "Skills:" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "1) Skill 1" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "2) Skill 2" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "3) Skill 3" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "4) Skill 4" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "5) Skill 5" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "6) Go Back" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               STRING "Enter your choice:" 
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM WRITE-OUTPUT
               
               PERFORM READ-INPUT
               MOVE IN-LINE TO CHOICE
               
               EVALUATE CHOICE
                   WHEN "1" THRU "5"
                       STRING "This skill is under construction."
                           DELIMITED BY SIZE INTO OUT-LINE
                       PERFORM WRITE-OUTPUT
                   WHEN "6"
                       MOVE 0 TO SKILL-FLAG
               END-EVALUATE
           END-PERFORM.

       COUNT-ACCOUNTS.
           MOVE 0 TO ACCOUNT-COUNT.
           MOVE 0 TO EOF-FLAG.
           OPEN INPUT ACCOUNTS-FILE.
           PERFORM UNTIL EOF-FLAG = 1
               READ ACCOUNTS-FILE
                   AT END MOVE 1 TO EOF-FLAG
                   NOT AT END ADD 1 TO ACCOUNT-COUNT
               END-READ
           END-PERFORM.
           CLOSE ACCOUNTS-FILE.

       FIND-USERNAME.
           MOVE 0 TO FOUND-FLAG.
           MOVE SPACES TO FOUND-PASSWORD.
           MOVE 0 TO EOF-FLAG.
           OPEN INPUT ACCOUNTS-FILE.
           PERFORM UNTIL EOF-FLAG = 1
               READ ACCOUNTS-FILE
                   AT END MOVE 1 TO EOF-FLAG
                   NOT AT END
                       IF ACCOUNT-USERNAME = USERNAME
                           MOVE 1 TO FOUND-FLAG
                           MOVE ACCOUNT-PASSWORD TO FOUND-PASSWORD
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE ACCOUNTS-FILE.

       VALIDATE-PASSWORD.
           MOVE 0 TO VALID-FLAG.
           MOVE 0 TO HAS-CAPITAL.
           MOVE 0 TO HAS-DIGIT.
           MOVE 0 TO HAS-SPECIAL.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(PASSWORD)) 
               TO PASS-LENGTH.

           IF PASS-LENGTH < 8 OR PASS-LENGTH > 12
               EXIT PARAGRAPH
           END-IF.

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > PASS-LENGTH
               MOVE FUNCTION ORD(PASSWORD(I:1)) TO CHAR-CODE
               
               IF CHAR-CODE >= 65 AND CHAR-CODE <= 90
                   MOVE 1 TO HAS-CAPITAL
               END-IF
               
               IF CHAR-CODE >= 48 AND CHAR-CODE <= 57
                   MOVE 1 TO HAS-DIGIT
               END-IF
               
               IF (CHAR-CODE >= 33 AND CHAR-CODE <= 47) OR
                  (CHAR-CODE >= 58 AND CHAR-CODE <= 64) OR
                  (CHAR-CODE >= 91 AND CHAR-CODE <= 96) OR
                  (CHAR-CODE >= 123 AND CHAR-CODE <= 126)
                   MOVE 1 TO HAS-SPECIAL
               END-IF
           END-PERFORM.

           IF HAS-CAPITAL = 1 AND HAS-DIGIT = 1 
               AND HAS-SPECIAL = 1
               MOVE 1 TO VALID-FLAG
           END-IF.

       SAVE-ACCOUNT.
           OPEN EXTEND ACCOUNTS-FILE.
           MOVE USERNAME TO ACCOUNT-USERNAME.
           MOVE PASSWORD TO ACCOUNT-PASSWORD.
           WRITE ACCOUNT-RECORD.
           CLOSE ACCOUNTS-FILE.

       READ-INPUT.
           READ INPUT-FILE
               AT END
                   CLOSE INPUT-FILE
                   CLOSE ACCOUNTS-FILE
                   CLOSE OUTPUT-FILE
                   STOP RUN
               NOT AT END
                   MOVE INPUT-RECORD TO IN-LINE
                   PERFORM WRITE-OUTPUT
           END-READ.

       WRITE-OUTPUT.
           DISPLAY OUT-LINE.
           WRITE OUTPUT-RECORD FROM OUT-LINE.