       IDENTIFICATION DIVISION.
       PROGRAM-ID. IN-COLLEGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNTS-FILE ASSIGN TO "accounts.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ACC-STATUS.
           SELECT PROFILE-FILE ASSIGN TO "profiles.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS PROFILE-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(256).

       FD ACCOUNTS-FILE.
       01 ACCOUNT-RECORD.
           05 ACCOUNT-USERNAME PIC X(20).
           05 ACCOUNT-PASSWORD PIC X(20).

       FD PROFILE-FILE.
       01 PROFILE-RECORD.
           05 PF-USERNAME PIC X(20).
           05 PF-FIRST-NAME PIC X(30).
           05 PF-LAST-NAME PIC X(30).
           05 PF-UNIVERSITY PIC X(60).
           05 PF-MAJOR PIC X(50).
           05 PF-GRAD-YEAR PIC X(4).
           05 PF-ABOUT-ME PIC X(200).
           05 PF-EXP-COUNT PIC 9.
           05 PF-EXP-TABLE.
               10 PF-EXP-ENTRY OCCURS 3 TIMES.
                   15 PF-EXP-TITLE PIC X(50).
                   15 PF-EXP-COMPANY PIC X(60).
                   15 PF-EXP-DATES PIC X(40).
                   15 PF-EXP-DESC PIC X(100).
           05 PF-EDU-COUNT PIC 9.
           05 PF-EDU-TABLE.
               10 PF-EDU-ENTRY OCCURS 3 TIMES.
                   15 PF-EDU-DEGREE PIC X(50).
                   15 PF-EDU-UNIVERSITY PIC X(60).
                   15 PF-EDU-YEARS PIC X(30).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(256).

       WORKING-STORAGE SECTION.
       01 OUT-LINE PIC X(256) VALUE SPACES.
       01 IN-LINE PIC X(256).
       01 USERNAME PIC X(20).
       01 PASSWORD PIC X(20).
       01 CURRENT-USER PIC X(20) VALUE SPACES.
       01 CHOICE PIC X(1).
       01 FOUND-FLAG PIC 9 VALUE 0.
       01 FOUND-PASSWORD PIC X(20).
       78 MAX-USERS VALUE 5.

       01 USER-TABLE.
           05 USER-ENTRY OCCURS MAX-USERS TIMES.
               10 UT-USERNAME PIC X(20).
               10 UT-PASSWORD PIC X(20).
       01 UT-COUNT PIC 99 VALUE 0.
       01 ACC-EOF PIC 9 VALUE 0.
       01 ACC-STATUS PIC XX VALUE "00".

      *> Epic 2 profile table. One profile is linked to one username.
       01 PROFILE-TABLE.
           05 PROFILE-ENTRY OCCURS MAX-USERS TIMES.
               10 PT-USERNAME PIC X(20).
               10 PT-FIRST-NAME PIC X(30).
               10 PT-LAST-NAME PIC X(30).
               10 PT-UNIVERSITY PIC X(60).
               10 PT-MAJOR PIC X(50).
               10 PT-GRAD-YEAR PIC X(4).
               10 PT-ABOUT-ME PIC X(200).
               10 PT-EXP-COUNT PIC 9.
               10 PT-EXP-TABLE.
                   15 PT-EXP-ENTRY OCCURS 3 TIMES.
                       20 PT-EXP-TITLE PIC X(50).
                       20 PT-EXP-COMPANY PIC X(60).
                       20 PT-EXP-DATES PIC X(40).
                       20 PT-EXP-DESC PIC X(100).
               10 PT-EDU-COUNT PIC 9.
               10 PT-EDU-TABLE.
                   15 PT-EDU-ENTRY OCCURS 3 TIMES.
                       20 PT-EDU-DEGREE PIC X(50).
                       20 PT-EDU-UNIVERSITY PIC X(60).
                       20 PT-EDU-YEARS PIC X(30).

       01 PROFILE-WORK.
           05 W-USERNAME PIC X(20).
           05 W-FIRST-NAME PIC X(30).
           05 W-LAST-NAME PIC X(30).
           05 W-UNIVERSITY PIC X(60).
           05 W-MAJOR PIC X(50).
           05 W-GRAD-YEAR PIC X(4).
           05 W-ABOUT-ME PIC X(200).
           05 W-EXP-COUNT PIC 9.
           05 W-EXP-TABLE.
               10 W-EXP-ENTRY OCCURS 3 TIMES.
                   15 W-EXP-TITLE PIC X(50).
                   15 W-EXP-COMPANY PIC X(60).
                   15 W-EXP-DATES PIC X(40).
                   15 W-EXP-DESC PIC X(100).
           05 W-EDU-COUNT PIC 9.
           05 W-EDU-TABLE.
               10 W-EDU-ENTRY OCCURS 3 TIMES.
                   15 W-EDU-DEGREE PIC X(50).
                   15 W-EDU-UNIVERSITY PIC X(60).
                   15 W-EDU-YEARS PIC X(30).

       01 PT-COUNT PIC 99 VALUE 0.
       01 PROFILE-EOF PIC 9 VALUE 0.
       01 PROFILE-STATUS PIC XX VALUE "00".
       01 PROFILE-FOUND PIC 9 VALUE 0.
       01 PROFILE-INDEX PIC 99 VALUE 0.
       01 PROFILE-VALID PIC 9 VALUE 0.
       01 GRAD-YEAR-NUM PIC 9(4) VALUE 0.
       01 ENTRY-NUM PIC 9 VALUE 0.

       01 VALID-FLAG PIC 9 VALUE 0.
       01 LOGIN-OK PIC 9 VALUE 0.
       01 SCREEN-CODE PIC 99 VALUE 1.
       01 PASS-LENGTH PIC 99.
       01 HAS-CAPITAL PIC 9 VALUE 0.
       01 HAS-DIGIT PIC 9 VALUE 0.
       01 HAS-SPECIAL PIC 9 VALUE 0.
       01 I PIC 999.
       01 CHAR-CODE PIC 999.

       PROCEDURE DIVISION.

       MAIN-SECTION SECTION.
       MAIN.
           PERFORM IO-OPEN-FILES.
           PERFORM STORE-LOAD-ALL.
           PERFORM STORE-LOAD-PROFILES.

           STRING "Welcome to InCollege!"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           MOVE 1 TO SCREEN-CODE
           PERFORM NAV-DISPATCH UNTIL SCREEN-CODE = 0.
           PERFORM IO-TERMINATE.

       NAV-SECTION SECTION.
       NAV-DISPATCH.
           EVALUATE SCREEN-CODE
               WHEN 1
                   PERFORM NAV-TOP-MENU
               WHEN 2
                   PERFORM NAV-CREATE-ACCOUNT
               WHEN 3
                   PERFORM NAV-LOGIN
               WHEN 4
                   PERFORM NAV-POST-LOGIN-MENU
               WHEN 5
                   PERFORM NAV-SKILL-MENU
               WHEN 6
                   PERFORM NAV-PROFILE-EDIT
               WHEN 7
                   PERFORM NAV-PROFILE-VIEW
               WHEN OTHER
                   MOVE 0 TO SCREEN-CODE
           END-EVALUATE.

       NAV-TOP-MENU.
           STRING "Log In"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Create New Account"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Enter your choice:"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-PROMPT-AND-READ
           MOVE IN-LINE TO CHOICE

           EVALUATE CHOICE
               WHEN "1"
                   MOVE 3 TO SCREEN-CODE
               WHEN "2"
                   MOVE 2 TO SCREEN-CODE
               WHEN OTHER
                   IF IN-LINE = "logout"
                       MOVE 0 TO SCREEN-CODE
                   ELSE
                       STRING "Invalid choice, please try again"
                           DELIMITED BY SIZE INTO OUT-LINE
                       PERFORM IO-WRITE-LINE
                       MOVE 1 TO SCREEN-CODE
                   END-IF
           END-EVALUATE.

       NAV-CREATE-ACCOUNT.
           IF UT-COUNT >= MAX-USERS
               STRING "All permitted accounts have been created, "
                   "please come back later"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-WRITE-LINE
               MOVE 1 TO SCREEN-CODE
               EXIT PARAGRAPH
           END-IF.

           STRING "Please enter your username:"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-PROMPT-AND-READ
           MOVE IN-LINE TO USERNAME

           STRING "Please enter your password:"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-PROMPT-AND-READ
           MOVE IN-LINE TO PASSWORD

           MOVE 1 TO SCREEN-CODE
           PERFORM ACCT-CREATE.

       NAV-LOGIN.
           STRING "Please enter your username:"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-PROMPT-AND-READ
           MOVE IN-LINE TO USERNAME

           IF USERNAME = "logout"
               MOVE 0 TO SCREEN-CODE
               EXIT PARAGRAPH
           END-IF

           STRING "Please enter your password:"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-PROMPT-AND-READ
           MOVE IN-LINE TO PASSWORD

           PERFORM ACCT-TRY-LOGIN
           IF LOGIN-OK = 1
               STRING "You have successfully logged in."
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-WRITE-LINE

               STRING "Welcome, " FUNCTION TRIM(CURRENT-USER) "!"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-WRITE-LINE

               MOVE 4 TO SCREEN-CODE
           ELSE
               STRING "Incorrect username/password, "
                   DELIMITED BY SIZE
                   "please try again"
                   DELIMITED BY SIZE
                   INTO OUT-LINE
               PERFORM IO-WRITE-LINE
               MOVE 3 TO SCREEN-CODE
           END-IF.

       NAV-POST-LOGIN-MENU.
           STRING "1. Create/Edit My Profile"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "2. View My Profile"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "3. Search for a job"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "4. Find someone you know"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "5. Learn a New Skill"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "6. Log out"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Enter your choice:"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-PROMPT-AND-READ
           MOVE IN-LINE TO CHOICE

           EVALUATE CHOICE
               WHEN "1"
                   MOVE 6 TO SCREEN-CODE
               WHEN "2"
                   MOVE 7 TO SCREEN-CODE
               WHEN "3"
                   STRING "Job search/internship is under "
                       DELIMITED BY SIZE
                       "construction."
                       DELIMITED BY SIZE
                       INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
                   MOVE 4 TO SCREEN-CODE
               WHEN "4"
                   STRING "Find someone you know is under "
                       DELIMITED BY SIZE
                       "construction."
                       DELIMITED BY SIZE
                       INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
                   MOVE 4 TO SCREEN-CODE
               WHEN "5"
                   MOVE 5 TO SCREEN-CODE
               WHEN "6"
                   MOVE 0 TO SCREEN-CODE
               WHEN OTHER
                   STRING "Invalid choice, please try again"
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
                   MOVE 4 TO SCREEN-CODE
           END-EVALUATE.

       NAV-SKILL-MENU.
           STRING "Learn a New Skill:"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Skill 1"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Skill 2"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Skill 3"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Skill 4"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Skill 5"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Go Back"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Enter your choice:"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-PROMPT-AND-READ
           MOVE IN-LINE TO CHOICE

           EVALUATE CHOICE
               WHEN "1" THRU "5"
                   STRING "This skill is under construction."
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
                   MOVE 5 TO SCREEN-CODE
               WHEN "6"
                   MOVE 4 TO SCREEN-CODE
               WHEN OTHER
                   STRING "Invalid choice, please try again"
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
                   MOVE 5 TO SCREEN-CODE
           END-EVALUATE.

      *> Create and edit use the same flow. If a profile exists, it is
      *> loaded first and then replaced by the new values from the file.
       NAV-PROFILE-EDIT.
           PERFORM STORE-FIND-PROFILE

           IF PROFILE-FOUND = 1
               MOVE PROFILE-ENTRY(PROFILE-INDEX)
                   TO PROFILE-WORK
           ELSE
               INITIALIZE PROFILE-WORK
               MOVE CURRENT-USER TO W-USERNAME
           END-IF

           STRING "--- Create/Edit Profile ---"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           PERFORM PROFILE-READ-FIRST-NAME
           PERFORM PROFILE-READ-LAST-NAME
           PERFORM PROFILE-READ-UNIVERSITY
           PERFORM PROFILE-READ-MAJOR
           PERFORM PROFILE-READ-GRAD-YEAR
           PERFORM PROFILE-READ-ABOUT-ME
           PERFORM PROFILE-READ-EXPERIENCE
           PERFORM PROFILE-READ-EDUCATION

           MOVE CURRENT-USER TO W-USERNAME
           PERFORM STORE-SAVE-PROFILE

           STRING "Profile saved successfully!"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           MOVE 4 TO SCREEN-CODE.

       NAV-PROFILE-VIEW.
           PERFORM STORE-FIND-PROFILE

           IF PROFILE-FOUND = 0
               STRING "You have not created a profile yet."
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-WRITE-LINE
               MOVE 4 TO SCREEN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE PROFILE-ENTRY(PROFILE-INDEX) TO PROFILE-WORK

           STRING "--- Your Profile ---"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Name: " FUNCTION TRIM(W-FIRST-NAME)
               " " FUNCTION TRIM(W-LAST-NAME)
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "University: " FUNCTION TRIM(W-UNIVERSITY)
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Major: " FUNCTION TRIM(W-MAJOR)
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           STRING "Graduation Year: " W-GRAD-YEAR
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           IF W-ABOUT-ME NOT = SPACES
               STRING "About Me: " FUNCTION TRIM(W-ABOUT-ME)
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-WRITE-LINE
           END-IF

           IF W-EXP-COUNT > 0
               STRING "Experience:"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-WRITE-LINE

               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > W-EXP-COUNT
                   STRING " Title: "
                       FUNCTION TRIM(W-EXP-TITLE(I))
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE

                   STRING " Company: "
                       FUNCTION TRIM(W-EXP-COMPANY(I))
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE

                   STRING " Dates: "
                       FUNCTION TRIM(W-EXP-DATES(I))
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE

                   IF W-EXP-DESC(I) NOT = SPACES
                       STRING " Description: "
                           FUNCTION TRIM(W-EXP-DESC(I))
                           DELIMITED BY SIZE INTO OUT-LINE
                       PERFORM IO-WRITE-LINE
                   END-IF
               END-PERFORM
           END-IF

           IF W-EDU-COUNT > 0
               STRING "Education:"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-WRITE-LINE

               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > W-EDU-COUNT
                   STRING " Degree: "
                       FUNCTION TRIM(W-EDU-DEGREE(I))
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE

                   STRING " University: "
                       FUNCTION TRIM(W-EDU-UNIVERSITY(I))
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE

                   STRING " Years: "
                       FUNCTION TRIM(W-EDU-YEARS(I))
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
               END-PERFORM
           END-IF

           STRING "--------------------"
               DELIMITED BY SIZE INTO OUT-LINE
           PERFORM IO-WRITE-LINE

           MOVE 4 TO SCREEN-CODE.

       PROFILE-SECTION SECTION.
       PROFILE-READ-FIRST-NAME.
           MOVE 0 TO PROFILE-VALID
           PERFORM UNTIL PROFILE-VALID = 1
               STRING "Enter First Name:"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ

               IF FUNCTION TRIM(IN-LINE) = SPACES
                   STRING "First Name is required. Please try again."
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
               ELSE
                   MOVE IN-LINE TO W-FIRST-NAME
                   MOVE 1 TO PROFILE-VALID
               END-IF
           END-PERFORM.

       PROFILE-READ-LAST-NAME.
           MOVE 0 TO PROFILE-VALID
           PERFORM UNTIL PROFILE-VALID = 1
               STRING "Enter Last Name:"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ

               IF FUNCTION TRIM(IN-LINE) = SPACES
                   STRING "Last Name is required. Please try again."
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
               ELSE
                   MOVE IN-LINE TO W-LAST-NAME
                   MOVE 1 TO PROFILE-VALID
               END-IF
           END-PERFORM.

       PROFILE-READ-UNIVERSITY.
           MOVE 0 TO PROFILE-VALID
           PERFORM UNTIL PROFILE-VALID = 1
               STRING "Enter University/College Attended:"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ

               IF FUNCTION TRIM(IN-LINE) = SPACES
                   STRING "University/College is required. "
                       "Please try again."
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
               ELSE
                   MOVE IN-LINE TO W-UNIVERSITY
                   MOVE 1 TO PROFILE-VALID
               END-IF
           END-PERFORM.

       PROFILE-READ-MAJOR.
           MOVE 0 TO PROFILE-VALID
           PERFORM UNTIL PROFILE-VALID = 1
               STRING "Enter Major:"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ

               IF FUNCTION TRIM(IN-LINE) = SPACES
                   STRING "Major is required. Please try again."
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
               ELSE
                   MOVE IN-LINE TO W-MAJOR
                   MOVE 1 TO PROFILE-VALID
               END-IF
           END-PERFORM.

       PROFILE-READ-GRAD-YEAR.
           MOVE 0 TO PROFILE-VALID
           PERFORM UNTIL PROFILE-VALID = 1
               STRING "Enter Graduation Year (YYYY):"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ

               IF FUNCTION LENGTH(FUNCTION TRIM(IN-LINE)) = 4
                   AND IN-LINE(1:4) IS NUMERIC
                   MOVE IN-LINE(1:4) TO GRAD-YEAR-NUM

                   IF GRAD-YEAR-NUM >= 2026
                       AND GRAD-YEAR-NUM <= 2033
                       MOVE IN-LINE(1:4) TO W-GRAD-YEAR
                       MOVE 1 TO PROFILE-VALID
                   ELSE
                       STRING "Graduation Year must be between "
                           "2026 and 2033."
                           DELIMITED BY SIZE INTO OUT-LINE
                       PERFORM IO-WRITE-LINE
                   END-IF
               ELSE
                   STRING "Graduation Year must be a 4-digit "
                       "number."
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
               END-IF
           END-PERFORM.

       PROFILE-READ-ABOUT-ME.
           MOVE 0 TO PROFILE-VALID
           PERFORM UNTIL PROFILE-VALID = 1
               STRING "Enter About Me (optional, max 200 chars, "
                   "enter blank line to skip):"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ

               IF FUNCTION LENGTH(FUNCTION TRIM(IN-LINE)) <= 200
                   MOVE IN-LINE(1:200) TO W-ABOUT-ME
                   MOVE 1 TO PROFILE-VALID
               ELSE
                   STRING "About Me must be 200 characters or less."
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
               END-IF
           END-PERFORM.

       PROFILE-READ-EXPERIENCE.
           MOVE 0 TO W-EXP-COUNT
           MOVE SPACES TO W-EXP-TABLE
           MOVE 0 TO ENTRY-NUM

           PERFORM UNTIL ENTRY-NUM >= 3
               STRING "Add Experience (optional, max 3 entries. "
                   "Enter 'DONE' to finish):"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ

               IF FUNCTION UPPER-CASE(FUNCTION TRIM(IN-LINE))
                   = "DONE"
                   EXIT PERFORM
               END-IF

               ADD 1 TO ENTRY-NUM
               MOVE ENTRY-NUM TO W-EXP-COUNT

               STRING "Experience #" ENTRY-NUM " - Title:"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ
               MOVE IN-LINE TO W-EXP-TITLE(ENTRY-NUM)

               STRING "Experience #" ENTRY-NUM
                   " - Company/Organization:"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ
               MOVE IN-LINE TO W-EXP-COMPANY(ENTRY-NUM)

               STRING "Experience #" ENTRY-NUM
                   " - Dates (e.g., Summer 2024):"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ
               MOVE IN-LINE TO W-EXP-DATES(ENTRY-NUM)

               STRING "Experience #" ENTRY-NUM
                   " - Description (optional, max 100 chars, "
                   "blank to skip):"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ
               MOVE IN-LINE(1:100) TO W-EXP-DESC(ENTRY-NUM)
           END-PERFORM.

       PROFILE-READ-EDUCATION.
           MOVE 0 TO W-EDU-COUNT
           MOVE SPACES TO W-EDU-TABLE
           MOVE 0 TO ENTRY-NUM

           PERFORM UNTIL ENTRY-NUM >= 3
               STRING "Add Education (optional, max 3 entries. "
                   "Enter 'DONE' to finish):"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ

               IF FUNCTION UPPER-CASE(FUNCTION TRIM(IN-LINE))
                   = "DONE"
                   EXIT PERFORM
               END-IF

               ADD 1 TO ENTRY-NUM
               MOVE ENTRY-NUM TO W-EDU-COUNT

               STRING "Education #" ENTRY-NUM " - Degree:"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ
               MOVE IN-LINE TO W-EDU-DEGREE(ENTRY-NUM)

               STRING "Education #" ENTRY-NUM
                   " - University/College:"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ
               MOVE IN-LINE TO W-EDU-UNIVERSITY(ENTRY-NUM)

               STRING "Education #" ENTRY-NUM
                   " - Years Attended (e.g., 2023-2025):"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-PROMPT-AND-READ
               MOVE IN-LINE TO W-EDU-YEARS(ENTRY-NUM)
           END-PERFORM.

       ACCT-SECTION SECTION.
       ACCT-CREATE.
           IF USERNAME = SPACES
               STRING "Invalid username, please try again"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-WRITE-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM STORE-FIND
           IF FOUND-FLAG = 1
               STRING "Username already exists, please try again"
                   DELIMITED BY SIZE INTO OUT-LINE
               PERFORM IO-WRITE-LINE
           ELSE
               PERFORM ACCT-VALIDATE-PASSWORD
               IF VALID-FLAG = 1
                   PERFORM STORE-ADD
                   STRING "Account created successfully."
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
               ELSE
                   STRING "Invalid password: must be 8-12 "
                       "characters and include an uppercase "
                       "letter, a digit, and a special character."
                       DELIMITED BY SIZE INTO OUT-LINE
                   PERFORM IO-WRITE-LINE
               END-IF
           END-IF.

       ACCT-TRY-LOGIN.
           MOVE 0 TO LOGIN-OK
           PERFORM STORE-FIND

           IF FOUND-FLAG = 1 AND FOUND-PASSWORD = PASSWORD
               MOVE USERNAME TO CURRENT-USER
               MOVE 1 TO LOGIN-OK
           END-IF.

       ACCT-VALIDATE-PASSWORD.
           MOVE 0 TO VALID-FLAG
           MOVE 0 TO HAS-CAPITAL
           MOVE 0 TO HAS-DIGIT
           MOVE 0 TO HAS-SPECIAL
           MOVE FUNCTION LENGTH(FUNCTION TRIM(PASSWORD))
               TO PASS-LENGTH

           IF PASS-LENGTH < 8 OR PASS-LENGTH > 12
               EXIT PARAGRAPH
           END-IF

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
           END-PERFORM

           IF HAS-CAPITAL = 1 AND HAS-DIGIT = 1
               AND HAS-SPECIAL = 1
               MOVE 1 TO VALID-FLAG
           END-IF.

       STORE-SECTION SECTION.
       STORE-LOAD-ALL.
           MOVE 0 TO UT-COUNT
           MOVE 0 TO ACC-EOF
           OPEN INPUT ACCOUNTS-FILE

           IF ACC-STATUS NOT = "00"
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL ACC-EOF = 1
               READ ACCOUNTS-FILE
                   AT END
                       MOVE 1 TO ACC-EOF
                   NOT AT END
                       IF UT-COUNT < MAX-USERS
                           ADD 1 TO UT-COUNT
                           MOVE ACCOUNT-USERNAME
                               TO UT-USERNAME(UT-COUNT)
                           MOVE ACCOUNT-PASSWORD
                               TO UT-PASSWORD(UT-COUNT)
                       END-IF
               END-READ
           END-PERFORM

           CLOSE ACCOUNTS-FILE.

       STORE-FIND.
           MOVE 0 TO FOUND-FLAG
           MOVE SPACES TO FOUND-PASSWORD

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > UT-COUNT
               IF UT-USERNAME(I) = USERNAME
                   MOVE 1 TO FOUND-FLAG
                   MOVE UT-PASSWORD(I) TO FOUND-PASSWORD
               END-IF
           END-PERFORM.

       STORE-ADD.
           ADD 1 TO UT-COUNT
           MOVE USERNAME TO UT-USERNAME(UT-COUNT)
           MOVE PASSWORD TO UT-PASSWORD(UT-COUNT).

       STORE-FLUSH-ALL.
           IF UT-COUNT > 0
               OPEN OUTPUT ACCOUNTS-FILE
               IF ACC-STATUS = "00"
                   PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > UT-COUNT
                       MOVE UT-USERNAME(I) TO ACCOUNT-USERNAME
                       MOVE UT-PASSWORD(I) TO ACCOUNT-PASSWORD
                       WRITE ACCOUNT-RECORD
                   END-PERFORM
                   CLOSE ACCOUNTS-FILE
               END-IF
           END-IF.

       STORE-LOAD-PROFILES.
           MOVE 0 TO PT-COUNT
           MOVE 0 TO PROFILE-EOF
           OPEN INPUT PROFILE-FILE

           IF PROFILE-STATUS NOT = "00"
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL PROFILE-EOF = 1
               READ PROFILE-FILE
                   AT END
                       MOVE 1 TO PROFILE-EOF
                   NOT AT END
                       IF PT-COUNT < MAX-USERS
                           ADD 1 TO PT-COUNT
                           MOVE PROFILE-RECORD
                               TO PROFILE-ENTRY(PT-COUNT)
                       END-IF
               END-READ
           END-PERFORM

           CLOSE PROFILE-FILE.

       STORE-FIND-PROFILE.
           MOVE 0 TO PROFILE-FOUND
           MOVE 0 TO PROFILE-INDEX

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PT-COUNT
               IF PT-USERNAME(I) = CURRENT-USER
                   MOVE 1 TO PROFILE-FOUND
                   MOVE I TO PROFILE-INDEX
               END-IF
           END-PERFORM.

       STORE-SAVE-PROFILE.
           PERFORM STORE-FIND-PROFILE

           IF PROFILE-FOUND = 0
               IF PT-COUNT < MAX-USERS
                   ADD 1 TO PT-COUNT
                   MOVE PT-COUNT TO PROFILE-INDEX
               ELSE
                   EXIT PARAGRAPH
               END-IF
           END-IF

           MOVE PROFILE-WORK TO PROFILE-ENTRY(PROFILE-INDEX).

       STORE-FLUSH-PROFILES.
           IF PT-COUNT > 0
               OPEN OUTPUT PROFILE-FILE
               IF PROFILE-STATUS = "00"
                   PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > PT-COUNT
                       MOVE PROFILE-ENTRY(I) TO PROFILE-RECORD
                       WRITE PROFILE-RECORD
                   END-PERFORM
                   CLOSE PROFILE-FILE
               END-IF
           END-IF.

       IO-SECTION SECTION.
      *> All output goes through this routine so console and file match.
       IO-WRITE-LINE.
           DISPLAY FUNCTION TRIM(OUT-LINE TRAILING)
           WRITE OUTPUT-RECORD FROM OUT-LINE
           MOVE SPACES TO OUT-LINE.

      *> Input is always read from InCollege-Input.txt. It is also
      *> echoed because that behavior was required in Epic 1.
       IO-READ-LINE.
           READ INPUT-FILE
               AT END
                   PERFORM IO-TERMINATE
               NOT AT END
                   MOVE INPUT-RECORD TO IN-LINE
                   MOVE IN-LINE TO OUT-LINE
                   PERFORM IO-WRITE-LINE
           END-READ.

       IO-PROMPT-AND-READ.
           PERFORM IO-WRITE-LINE
           PERFORM IO-READ-LINE.

       IO-OPEN-FILES.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE.

       IO-TERMINATE.
           PERFORM STORE-FLUSH-ALL
           PERFORM STORE-FLUSH-PROFILES
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.
