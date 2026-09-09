# InCollege — test suite

Automated tests for the InCollege COBOL program. Each test is a pair: an input
file in `inputs/` and the expected output in `expected/`. The runner feeds each
input to the program and compares its output to the expected file.

## Run

From this folder, in the dev container:

```
cobc -x RunTests.cob -o runtests   # compile the test runner (one time / after edits)
./runtests                         # builds InCollege.cbl, runs every test
```

`runtests` compiles `InCollege.cbl` itself, runs every test, prints one line per
test (`PASS`/`FAIL <name>`) and a score, and writes each failure's diff to
`results/<name>.diff` (`<` = expected, `>` = your program).

## Layout

```
inputs/     one <name>-Input.txt per test
expected/   matching <name>.txt — the correct output for that test
RunTests.cob   the COBOL test runner
```

## Adding a test (works for every epic)

Drop two files with the same base name:
- `inputs/<name>-Input.txt`   — the keystrokes, one per line
- `expected/<name>.txt`       — the exact output the program should produce

The runner finds it automatically — no code changes. Rules:
- Names must be unique across the whole `inputs/` folder.
- Multi-launch persistence tests: name the parts `<name>-PhaseA`, `<name>-PhaseB`,
  … They run in order and share one saved-accounts file; every other test starts
  from an empty account store.
- Old tests stay in the folder and keep running as the project grows — that's the
  regression net. If a later epic legitimately changes existing behavior, update
  the affected `expected/*.txt` files to match.

## Output contract (Epic #1)

Expected outputs follow the Epic #1 sample (spec pp. 4-5). Inputs are **not**
echoed — only prompts and messages appear. Comparison ignores trailing spaces and
trailing blank lines. Exact lines:

- Startup (once): `Welcome to InCollege!`
- Initial menu: `Log In` / `Create New Account` / `Enter your choice:`
- Prompts: `Please enter your username:` / `Please enter your password:`
- Login ok: `You have successfully logged in.` then `Welcome, <username>!`
- Login fail: `Incorrect username/password, please try again`
- Register ok: `Account created successfully.`
- Duplicate user: `Username already exists, please try again`
- Blank user: `Invalid username, please try again`
- Bad password: `Invalid password: must be 8-12 characters and include an uppercase letter, a digit, and a special character.`
- Account limit (checked before prompting): `All permitted accounts have been created, please come back later`
- Post-login menu: `1. Search for a job` / `2. Find someone you know` / `3. Learn a new skill` / `4. Log out` / `Enter your choice:`
- `1` -> `Job search/internship is under construction.`; `2` -> `Find someone you know is under construction.`; `4` -> program ends
- Skill menu: `Learn a New Skill:` / `Skill 1`…`Skill 5` / `Go Back` / `Enter your choice:`; `1`-`5` -> `This skill is under construction.`; `6` -> back
- Any bad menu choice: `Invalid choice, please try again`

Some of these strings aren't fixed by the spec (register success, duplicate,
invalid-username, invalid-password, invalid-choice) — they're the team's chosen
wording. To change one, edit the matching `expected/*.txt` files directly.
