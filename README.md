# R Coding Contest - Online Judge (Shiny)

This repository contains a small Shiny-based **online judge** for R coding problems, designed for internal coding contests (e.g. AtCoder-style exercises).

The app:

- lets users select a problem,
- lets them write and submit R code in the browser,
- automatically runs hidden test cases,
- shows the result (Accepted: AC / Time Limit Exceeded: TLE / Wrong Answer: WA / Runtime Error: RE + runtime),
- and optionally records scores in a simple ranking table (opt-in).

---

## Folder structure

    .
    ├─ app.R
    └─ problems/
       └─ <problem_id>/
          └─ problem.R

- `app.R`  
  Shiny app (UI + server, judge logic, ranking).

- `problems/<problem_id>/problem.R`  
  Each problem lives in its own folder and defines a `problem` list with:
  - `id` – problem ID (e.g. `"brackets"`)
  - `title` – problem title
  - `description` – problem statement shown in the app
  - `function_name` – required function name in user code
  - `template_code` – starter code preloaded into the editor
  - `time_limit_sec` – time limit for all tests
  - `make_tests()` – function that returns a list of test cases  
    (each test: `list(input = list(...), expected = <value>)`)
  - optional: `compare` – custom comparison function for expected vs actual

You can add more problems by creating additional subfolders under `problems/` with their own `problem.R`.

---

## Requirements

- R (>= 4.x recommended)
- Packages:
  - `shiny`

Install packages via:

    install.packages("shiny")

(If you add more problems, they may require additional packages.)

---

## How to run locally

1. Clone this repository:

       git clone https://github.com/<your-org>/<your-repo>.git
       cd <your-repo>

2. Open the project in RStudio (or Posit Cloud), then run:

       shiny::runApp()

3. Your browser will open the app.  
   - Select a problem on the left.  
   - Click “Load / Reset template” to load starter code.  
   - Edit the function and click **“Submit to Judge”**.

If all tests pass within the time limit, you get **AC (Accepted)**.

---

## Scoreboard / ranking

The app stores scores in a simple RDS file:

- File: `scores.rds` (created automatically in the app directory)
- Columns:
  - `problem_id`
  - `problem_title`
  - `user_name`
  - `status` (AC / TLE / WA / RE)
  - `passed`, `total`
  - `elapsed` (seconds)
  - `timestamp` (datetime)

Users who want to appear in the ranking:

1. Run the judge.  
2. Enter a display name.  
3. Check: “I want to add my latest result to the ranking table”.  
4. Click **“Save my score”**.

For each `(problem_id, user_name)` pair, **only the best result** is kept.

> Note: For shared deployments (e.g. Posit Connect), using **one R process** for this app is recommended if you keep the RDS-based scoreboard, to avoid concurrent write issues.

---

## Deploying to Posit Cloud / Posit Connect

### Posit Cloud

1. Create a new project.  
2. Upload:
   - `app.R`
   - the `problems/` folder.
3. In the Console, run:

       shiny::runApp()

4. (Optional) Use the “Publish” button if you want to deploy it as a shared app.

### Posit Connect

1. From RStudio / Posit Cloud, use the “Publish to Posit Connect” button.  
2. Ensure that the app’s working directory is writable so `scores.rds` can be created.  
3. (Optional but recommended) Configure the app to run with a **single R process** if you expect many writes to the scoreboard.

---

## Adding new problems

To add another problem (e.g. `sum_two_integers`):

1. Create a folder:

       problems/sum_two_integers/problem.R

2. In that file, define a `problem` object similar to `problems/brackets/problem.R`:
   - Set `id = "sum_two_integers"`.  
   - Set `function_name = "sum_two_integers"` (or any name you prefer).  
   - Write `make_tests()` to generate input/expected pairs.

The app will automatically pick up any problem under `problems/` at startup.


## License

(Choose a license appropriate for your organization, e.g. MIT, or “Internal use only”.)

    TODO: Add license information here.
