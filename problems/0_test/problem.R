# problems/0_test/problem.R
# Problem definition: Test

# Test generator for the bracket problem
make_test_tests <- function() {
  list(
    list(input = list(),            expected = TRUE)
  )
}

problem <- list(
  id = "test",
  title = "Test",
  function_name = "test",
  time_limit_sec = 2,
  description = paste(
    "This problem is used to test the function of the Online Judge",
    "",
    "Implement a function test() with the following specification:",
    "- Input : None",
    "- Output: TRUE",
    "",
    "Example:",
    'test()        # TRUE',
    sep = "\n"
  ),
  template_code = paste(
    "## Implement the function below.",
    "## You may write any helper functions you like, but the main entry point",
    "## MUST be named test and have no arguments.",
    "",
    "test <- function() {",
    "  # TODO: implement your algorithm here.",
    "  #",
    "  TRUE",
    "}",
    sep = "\n"
  ),
  make_tests = make_test_tests
)
