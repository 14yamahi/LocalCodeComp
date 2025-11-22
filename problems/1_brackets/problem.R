# problems/brackets/problem.R
# Problem definition: Bracket Sequence Validator

# Test generator for the bracket problem
make_bracket_tests <- function() {
  tests <- list()

  # Basic small cases
  basic <- list(
    list(input = list(s = ""),            expected = TRUE),
    list(input = list(s = "()"),          expected = TRUE),
    list(input = list(s = "[]"),          expected = TRUE),
    list(input = list(s = "{}"),          expected = TRUE),
    list(input = list(s = "([{}])"),      expected = TRUE),
    list(input = list(s = "()[]{}"),      expected = TRUE),
    list(input = list(s = "([)]"),        expected = FALSE),
    list(input = list(s = "((("),         expected = FALSE),
    list(input = list(s = ")))"),         expected = FALSE),
    list(input = list(s = "(()"),         expected = FALSE),
    list(input = list(s = "())"),         expected = FALSE),
    list(input = list(s = "([{}{}])"),    expected = TRUE),
    list(input = list(s = "([{}{}]))"),   expected = FALSE),
    list(input = list(s = "{[()]}"),      expected = TRUE),
    list(input = list(s = "{[(])}"),      expected = FALSE)
  )

  tests <- c(tests, basic)

  # Larger cases for performance testing
  long_ok  <- paste(rep("()[]{}", 2000), collapse = "")
  long_ok2 <- paste0("{", paste(rep("[]()", 2000), collapse = ""), "}")
  long_bad <- sub("\\)", "]", long_ok)  # break one bracket

  big <- list(
    list(input = list(s = long_ok),  expected = TRUE),
    list(input = list(s = long_ok2), expected = TRUE),
    list(input = list(s = long_bad), expected = FALSE)
  )

  tests <- c(tests, big)

  tests
}

problem <- list(
  id = "brackets",
  title = "Bracket Sequence Validator",
  function_name = "is_valid_brackets",
  time_limit_sec = 2,
  description = paste(
    "You are given a string s consisting only of the characters '(', ')', '[', ']', '{', '}'.",
    "",
    "Implement a function is_valid_brackets(s) with the following specification:",
    "- Input : a single string s.",
    "- Output: TRUE if s is a valid bracket sequence, FALSE otherwise.",
    "",
    "A bracket sequence is valid if:",
    "- Every opening bracket has a corresponding closing bracket of the same type, and",
    "- Brackets are properly nested (no crossing).",
    "",
    "Examples:",
    'is_valid_brackets("()")        # TRUE',
    'is_valid_brackets("([{}])")    # TRUE',
    'is_valid_brackets("([)]")      # FALSE',
    'is_valid_brackets("((")        # FALSE',
    "",
    "Your implementation will be tested on many hidden test cases, including long strings.",
    "Try to make your solution efficient (ideally O(n) in the length of s).",
    sep = "\n"
  ),
  template_code = paste(
    "## Implement the function below.",
    "## You may write any helper functions you like, but the main entry point",
    "## MUST be named is_valid_brackets and have a single argument s (a string).",
    "",
    "is_valid_brackets <- function(s) {",
    "  # TODO: implement your algorithm here.",
    "  #",
    "  # Hint (you do NOT have to follow this hint):",
    "  #   chars <- strsplit(s, \"\")[[1]]",
    "  #   stack <- character(0)",
    "  #   pairs <- c(\")\" = \"(\", \"]\" = \"[\", \"}\" = \"{\")",
    "  #   for (ch in chars) {",
    "  #     if (ch %in% c(\"(\", \"[\", \"{\")) {",
    "  #       stack <- c(stack, ch)",
    "  #     } else if (ch %in% c(\")\", \"]\", \"}\")) {",
    "  #       if (length(stack) == 0) return(FALSE)",
    "  #       top <- stack[length(stack)]",
    "  #       stack <- stack[-length(stack)]",
    "  #       if (pairs[[ch]] != top) return(FALSE)",
    "  #     }",
    "  #   }",
    "  #   return(length(stack) == 0)",
    "  #",
    "  TRUE  # dummy implementation (must be replaced)",
    "}",
    sep = "\n"
  ),
  make_tests = make_bracket_tests
)
