
# problems/strsearch/problem.R
# Problem definition: Character Location Finder

# Test generator for the character location problem
make_strsearch_tests <- function() {
  compute_expected <- function(s, f) {
    # Split into individual characters
    s_chars <- strsplit(s, "", useBytes = FALSE)[[1]]
    f_chars <- unique(strsplit(f, "", useBytes = FALSE)[[1]])
    # Find positions (1-based) where s contains any character from f
    pos <- which(s_chars %in% f_chars)
    if (length(pos) == 0) "" else paste(pos, collapse = ", ")
  }

  tests <- list()

  # Basic small cases
  basic <- list(
    list(input = list(s = "", f = ""),                           expected = ""),
    list(input = list(s = "", f = "abc"),                        expected = ""),
    list(input = list(s = "abc", f = ""),                        expected = ""),
    list(input = list(s = "asddssd*asdaffg", f = "*"),           expected = "8"),
    list(input = list(s = "hello there", f = "eo"),              expected = "2, 5, 9, 11"),
    list(input = list(s = "abc", f = "z"),                       expected = ""),
    list(input = list(s = "banana", f = "ana"),                  expected = "2, 3, 4, 5, 6"), # duplicates in f ignored
    list(input = list(s = "AaAa", f = "a"),                      expected = "2, 4"),         # case-sensitive
    list(input = list(s = "AaAa", f = "A"),                      expected = "1, 3"),
    list(input = list(s = "a b  c", f = " "),                    expected = "2, 4, 5"),      # whitespace
    list(input = list(s = ".,!?;", f = "!?."),                   expected = "1, 3, 4"),      # punctuation
    list(input = list(s = "こんにちは世界", f = "に世"),            expected = "3, 6"),         # Unicode (Japanese)
    list(input = list(s = "寿司🍣うまい", f = "寿🍣"),               expected = "1, 3")          # Emoji + Kanji
  )

  tests <- c(tests, basic)

  # Larger cases for performance and robustness
  long_letters <- paste(rep(letters, 2000), collapse = "")                 # length 52,000
  long_vowels  <- "aeiou"
  long_digits  <- paste(rep(0:9, 5000), collapse = "")                     # length 50,000
  long_even    <- "02468"
  long_nomatch <- paste(rep("x", 40000), collapse = "")                    # length 40,000

  big <- list(
    list(input = list(s = long_letters, f = long_vowels),
         expected = compute_expected(long_letters, long_vowels)),
    list(input = list(s = long_digits,  f = long_even),
         expected = compute_expected(long_digits, long_even)),
    list(input = list(s = long_nomatch, f = "abc"),
         expected = compute_expected(long_nomatch, "abc"))
  )

  tests <- c(tests, big)

  tests
}

problem <- list(
  id = "strsearch",
  title = "Character Location Finder",
  function_name = "find_chr_location",
  time_limit_sec = 2,
  description = paste(
    "You are given two strings:",
    "- s: the string to search in (may contain any characters, including whitespace and Unicode).",
    "- f: a set of characters to find within s.",
    "",
    "Implement a function find_chr_location(s, f) with the following specification:",
    "- Input : two strings s and f.",
    "- Output: a single character string containing the 1-based indices in s where any character",
    "          from f occurs, in ascending order, separated by ', '.",
    "- If there are no matches, return the empty string \"\".",
    "",
    "Details:",
    "- Matching is character-by-character and case-sensitive.",
    "- Treat f as a set of characters (ignore duplicates in f).",
    "- Count all occurrences in s, including whitespace, punctuation, and Unicode.",
    "",
    "Examples:",
    'find_chr_location("hello there", "eo")    # "2, 5, 9, 11"',
    'find_chr_location("asddssd*asdaffg", "*") # "8"',
    'find_chr_location("abc", "z")             # ""',
    "",
    "Your implementation will be tested on many hidden test cases, including long strings.",
    "Try to make your solution efficient (ideally O(n) in the length of s).",
    sep = "\n"
  ),
  template_code = paste(
    "## Implement the function below.",
    "## You may write any helper functions you like, but the main entry point",
    "## MUST be named find_chr_location and have a single argument s (a string).",
    "",
    "find_chr_location <- function(s, f) {",
    "  # TODO: implement your algorithm here.",
    "  #",
    "  ''  # dummy implementation (must be replaced)",
    "}",
    sep = "\n"
  ),
  make_tests = make_strsearch_tests
)
