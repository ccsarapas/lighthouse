# Conditionally suppress warnings or messages

Evaluates an expression while selectively suppressing warnings or
messages based on their content.

## Usage

``` r
suppress_warnings_if(
  expr,
  msg_contains = "",
  fixed = TRUE,
  perl = !fixed,
  ignore.case = FALSE,
  negate = FALSE,
  classes = "warning"
)

suppress_messages_if(
  expr,
  msg_contains = "",
  fixed = TRUE,
  perl = !fixed,
  ignore.case = FALSE,
  negate = FALSE,
  classes = "message"
)
```

## Arguments

- expr:

  The expression to evaluate.

- msg_contains:

  A string to match against the message. Defaults to an empty string
  (matches all messages).

- fixed:

  A logical indicating whether `msg_contains` should be matched as a
  fixed string..

- perl:

  A logical indicating whether Perl-compatible regular expressions
  should be used for `msg_contains`.

- ignore.case:

  A logical indicating whether the case of `msg_contains` should be
  ignored.

- negate:

  A logical indicating whether the message matching should be negated
  (e.g., suppress messages that do *not* match `msg_contains`).

- classes:

  A character vector of warning or message classes to suppress.

## Value

The result of evaluating `expr`, with specified warnings or messages
suppressed.

## Examples

``` r
# Suppress warnings containing specific text
suppress_warnings_if(warning("This is a warning"), "This")

# Suppress messages unless they contain specific text
suppress_messages_if(
  message("13 files processed"),
  "\\d{2, }",
  fixed = FALSE,
  negate = TRUE
)
```
