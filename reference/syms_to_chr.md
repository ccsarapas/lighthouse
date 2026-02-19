# Print symbols as a character vector

A helper for converting a list of symbols to a character vector.
Primarily intended to help convert old code given updates to
[`summary_report()`](https://ccsarapas.github.io/lighthouse/reference/summary_report.md).

## Usage

``` r
syms_to_chr(..., width = 80, indent = 0, indent_first = indent)
```

## Arguments

- ...:

  symbols.

- width:

  maximum width of each line of output.

- indent:

  number of spaces to indent.

- indent_first:

  number of spaces to indent the first line.

## Examples

``` r
syms_to_chr(
  various, very, video, view, village, visit, vote, wage, wait, walk, wall,
  want, war, warm, wash, waste, watch, water, way, we, wear, wednesday, wee,
  week, weigh, welcome, well, west, what, when, where, whether, which, white,
  who, whole, why, wide, wife, will, win, wind, window, wish, with, within,
  without, woman, wonder, wood, word, work, world, worry, worse, worth, would,
  write, wrong, year, yes, yesterday, yet, you, young
)
#> c("various", "very", "video", "view", "village", "visit", "vote", "wage",
#>   "wait", "walk", "wall", "want", "war", "warm", "wash", "waste", "watch",
#>   "water", "way", "we", "wear", "wednesday", "wee", "week", "weigh", "welcome",
#>   "well", "west", "what", "when", "where", "whether", "which", "white", "who",
#>   "whole", "why", "wide", "wife", "will", "win", "wind", "window", "wish",
#>   "with", "within", "without", "woman", "wonder", "wood", "word", "work",
#>   "world", "worry", "worse", "worth", "would", "write", "wrong", "year", "yes",
#>   "yesterday", "yet", "you", "young")
```
