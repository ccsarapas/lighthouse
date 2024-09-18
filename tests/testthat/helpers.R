mtcars2 <- transform(mtcars, trans = ifelse(am == 0, "auto", "manual"))

ts_data <- tibble::tibble(
    chr = c("a", "c", "b"),
    fct = factor(c("lvl 1", "lvl 2", "lvl 1"), levels = c("lvl 1", "lvl 2")),
    fct_empty = factor(
      c("lvl 1", "lvl 2", "lvl 1"),
      levels = c("lvl 1", "lvl 2", "empty lvl 3")
    ),
    num = c(9, 11, 2),
    num_bin = c(0, 1, 0),
    num_bin_all_0 = c(0, 0, 0),
    num_two_vals = c(0, 2, 0),
    lgl = c(TRUE, TRUE, FALSE),
    lgl_all_FALSE = c(FALSE, FALSE, FALSE),
    date = as.Date(c("2023-06-15", "2022-09-02", "2023-06-15")),
    POSIXct = as.POSIXct(
      c("2022-02-13 16:12:47", "2023-11-23 09:00:07", "2023-05-04 11:41:05")
    ),
    POSIXlt = as.POSIXlt(
      c("2023-05-18 12:16:03", "2023-12-01 00:15:00", "2023-12-01 04:15:30")
    )
  ) %>% 
  dplyr::mutate(dplyr::across(
    c(chr, num, lgl, lgl_all_FALSE),
    \(x) c(x[1:2], NA),
    .names = "{.col}_na"
  ))
