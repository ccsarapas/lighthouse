#' Open dataframe in Excel
#'
#' Saves dataframe as .csv in R temp directory, then opens in Excel. The .csv
#' will have a randomly-generated name unless otherwise specified in `name`.
#'
#' @param df The dataframe to open in Excel.
#' @param name (Optional) The name to use for the .csv file. If not provided, a random name will be generated.
#' @param na (Optional) The string to use for missing values in the .csv file. Defaults to an empty string.
#'
#' @export
in_excel <- function(df, name, na = "") {
  csv_dir <- file.path(tempdir(), "csv")
  if (!dir.exists(csv_dir)) {
    dir.create(csv_dir)
  }
  if (missing(name)) {
    csv_path <- tempfile(tmpdir = csv_dir, fileext = ".csv")
  } else {
    csv_path <- file.path(csv_dir, paste0(name, ".csv"))
  }
  readr::write_excel_csv(df, csv_path, na = na)
  shell.exec(csv_path)
}


#' Print specified number of tibble rows
#'
#' @export
print_n <- function(x, n, ...) print(x, n = n, ...)

#' Print all tibble rows
#'
#' Actually limits printing to `RStudioPreference` `"console_max_lines"` (or
#' 1000 lines if not running in RStudio) unless otherwise specified in `max`.
#' Works only with `tibble`s, not `base::data.frame`s.
#'
#' @export
print_all <- function(x, ..., max = NULL) {
  if (is.null(max)) {
    if (Sys.getenv("RSTUDIO") == 1) {
      max <- rstudioapi::readRStudioPreference(
        "console_max_lines",
        default = 1000
      )
    } else {
      max <- 1000
    }
  }
  print(x, n = max, ...)
}

#' Add a plot to a PowerPoint slide
#'
#' This function adds a new slide to a PowerPoint presentation with a plot
#' centered beneath the title, scaled to be as large as possible within the
#' specified margins while preserving aspect ratio.
#'
#' @param pptx an object of class rpptx, as created by officer::read_pptx()
#' @param title optional slide title
#' @param plot the plot to add. Default is the last plot created.
#' @param w plot width in inches
#' @param h plot height in inches
#' @param bg background color of the plot area
#' @param w_margin horizontal margin in inches
#' @param h_margin vertical margin in inches
#' @param layout slide layout to use
#' @param ... additional arguments passed to officer::ph_with()
#'
#' @return An updated rpptx object
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(officer)
#'
#' plot <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point()
#'
#' pptx <- read_pptx()
#' pptx <- pptx |>
#' add_plot_slide("Larger Elements", plot, w = 5, h = 3.5) |>
#'   add_plot_slide("Smaller Elements", plot, w = 10, h = 7) |>
#'   add_plot_slide("Wider", plot, w = 9, h = 3) |>
#'   add_plot_slide("Taller", plot, w = 4, h = 6)
#'
#' path <- paste0(tempfile(), ".pptx")
#' print(pptx, target = path)
#' file.open(path)
#'
#' invisible(file.remove(path))
#' }
#'
#' @export
add_plot_slide <- function(pptx,
                           title = NULL,
                           plot = ggplot2::last_plot(),
                           w = 7,
                           h = 4,
                           bg = "white",
                           w_margin = 0.15,
                           h_margin = 0.15,
                           layout = "Title and Content",
                           ...) {
  rlang::check_installed("officer")
  pptx <- officer::add_slide(pptx, layout = layout)
  if (!is.null(title)) {
    pptx <- officer::ph_with(pptx, title, location = officer::ph_location_type("title"))
  }
  if (!is.null(plot)) {
    size <- officer::slide_size(pptx)
    w_slide <- size$width
    h_slide <- size$height
    title_props <- officer::layout_properties(pptx, layout = layout)
    h_title <- title_props[title_props$type == "title", "cy"]
    top_title <- title_props[title_props$type == "title", "offx"]
    w_max <- w_slide - (2 * w_margin)
    h_max <- h_slide - h_title - top_title - (2 * h_margin)
    ar_crit <- w_max / h_max
    ar <- w/h
    if (ar > ar_crit) {
      w_out <- w_max
      h_out <- w_out / ar
    } else {
      h_out <- h_max
      w_out <- h_out * ar
    }
    scale <- w_out / w
    l_out <- (w_slide - w_out) / 2
    t_out <- top_title + h_title + ((h_slide - top_title - h_title - h_out) / 2)
    pptx <- officer::ph_with(
      pptx,
      plot,
      location = officer::ph_location(left = l_out, top = t_out, w = w_out, h = h_out),
      scale = scale,
      bg = bg,
      ...
    )
  }
  pptx
}

#' Write a styled data frame to an Excel file
#'
#' A wrapper around `openxlsx::write.xlsx()` with default styling options
#' including frozen, bolded row headings and auto column widths.
#'
#' @param x The data frame to write.
#' @param file The path to the output Excel file.
#' @param asTable Whether to write as an Excel table.
#' @param overwrite Whether to overwrite an existing file.
#' @param ... Additional arguments passed to `openxlsx::write.xlsx()`.
#'
#' @export
write_xlsx_styled <- function(x, file, asTable = FALSE, overwrite = TRUE, ...) {
  rlang::check_installed("openxlsx")
  openxlsx::write.xlsx(
    x,
    file,
    asTable,
    overwrite,
    headerStyle = openxlsx::createStyle(textDecoration = "bold"),
    firstRow = TRUE,
    colWidths = "auto",
    ...
  )
}

