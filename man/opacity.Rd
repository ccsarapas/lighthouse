% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{opacity}
\alias{opacity}
\alias{after_opacity}
\alias{before_opacity}
\title{Translate colors before and after alpha blending}
\usage{
after_opacity(color, alpha, bg = "white")

before_opacity(color, alpha, bg = "white")
}
\arguments{
\item{color}{The starting color as a color name, hex code, or RGB triplet.}

\item{alpha}{The opacity of the foreground color, a number between 0 and 1.}

\item{bg}{The background color for blending, as a color name, hex code, or RGB triplet. Defaults to "white".}
}
\value{
\itemize{
\item \code{before_opacity}: The original color before alpha blending, as a hex code.
\item \code{after_opacity}: The blended color after alpha blending, as a hex code.
}
}
\description{
These functions translate colors between their original RGB values and their
RGB values after alpha blending with a background color. \code{before_opacity}
calculates the original color given the blended color, while \code{after_opacity}
calculates the blended color given the original color.
}
\examples{
red <- "red"
red_50 <- after_opacity(red, 0.5)
red_back <- before_opacity(red_50, 0.5)
scales::show_col(c(red, red_50, red_back), ncol = 3)

color_blends <- sapply(
  c("red", "blue", "yellow", "white", "black", "gray50"),
  after_opacity,
  color = "red",
  alpha = 0.5
)
scales::show_col(color_blends)

}
