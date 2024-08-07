% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operators.R
\name{not-in}
\alias{not-in}
\alias{\%!in\%}
\title{Match values not in vector}
\usage{
lhs \%!in\% rhs
}
\description{
Infix operator returning \code{TRUE} for elements of left operand (\code{lhs}) not
found in right operand (\code{rhs}). Equivalent to \code{!(lhs \%in\% rhs)}.
}
\examples{

"April" \%!in\% month.name
"Junvember" \%!in\% month.name

some_letters <- sample(letters, 10)
letters[letters \%in\% some_letters]
letters[letters \%!in\% some_letters]

}
