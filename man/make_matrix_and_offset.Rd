% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper-functions.R
\name{make_matrix_and_offset}
\alias{make_matrix_and_offset}
\title{Create Components Needed by a Scaled SVD Object}
\usage{
make_matrix_and_offset(x, transform = c("log", "logit", "none"), n_comp = 10)
}
\arguments{
\item{x}{A matrix with value such as rates,
probabilities, or means.}

\item{transform}{\code{"log"}, \code{"logit"}, or \code{"none"}.
Defaults to \code{"log"}.}

\item{n_comp}{Number of components.}
}
\value{
A named list with two elements:
\itemize{
\item \code{matrix}, a numeric matrix
\item \code{offset}, a numeric vector
}
}
\description{
Use the \link[base:svd]{Singular Value Decomposition}
(SVD) to construct a parsimonious representation
of a set of rates, probabilities, means, or
other values. The construction proceeds as follows:
\itemize{
\item transform values in matrix \code{x} (eg take logs)
\item carry out a SVD on the transformed version of \code{x}
\item centre and scale the results from the SVD to produce
matrix \code{matrix} and vector \code{offset}.
}
}
\details{
If
\itemize{
\item \eqn{X} is a the matrix of transformed values
of \code{x}
\item \eqn{F} is the matrix called \code{matrix},
\item \eqn{g} is the vector called \code{offset}, and
\item \eqn{\alpha} is a vector of standard normal variates,
}

and

\deqn{\beta = F \alpha + g}

then \eqn{\beta} should look like a randomly-selected
column from \eqn{X}.

Matrix \code{x} typically has age along the rows,
and some combination of classification variables,
such as country and time, along the columns.
One exception is when the SVD is used to capture
the relationship between female and male rates,
in which case rows are formed by interacting
sex and age. See below for an example.

When \code{scale} is \code{"log"} or \code{"logit"},
\code{make_matrix_and_offset()} converts any \code{0}s in
\code{x} to values just above \code{0} before
applying the log or logit function.

When \code{scale} is \code{"logit"},
\verb{make_matrix_and_offset() also converts any }1\verb{s to values just below }1`.
}
\examples{
x <- matrix(rgamma(n = 150, shape = 1),
            nrow = 10,
            ncol = 15)
x
make_matrix_and_offset(x)
make_matrix_and_offset(x, transform = "none")
make_matrix_and_offset(x, n_comp = 2)
}
