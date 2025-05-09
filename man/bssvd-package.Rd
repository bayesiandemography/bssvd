% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bssvd-package.R
\docType{package}
\name{bssvd-package}
\alias{bssvd}
\alias{bssvd-package}
\title{Functions to Create Objects Describing Regularities in
Demographic Rates}
\description{
Functions for preparing data for objects that describe
regularities in demographic rates. The descriptions
are obtained by applying singular value decompositions
to data in international databases. These packages
support package \strong{bage}. They are designed for use
by developers, not data analysts.
}
\section{Human Fertility Database}{

\itemize{
\item \code{\link[=data_ssvd_hfd]{data_ssvd_hfd()}} Prepare HFD data to created scaled SVD object
\item \code{\link[=hfd_tidy]{hfd_tidy()}} Initial tidying of HFD data
\item \code{\link[=coef_hfd]{coef_hfd()}} Extract coef from scaled SVD of HFD data
\item \code{\link[=asfr_subset]{asfr_subset()}} Subset of HFD data
}
}

\section{Human Mortality Database}{

\itemize{
\item \code{\link[=data_ssvd_hmd]{data_ssvd_hmd()}} Prepare HMD data to created scaled SVD object
\item \code{\link[=coef_hmd]{coef_hmd()}} Extract coef from scaled SVD of HMD data
}
}

\section{OECD Labour Force Participation data}{

\itemize{
\item \code{\link[=data_ssvd_lfp]{data_ssvd_lfp()}} Prepare labour force data to created scaled-SVD object
\item \code{\link[=coef_lfp]{coef_lfp()}} Extract coef from scaled SVD of labour force data
\item \code{\link[=oecd_lfp_subset]{oecd_lfp_subset()}} Subset of labour force data
}
}

\section{General purpose functions}{

\itemize{
\item \code{\link[=make_matrix_and_offset]{make_matrix_and_offset()}} Create components for scaled SVD object
}
}

\seealso{
Useful links:
\itemize{
  \item \url{https://bayesiandemography.github.io/bssvd/}
}

}
\author{
\strong{Maintainer}: John Bryant \email{john@bayesiandemography.com}

}
\keyword{internal}
