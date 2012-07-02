\name{caTools}
\alias{rowSubsetCa}
\alias{colSubsetCa}
\alias{rowCtr}
\alias{colCtr}
\title{Correspondence analysis helper functions}
\description{Restrict a correspondence analysis object to some rows or columns, and get row and
             column contributions.}
\usage{
rowSubsetCa(obj, indices)
colSubsetCa(obj, indices)
rowCtr(obj, dim)
colCtr(obj, dim)
}
\arguments{
    \item{obj}{A correspondence analysis object as returned by \code{link{ca}}.}
    \item{indices}{An integer vector of indices of rows/columns to be kept.}
    \item{dim}{An integer vector of dimensions to which point contributions should be computed.}
}
\details{These functions are used to extend the features of the \code{ca} package.

         \code{rowSubsetCa} and \code{colSubsetCa} take a \code{link{ca}} object and return it, keeping
         only the rows/columns that were specified. These objects are only meant for direct plotting,
         as they do not contain the full CA results: using them for detailed analysis would be
         misleading.

         \code{rowCtr} and \code{colCtr} return the absolute contributions of all rows/columns to the
         specified axes of the CA. If several dimensions are passed, the result is the sum of the
         contributions to each axis.
}

\seealso{\code{\link{showCorpusCaDlg}}, \code{\link{plotCorpusCa}}, \code{\link{plot.ca}},
         \code{\link{ca}}}
