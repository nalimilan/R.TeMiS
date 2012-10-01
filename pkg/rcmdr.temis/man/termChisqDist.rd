\name{termChistDist}
\alias{termChistDist}
\title{Show terms co-occurrences}
\description{Show terms that are the most associated with one or several reference terms.}
\usage{
termChistDist(term, dtm, n = 5, variable = NULL)
}
\arguments{
  \item{term}{A character vector of length 1 corresponding to the name of a column of \code{dtm}.}
  \item{dtm}{A document-term matrix.}
  \item{n}{The number of terms to return.}
  \item{variable}{An optional vector of the same length as the number of rows in \code{dtm}, giving
    the levels by which results should be reported.}
}
\details{This function allows printing the terms that are most associated with one or several
         given terms, according to the document-term matrix of the corpus. Co-occurrence is measured
         by the Chi-squared distance between the (column) profiles of two terms in the matrix: the
         smaller the distance, the more terms have similar occurrence patterns.

         When a variable is selected, the operation is run separately on each sub-matrix constituted
         by the documents that are members of the variable level. If the term does not appear in a
         level, \code{NA} is returned.
}

\seealso{\code{\link{termsCoocDlg}}, \code{\link{DocumentTermMatrix}}, \code{\link{restrictTermsDlg}},
         \code{\link{listTerms}}, \code{\link{freqTermsDlg}} }
