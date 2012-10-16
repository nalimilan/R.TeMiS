\name{termCoocDlg}
\alias{termCoocDlg}
\title{Show terms co-occurrences}
\description{Show terms that are the most associated with one or several reference terms.}
\details{This dialog allows printing the terms that are most associated with one or several
         given terms, according to the document-term matrix of the corpus. Co-occurrence is measured
         by the Chi-squared distance between the (column) profiles of two terms in the matrix: the
         smaller the distance, the more terms have similar occurrence patterns.

         When a variable is selected, the operation is run separately on each sub-matrix constituted
         by the documents that are members of the variable level. If the term does not appear in a
         level, \code{NA} is returned.

         When several terms are entered, the operation is simply run several times separately.
}

\seealso{\code{\link{termChisqDist}}, \code{\link{DocumentTermMatrix}}, \code{\link{restrictTermsDlg}},
         \code{\link{termsDictionary}}, \code{\link{freqTermsDlg}} }
