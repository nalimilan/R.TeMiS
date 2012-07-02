\name{termsAssocDlg}
\alias{termsAssocDlg}
\title{Show associated terms}
\description{Show terms that are the most associated with one or several reference terms.}
\details{This dialog allows printing the terms that are most correlated with one or several
         given terms, according to the document-term matrix of the corpus. This corresponds to
         a within-document correlation, and does not for example measure correlation within
         meta-data categories.

         If no term is correlated with the reference term, no term will be shown: run the operation
         with a lower coefficient to see terms.

         When several terms are entered, the operation is run several times separately: the result
         does not correspond to the terms correlated with all of the reference terms taken together.
}

\seealso{\code{\link{findAssocs}}, \code{\link{DocumentTermMatrix}}, \code{\link{excludeTermsDlg}},
         \code{\link{listTerms}}, \code{\link{freqTermsDlg}} }
