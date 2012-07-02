\name{termsAssocDlg}
\alias{termsAssocDlg}
\title{Show associated terms}
\description{Show terms that are the most associated with one or several reference terms.}
\details{This dialog allows printing the terms that are most correlated with one or several
         given terms, according to the document-term matrix of the corpus. This corresponds to
         a within-document correlation. When a variable is selected, the operation is run separately
         on each sub-matrix constituted by the documents that are members of the variable level.

         If no term is correlated with the reference term, no term will be shown, and \code{numeric(0)}
         will be reported: run the operation with a lower correlation limit to get terms.

         When several terms are entered, the operation is run several times separately: the result
         does not correspond to the terms correlated with all of the reference terms taken together.
}

\seealso{\code{\link{findAssocs}}, \code{\link{DocumentTermMatrix}}, \code{\link{restrictTermsDlg}},
         \code{\link{listTerms}}, \code{\link{freqTermsDlg}} }
