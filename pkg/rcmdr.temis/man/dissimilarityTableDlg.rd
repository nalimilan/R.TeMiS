\name{dissimilarityTableDlg}
\alias{dissimilarityTableDlg}
\title{Documents/Variables Dissimilarity Table}
\description{Build a dissimilarity table reporting Chi-squared distances between documents and/or
             levels of a variable.}
\details{This dialog can be used in two main ways. If "Document" or one variable is
         selected for both rows and columns, the one-to-one dissimilarity between all documents
         or levels of the variable will be reported. If a different variables are chosen for
         rows and for columns, a cross-dissimilarity table will be created; such a table can be
         used to assess whether a document or variable level is closer to another variable level.

         In all cases, the reported value is the Chi-squared distance between the two documents or
         variable levels, computed from the total document-term matrix (aggregated for variables).
}

\seealso{\code{\link{corpusDissimilarity}}, \code{\link{setCorpusVariables}}, \code{\link{meta}},
         \code{\link{DocumentTermMatrix}}, \code{\link{dist}} }
