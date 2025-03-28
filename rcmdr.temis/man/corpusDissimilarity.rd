\name{corpusDissimilarity}
\alias{corpusDissimilarity}
\title{Cross-Dissimilarity Table}
\description{Build a cross-dissimilarity table reporting Chi-squared distances from two document-term
             matrices of the same corpus.}
\usage{
corpusDissimilarity(x, y)
}
\arguments{
  \item{x}{a document-term matrix}
  \item{y}{a document-term matrix}
}
\details{This function can be used to build a cross-dissimilarity table from two different variables
         of a corpus. It takes two versions of a document-term matrix, aggregated in different ways,
         and returns the Chi-squared distance between each combination of the tow matrices' rows. Thus,
         the resulting table has rows of \code{x} for rows, and rows of \code{y} for columns.
}

\seealso{\code{\link{dissimilarityTableDlg}}, \code{\link[tm]{DocumentTermMatrix}}, \code{\link{dist}} }
