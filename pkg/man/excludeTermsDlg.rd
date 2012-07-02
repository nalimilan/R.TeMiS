\name{excludeTermsDlg}
\alias{excludeTermsDlg}
\title{Exclude terms from the analysis}
\description{Remove terms from the document-term matrix of a corpus to exlclude them from further analyses.}
\details{This dialog allows removing columns of the document-term matrix so that the terms corresponding
         to them are no longer taken into account. The terms are not removed from the corpus's documents,
         but won't be considered by any operations run later, from listing terms of the corpus to computing
         a correspondence analysis.
}

\seealso{\code{\link{findAssocs}}, \code{\link{DocumentTermMatrix}},
         \code{\link{listTerms}}, \code{\link{freqTermsDlg}} }
