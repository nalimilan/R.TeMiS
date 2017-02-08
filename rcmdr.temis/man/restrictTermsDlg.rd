\name{restrictTermsDlg}
\alias{restrictTermsDlg}
\title{Select or exclude terms}
\description{Remove terms from the document-term matrix of a corpus to exclude them from further analyses.}
\details{This dialog allows to only retain specified terms when you want to concentrate your analysis on an
         identified vocabulary, or to exclude a few terms that are known to interfere with the analysis.

         Terms that are not retained or that are excluded are removed from the document-term matrix, and are
         thus no longer taken into account by any operations run later, like listing terms of the corpus or
         computing a correspondence analysis. They are not removed from the corpus's documents.
}

\seealso{\code{\link{DocumentTermMatrix}},
         \code{\link{termsDictionary}}, \code{\link{freqTermsDlg}} }
