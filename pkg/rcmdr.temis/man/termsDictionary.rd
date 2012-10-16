\name{termsDictionary}
\alias{termsDictionary}
\alias{doTermsDictionary}
\title{Dictionary of terms found in a corpus}
\description{List all of the words that were found in the corpus, and stemmed terms present
             in the document-term matrix.}
\details{If stemming was not enabled, this command simply returns the names of the columns of
         the document-term matrix corresponding to the corpus. If stemming was enabled, words
         found in the corpus before stemming are printed together with the corresponding stemmed
         term that was eventually added to the document-term matrix.

         Words that were removed via processing at import time (stopwords, numbers) are not shown,
         but those removed manually via the Text mining ->Terms->Exclude terms from analysis... menu,
         are listed even if they have been removed from the document-term matrix.
}
\seealso{\code{\link{DocumentTermMatrix}}, \code{\link{restrictTermsDlg}}, \code{\link{freqTermsDlg}},
         \code{\link{termCoocDlg}} }
