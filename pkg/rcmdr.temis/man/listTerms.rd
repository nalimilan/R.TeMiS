\name{listTerms}
\alias{listTerms}
\title{List all terms found in a corpus}
\description{List all of the terms present in the document-term matrix of a corpus.}
\details{This command simply returns the names of the columns of the document-term
         matrix corresponding to the corpus. Terms appear in their processed form:
         they do not correspond to the full words present in the original text. Notably,
         these terms may have been stemmed, and many words may have been removed, either
         via processing at import time (stopwords, numbers) or manually, via the Text mining
         ->Terms->Exclude terms from analysis... menu.
}
\seealso{\code{\link{DocumentTermMatrix}}, \code{\link{restrictTermsDlg}}, \code{\link{freqTermsDlg}},
         \code{\link{termCoocDlg}} }
