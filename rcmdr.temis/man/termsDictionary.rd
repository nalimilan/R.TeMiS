\name{termsDictionary}
\alias{termsDictionary}
\alias{termsDictionaryAlpha}
\alias{termsDictionaryOcc}
\title{Dictionary of terms found in a corpus}
\usage{
termsDictionary(dtm, order = c("alphabetic", "occurrences"))
}
\arguments{
  \item{dtm}{a document-term matrix.}
  \item{order}{whether to sort words alphabetically, or by number of (stemmed) occurrences.}
}
\description{List all of the words that were found in the corpus, and stemmed terms present
             in the document-term matrix, together with their number of occurrences.}
\details{Words found in the corpus before stopwords removal and stemming are printed, together with
         the corresponding stemmed term that was eventually added to the document-term matrix, if stemming
         was enabled. Occurrences found before and after stemming are also shown.

         The column \dQuote{Stopword?} indicates whether the corresponding word is present in the list
         of stopwords for the corpus language. Words that were actually removed, either automatically by
         stopwords removal at import time, or manually via the Text mining->Terms->Exclude terms from analysis...
         menu, are signalled in the \dQuote{Removed?} column. All other words are present in the final
         document-term matrix, in their original or in their stemmed form.
}
\seealso{\code{\link[tm]{DocumentTermMatrix}}, \code{\link{restrictTermsDlg}}, \code{\link{freqTermsDlg}},
         \code{\link{termCoocDlg}} }
