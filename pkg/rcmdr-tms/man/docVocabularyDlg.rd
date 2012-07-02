\name{docVocabularyDlg}
\alias{docVocabularyDlg}
\title{Term frequency table per document}
\description{Build a table of vocabulary over documents of a corpus.}
\details{This dialog allows creating tables providing three vocabulary measures for each document
         in a corpus: total number of terms, number of unique terms (i.e. number of terms appearing
         at least once in the document), vocabulary diversity index (ratio of the latter over the
         former figure). Note all these measures are calculated from the document-term matrix, which
         means they do not include words that were removed by treatments ran at the import step, and
         that words different in the original text might become identical terms if stemming was
         performed. This can be considered the "correct" measures, since the purpose of corpus
         processing is exactly that: mark different forms of the same term as similar to allow for
         statistical analyses.

         Two additional columns are provided to account for the value of the three measures in the
         whole corpus. "Corpus mean" is simply the average value of measures over all documents.
         "Corpus total" is the sum of the number of (unique) terms, and the vocabulary diversity of
         the corpus when taken as a single document. Both versions of the diversity measure are
         legitimate, but prompt different interpretations that shouldn't be confused.

         When plotting the number of (unique) terms, corpus total is not shown, since it would always
         exceed by far the values for separate documents.
}

\seealso{\code{\link{DocumentTermMatrix}}, \code{\link{table}}, \code{\link{barplot}},
         \code{\link{varVocabularyDlg}} }
