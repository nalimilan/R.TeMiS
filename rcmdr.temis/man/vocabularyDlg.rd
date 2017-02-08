\name{vocabularyDlg}
\alias{vocabularyDlg}
\title{Vocabulary Summary}
\description{Build vocabulary summary table over documents or a meta-data variable of a corpus.}
\details{This dialog allows creating tables providing several vocabulary measures
         for each document of a corpus, or each of the categories of a corpus variable:
         \itemize{
             \item{total number of terms}
             \item{number and percent of unique words, i.e. of words appearing at least once}
             \item{number and percent of hapax legomena, i.e. terms appearing once and only once}
             \item{total number of words}
             \item{number and percent of long words (\dQuote{long} being defined as \dQuote{at
                   least 7 characters}}
             \item{number and percent of very long words (\dQuote{very long} being defined as
                   \sQuote{at least 10 characters}}
             \item{average word length}
         }

         \emph{Words} are defined as the forms of two or more characters present in the texts
         before stemming and stopword removal. On the contrary, unique \emph{terms} are extracted
         from the global document-term matrix, which means they do not include words that were
         removed by treatments ran at the import step, and that words different in the original
         text might become identical terms if stemming was performed. This can be considered the
         \dQuote{correct} measure, since the purpose of corpus processing is exactly that: mark
         different forms of the same term as similar to allow for statistical analyses.

         Two different units can be selected for the analysis. If \dQuote{Document} is selected, values
         reported for each level correspond to the mean of the values for each of its documents;
         a mean column for the whole corpus is also provided. If \dQuote{Level} is selected, these values
         correspond to the sum of the number of terms for each of the categories' documents,
         to the percentage of terms (ratio of the summed numbers of terms) and the average word
         length of the level when taken as a single document. Both versions of this measure are
         legitimate, but prompt different interpretations that should not be confused; on the contrary,
         interpretation of the summed or mean number of (long) terms is immediate.

         This distinction does not make sense when documents (not levels of a variable) are used as the
         unit of analysis: in this case, \dQuote{level} in the above explanation corresponds to
         \dQuote{document}, and two columns are provided about the whole corpus. \dQuote{Corpus mean}
         is simply the average value of measures over all documents; \dQuote{Corpus total} is the sum
         of the number of terms, the percentage of terms  (ratio of the summed numbers of terms)
         and the average word length in the corpus when taken as a single document. See
         \code{\link{vocabularyTable}} for more details.
}

\seealso{\code{\link{vocabularyTable}}, \code{\link{setCorpusVariables}},
         \code{\link{meta}}, \code{\link{DocumentTermMatrix}}, \code{\link{table}},
         \code{\link{barchart}} }
