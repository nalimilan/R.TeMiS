\name{vocabularyTable}
\alias{vocabularyTable}
\title{Vocabulary summary table}
\description{Build a table summarizing vocabulary, optionally over a variable.}
\usage{
vocabularyTable(termsDtm, wordsDtm, variable = NULL, unit = c("document", "global"))
}
\arguments{
  \item{termsDtm}{A document-term matrix containing terms (i.e. extracted from a possibly stemmed corpus).}
  \item{wordsDtm}{A document-term matrix contaning words (i.e. extracted from a plain corpus).}
  \item{variable}{A vector with one element per document indicating to which category it belongs.
                  If \code{NULL}, per-document measures are returned.}
  \item{unit}{When \code{variable} is not \code{NULL}, defines the way measures are aggregated
              (see below).}
}
\details{This dialog allows creating tables providing several vocabulary measures
         for each document or each category of documents in the corpus:
         \itemize{
             \item{total number of terms}
             \item{number and percent of unique terms (i.e. appearing at least once)}
             \item{number and percent of hapax legomena (i.e. terms appearing once and only once)}
             \item{total number of words}
             \item{number and percent of long words (\dQuote{long} being defined as \dQuote{at
                   least seven characters}}
             \item{number and percent of very long words (\dQuote{very long} being defined as
                   \dQuote{at least ten characters}}
             \item{average word length}
         }

         \emph{Words} are defined as the forms of two or more characters present in the texts
         before stemming and stopword removal. On the contrary, unique \emph{terms} are extracted
         from the global document-term matrix, which means they do not include words that were
         removed by treatments ran at the import step, and that words different in the original
         text might become identical terms if stemming was performed. This can be considered the
         \dQuote{correct} measure, since the purpose of corpus processing is exactly that: mark
         different forms of the same term as similar to allow for statistical analyses.

         Please note that percentages for \emph{terms} and \emph{words} are computed with regard
         respectively to the total number of terms and of words, so the denominators are not the
         same for all measures. See \code{\link{vocabularyDlg}}.

         When \code{variable} is not \code{NULL}, \code{unit} defines two different ways of
         aggregating per-document statistics into per-category measures:
         \itemize{
             \item{\code{document}: }{Values computed for each document are simply averaged for
                                      each category.}
             \item{\code{global}: }{Values are computed for each category taken as a whole: word
                                    counts are summed for each category, and ratios and average
                                    are calculated for this level only, from the summed counts.}
         }
         In both cases, the \dQuote{Corpus} column follows the above definition.
}

\seealso{\code{\link{vocabularyDlg}}, code{\link{DocumentTermMatrix}}, \code{\link{table}}, }
