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
  \item{variable}{A vector of the same length as \code{lengthDtm} giving indexes according
                  to which categories should be defined. If \code{NULL}, per-document measures
                  are returned.}
  \item{unit}{When \code{variable} is not \code{NULL}, defines the way measures are agregated
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

         Please note that percentages for \emph{terms} and \emph{words} are computed with regard
         respectively to the total number of terms and of words, so the denominators are not the
         same for all measures. See \code{\link{docVocabularyDlg}}.

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

\seealso{\code{\link{docVocabularyDlg}}, \code{\link{varVocabularyDlg}},
         \code{\link{DocumentTermMatrix}}, \code{\link{table}}, }
