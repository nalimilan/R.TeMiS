\name{wordLengthsTable}
\alias{wordLengthsTable}
\title{Word lengths table}
\description{Build a table of word lengths, optionally over a variable.}
\usage{
wordLengthsTable(lengthDtms, variable = NULL, unit = c("document", "global"))
}
\arguments{
  \item{lengthDtms}{A document-term matrix.}
  \item{variable}{A vector of the same length as \code{lengthDtm} giving indexes according
                  to which categories should be defined. If \code{NULL}, per-document measures
                  are returned.}
  \item{unit}{When \code{variable} is not \code{NULL}, defines the way measures are agregated
              (see below).}
}
\details{This dialog allows creating tables providing seven vocabulary complexity measures
         for each document or each category of documents in the corpus:
         \itemize{
             \item{total number of words}
             \item{number and percent of long words (\sQuote{long} being defined as \sQuote{at
                   least seven characters}}
             \item{number and percent of very long words (\sQuote{very long} being defined as
                   \sQuote{at least ten characters}}
             \item{average word length}
         }

         When \code{variable} is not \code{NULL}, \code{unit} defines two different ways of
         aggregating per-document statistics into per-category measures:
         \itemize{
             \item{\code{document}: }{Values computed for each document are simply averaged for
                                    each category.}
             \item{\code{global}: }{Values are computed for each category taken as a whole: word
                                  counts are summed for each category, and ratios and average
                                  are calculated for this level only, from the summed counts.}
         }
         In both cases, the \sQuote{Corpus} column follows the above definition.
}

\seealso{\code{\link{docWordLengthsDlg}}, \code{\link{varWordLengthsDlg}},
         \code{\link{DocumentTermMatrix}}, \code{\link{table}}, }
