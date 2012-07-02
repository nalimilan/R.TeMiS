\name{docWordLengthsDlg}
\alias{docWordLengthsDlg}
\title{Word lengths per document}
\description{Build a table of word lengths over documents of a corpus.}
\details{This dialog allows creating tables providing seven vocabulary complexity measures
         for each document in a corpus:
         \itemize{
             \item{total number of words}
             \item{number and percent of long words (\sQuote{long} being defined as \sQuote{at
                   least seven characters}}
             \item{number and percent of very long words (\sQuote{very long} being defined as
                   \sQuote{at least ten characters}}
             \item{average word length}
         }

         These measures are calculated from a new document-term matrix: they do not depend on the
         processing options applied to the corpus when importing (e.g. stemming). Thus, \sQuote{words},
         (as opposed to \sQuote{terms}) are defined as any text unit of two or more characters;
         numbers and punctuation are removed, but no other changes are done to the text.

         Two additional columns are provided to account for the value of the three measures in the
         whole corpus. "Corpus mean" is simply the average value of measures over all documents.
         "Corpus total" is the sum of the number of (long) terms, the percentage of long terms and
         the average word length in the corpus when taken as a single document. Both versions of
         this measure are legitimate, but prompt different interpretations that should not be confused
         (though they are usually very close); on the contrary, interpretation of the summed or mean
         number of (long) terms is immediate. See \code{\link{wordLengthsTable}} for more details.
}

\seealso{\code{\link{wordLengthsTable}}, \code{\link{varWordLengthsDlg}} ,
         \code{\link{DocumentTermMatrix}}, \code{\link{table}}, \code{\link{barplot}} }
