\name{termFreqDlg}
\alias{termFreqDlg}
\title{Term frequency table per document or per meta-data variable}
\description{Build a contingency table of term occurrences per document, or over a meta-data variable
             of a corpus.}
\details{This dialog allows creating tables crossing terms occurrences and documents, possibly grouped
         by the levels of a meta-data variable: for each of themn, the sum of the term occurrences
         found in all of the documents included in it is computed. Three measures are provided:
         \itemize{
         \item{Row percent corresponds to the part of chosen term's occurrences over all terms
         found in a given category (i.e., the sum of word counts of all documents from the category
         after processing). This conceptually corresponds to line percents, except that only the
         columns of the document-term matrix that match the given terms are shown.}
         \item{Column percent corresponds to the part of the chosen term's occurrences that
         appear in each of the documents from a given category. This measure corresponds to the
         strict definition of column percents.}
         \item{Absolute counts returns the relevant part of the document-term matrix, but summed
         after grouping documents according to their category.}
         }

         The kind of plot to be drawn is automatically chosen from the selected measure. Row
         percents lead to bar plots, since the total sum of shown columns (terms) doesn't add up
         to 100%. On the other hand, and for the same reason, column percents prompt pie charts
         to be drawn. Absolute counts are also represented with bar plots, so that the vertical
         axis reports number of occurrences.

         When either several pie charts are drawn for each word, or a single word has been entered,
         the string \dQuote{\%T} in the plot title will be replaced with the name of the term.
         In all cases, the string \dQuote{\%V} will be replaced with the name of the selected variable.
}

\seealso{\code{\link{setCorpusVariables}}, \code{\link{meta}}, \code{\link{DocumentTermMatrix}},
         \code{\link{table}}, \code{\link{barplot}}, \code{\link{pie}} }
