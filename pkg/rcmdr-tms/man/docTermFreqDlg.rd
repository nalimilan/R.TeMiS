\name{docTermFreqDlg}
\alias{docTermFreqDlg}
\title{Term frequency table per document}
\description{Build a contingency table of term occurrences among documents of a corpus.}
\details{This dialog allows creating tables crossing terms occurrences and documents.
         Three measures are provided:\cr
         \itemize{
         \item{Row percent corresponds to the part of chosen term's occurrences over all terms
         found in a given document (i.e., the document word count after processing). This
         conceptually corresponds to line percents, except that only the columns of the
         document-term matrix that match the given terms are shown.}
         \item{Column percent corresponds to the part of the chosen term's occurrences that
         appear in each of the documents. This measure corresponds to the strict definition
         of column percents.}
         \item{Absolute counts merely returns the relevant part of the document-term matrix.}
         }

         The kind of plot to be drawn is automatically chosen from the selected measure. Row
         percents lead to bar plots, since the total sum of shown columns (terms) doesn't add up
         to 100%. On the other hand, and for the same reason, column percents prompt pie charts
         to be drawn. Absolute counts are also represented with bar plots, so that the vertical
         axis reports number of occurrences.

         When either several pie charts are drawn for each word, or a single word has been entered,
         the string '%T' in the plot title will be replaced with the name of the term.
}

\seealso{\code{\link{DocumentTermMatrix}}, \code{\link{table}}, \code{\link{barplot}}, \code{\link{pie}} }
