\name{varWordLengthsDlg}
\alias{varWordLengthsDlg}
\title{Word lengths per variable}
\description{Build a table of word lengths over meta-data variables of a corpus.}
\details{This dialog allows creating tables providing seven vocabulary complexity measures
         for each of the categories of a corpus variable:
         \itemize{
             \item{total number of words}
             \item{number and percent of long words (\sQuote{long} being defined as \sQuote{at
                   least seven characters}}
             \item{number and percent of very long words (\sQuote{very long} being defined as
                   \sQuote{at least ten characters}}
             \item{average word length}
         }

         Two different units can be selected for the analysis. If "Document" is selected, values
         reported for each category correspond to the mean of the values for each of its documents;
         a mean column for the whole corpus is also provided. If "Category" is selected, these values
         correspond to the sum of the number of (long) terms for each of the categories' documents,
         to the percentage of long terms (ratio of the summed numbers of terms) and the average word
         length of the category when taken as a single document. Both versions of this measure are
         legitimate, but prompt different interpretations that should not be confused (though they
         are usually very close); on the contrary, interpretation of the summed or mean number of
         (long) terms is immediate. See \code{\link{wordLengthsTable}} for more details.
}

\seealso{\code{\link{wordLengthsTable}}, \code{\link{docWordLengthsDlg}},
         \code{\link{setCorpusVariables}},
         \code{\link{meta}}, \code{\link{DocumentTermMatrix}}, \code{\link{table}},
         \code{\link{barplot}} }
