\name{varVocabularyDlg}
\alias{varVocabularyDlg}
\title{Word lengths per variable}
\description{Build a table of word lengths over meta-data variables of a corpus.}
\details{This dialog allows creating tables providing several vocabulary measures
         for each of the categories of a corpus variable:
         \itemize{
             \item{total number of terms}
             \item{number and percent of unique words, i.e. of words appearing at least once}
             \item{total number of words}
             \item{number and percent of long words (\dQuote{long} being defined as \dQuote{at
                   least 7 characters}}
             \item{number and percent of very long words (\dQuote{very long} being defined as
                   \sQuote{at least 10 characters}}
             \item{average word length}
         }

         Unique \emph{terms} (as opposed to \emph{words}) are extracted from the global document-term
         matrix, which means they do not include words that were removed by treatments ran at the
         import step, and that words different in the original text might become identical terms
         if stemming was performed. This can be considered the "correct" measures, since the purpose
         of corpus processing is exactly that: mark different forms of the same term as similar
         to allow for statistical analyses.

         On the contrary, long words and word length are extracted from a new document-term matrix:
         they do not depend on the processing options applied to the corpus when importing (e.g.
         stemming). Thus, \emph{words}, are defined as any text unit of two or more characters;
         numbers and punctuation are removed, but no other changes are done to the text.

         Two different units can be selected for the analysis. If "Document" is selected, values
         reported for each category correspond to the mean of the values for each of its documents;
         a mean column for the whole corpus is also provided. If "Category" is selected, these values
         correspond to the sum of the number of terms for each of the categories' documents,
         to the percentage of terms (ratio of the summed numbers of terms) and the average word
         length of the category when taken as a single document. Both versions of this measure are
         legitimate, but prompt different interpretations that should not be confused (though they
         are usually very close); on the contrary, interpretation of the summed or mean number of
         (long) terms is immediate. See \code{\link{vocabularyTable}} for more details.
}

\seealso{\code{\link{vocabularyTable}}, \code{\link{docVocabularyDlg}},
         \code{\link{setCorpusVariables}},
         \code{\link{meta}}, \code{\link{DocumentTermMatrix}}, \code{\link{table}},
         \code{\link{barplot}} }
