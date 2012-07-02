\name{varVocabularyDlg}
\alias{varVocabularyDlg}
\title{Term frequency table per meta-data variable}
\description{Build a table of vocabulary over meta-data variables of a corpus.}
\details{This dialog allows creating tables providing three vocabulary measures for each of the
         categories of a corpus variable: total number of terms, number of unique terms (i.e. number of terms appearing
         at least once in the document), vocabulary diversity index (ratio of the latter over the
         former figure). Note all these measures are calculated from the document-term matrix, which
         means they do not include words that were removed by treatments ran at the import step, and
         that words different in the original text might become identical terms if stemming was
         performed. This can be considered the "correct" measures, since the purpose of corpus
         processing is exactly that: mark different forms of the same term as similar to allow for
         statistical analyses.

         Two different units can be selected for the analysis. If "Document" is selected, values
         reported for each category correspond to the mean of the values for each of its documents;
         a mean column for the whole corpus is also provided. If "Category" is selected, these values
         correspond to the sum of the number of (unique) terms for each of the categories' documents,
         and to the vocabulary diversity of the category when taken as a single document (ratio of the
         summed numbers of terms). Both versions of the diversity measure are legitimate, but prompt
         different interpretations that shouldn't be confused; on the contrary, interpretation of the
         summed or mean number of (unique) terms is immediate.

         When plotting the number of (unique) terms with "category" as unit, corpus total is not shown,
         since it would always exceed by far the values for separate categories.
}

\seealso{\code{\link{setCorpusMetadata}}, \code{\link{meta}}, \code{\link{DocumentTermMatrix}},
         \code{\link{table}}, \code{\link{barplot}}, \code{\link{docVocabularyDlg}} }
