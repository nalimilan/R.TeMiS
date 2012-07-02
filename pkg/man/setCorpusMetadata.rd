\name{setCorpusMetadata}
\alias{setCorpusMetadata}
\title{Set corpus meta-data}
\description{Set corpus meta-data variables from the active data set.}
\details{This command creates one corpus meta-data variable from each column
         of the active data set. Before doing so, it erases the previously
         set meta-data.

         The active data set may contain as many variables (columns) as needed,
         but must contain exactly one row for each document in the corpus, as
         reported at import time. For convenience, a data set containing one example
         variable and as many rows as required, called \code{corpusMetaData} is
         created after importing the corpus, and defined as the active data set.
         It is meant to ease entering information about the documents, but has no
         special meaning: the \code{setCorpusMetaData} command only uses the active
         data set, even if it is different from this \code{corpusMetaData} stub.

         All analyses performed on the corpus are based on this meta-data, and never
         on the active data set. Thus, you need to call this function every time
         you want to take into account changes made to the data set.
}

\seealso{\code{\link{meta}}, \code{\link{processCorpusDlg}}}
