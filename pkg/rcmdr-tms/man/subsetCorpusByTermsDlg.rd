\name{subsetCorpusByTermsDlg}
\alias{subsetCorpusByTermsDlg}
\title{Subset Corpus by Terms}
\description{Create a subset of the corpus by retaining only the documents which contain (or not)
             specified terms.}
\details{This operation will restrict the corpus, document-term matrix and the \dQuote{corpusVars}
         data set so that they only contain documents with or without specified terms.
         Previously run analyses like correspondence analysis or hierarchical clustering will be
         removed to prevent confusion.

         If you choose to save the original corpus, you will be able to restore it later from the
         Text mining -> Subset corpus -> Restore original corpus menu. Warning: checking this option
         will erase an existing backup if present. Like subsetting, restoring the original corpus
         removes existing correspondence analysis and hierarchical clustering objects.

         If you specify both terms that should and terms that should not be present, it is possible
         that no document matches this condition, in which case an error is produced before subsetting
         the corpus.
}

\seealso{\code{\link{setCorpusVariables}}, \code{\link{meta}}, \code{\link{DocumentTermMatrix}}}
