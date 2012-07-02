\name{subsetCorpusByTermsDlg}
\alias{subsetCorpusByTermsDlg}
\title{Subset Corpus by Terms}
\description{Create a subset of the corpus by retaining only the documents which contain (or not)
             specified terms.}
\details{This operation will restrict the corpus, document-term matrix and the \dQuote{corpusVars}
         data set so that they only contain documents with or without specified terms.
         Note that \emph{the original corpus will not be preserved}: if you still need it, or are
         unsure of the result, be sure to save it using File -> Save environment as... to reload
         it when needed. Previously run analyses like correspondence analysis or hierarchical
         clustering will also be removed since they rely on the original corpus.

         If you specify both terms that should and terms that should not be present, it is possible
         that no document matches this condition, in which case an error is produced before subsetting
         the corpus.
}

\seealso{\code{\link{setCorpusVariables}}, \code{\link{meta}}, \code{\link{DocumentTermMatrix}}}
