\name{subsetCorpusByVarDlg}
\alias{subsetCorpusByVarDlg}
\title{Subset Corpus by Levels of a Variable}
\description{Create a subset of the corpus by retaining only the documents for which the chosen
             variable is equal to specified levels.}
\details{This operation will restrict the corpus, document-term matrix and the \dQuote{corpusVars}
         data set so that they only contain documents matching the chosen level(s) of the variable.
         Note that \emph{the original corpus will not be preserved}: if you still need it, or are
         unsure of the result, be sure to save it using File -> Save environment as... to reload
         it when needed. Previously run analyses like correspondence analysis or hierarchical
         clustering will also be removed since they rely on the original corpus.
}

\seealso{\code{\link{setCorpusVariables}}, \code{\link{meta}}, \code{\link{DocumentTermMatrix}}}
