\name{subsetCorpusByVarDlg}
\alias{subsetCorpusByVarDlg}
\title{Subset Corpus by Levels of a Variable}
\description{Create a subset of the corpus by retaining only the documents for which the chosen
             variable is equal to specified levels.}
\details{This operation will restrict the corpus, document-term matrix and the \dQuote{corpusVars}
         data set so that they only contain documents with or without specified terms.
         Previously run analyses like correspondence analysis or hierarchical clustering will be
         removed to prevent confusion.

         If you choose to save the original corpus, you will be able to restore it later from the
         Text mining -> Subset corpus -> Restore original corpus menu. Warning: checking this option
         will erase an existing backup if present. Like subsetting, restoring the original corpus
         removes existing correspondence analysis and hierarchical clustering objects.
}

\seealso{\code{\link{setCorpusVariables}}, \code{\link{meta}}, \code{\link{DocumentTermMatrix}}}
