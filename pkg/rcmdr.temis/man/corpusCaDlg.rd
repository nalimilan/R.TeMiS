\name{corpusCaDlg}
\alias{corpusCaDlg}
\title{Correspondence analysis from a tm corpus}
\description{Compute a simple correspondence analysis on the document-term matrix of a tm corpus.}
\details{This dialog wraps the \code{\link{runCorpusCa}} function. The function \code{runCorpusCa}
         runs a correspondence analysis (CA) on the  document-term matrix.

         If no variable is selected in the list (the default), a CA is run on the full document-term
         matrix (possibly skipping sparse terms, see below). If one or more variables are chosen,
         the CA will be based on a stacked table whose rows correspond to the levels of the variable:
         each cell contains the sum of occurrences of a given term in all the documents of the level.
         Documents that contain a \code{NA} are skipped for this variable, but taken into account for
         the others, if any.

         In all cases, variables that have not been selected are added as passive rows. If at least one variable
         is selected, documents are also passive rows, while they are active otherwise.

         The first slider ('sparsity') allows skipping less significant terms to use less memory, especially
         with large corpora. The second slider ('dimensions to retain') allows choosing the number of
         dimensions that will be printed, but has no effect on the computation of the correspondance analysis.
}

\seealso{\code{\link{runCorpusCa}}, \code{\link{ca}}, \code{\link{meta}}, \code{\link{removeSparseTerms}},
         \code{\link{DocumentTermMatrix}} }
