\name{corpusCaDlg}
\alias{corpusCaDlg}
\title{Correspondence analysis from a tm corpus}
\description{Compute a simple correspondence analysis on the document-term matrix of a tm corpus.}
\details{This dialog wraps the \code{\link{runCorpusCa}} function: see its help page for details.

         The first slider ('sparsity') allows skipping less significant terms to use less memory with
         large corpora. The second one ('dimensions to retain') allows choosing the number of dimensions
         that will be printed, but has no effect on the computation of the correspondance analysis.}

\seealso{\code{\link{runCorpusCa}}, \code{\link{ca}}, \code{\link{meta}}, \code{\link{removeSparseTerms}},
         \code{\link{DocumentTermMatrix}} }
