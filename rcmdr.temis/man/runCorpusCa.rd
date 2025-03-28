\name{runCorpusCa}
\alias{runCorpusCa}
\title{Correspondence analysis from a tm corpus}
\description{Compute a simple correspondence analysis on the document-term matrix of a tm corpus.}
\usage{runCorpusCa(corpus, dtm = NULL, variables = NULL, sparsity = 0.9, ...)}
\arguments{
  \item{corpus  }{A \pkg{tm} corpus.}
  \item{dtm}{an optional document-term matrix to use; if missing, \code{\link[tm]{DocumentTermMatrix}}
             will be called on \code{corpus} to create it.}
  \item{variables}{a character vector giving the names of meta-data variables to aggregate the
                   document-term matrix (see \dQuote{Details} below).}
  \item{sparsity}{Optional sparsity threshold (between 0 and 1) below which terms should be
                  skipped. See \code{\link[tm]{removeSparseTerms}} from tm.}
  \item{...     }{Additional parameters passed to \code{\link{ca}}.}
}
\details{The function \code{runCorpusCa} runs a correspondence analysis (CA) on the
         document-term matrix that can be extracted from a \pkg{tm} corpus by calling
         the \code{\link[tm]{DocumentTermMatrix}} function, or directly from the \code{dtm}
         object if present.

         If no variable is passed via the \code{variables} argument, a CA is run on the
         full document-term matrix (possibly skipping sparse terms, see below). If one or more
         variables are chosen, the CA will be based on a stacked table whose rows correspond to
         the levels of the variables: each cell contains the sum of occurrences of a given term in
         all the documents of the level. Documents that contain a \code{NA} are skipped for this
         variable, but taken into account for the others, if any.

         In all cases, variables that have not been selected are added as supplementary rows. If at least
         one variable is passed, documents are also supplementary rows, while they are active otherwise.

         The \code{sparsity} argument is passed to \code{\link[tm]{removeSparseTerms}}
         to remove less significant terms from the document-term matrix. This is
         especially useful for big corpora, which matrices can grow very large, prompting
         \code{ca} to take up too much memory.}
\value{A \code{ca} object as returned by the \code{\link{ca}} function.}

\seealso{\code{\link{ca}}, \code{\link[tm]{meta}}, \code{\link[tm]{removeSparseTerms}},
         \code{\link[tm]{DocumentTermMatrix}} }
