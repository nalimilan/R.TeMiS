\name{corpusCa}
\alias{corpusCa}
\title{Correspondence analysis from a tm corpus}
\description{Compute a simple correspondence analysis on the document-term matrix of a tm corpus.}
\usage{corpusCa(corpus, sparsity = 0.9, ...)}
\details{The function \code{corpusCa} runs a correspondence analysis (CA) on the
         document-term matrix that can be extracted from a \pkg{tm} corpus by calling
         the \code{\link{DocumentTermMatrix}} function, or directly from the \code{dtm}
         object if present.

         Before calling \code{\link{ca}} on the matrix, the function adds meta-data
         variables as supplementary (passive) rows, creating dummy variables for all
         levels of all meta-data of the corpus; more precisely, these passive rows
         correspond to the per-column sum of all documents included in the category.
         Thus, they give the mean point of the documents in the category.

         The \code{sparsity} argument is passed to \code{\link{removeSparseTerms}}
         to remove less significant terms from the document-term matrix. This is
         especially useful for big corpora, which matrices can grow very large, prompting
         \code{ca} to take up too much memory.}
\arguments{
  \item{corpus  }{A \pkg{tm} corpus.}
  \item{sparsity}{Optional sparsity threshold (between 0 and 1) below which terms should be
                  skipped. See \code{removeSparseTerms} from tm.}
  \item{...     }{Additional parameters passed to \code{\link{ca}}.}
          }
\value{A \code{ca} object as returned by the \code{\link{ca}} function.}

\seealso{\code{\link{ca}}, \code{\link{meta}}, \code{\link{removeSparseTerms}},
         \code{\link{DocumentTermMatrix}} }
