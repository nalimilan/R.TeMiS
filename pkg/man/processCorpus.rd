\name{processCorpusDlg}
\alias{processCorpusDlg}
\title{Process a text corpus}
\description{Process a text corpus and extract its document-term matrix.}
\details{This dialog provides a few processing options that will most likely be
         all run in order to get a meaningful set of terms from a text corpus.
         Among them, stopwords removal and stemming equire you to select the
         language used in the corpus: at the moment supported languages are
         'danish', 'dutch', 'english','finnish', 'french', 'german', 'hungarian',
         'italian', 'norwegian', 'portuguese', 'russian', 'spanish', and 'swedish'
         (to specify in their english name).

         Once the corpus has been imported, its document-term matrix is extracted.

         See \code{\link{importCorpusFromDir}}, \code{\link{importCorpusFromFile}}
         for the specifics of these two importing methods.
}
\references{Ingo Feinerer, Kurt Hornik, and David Meyer. Text mining infrastructure in R. Journal of Statistical Software, 25(5):1-54, March 2008. Available at \url{http://www.jstatsoft.org/v25/i05}.\cr\cr
            Ingo Feinerer. An introduction to text mining in R. R News, 8(2):19-22, October 2008. Available at \url{http://cran.r-project.org/doc/Rnews/Rnews_2008-2.pdf}}

\seealso{\code{\link{importCorpusFromDir}}, \code{\link{importCorpusFromFile}},
         \code{\link{Corpus}}, \code{\link{DocumentTermMatrix}}, \code{\link{restrictTermsDlg}},
         \code{\link{tolower}}, \code{\link{removePunctuation}}, \code{\link{removeNumbers}},
         \code{\link{stopwords}},  \code{\link{stemDocument}},  \code{\link{tm_map}} }
