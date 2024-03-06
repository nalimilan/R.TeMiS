\name{AlcesteSource}
\alias{AlcesteSource}
\alias{getElem.AlcesteSource}
\alias{eoi.AlcesteSource}
\title{Alceste Source}
\description{
  Construct a source for an input containing a set of texts
  saved in the Alceste format in a single text file.
}
\usage{
  AlcesteSource(x, encoding = "auto")
}
\arguments{
  \item{x}{Either a character identifying the file or a connection.}
  \item{encoding}{A character string: if non-empty declares the encoding
      used when reading the file, so the character data can be
      re-encoded.  See the \sQuote{Encoding} section of the help for
      \code{\link{file}}. The default, \dQuote{auto},
      uses \code{\link[stringi]{stri_enc_detect}} to try to guess the
      encoding; this may fail, in which case the native encoding is used.}
}
\details{
  Several texts are saved in a single Alceste-formatted file, separated
  by lines starting with \dQuote{***} or digits, followed by starred
  variables (see links below). These variables are set as document
  meta-data that can be accessed via the \code{\link{meta}} function.

  Currently, \dQuote{theme} lines starting with \dQuote{-*} are ignored.
}
\value{
  An object of class \code{AlcesteSource} which extends the class
  \code{Source} representing set of articles from Alceste.
}
\author{
  Milan Bouchet-Valat
}
\examples{
    library(tm)
    file <- system.file("texts", "alceste_test.txt", 
                        package = "tm.plugin.alceste")
    corpus <- Corpus(AlcesteSource(file))

    # See the contents of the documents
    inspect(corpus)

    # See meta-data associated with first article
    meta(corpus[[1]])
}
\seealso{
  \url{https://image-zafar.com/sites/default/files/telechargements/formatage_alceste.pdf}
  (in French) about the Alceste format

  \code{\link{readAlceste}} for the function actually parsing
  individual articles.

  \code{\link{getSources}} to list available sources.
}
