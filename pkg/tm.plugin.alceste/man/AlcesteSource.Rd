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
  AlcesteSource(x, fileEncoding = "UTF-8", encoding = "unknown")
}
\arguments{
  \item{x}{Either a character identifying the file or a connection.}
  \item{encoding}{A character giving the encoding of \code{x}. Files
      exported from Alceste often specify an incorrect encoding,
      in which case you will need to find out the correct one.}
  \item{fileEncoding}{A character string: if non-empty declares the encoding used
          when readin the file, so the character data can be
          re-encoded.  See the \sQuote{Encoding} section of the help for
          \code{\link{file}}.
  \item{encoding}{Encoding to be assumed for input strings.  It is used to mark
          character strings as known to be in Latin-1 or UTF-8 (see
          \sQuote{encoding}): it is not used to re-encode the input, but
          allows R to handle encoded strings in their native encoding
          (if one of those two).
}
\details{
  Several texts are saved in a single Alceste-formatted file, separated
  by lines starting with \dQuote{***} or digits, followed by starred
  variables (see links below). These variables are set as document
  meta-data that can be accessed via the \code{\link{meta}} function.

  Currently, \dQuote{theme} lines starting with \dQuote{-*} are ignored.

  Please note that as plain text files, Alceste files do not contain any
  encoding information. Thus, you need to specify the correct encoding
  manually via \sQuote{fileEncoding}.
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
  \url{http://www.image-zafar.com/sites/default/files/telechargements/formatage_alceste.pdf}
  (in French) about the Alceste format

  \code{\link{readAlceste}} for the function actually parsing
  individual articles.

  \code{\link{getSources}} to list available sources.
}
