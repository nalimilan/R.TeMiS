\name{EuropresseSource}
\alias{EuropresseSource}
\alias{getElem.EuropresseSource}
\alias{eoi.EuropresseSource}
\title{Europresse Source}
\description{
  Construct a source for an input containing a set of articles
  exported from Europresse in the \acronym{HTML} format.
}
\usage{
  EuropresseSource(x, encoding = "UTF-8")
}
\arguments{
  \item{x}{Either a character identifying the file or a connection.}
  \item{encoding}{A character giving the encoding of \code{x}. Files
      exported from Europresse often specify an incorrect encoding,
      in which case you will need to find out the correct one.}
}
\details{
  This function imports the body of the articles, but also sets several
  meta-data variables on individual documents:
  \itemize{
    \item \code{datetimestamp}: The publication date.
    \item \code{heading}: The title of the article.
    \item \code{origin}: The newspaper the article comes from.
    \item \code{section}: If available, the part of the newspaper containing
    the article.
    \item \code{pages}: If available, the pages where the article appeared.
  }

  Please note that it commonly happens that the encoding specified in
  Europresse \acronym{HTML} files does not correspond to the one actually
  used in the text: in that case, you will need to find out the correct
  encoding and specify it manually.
}
\value{
  An object of class \code{EuropresseSource} which extends the class
  \code{Source} representing set of articles from Europresse.
}
\author{
  Milan Bouchet-Valat
}
\examples{
    library(tm)
    file <- system.file("texts", "europresse_test2.html",
                        package = "tm.plugin.europresse")
    corpus <- Corpus(EuropresseSource(file))

    # See the contents of the documents
    inspect(corpus)

    # See meta-data associated with first article
    meta(corpus[[1]])

    \dontshow{
    file <- system.file("texts", "europresse_test1.html",
                        package = "tm.plugin.europresse")
    corpus <- Corpus(EuropresseSource(file))
    }
}
\seealso{
  \code{\link{readEuropresseHTML2}} for the function actually parsing
  individual articles.

  \code{\link{getSources}} to list available sources.
}
