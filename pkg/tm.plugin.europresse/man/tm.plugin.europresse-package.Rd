\name{tm.plugin.europresse-package}
\alias{tm.plugin.europresse-package}
\alias{tm.plugin.europresse}
\docType{package}
\title{
  A plug-in for the tm text mining framework to import articles from Europresse
}
\description{
  This package provides a tm Source to create corpora from
  articles exported from the Europresse content provider as
  \acronym{HTML} files.
}
\details{
  Typical usage is to create a corpus from \acronym{HTML} files
  exported from Europresse (here called \code{myEuropresseArticles.html}).
  Frequently, it is necessary to specify the encoding of the texts
  via \code{link{EuropresseSource}}'s \code{encoding} argument.

  \preformatted{
    # Import corpus
    source <- EuropresseSource("myEuropresseArticles.html")
    corpus <- Corpus(source)

    # See how many articles were imported
    corpus

    # See the contents of the first article and its meta-data
    inspect(corpus[1])
    meta(corpus[[1]])
  }

  See \code{link{EuropresseSource}} for more details and real examples.
}
\author{
  Milan Bouchet-Valat <nalimilan@club.fr>
}
\references{
  \url{http://www.europresse.com/}
}
