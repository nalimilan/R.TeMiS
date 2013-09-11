\name{tm.plugin.factiva-package}
\alias{tm.plugin.factiva-package}
\alias{tm.plugin.factiva}
\docType{package}
\title{
  A plug-in for the tm text mining framework to import articles from Factiva
}
\description{
  This package provides a tm Source to create corpora from
  articles exported from Dow Jones's Factiva content provider as
  \acronym{XML} or \acronym{HTML} files.
}
\details{
\tabular{ll}{
  Package: \tab tm.plugin.factiva\cr
  Type: \tab Package\cr
  Version: \tab 1.0\cr
  Date: \tab 2012-09-11\cr
  License: \tab GPL (>= 2)\cr
}
  Typical usage is to create a corpus from a \acronym{XML} or \acronym{HTML} files
  exported from Factiva (here called \code{myFactivaArticles.xml}). Setting
  \code{language=NA} allows the language to be set automatically from the
  information provided by Factiva:

  \preformatted{
    # Import corpus
    source <- FactivaSource("myFactivaArticles.xml")
    corpus <- Corpus(source, list(language=NA))

    # See how many articles were imported
    corpus

    # See the contents of the first article and its meta-data
    inspect(corpus[1])
    meta(corpus[[1]])
  }

  Currently, only \acronym{HTML} files saved in French are supported.
  Please send the maintainer examples of LexisNexis files in your
  language if you want it to be supported.

  See \code{link{FactivaSource}} for more details and real examples.
}
\author{
  Milan Bouchet-Valat <nalimilan@club.fr>
}
\references{
  \url{http://global.factiva.com/}
}
