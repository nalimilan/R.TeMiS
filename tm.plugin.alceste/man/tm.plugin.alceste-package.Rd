\name{tm.plugin.alceste-package}
\alias{tm.plugin.alceste-package}
\alias{tm.plugin.alceste}
\docType{package}
\title{
  A plug-in for the tm text mining framework to import corpora from Alceste files
}
\description{
  This package provides a tm Source to create corpora from
  files formatted in the format used by the Alceste application.
}
\details{
  Typical usage is to create a corpus from an Alceste file
  prepared manually (here called \code{myAlcesteCorpus.txt}).
  Frequently, it is necessary to specify the encoding of the texts
  via \code{link{AlcesteSource}}'s \code{encoding} argument.

  \preformatted{
    # Import corpus
    source <- europresseSource("myAlcesteCorpus.txt")
    corpus <- Corpus(source)

    # See how many articles were imported
    corpus

    # See the contents of the first article and its meta-data
    inspect(corpus[1])
    meta(corpus[[1]])
  }

  See \code{link{AlcesteSource}} for more details and real examples.
}
\author{
  Milan Bouchet-Valat <nalimilan@club.fr>
}
\references{
  \url{https://image-zafar.com/Logicieluk.html}
}
