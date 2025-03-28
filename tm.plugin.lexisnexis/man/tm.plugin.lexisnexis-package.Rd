\name{tm.plugin.lexisnexis-package}
\alias{tm.plugin.lexisnexis-package}
\alias{tm.plugin.lexisnexis}
\docType{package}
\title{
  A plug-in for the tm text mining framework to import articles from LexisNexis
}
\description{
  This package provides a tm Source to create corpora from
  articles exported from the LexisNexis content provider as
  \acronym{HTML} files.
}
\details{
  Typical usage is to create a corpus from \acronym{HTML} files
  exported from LexisNexis (here called \code{myLexisNexisArticles.html}).
  Setting \code{language=NA} allows the language to be set automatically
  from the information provided by Factiva:

  \preformatted{
    # Import corpus
    source <- LexisNexisSource("myLexisNexisArticles.html")
    corpus <- Corpus(source, readerControl = list(language = NA))

    # See how many articles were imported
    corpus

    # See the contents of the first article and its meta-data
    inspect(corpus[1])
    meta(corpus[[1]])
  }

  Currently, only \acronym{HTML} files saved in English and French are supported.
  Please send the maintainer examples of LexisNexis files in your
  language if you want it to be supported.

  See \code{link{LexisNexisSource}} for more details and real examples.
}
\author{
  Milan Bouchet-Valat <nalimilan@club.fr>
}
\references{
  \url{https://www.lexisnexis.com/}
}
