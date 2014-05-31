\name{LexisNexisSource}
\alias{LexisNexisSource}
\alias{getElem.LexisNexisSource}
\alias{eoi.LexisNexisSource}
\title{LexisNexis Source}
\description{
  Construct a source for an input containing a set of articles
  exported from LexisNexis in the \acronym{HTML} format.
}
\usage{
  LexisNexisSource(x, encoding = "UTF-8")
}
\arguments{
  \item{x}{Either a character identifying the file or a connection.}
  \item{encoding}{A character giving the encoding of \code{x}. It will
    be ignored unless the \acronym{HTML} input does not include
    this information, which should normally not happen with files exported
    from LexisNexis.}
}
\details{
  This function imports the body of the articles, but also sets several
  meta-data variables on individual documents:
  \itemize{
    \item \code{datetimestamp}: The publication date.
    \item \code{heading}: The title of the article.
    \item \code{origin}: The newspaper the article comes from.
    \item \code{intro}: The short introduction accompanying the article.
    \item \code{section}: The part of the newspaper containing the article.
    \item \code{subject}: One or several keywords defining the subject.
    \item \code{coverage}: One or several keywords identifying the covered regions.
    \item \code{company}: One or several keywords identifying the covered companies.
    \item \code{stocksymbol}: One or several keywords identifying the stock exchange
      symbols of the covered companies.
    \item \code{industry}: One or several keywords identifying the covered industries.
    \item \code{type}: The type of source from which the document originates.
    \item \code{wordcount}: The number of words in the article.
    \item \code{publisher}: The publisher of the newspaper.
    \item \code{rights}: The copyright information associated with the article.
    \item \code{language}: This information is set automatically if
      \code{readerControl = list(language = NA)} is passed (see the example below).
      Else, the language specified manually is set for all articles. If omitted,
      the default, "en", is used.
  }

  Please note that dates are not guaranteed to be parsed correctly
  if the machine from which the \acronym{HTML} file was exported
  uses a locale different from that of the machine where it is read.

  Currently, only \acronym{HTML} files saved in French are supported.
  Please send the maintainer examples of LexisNexis files in your
  language if you want it to be supported.
}
\value{
  An object of class \code{LexisNexisSource} which extends the class
  \code{Source} representing set of articles from LexisNexis.
}
\author{
  Milan Bouchet-Valat
}
\examples{
    library(tm)
    file <- system.file("texts", "lexisnexis_test.html", 
                        package = "tm.plugin.lexisnexis")
    corpus <- Corpus(LexisNexisSource(file))

    # See the contents of the documents
    inspect(corpus)

    # See meta-data associated with first article
    meta(corpus[[1]])
}
\seealso{
  \code{\link{readLexisNexisHTML}} for the function actually parsing
  individual articles.

  \code{\link{getSources}} to list available sources.
}
