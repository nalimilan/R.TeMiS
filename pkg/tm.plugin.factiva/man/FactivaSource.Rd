\name{FactivaSource}
\alias{FactivaSource}
\alias{getElem.FactivaSource}
\alias{eoi.FactivaSource}
\title{Factiva Source}
\description{
  Construct a source for an input containing a set of articles
  exported from Factiva in the \acronym{XML} or \acronym{HTML} formats.
}
\usage{
  FactivaSource(x, format = c("auto", "XML", "HTML"))
}
\arguments{
  \item{x}{Either a character identifying the file or a connection.}
  \item{format}{The format of the file or connection identified by \code{x}
    (see \dQuote{Details}).}
}
\details{
  This function can be used to import both \acronym{XML} and \acronym{HTML} files.
  If \code{format} is set to \dQuote{auto} (the default), the file extension is used
  to guess the format: if the file name ends with \dQuote{.xml} or \dQuote{.XML}, 
  \acronym{XML} is assumed; else, the file is assumed to be in the \acronym{HTML} format.

  This function imports the body of the articles, but also sets several
  meta-data variables on individual documents:
  \itemize{
    \item \code{datetimestamp}: The publication date.
    \item \code{heading}: The title of the article.
    \item \code{origin}: The newspaper the article comes from.
    \item \code{edition}: The (local) variant of the newspaper.
    \item \code{section}: The part of the newspaper containing the article.
    \item \code{subject}: One or several keywords defining the subject.
    \item \code{company}: One or several keywords identifying the covered companies.
    \item \code{industry}: One or several keywords identifying the covered industries.
    \item \code{infocode}: One or several Information Provider Codes (IPC).
    \item \code{infodesc}: One or several Information Provider Descriptions (IPD).
    \item \code{coverage}: One or several keywords identifying the covered regions.
    \item \code{wordcount}: The number of words in the article.
    \item \code{publisher}: The publisher of the newspaper.
    \item \code{rights}: The copyright information associated with the article.
    \item \code{language}: This information is set automatically if
      \code{readerControl = list(language = NA)} is passed (see the example below).
      Else, the language specified manually is set for all articles. If omitted,
      the default, "en", is used.
  }

  It is advised to export articles from Factiva in the \acronym{XML} format rather
  than in \acronym{HTML} when possible, since the latter does not provide completely
  clean information. In particular, dates are not guaranteed to be parsed correctly
  if the machine from which the \acronym{HTML} file was exported uses a locale different
  from that of the machine where it is read.
}
\note{
  It has been found that some Factiva articles contain unescaped characters
  that are not authorized in \acronym{XML} files. If such articles are included in
  the input you are trying to import, the \acronym{XML} parser will fail printing
  a few error messages, and the corpus will not be created at all.

  If you experience this bug, please report this to the Factiva Customer
  Service, which will fix the incriminated article; feel free to ask the
  maintainer of the present package if needed. In the meantime, you can
  exclude the problematic article from the \acronym{XML} file: to identify it,
  proceed by exporting only one half of the original corpus at a time, as many times
  as needed, and see when it fails; you will eventually find the culprit.
  (If you know \acronym{XML}, you can use an \acronym{XML} validator to find the
  relevant part of the file, and fix it by hand.)
}
\value{
  An object of class \code{XMLSource} which extends the class
  \code{Source} representing set of articles from Factiva.
}
\author{
  Milan Bouchet-Valat
}
\examples{
\dontrun{
    ## For an XML file
    library(tm)
    file <- system.file("texts", "reut21578-factiva.xml",
                        package = "tm.plugin.factiva")
    source <- FactivaSource(file)
    corpus <- Corpus(source, readerControl = list(language = NA))

    # See the contents of the documents
    inspect(corpus)

    # See meta-data associated with first article
    meta(corpus[[1]])
}

    ## For an HTML file
    library(tm)
    file <- system.file("texts", "factiva_test.html", 
                        package = "tm.plugin.factiva")
    source <- FactivaSource(file)
    corpus <- Corpus(source, readerControl = list(language = NA))

    # See the contents of the documents
    inspect(corpus)

    # See meta-data associated with first article
    meta(corpus[[1]])
}
\seealso{
  \code{\link{readFactivaXML}} and \code{\link{readFactivaHTML}} for the functions
  actually parsing individual articles.

  \code{\link{getSources}} to list available sources.
}
