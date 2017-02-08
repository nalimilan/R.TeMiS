\name{readFactiva}
\alias{readFactivaXML}
\alias{readFactivaHTML}
\title{Read in a Factiva article in XML or HTML formats}
\description{
  Read in an article exported from Factiva in \acronym{XML} or \acronym{HTML} formats.
}
\usage{
  readFactivaXML(elem, language, id)
  readFactivaHTML(elem, language, id)
}
\arguments{
  \item{elem}{A \code{list} with the named element \code{content} which
    must hold the document to be read in.}
  \item{language}{A \code{character} vector giving the text's language.
    If set to \code{NA}, the language will automatically be set to the value
    reported in the document (which is usually correct).}
  \item{id}{A \code{character} vector representing a unique identification
    string for the returned text document.}
}
\value{
   A \code{PlainTextDocument} with the contents of the article and the available meta-data set.
}
\seealso{
  \code{\link{getReaders}} to list available reader functions.
}
\author{
  Milan Bouchet-Valat
}
