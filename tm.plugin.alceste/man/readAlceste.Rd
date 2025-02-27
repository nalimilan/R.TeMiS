\name{readAlceste}
\alias{readAlceste}
\title{Read in a text in the Alceste format}
\description{
  Read in a text in the Alceste format using starred variables.
}
\usage{
  readAlceste(elem, language, id)
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
  \code{\link[tm:getReaders]{getReaders}} to list available reader functions.
}
\author{
  Milan Bouchet-Valat
}
