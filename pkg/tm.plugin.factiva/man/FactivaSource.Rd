\name{FactivaSource}
\alias{FactivaSource}
\title{Factiva Source}
\description{
  Construct a source for an input containing a set of articles
  exported from Factiva in the \acronym{XML} format.
}
\usage{
  FactivaSource(x, encoding = "UTF-8")
}
\arguments{
  \item{x}{Either a character identifying the file or a connection.}
  \item{encoding}{A character giving the encoding of \code{x}. It will
    be ignored unless the \acronym{XML} input does not include this
    information, which should normally not happen with files exported
    from Factiva.}
}
\details{
  This function imports the body of the articles, but also sets several
  meta-data variables on individual documents:
  \itemize{
    \item \code{DateTimeStamp}: The publication date.
    \item \code{Heading}: The title of the article.
    \item \code{Origin}: The newspaper the article comes from.
    \item \code{Edition}: The (local) variant of the newspaper.
    \item \code{Section}: The part of the newspaper containing the article.
    \item \code{Subject}: One or several keywords defining the subject.
    \item \code{Coverage}: One or several keywords identifying the covered regions.
    \item \code{WordCount}: The number of words in the article.
    \item \code{Pages}: The number of pages of the article.
    \item \code{Publisher}: The publisher of the newspaper.
    \item \code{Rights}: The copyright information associated with the article.
  }

  \itemize{
    \item \code{Language}: This information is set automatically if
      \code{readerControl=list(language=NA)} is passed (see the example below).
      Else, the language specified manually is set for all articles. If omitted,
      the default, "en", is used.
  }
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
    # This assumes exported articles are saved in myFactivaArticles.xml
    # in the working directory
    source <- FactivaSource("myFactivaArticles.xml")
    corpus <- Corpus(source, readerControl=list(language=NA))
  }
}
\seealso{
  \code{\link{readFactiva}} for the function actually parsing individual articles.

  \code{\link{getSources}} to list available sources.
}
