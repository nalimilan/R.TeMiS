\name{stemDocumentRstem}
\alias{stemDocumentRstem}
\alias{stemDocumentRstem.PlainTextDocument}
\alias{stemDocumentRstem.character}
\title{Stem Words using \pkg{Rstem}}
\description{
  Stem words in a text document using Porter's stemming algorithm provided by \pkg{Rstem}.
}
\usage{
\method{stemDocumentRstem}{PlainTextDocument}(x, language = tm:::map_IETF_Snowball(Language(x)))
\method{stemDocumentRstem}{character}(x, language = "english")
}
\arguments{
  \item{x}{A text document or a character vector.}
  \item{language}{A character setting the language to be used for stemming. Must be one of the
      values returned by \code{getStemLanguages()}.}
}
\details{
  The argument \code{language} is passed over to
  \code{\link[Rstem:wordStem]{wordStem}} as the name of
  the stemmer.
}
\examples{
data("crude")
crude[[1]]

\dontshow{
if(require(Rstem))
}
stemDocumentRstem(crude[[1]])

}
