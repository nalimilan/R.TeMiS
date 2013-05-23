\name{stemDocumentSnowballC}
\alias{stemDocumentSnowballC}
\alias{stemDocumentSnowballC.PlainTextDocument}
\alias{stemDocumentSnowballC.character}
\title{Stem Words using \pkg{SnowballC}}
\description{
  Stem words in a text document using Porter's stemming algorithm provided by \pkg{SnowballC}.
}
\usage{
\method{stemDocumentSnowballC}{PlainTextDocument}(x, language = Language(x))
\method{stemDocumentSnowballC}{character}(x, language = "porter")
}
\arguments{
  \item{x}{A text document or a character vector.}
  \item{language}{A character setting the language to be used for stemming. Must be one of the
      values returned by \code{\code{\link[SnowballC:getStemLanguages]{getStemLanguages}}},
      or a corresponding ISO-639 two- or three-letter code.}
}
\details{
  The argument \code{language} is passed over to
  \code{\link[SnowballC:wordStem]{wordStem}} as the name of
  the stemmer.
}
\examples{
data("crude")
crude[[1]]

\dontshow{
if(require(SnowballC))
}
stemDocumentSnowballC(crude[[1]])

}
