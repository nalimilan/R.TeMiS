\name{getStemLanguages}
\alias{getStemLanguages}
\title{Query the list of supported languages}
\description{
  This dynamically determines the names of the languages for which
  stemming is currently supported by this package.
}
\usage{
getStemLanguages()
}
%- maybe also 'usage' for other objects documented here.
\details{
  The language names in lower case are returned, though please note
  that two- and three- letter ISO-639 codes are also accepted by
  \code{\link{wordStem}} (see references for the list of codes).

  This queries the C code for the list of languages that were compiled
  when the package was installed which in turn is determined by the
  code that was included in the distributed package itself.
}
\value{
  A character vector giving the names of the languages.
}
\references{
  \url{http://snowball.tartarus.org/}

  \url{http://www.loc.gov/standards/iso639-2/php/code_list.php} for a
  list of ISO-639 language codes.
}
\author{Milan Bouchet-Valat}


\seealso{
  \code{\link{wordStem}}
}
\examples{
  getStemLanguages()
}

