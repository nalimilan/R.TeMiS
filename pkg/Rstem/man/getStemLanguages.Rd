\name{getStemLanguages}
\alias{getStemLanguages}
\title{Query the languages supported in this package}
\description{
  This dynamically determines the names of the languages
  for which stemming is supported by this package.
  This is controlled when the package is created (not installed)
  by downloading the  stemming algorithms for the different
  languages.

  This language support requires more support for Unicode
  and more complex text than simple strings.
}
\usage{
getStemLanguages()
}
%- maybe also 'usage' for other objects documented here.
\details{
  This queries the C code for the list of languages that
  were compiled when the package was installed
  which in turn is determined by the code that was
  included in the distributed package itself.
}
\value{
  A character vector giving the names of the
  languages.
}
\references{
  See \url{http://snowball.tartarus.org/}
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


 \seealso{
   \code{\link{wordStem}}
   \code{inst/scripts/download} in the source of the
     Rstem package.
   }
\examples{
 getStemLanguages()
}
\keyword{IO}
\keyword{utilities}

