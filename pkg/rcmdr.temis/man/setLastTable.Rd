\name{setLastTable}
\alias{setLastTable}
\title{Save the name of last table and give a title
}
\description{This function saves the name of the last created table to allow copying it
             to the HTML report using the \dQuote{Export results to report} menu, or
             directly using the \code{\link{copyTableToOutput}} function.}
\usage{
setLastTable(name, title = NULL)
}
\arguments{
  \item{name}{The name of the table, which must correspond to an object in the global
              environment.}
  \item{title}{The title to give to the table, which will be displayed in the report,
               or \code{NULL} for none.}
}
\details{The title is saved as the \dQuote{title} attribute of the object called as
         \code{name} in the global environment. You may need to call \code{activateMenus}
         so that the relevant menus are enabled.}
\author{Milan Bouchet-Valat}
\seealso{\code{\link{copyTableToOutput}}}
