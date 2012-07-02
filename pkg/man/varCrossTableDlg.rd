\name{varCrossTableDlg}
\alias{varCrossTableDlg}
\title{Two-way table of corpus meta-data variables}
\description{Build a two-way contingency table from a corpus's meta-data variables,
             optionally plotting the result.}
\details{This dialog provides a simple way of computing frequencies from a single meta-data
         variable of a \pkg{tm} corpus. It is merely a wrapper around different steps available from
         the Statistics and Plot menus, but operating on the corpus meta-data instead of the active
         data set.

         Plots are grouped according to the variable over which percentages are built (the first one
         for row percent, the second one for column percent), or according to the first variable if absolute counts
         are plotted. Thus, one can tweak grouping by changing either the order of the variables, or
         the type of computed percent.}

\seealso{\code{\link{setCorpusMetadata}}, \code{\link{meta}}, \code{\link{table}},
         \code{\link{barplot}}, \code{\link{pie}} }
