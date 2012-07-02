\name{plotCorpusCaDlg}
\alias{plotCorpusCaDlg}
\title{Plot a correspondence analysis from a tm corpus}
\description{Plot a correspondence analysis (CA) previously computed from a tm corpus.}
\details{This dialog allows plotting a previously computed correspondence analysis
         (see \code{\link{corpusCaDlg}}).

         It allows plotting any dimensions of the CA together, showing either documents, terms,
         or meta-data variables set on the corpus using the Text Mining->Set corpus meta-data menu.

         Compared with most correpondence analyses, CAs of a corpus tend to have many rows (documents),
         and especially columns (terms). Thus, the dialog provides two sliders ('Number of items to plot')
         allowing to draw only a subset of them, chosen to be the most contributive to the chosen dimension.
         These items are the most useful to interpret the axes, given the limited reflexive power of the
         human mind: the default of 50 documents and terms is likely to be a good compromise, allowing for
         a readable figure.

         The 'Draw point symbols for' checkboxes allow representing documents and terms masses (corresponding
         to the size of the symbols) and relative contributions (corresponding to the color intensities). See
         the \code{contrib} argument to \code{\link{plot.ca}} for details.
        }
\seealso{\code{\link{runCorpusCa}}, \code{\link{plot.ca}}, \code{\link{ca}} }
