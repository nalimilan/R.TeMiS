\name{showCorpusCaDlg}
\alias{showCorpusCaDlg}
\alias{showCorpusCa}
\title{Show a correspondence analysis from a tm corpus}
\description{Displays a correspondence analysis previously computed from a tm corpus.}
\details{This dialog allows plotting and showing most contributive terms and documents from a
         previously computed correspondence analysis (see \code{\link{corpusCaDlg}}).
         It allows plotting any dimensions of the CA together, showing either documents, terms,
         or variables set on the corpus using the Text mining->Manage corpus->Set corpus variables menu.

         Compared with most correpondence analyses, CAs of a corpus tend to have many rows (documents)
         and columns (terms). Thus, the dialog provides two sliders ('Number of items to plot')
         allowing to show only a subset of them, the most contributive to the chosen dimension.
         These items are the most useful to interpret the axes, given the limited reflexive power of the
         human mind.

         The text window shows the terms and documents most contributive to the chosen axis, together with
         their position, their contribution to the inertia of the axis(\dQuote{Absolute Contribution})
         in permills of the principal inertia, and the contribution of the axis to their inertia x 1000
         (\dQuote{Relative Contribution}). (For supplementary variables, absolute contributions are not
         reported as they do not exist by definition.) The part of total inertia represented by each axis
         is shown first, but the rest of the window only deals with the selected axis (horizontal or vertical).

         The 'Draw point symbols for' checkboxes allow representing documents and terms masses (corresponding
         to the size of the symbols) and relative contributions (corresponding to the color intensities). See
         the \code{contrib} argument to \code{\link{plot.ca}} for details.
        }
\seealso{\code{\link{corpusCaDlg}}, \code{\link{plotCorpusCa}}, \code{\link{runCorpusCa}}, \code{\link{ca}} }
