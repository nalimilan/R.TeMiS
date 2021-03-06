\name{output}
\alias{setOutputFile}
\alias{initOutputFile}
\alias{openOutputFile}
\alias{copyTableToOutput}
\alias{copyPlotToOutput}
\alias{enableBlackAndWhite}
\alias{disableBlackAndWhite}
\alias{HTML.list}
\alias{HTML.ca}
\alias{summary.ca}
\title{Output results to HTML file}
\description{Functions to output tables and plots resulting from analysis of the corpus to an
             \acronym{HTML file}.}
\details{\code{setOutputFile} is automatically called the first time an attempt to save a result
         to the output file happens. It can also be called from the \dQuote{Export results to report}
         menu.

         \code{openOutputFile} launches the configured web browser (see \code{\link{browseURL}}) to
         open the current output file. It is automatically called the first time a new output file is
         set (i.e. when \code{setOutputFile} is run).

         \code{copyTableToOutput} and \code{copyPlotToOutput} export objects to the select output
         \acronym{HTML} file, using the titles that were configured when the objects where created.
         For plots, a plotting device must be currently open. The graph is saved in the \acronym{PNG}
         format with a reasonably high quality. For tables, the last created table is used.

         \code{enableBlackAndWhite} and \code{disableBlackAndWhite} functions can be used to produce
         black and white only graphics adapted for printing and publication. They affect the on-screen
         device as well as the plot copied to the output file, so that the plot can be checked for
         readability before exporting it.

         \code{HTML.list} outputs a list to the HTML report, printing each element of the list right after
         its name. \code{HTML.ca} outputs a correspondence analysis object of class \code{ca} to the HTML
         report. \code{summary.ca} is a slightly modified version of \code{\link[ca]{summary.ca}} from the
         \dQuote{ca} package to accept non-ASCII characters and not abbreviate document names and terms;
         it is used by \code{HTML.ca} internally.
}

