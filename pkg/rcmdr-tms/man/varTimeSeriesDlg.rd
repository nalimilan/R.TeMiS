\name{varTimeSeriesDlg}
\alias{varTimeSeriesDlg}
\title{Corpus Temporal Evolution}
\description{Variation of the number of documents in the corpus over time, possibly grouped
             by variable.}
\details{This dialog allows computing and plotting the number of documents over a time variable.
         The format used by the chosen time variable has to be specified so that it is handled
         correctly. The format codes allowed are those recognized  by \code{\link{strptime}}
         (see \code{?strptime}), in particular:

         \describe{
             \item{\sQuote{\%a}}{Abbreviated weekday name in the current locale. (Also
                       matches full name.)}

             \item{\sQuote{\%A}}{Full weekday name in the current locale. (Also matches
                       abbreviated name.)}

             \item{\sQuote{\%b}}{Abbreviated month name in the current locale. (Also matches
                       full name.)}

             \item{\sQuote{\%B}}{Full month name in the current locale. (Also matches
                       abbreviated name.)}

             \item{\sQuote{\%d}}{Day of the month as decimal number (01-31).}

             \item{\sQuote{\%H}}{Hours as decimal number (00-23).}

             \item{\sQuote{\%I}}{Hours as decimal number (01-12).}

             \item{\sQuote{\%m}}{Month as decimal number (01-12).}

             \item{\sQuote{\%M}}{Minute as decimal number (00-59).}

             \item{\sQuote{\%p}}{AM/PM indicator in the locale.  Used in conjunction with \sQuote{\%I}
                       and not with \sQuote{\%H}.}

             \item{\sQuote{\%S}}{Second as decimal number (00-61).}

             \item{\sQuote{\%y}}{Year without century (00-99).}

             \item{\sQuote{\%Y}}{Year with century.}
         }

             The rolling mean is left-aligned, meaning that the number of documents reported for a
             point reflects the average of the values of the points occurring \emph{after} it. When percents
             of documents are plotted, time units with no document in the corpus are not plotted, since they
             have no defined value (0/0, reported as \code{NaN}); when a rolling mean is applied, the values
             are simply ignored, i.e. the mean is computed over the chosen window without the missing points.}
\seealso{\code{\link{setCorpusVariables}}, \code{\link{meta}}, \code{\link{zoo}}, \code{\link{xyplot}} }

