\name{termTimeSeriesDlg}
\alias{termTimeSeriesDlg}
\title{Temporal Evolution of Occurrences}
\description{Variation over time of frequencies of one or several terms in the corpus, or of one
             term by levels of a variable.}
\details{This dialog allows computing and plotting the absolute number or row/column percent 
         of occurrences of terms over a time  variable, or of one term by levels of a variable.
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

             \item{\sQuote{\%U}}{Week of the year as decimal number (00-53) using Sunday as
                       the first day 1 of the week (and typically with the first
                       Sunday of the year as day 1 of week 1).  The US convention.}

             \item{\sQuote{\%W}}{Week of the year as decimal number (00-53) using Monday as
                       the first day 1 of the week (and typically with the first
                       Monday of the year as day 1 of week 1).  The UK convention.}

             \item{\sQuote{\%p}}{AM/PM indicator in the locale.  Used in conjunction with \sQuote{\%I}
                       and not with \sQuote{\%H}.}

             \item{\sQuote{\%S}}{Second as decimal number (00-61).}

             \item{\sQuote{\%y}}{Year without century (00-99).}

             \item{\sQuote{\%Y}}{Year with century.}
         }

         \dQuote{Time units} are chosen automatically according to the values of the time variable:
         it is set to the smallest unit in which all time values can be uniquely expressed.
         For example, if free dates are entered, the unit will be days; if times are entered but minutes
         are always 0, hours will be used; finally, if times are fully specified, seconds will be used as
         the time unit. The chosen unit appears in the vertical axis label of the plot.

         Three measures of term occurrences are provided (when no variable is selected, \dQuote{category}
         below corresponds to the whole corpus):\cr
         \itemize{
             \item{Row percent corresponds to the part of chosen term's occurrences over all terms
                   found in a given category (i.e., the sum of word counts of all documents from the category 
                   after processing) at each time point. This conceptually corresponds to line percents,
                   except that only the columns of the document-term matrix that match the given terms are shown.}
             \item{Column percent corresponds to the part of the chosen term's occurrences that
                   appear in each of the documents from a given category at each time point. This measure
                   corresponds to the strict definition of column percents.}
             \item{Absolute counts returns the relevant part of the document-term matrix, but summed
                   after grouping documents according to their category.}
         }

         The rolling mean is left-aligned, meaning that the number of documents reported for a
         point reflects the average of the values of the points occurring \emph{after} it. When percents
         of occurrences are plotted, time units with no occurrence in the corpus are not plotted, since they
         have no defined value (0/0, reported as \code{NaN}); when a rolling mean is applied, the values
         are simply ignored, i.e. the mean is computed over the chosen window without the missing points.}
\seealso{\code{\link{setCorpusVariables}}, \code{\link[tm]{meta}}, \code{link[zoo]{zoo}}, \code{link[lattice]{xyplot}},
         \code{\link{varTimeSeriesDlg}}, \code{\link{recodeTimeVarDlg}} }

