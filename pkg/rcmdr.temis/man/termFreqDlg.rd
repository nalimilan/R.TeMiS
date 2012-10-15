\name{termFreqDlg}
\alias{termFreqDlg}
\title{Term frequencies in the corpus}
\description{Study frequencies of chosen terms in the corpus, among documents, or among levels of
         a variable.}
\details{This dialog allows creating a table providing information about the frequency of chosen
         terms among documents or levels of a variable. If \dQuote{None (whole corpus)} is selected,
         the absolute frequency of the chosen terms and their percents in occurrences of all terms
         in the corpus are returned. If \dQuote{Document} or a variable is chosen, details about the
         association of the term with documents or levels are shown:
         \describe{
         \item{\dQuote{\% Term/Level}:}{the percent of the term's occurrences in all terms occurrences in the level.}
         \item{\dQuote{\% Level/Term}:}{the percent of the term's occurrences that appear in the level
             (rather than in other levels).}
         \item{\dQuote{Global \%}:}{the percent of the term's occurrences in all terms occurrences in the corpus.}
         \item{\dQuote{Level}:}{the number of occurrences of the term in the level (\dQuote{internal}).}
         \item{\dQuote{Global}:}{the number of occurrences of the term in the corpus.}
         \item{\dQuote{t value}:}{the quantile of a normal distribution corresponding the probability \dQuote{Prob.}.}
         \item{\dQuote{Prob.}:}{the probability of observing such an extreme (high or low) number of occurrences of
             the term in the level, under an hypergeometric distribution.}
         }

         The probability is that of observing such extreme frequencies of the considered term in the level,
         under an hypergeometric distribution based on its global frequency in the corpus and on the
         number of occurrences of all terms in the document or variable level considered.
         The positive or negative character of the association is visible from the sign of the t value,
         or by comparing the value of the \dQuote{\% Term/Level} column with that of the \dQuote{Global \%}
         column.

         The kind of plot to be drawn is automatically chosen from the selected measure. Row
         percents lead to bar plots, since the total sum of shown columns (terms) doesn't add up
         to 100%. On the other hand, and for the same reason, column percents prompt pie charts
         to be drawn. Absolute counts are also represented with bar plots, so that the vertical
         axis reports number of occurrences.

         When either several pie charts are drawn for each word, or a single word has been entered,
         the string \dQuote{\%T} in the plot title will be replaced with the name of the term.
         In all cases, the string \dQuote{\%V} will be replaced with the name of the selected variable.
}

\seealso{\code{\link{termFrequencies}}, \code{\link{setCorpusVariables}}, \code{\link{meta}},
         \code{\link{DocumentTermMatrix}}, \code{\link{barchart}}, \code{\link{pie}} }
