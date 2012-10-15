\name{freqTermsDlg}
\alias{freqTermsDlg}
\title{List most frequent terms of a corpus}
\description{List terms with the highest number of occurrences in the document-term matrix of a corpus.}
\details{This dialog allows printing the most frequent terms of the corpus. If a variable is chosen,
         the returned terms correspond to those with the highest total among the documents within each level
         of the variable. If \dQuote{None (whole corpus)} is selected,
         the absolute frequency of the chosen terms and their percents in occurrences of all terms
         in the whole corpus are returned. If \dQuote{Document} or a variable is chosen, details about the
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
}

\seealso{\code{\link{frequentTerms}}, \code{\link{setCorpusVariables}}, \code{\link{meta}},
         \code{\link{restrictTermsDlg}}, \code{\link{listTerms}} }
