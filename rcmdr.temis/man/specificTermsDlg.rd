\name{specificTermsDlg}
\alias{specificTermsDlg}
\title{List terms specific of a document or level}
\description{List terms most associated (positively or negatively) with each document or each
             of a variable's levels.}
\details{Specific terms reported here are those whose observed frequency in the document or level has
         the lowest probability under an hypergeometric distribution, based on their global frequencies
         in the corpus and on the number of occurrences in the document or variable level considered.
         The positive or negative character of the association is visible from the sign of the t value,
         or by comparing the value of the \dQuote{\% Term/Level} column with that of the \dQuote{Global \%}
         column.

         All terms with a probability below the value chosen using the first slider are reported, ignoring
         terms with fewer occurrences in the whole corpus than the value of the second slider (these terms
         can often have a low probability but are too rare to be of interest). The last slider allows limiting
         the number of terms that will be shown for each level.

         The result is a list of matrices, one for each level of the chosen variable, with five columns:
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
}

\seealso{\code{\link{specificTerms}}, \code{\link{setCorpusVariables}}, \code{\link{meta}},
         \code{\link{restrictTermsDlg}}, \code{\link{termsDictionary}} }
