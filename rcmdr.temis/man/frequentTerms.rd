\name{frequentTerms}
\alias{frequentTerms}
\title{List most frequent terms of a corpus}
\description{List terms with the highest number of occurrences in the document-term matrix of a corpus,
             possibly grouped by the levels of a variable.}
\usage{
frequentTerms(dtm, variable = NULL, n = 25)
}
\arguments{
  \item{dtm}{a document-term matrix.}
  \item{variable}{a vector whose length is the number of rows of \code{dtm}, or \code{NULL} to report most
        frequent terms by document; use \code{NA} to report most frequent terms in the whole corpus.}
  \item{n}{the number of terms to report for each level.}
}
\details{The probability is that of observing such extreme frequencies of the considered term in the level,
         under an hypergeometric distribution based on its global frequency in the corpus and on the
         number of occurrences of all terms in the document or variable level considered.
         The positive or negative character of the association is visible from the sign of the t value,
         or by comparing the value of the \dQuote{\% Term/Level} column with that of the \dQuote{Global \%}
         column.
}
\value{If \code{variable = NA}, one matrix with columns \dQuote{Global} and \code{Global \%} (see below).
       Else, a list of matrices, one for each level of the variable, with seven columns:
  \item{\dQuote{\% Term/Level}}{the percent of the term's occurrences in all terms occurrences in the level.}
  \item{\dQuote{\% Level/Term}}{the percent of the term's occurrences that appear in the level
        (rather than in other levels).}
  \item{\dQuote{Global \%}}{the percent of the term's occurrences in all terms occurrences in the corpus.}
  \item{\dQuote{Level}}{the number of occurrences of the term in the level (\dQuote{internal}).}
  \item{\dQuote{Global}}{the number of occurrences of the term in the corpus.}
  \item{\dQuote{t value}}{the quantile of a normal distribution corresponding the probability \dQuote{Prob.}.}
  \item{\dQuote{Prob.}}{the probability of observing such an extreme (high or low) number of occurrences of the
        term in the level, under an hypergeometric distribution.}
}
\author{Milan Bouchet-Valat}
\seealso{\code{\link{specificTerms}}, \code{\link[tm]{DocumentTermMatrix}}}

