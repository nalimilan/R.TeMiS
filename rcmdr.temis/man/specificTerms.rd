\name{specificTerms}
\alias{specificTerms}
\title{List terms specific of a document or level}
\description{List terms most associated (positively or negatively) with each document or each
             of a variable's levels.}
\usage{
specificTerms(dtm, variable, p = 0.1, n.max = 25, sparsity = 0.95, min.occ = 2)
}
\arguments{
  \item{dtm}{a document-term matrix.}
  \item{variable}{a vector whose length is the number of rows of \code{dtm}, or \code{NULL} to report specific
        terms by document.}
  \item{p}{the maximum probability up to which terms should be reported.}
  \item{n.max}{the maximum number of terms to report for each level.}
  \item{sparsity}{Optional sparsity threshold (between 0 and 1) below which terms should be
                  skipped. See \code{\link[tm]{removeSparseTerms}} from tm.}
  \item{min.occ}{the minimum number of occurrences in the whole \code{dtm} below which
        terms should be skipped.}
}
\details{Specific terms reported here are those whose observed frequency in the document or level has
         the lowest probability under an hypergeometric distribution, based on their global frequencies
         in the corpus and on the number of occurrences of all terms in the document or variable level considered.
         The positive or negative character of the association is visible from the sign of the t value,
         or by comparing the value of the \dQuote{\% Term/Level} column with that of the \dQuote{Global \%}
         column.

         All terms with a probability below \code{p} are reported, up to \code{n.max} terms for each category.
}
\value{A list of matrices, one for each level of the variable, with seven columns:
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
\seealso{\code{\link{frequentTerms}}, \code{\link[tm]{DocumentTermMatrix}}, \code{\link[tm]{removeSparseTerms}}}

