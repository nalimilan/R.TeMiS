\name{cooccurrentTerms}
\alias{cooccurrentTerms}
\title{Show terms co-occurrences}
\description{Show terms that are the most associated with one or several reference terms.}
\usage{
cooccurrentTerms(term, dtm, variable = NULL, p = 0.1, n.max = 25,
                 sparsity = 0.95, min.occ = 2)
}
\arguments{
  \item{dtm}{A document-term matrix.}
  \item{term}{A character vector of length 1 corresponding to the name of a column of \code{dtm}.}
  \item{variable}{An optional vector of the same length as the number of rows in \code{dtm}, giving
    the levels by which results should be reported.}
  \item{p}{the maximum probability up to which terms should be reported.}
  \item{n.max}{the maximum number of terms to report for each level.}
  \item{sparsity}{Optional sparsity threshold (between 0 and 1) below which terms should be
                  skipped. See \code{\link{removeSparseTerms}} from tm.}
  \item{min.occ}{the minimum number of occurrences in the whole \code{dtm} below which
        terms should be skipped.}
}
\details{This function allows printing the terms that are most associated with one or several
         given terms, according to the document-term matrix of the corpus. Co-occurrent terms
         are those which are specific to documents which contain the given term(s). The output
         is the same as that returned by the \dQuote{Terms specific of levels...} dialog
         (see \code{\link{specificTermsDlg}}), using a dummy variable indicating whether the term
         is present or not in each document.

         When a variable is selected, the operation is run separately on each sub-matrix constituted
         by the documents that are members of the variable level. If the term does not appear in a
         level, \code{NA} is returned.
}
\value{
         The result is either a matrix (when \code{variable = NULL}) or a list of matrices,
         one for each level of the chosen variable, with seven columns:
         \describe{
         \item{\dQuote{\% Term/Cooc.}:}{the percent of the term's occurrences in all terms occurrences
             in documents where the chosen term is also present.}
         \item{\dQuote{\% Cooc./Term}:}{the percent of the term's occurrences that appear in documents
             where the chosen term is also present (rather than in documents where it does not appear),
             i.e. the percent of cooccurrences for the term.}
         \item{\dQuote{Global \%} or \dQuote{Level \%}:}{the percent of the term's occurrences
             in all terms occurrences in the corpus (or in the subset of the corpus
             corresponding to the variable level).}
         \item{\dQuote{Cooc.}:}{the number of cooccurrences of the term.}
         \item{\dQuote{Global} or \dQuote{Level}:}{the number of occurrences of the term in the corpus
             (or in the subset of the corpus corresponding to the variable level).}
         \item{\dQuote{t value}:}{the quantile of a normal distribution corresponding the probability \dQuote{Prob.}.}
         \item{\dQuote{Prob.}:}{the probability of observing such an extreme (high or low) number of occurrences of
             the term in documents where the chosen term is also present, under an hypergeometric distribution.}
         }
}

\seealso{\code{\link{termCoocDlg}}, \code{\link{specificTerms}}, \code{\link{DocumentTermMatrix}},
         \code{\link{restrictTermsDlg}}, \code{\link{termsDictionary}}, \code{\link{freqTermsDlg}} }
