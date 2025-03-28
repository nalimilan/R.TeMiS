\name{termCoocDlg}
\alias{termCoocDlg}
\title{Show co-occurrent terms}
\description{Show terms that are the most associated with one or several reference terms.}
\details{This dialog allows printing the terms that are most associated with one or several
         given terms, according to the document-term matrix of the corpus. Co-occurrent terms
         are those which are specific to documents which contain the given term(s). The output
         is the same as that returned by the \dQuote{Terms specific of levels...} dialog
         (see \code{\link{specificTermsDlg}}), using a dummy variable indicating whether the term
         is present or not in each document.

         When a variable is selected, the operation is run separately on each sub-matrix constituted
         by the documents that are members of the variable level. If the term does not appear in a
         level, \code{NA} is returned.

         When several terms are entered, the operation is simply run several times separately.

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

\seealso{\code{\link{specificTermsDlg}}, \code{\link[tm]{DocumentTermMatrix}},
         \code{\link{restrictTermsDlg}}, \code{\link{termsDictionary}},
         \code{\link{freqTermsDlg}} }
