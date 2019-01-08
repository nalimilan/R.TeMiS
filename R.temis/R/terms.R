#' frequent_terms
#'
#' List terms with the highest number of occurrences in the document-term matrix of
#' a corpus, possibly grouped by the levels of a variable.
#'
#' @param dtm A `DocumentTermMatrix`.
#' @param variable An optional vector of values giving the groups for which most frequent
#'   terms should be reported.
#' @param n The maximal number of terms to report (for each group, if applicable).
#'
#' @return A list of matrices, one for each level of the variable, with columns:
#' - "\% Term/Level": the percent of the term's occurrences in all terms occurrences in the level.
#' - "\% Level/Term": the percent of the term's occurrences that appear in the level
#'   (rather than in other levels).
#' - "Global \%": the percent of the term's occurrences in all terms occurrences in the corpus.
#' - "Level": the number of occurrences of the term in the level ("internal").
#' - "Global": the number of occurrences of the term in the corpus.
#' - "t value": the quantile of a normal distribution corresponding the probability "Prob.".
#' - "Prob.": the probability of observing such an extreme (high or low) number of occurrences of
#'   the term in the level, under an hypergeometric distribution.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' frequent_terms(dtm)
#' frequent_terms(dtm, meta(corpus)$Date)
#'
#' @export
frequent_terms <- function(dtm, variable=NULL, n=25) {
  if(is.null(variable)) {
    counts <- sort(col_sums(dtm), decreasing=TRUE)[1:n]
    mat <- cbind(counts, counts/sum(dtm) * 100)
    colnames(mat) <- c(.gettext("Global occ."), .gettext("Global %"))
    return(mat)
  }

  if(length(unique(variable)) < 2)
    stop(.gettext("Please provide a variable with at least two levels."))

  if(!is.null(variable))
    dtm <- rollup(dtm, 1, variable)

  rs <- row_sums(dtm)
  tot <- sum(rs)

  cs <- col_sums(dtm)
  cs.tot <- cs/tot

  sapply(rownames(dtm), simplify=FALSE, function(l) {
    # rownames(dtm) == l is used below because "" is a possible level
    i <- rownames(dtm) == l

    # Empty documents create errors
    if(rs[i] == 0)
      return(numeric(0))

    rp <- as.matrix(dtm[l,]/rs[l])[1,]
    cp <- as.matrix(dtm[l,])[1,]/cs
    sup <- rp > cs.tot

    counts <- as.matrix(dtm[i,])[1,]

    # As this is a discrete distribution, we need to subtract one
    # to include the value when switching sides
    p.val <- phyper(ifelse(sup, counts - 1, counts), rs[l], tot - rs[l], cs)
    t.val <- qnorm(p.val)

    p.val[sup] <- 1 - p.val[sup]

    ord <- head(order(counts, decreasing=TRUE), n)
    ret <- cbind(term.clus=rp[ord] * 100, clus.term=cp[ord] * 100,
                 p.global=cs[ord]/tot * 100, n.int=counts[ord], n.global=cs[ord],
                 t.value=t.val[ord], p.value=round(p.val[ord], 4))
    colnames(ret) <- c(.gettext("% Term/Level"), .gettext("% Level/Term"), .gettext("Global %"),
                       .gettext("Level"), .gettext("Global occ."),
                       .gettext("t value"), .gettext("Prob."))

    ret[order(ret[, 4], decreasing=TRUE),]
  })
}

#' specific_terms
#'
#' List terms most associated (positively or negatively) with each document or each
#' of a variable's levels.
#'
#' Specific terms reported here are those whose observed frequency in the document or level has
#' the lowest probability under an hypergeometric distribution, based on their global frequencies
#' in the corpus and on the number of occurrences of all terms in the document or variable level considered.
#' The positive or negative character of the association is visible from the sign of the t value,
#' or by comparing the value of the "\% Term/Level" column with that of the "Global \%"
#' column.
#'
#' @param dtm A `DocumentTermMatrix`.
#' @param variable An optional vector of values giving the groups for which most frequent
#'   terms should be reported.
#' @param p The maximum p-value up to which terms should be reported.
#' @param n The maximal number of terms to report (for each group, if applicable).
#' @param sparsity Value between 0 and 1 indicating the proportion of documents
#'   with no occurrences of a term above which that term should be dropped. By default
#'   all terms are kept (`sparsity=1`).
#' @param min_occ The minimum number of occurrences in the whole `dtm` below which
#'   terms should be skipped.
#'
#' @return A list of matrices, one for each level of the variable, with columns:
#' - "\% Term/Level": the percent of the term's occurrences in all terms occurrences in the level.
#' - "\% Level/Term": the percent of the term's occurrences that appear in the level
#'   (rather than in other levels).
#' - "Global \%": the percent of the term's occurrences in all terms occurrences in the corpus.
#' - "Level": the number of occurrences of the term in the level ("internal").
#' - "Global": the number of occurrences of the term in the corpus.
#' - "t value": the quantile of a normal distribution corresponding the probability "Prob.".
#' - "Prob.": the probability of observing such an extreme (high or low) number of occurrences of
#'   the term in the level, under an hypergeometric distribution.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' specific_terms(dtm)
#' specific_terms(dtm, meta(corpus)$Date)
#'
#' @export
specific_terms <- function(dtm, variable=NULL, p=0.1, n=25, sparsity=1, min_occ=2) {
  if(!is.null(variable) && length(unique(variable)) < 2)
    stop(.gettext("Please provide a variable with at least two levels."))

  if(!is.null(variable))
    dtm <- rollup(dtm, 1, variable)

  # We need to compute these statistics before removing terms so that they are stable
  rs <- row_sums(dtm)
  tot <- sum(rs)

  if(sparsity < 1)
    dtm <- removeSparseTerms(dtm, sparsity)

  if(min_occ > 1)
    dtm <- dtm[, col_sums(dtm) >= min_occ]

  cs <- col_sums(dtm)
  cs.tot <- cs/tot

  sapply(rownames(dtm), simplify=FALSE, function(l) {
    # rownames(dtm) == l is used below because "" is a possible level
    i <- rownames(dtm) == l

    # Empty documents create errors
    if(rs[i] == 0)
      return(numeric(0))

    rp <- as.matrix(dtm[l,]/rs[l])[1,]
    cp <- as.matrix(dtm[l,])[1,]/cs
    sup <- rp > cs.tot

    counts <- as.matrix(dtm[i,])[1,]

    # As this is a discrete distribution, we need to subtract one
    # to include the value when switching sides
    p.val <- phyper(ifelse(sup, counts - 1, counts), rs[l], tot - rs[l], cs)
    t.val <- qnorm(p.val)

    p.val[sup] <- 1 - p.val[sup]

    keep <- which(p.val <= p)

    if(length(keep) == 0) return(numeric(0))

    ord <- head(intersect(order(p.val), keep), n)
    ret <- cbind(term.clus=rp[ord] * 100, clus.term=cp[ord] * 100,
                 p.global=cs[ord]/tot * 100, n.int=counts[ord], n.global=cs[ord],
                 t.value=t.val[ord], p.value=round(p.val[ord], 4))
    colnames(ret) <- c(.gettext("% Term/Level"), .gettext("% Level/Term"), .gettext("Global %"),
                       .gettext("Level"), .gettext("Global occ."),
                       .gettext("t value"), .gettext("Prob."))

    ret <- ret[order(ret[, 6], decreasing=TRUE),]
    n <- which(ret[,6] < 0)[1]
    if(!is.na(n)) {
      ret <- rbind(ret[1:(n-1),, drop=FALSE], NA, ret[n:nrow(ret),, drop=FALSE])
      rownames(ret)[n] <- stri_dup("-", max(nchar(rownames(ret), type="width")))
    }
    ret
  })
}

#' cooc_terms
#' 
#' Show terms that are the most associated (positively or negatively)
#' with a reference term.
#' 
#' Co-occurrent terms are those which are specific to documents which contain
#' the given term. The output is the same as that returned by [`specific_terms`].
#'
#' @param dtm A `DocumentTermMatrix`.
#' @param term A reference term appearing in `dtm`.
#' @param variable An optional vector of values giving the groups for which most frequent
#'   terms should be reported.
#' @param p The maximum p-value up to which terms should be reported.
#' @param n The maximal number of terms to report (for each group, if applicable).
#' @param sparsity Value between 0 and 1 indicating the proportion of documents
#'   with no occurrences of a term above which that term should be dropped. By default
#'   all terms are kept (`sparsity=1`).
#' @param min_occ The minimum number of occurrences in the whole `dtm` below which
#'   terms should be skipped.
#'
#' @return A list of matrices, one for each level of the variable, with columns:
#' - "\% Term/Level": the percent of the term's occurrences in all terms occurrences
#'   in documents where the chosen term is also present.
#' - "\% Level/Term": the percent of the term's occurrences that appear in documents
#'   where the chosen term is also present (rather than in documents where it does not appear),
#'   i.e. the percent of cooccurrences for the term..
#' - "Global \%": the percent of the term's occurrences in all terms occurrences
#'   in the corpus (or in the subset of the corpus corresponding to the variable level).
#' - "Level": the number of cooccurrences of the term.
#' - "Global": the number of occurrences of the term in the corpus
#'   (or in the subset of the corpus corresponding to the variable level).
#' - "t value": the quantile of a normal distribution corresponding the probability "Prob.".
#' - "Prob.": the probability of observing such an extreme (high or low) number of occurrences of
#'   the term in documents where the chosen term is also present, under an hypergeometric distribution.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' cooc_terms(dtm, "barrel")
#' cooc_terms(dtm, "barrel", meta(corpus)$Date)
#'
#' @export
cooc_terms <- function(dtm, term, variable=NULL,
                       p=0.1, n=25, sparsity=1, min_occ=2) {
    if(!term %in% colnames(dtm))
        stop(.gettext("'term' is not present in 'dtm'"))

    if(is.null(variable)) {
        specTerms <- specific_terms(dtm, as.vector(dtm[, term] > 0),
                                    p=p, n=n, sparsity=sparsity, min_occ=min_occ)[["TRUE"]]
        colnames(specTerms) <- c(.gettext("% Term/Cooc."), .gettext("% Cooc./Term"), .gettext("Global %"),
                                 .gettext("Cooc."), .gettext("Global"),
                                 .gettext("t value"), .gettext("Prob."))
        specTerms
    }
    else {
        sapply(levels(factor(variable)), simplify=FALSE, function(l) {
            subDtm <- dtm[variable == l,]
            if(sum(subDtm[, term]) == 0) {
                NA
            }
            else {
                specTerms <- specific_terms(subDtm, as.vector(subDtm[, term] > 0),
                                            p=p, n=n, sparsity=sparsity, min_occ=min_occ)[["TRUE"]]
                colnames(specTerms) <- c(.gettext("% Term/Cooc."), .gettext("% Cooc./Term"), .gettext("Level %"),
                                         .gettext("Cooc."), .gettext("Level"),
                                         .gettext("t value"), .gettext("Prob."))
                specTerms
            }
        })
    }
}

#' term_freq
#' 
#' Study frequencies of chosen terms in the corpus, among documents, or among levels of
#  a variable. The output is the same as that returned by [`specific_terms`].
#'
#' @param dtm A `DocumentTermMatrix`.
#' @param terms One or more reference term(s) appearing in `dtm`.
#' @param variable An optional vector of values giving the groups for which most frequent
#'   terms should be reported.
#' @param by_term Whether the third dimension of the array should be terms instead of levels.
#'
#' @return A list of matrices, one for each level of the variable, with columns:
#' - "\% Term/Level": the percent of the term's occurrences in all terms occurrences
#'   in documents where the chosen term is also present.
#' - "\% Level/Term": the percent of the term's occurrences that appear in documents
#'   where the chosen term is also present (rather than in documents where it does not appear),
#'   i.e. the percent of cooccurrences for the term..
#' - "Global \%": the percent of the term's occurrences in all terms occurrences
#'   in the corpus (or in the subset of the corpus corresponding to the variable level).
#' - "Level": the number of cooccurrences of the term.
#' - "Global": the number of occurrences of the term in the corpus
#'   (or in the subset of the corpus corresponding to the variable level).
#' - "t value": the quantile of a normal distribution corresponding the probability "Prob.".
#' - "Prob.": the probability of observing such an extreme (high or low) number of occurrences of
#'   the term in documents where the chosen term is also present, under an hypergeometric distribution.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' term_freq(dtm, "barrel")
#' term_freq(dtm, "barrel", meta(corpus)$Date)
#'
#' @export
term_freq <- function(dtm, terms, variable=NULL, by_term=FALSE) {
    wrongTerms <- terms[!terms %in% colnames(dtm)]
    if(length(wrongTerms) > 0)
        stop(sprintf(.ngettext(length(wrongTerms),
                               "Term \'%s\' does not exist in the corpus.",
                               "Terms \'%s\' do not exist in the corpus."),
                     paste(wrongTerms, collapse=.gettext("\', \'"))))

    if(length(variable) == 1 && is.na(variable)) {
        counts <- col_sums(dtm[, terms])
        mat <- cbind(counts, counts/sum(dtm) * 100)
        colnames(mat) <- c(.gettext("Global"), .gettext("Global %"))
        return(mat)
    }
    
    if(!is.null(variable) && length(unique(variable)) < 2)
        stop(.gettext("Please provide a variable with at least two levels."))
    
    if(!is.null(variable))
        dtm <- rollup(dtm, 1, variable)
    
    # We need to compute these statistics before removing terms so that they are stable
    rs <- row_sums(dtm)
    tot <- sum(rs)
    
    dtm <- dtm[, terms]
    
    cs <- col_sums(dtm)
    cs.tot <- cs/tot
    
    ret <- sapply(rownames(dtm), simplify="array", function(l) {
        # rownames(dtm) == l is used below because "" is a possible level
        i <- rownames(dtm) == l
        
        rp <- as.matrix(dtm[l,]/rs[l])[1,]
        cp <- as.matrix(dtm[l,])[1,]/cs
        sup <- rp > cs.tot
        
        counts <- as.matrix(dtm[i,])[1,]
        
        # As this is a discrete distribution, we need to subtract one
        # to include the value when switching sides
        p.val <- phyper(ifelse(sup, counts - 1, counts), rs[l], tot - rs[l], cs)
        t.val <- qnorm(p.val)
        
        p.val[sup] <- 1 - p.val[sup]
        
        ret <- cbind(term.clus=rp * 100, clus.term=cp * 100,
                     p.global=cs/tot * 100, n.int=counts, n.global=cs,
                     t.value=t.val, p.value=round(p.val, 4))
        colnames(ret) <- c(.gettext("% Term/Level"), .gettext("% Level/Term"), .gettext("Global %"),
                           .gettext("Level"), .gettext("Global"),
                           .gettext("t value"), .gettext("Prob."))
        ret
    })
    
    if(length(terms) == 1 || by_term == TRUE)
        aperm(ret, c(3, 2, 1))
    else
        ret
}