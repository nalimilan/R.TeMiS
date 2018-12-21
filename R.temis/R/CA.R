#' corpus_ca
#'
#' Run a correspondence analysis on a corpus.
#'
#' @param corpus A `Corpus` object.
#' @param dtm A `DocumentTermMatrix` object corresponding to `corpus` with one row per document.
#' @param variables An optional list of variables in [`meta(corpus)`][tm::meta] over which
#'   to aggregate `dtm`. If `NULL` (the default), the analysis is run on the unaggregated matrix.
#' @param ncp The number of axes to compute (5 by default). Note that this determines the number
#'   of axes that will be used for clustering by [`HCPC`][`FactoMineR::HCPC`].
#'   Pass `Inf` to compute all axes.
#' @param sparsity Value between 0 and 1 indicating the proportion of documents
#'   with no occurrences of a term above which that term should be dropped. By default
#'   all terms are kept (`sparsity=1`).
#' @param ... Additional arguments passed to [`FactoMineR::CA`].
#'
#' @return A `CA` object containing the correspondence analysis results.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' corpus_ca(corpus, dtm, ncp=3, sparsity=0.98)
#'
#' @export
corpus_ca <- function(corpus, dtm, variables=NULL, ncp=5, sparsity=1, ...) {
  if(!isTRUE(all.equal(names(corpus), rownames(dtm))))
    stop(.gettext("`dtm` must have one row per document in `corpus`, with the same names and in the same order."))

  if(!is.null(variables) && !all(variables %in% colnames(meta(corpus))))
    stop(.gettext("All items of 'variables' should be meta-data variables of the corpus."))

  oldMeta<-meta<-meta(corpus)[colnames(meta(corpus)) != "MetaID"]
  dict <- attr(dtm, "dict")

  # removeSparseTerms() does not accept 1
  if(sparsity < 1)
    dtm<-removeSparseTerms(dtm, sparsity)

  invalid<-which(apply(dtm,1,sum)==0)
  if(length(invalid) > 0) {
    dtm<-dtm[-invalid, , drop=FALSE]
    meta<-oldMeta[-invalid, , drop=FALSE]
    corpus<-corpus[-invalid]
  }

  ndocs<-nrow(dtm)
  nterms<-ncol(dtm)

  if(ndocs <= 1 || nterms <= 1)
    stop(.gettext("Please increase the value of the 'sparsity' parameter so that at least two documents and two terms are retained."))

  if(length(invalid) > 0) {
    message(sprintf(.gettext("%i documents have been skipped because they do not include any occurrence of the terms retained in the final document-term matrix. Increase the value of the 'sparsity' parameter if you want to include them. These documents are: %s."),
                    length(invalid), paste(names(invalid), collapse=", ")))
  }

  skippedVars1<-character()
  skippedVars2<-character()
  skippedLevs<-character()
  origVars<-character()

  dupLevels<-any(duplicated(unlist(lapply(meta, function(x) substr(unique(as.character(x[!is.na(x)])), 0, 30)))))


  varDtm <- NULL

  # Create mean dummy variables as rows
  # Keep in sync with showCorpusClustering()

  # Just in case variables have common levels, and are truncated to the same string
  vars <- colnames(meta)
  vars10<-make.unique(substr(vars, 0, 10))
  vars20<-make.unique(substr(vars, 0, 20))

  if(ncol(meta) > 0) {
    for(i in 1:ncol(meta)) {
      var<-vars[i]
      levs<-levels(factor(meta[,i]))
      totNLevels<-nlevels(factor(oldMeta[,i]))

      if(length(levs) == 0) {
        skippedVars1<-c(skippedVars1, var)
        next
      }
      else if(length(levs) > 100) {
        skippedVars2<-c(skippedVars2, var)
        next
      }
      else if(length(levs) < totNLevels) {
        skippedLevs<-c(skippedLevs, var)
      }

      # suppressWarnings() is used because rollup() warns when NAs are present
      suppressWarnings(mat<-rollup(dtm[1:ndocs, , drop=FALSE], 1, meta[i]))

      # If only one level is present, don't add the level name (e.g. YES),
      # except if all values are the same (in which case variable is useless but is more obvious that way)
      if(totNLevels == 1 && any(is.na(meta[,i])))
        rownames(mat)<-vars20[i]
      # In case of ambiguous levels of only numbers in levels, add variable names everywhere
      else if(dupLevels || !any(is.na(suppressWarnings(as.numeric(levs)))))
        rownames(mat)<-make.unique(paste(vars10[i], substr(levs, 0, 30), sep ="_" ))
      else # Most general case: no need to waste space with variable names
        rownames(mat)<-substr(levs, 0, 30)

      varDtm<-rbind(varDtm, mat)
      origVars<-c(origVars, rep(var, nrow(mat)))
    }
  }

  if(!is.null(variables) && sum(origVars %in% variables) < 2)
    stop(.gettext("Please select active variables so that at least two levels are present in the retained documents."))

  msg <- ""

  if(length(skippedVars1) > 0)
    msg <- sprintf(.gettext("Variable(s) %s have been skipped since they contain only missing values for retained documents."),
                   paste(skippedVars1, collapse=", "))

  if(length(skippedVars2) > 0) {
    msg2 <- sprintf(.gettext("Variable(s) %s have been skipped since they contain more than 100 levels."),
                    paste(skippedVars2, collapse=", "))
    if(msg != "")
      msg <- paste0(msg, "\n\n", msg2)
    else
      msg <- msg2
  }
  skippedVars <- unique(c(skippedVars1, skippedVars2))

  if(length(skippedLevs) > 0) {
    msg2 <- sprintf(.gettext("Some levels of variable(s) %s have been skipped since they contain only missing values for retained documents."),
                    paste(skippedLevs, collapse=", "))
    if(msg != "")
      msg <- paste0(msg, "\n\n", msg2)
    else
      msg <- msg2
  }

  if(msg != "")
    message(msg)

  newDtm <- as.matrix(rbind(dtm, varDtm))
  # Work around a bug in explor with characters which are not allowed in column names
  rownames(newDtm) <- make.names(rownames(newDtm))

  if(!is.null(variables))
    obj <- FactoMineR::CA(newDtm, row.sup=c(1:nrow(dtm), nrow(dtm) + which(!origVars %in% variables)),
                          ncp=ncp, graph=FALSE, ...)
  else if(nrow(newDtm) - ndocs > 0)
    obj <- FactoMineR::CA(newDtm, row.sup=(ndocs+1):nrow(newDtm), ncp=ncp, graph=FALSE, ...)
  else
    obj <- FactoMineR::CA(newDtm, ncp=ncp, graph=FALSE, ...)

  if(nrow(newDtm) - ndocs > 0) {
    obj$rowvars <- seq.int(ndocs + 1, nrow(newDtm))
    names(obj$rowvars) <- origVars
  }

  if(!is.null(dict))
    attr(obj, "dict") <- dict

  obj
}

#' contributive_docs
#'
#' Print documents which contribute the most to an axis of correspondence analysis.
#'
#' Occurrences of the `nterms` most contributive terms are highlighted.
#' If stemming or other transformations have been applied to original words
#' using [`combine_terms`], all original words which have been transformed
#' to the specified terms are highlighted.
#'
#' @param corpus A `Corpus` object.
#' @param ca A [`CA`][`FactoMineR::CA`] object.
#' @param axis The CA axis to consider.
#' @param ndocs The number of (most contributive) documents to print.
#' @param nterms The number of terms to highlight in documents.
#'
#' @return `Corpus` object (invisibly).
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' ca <- corpus_ca(corpus, dtm)
#' contributive_docs(corpus, ca, 1)
#'
#' # Also works when terms have been combined
#' dict <- dictionary(dtm)
#' dtm2 <- combine_terms(dtm, dict)
#' ca2 <- corpus_ca(corpus, dtm2)
#' contributive_docs(corpus, ca2, 1)
#'
#' @export
contributive_docs <- function(corpus, ca, axis, ndocs=10, nterms=25) {
  if(!inherits(corpus, "Corpus"))
    stop(.gettext("`corpus` must be a `Corpus` object"))

  if(!inherits(ca, "CA"))
    stop(.gettext("`ca` must be a `CA` object"))

  if(!axis %in% 1:ncol(ca$row$coord))
    stop(.gettext("`axis` must be an integer between 1 and the number of axes"))

  actdocs <- names(corpus)[1] %in% rownames(ca$row$coord)
  if(!actdocs)
    stop(.gettext("cannot show contributive documents for CA on aggregated matrix since documents are passive"))

  docscontr <- head(order(ca$row$coord[, axis], decreasing=TRUE), ndocs)

  posdocs <- docscontr[ca$row$coord[docscontr, axis] > 0]
  negdocs <- docscontr[ca$row$coord[docscontr, axis] < 0]

  msgpos <- .gettext("Most contributive documents on the positive side of axis %i:\n")
  msgneg <- .gettext("Most contributive documents on the negative side of axis %i:\n")
  termcontr <- head(order(ca$col$contrib[, axis], decreasing=TRUE), nterms)
  posterms <- termcontr[ca$col$coord[termcontr, axis] > 0]
  negterms <- termcontr[ca$col$coord[termcontr, axis] < 0]
  poswords <- rownames(ca$col$coord)[posterms]
  negwords <- rownames(ca$col$coord)[negterms]

  dict <- attr(ca, "dict")
  if(!is.null(dict)) {
      poswords <- rownames(dict)[dict[[.gettext("Term")]] %in% poswords]
      negwords <- rownames(dict)[dict[[.gettext("Term")]] %in% negwords]
  }

  # Used mostly to benefit from detection of terminal support
  redpat <- crayon::red(crayon::bold("X"))
  cat(crayon::red(crayon::bold(sprintf(msgpos, axis))))
  for(id in rownames(ca$row$coord)[posdocs]) {
    cat(crayon::bold(id), "\n")
    cat(gsub(sprintf("\\b(%s)\\b",
                     paste(poswords, collapse="|")),
             sub("X", "\\\\1", redpat),
             as.character(corpus[[id]]),
             ignore.case=TRUE))
    cat("\n")
  }

  # Used mostly to benefit from detection of terminal support
  bluepat <- crayon::blue(crayon::bold("X"))
  cat("\n\n", crayon::blue(crayon::bold(sprintf(msgneg, axis))))
  for(id in rownames(ca$row$coord)[negdocs]) {
    cat(crayon::bold(id), "\n")
    cat(gsub(sprintf("\\b(%s)\\b",
                     paste(negwords, collapse="|")),
             sub("X", "\\\\1", bluepat),
             as.character(corpus[[id]]),
             ignore.case=TRUE))
      cat("\n")
  }
}

#' extreme_docs
#'
#' Print documents which have the most extreme coordinations on an axis of correspondence analysis.
#'
#' Occurrences of the `nterms` most extreme terms are highlighted.
#' If stemming or other transformations have been applied to original words
#' using [`combine_terms`], all original words which have been transformed
#' to the specified terms are highlighted.
#'
#' @param corpus A `Corpus` object.
#' @param ca A [`CA`][`FactoMineR::CA`] object.
#' @param axis The CA axis to consider.
#' @param ndocs The number of (most contributive) documents to print.
#' @param nterms The number of terms to highlight in documents.
#'
#' @return `Corpus` object (invisibly).
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' ca <- corpus_ca(corpus, dtm)
#' contributive_docs(corpus, ca, 1)
#'
#' # Also works when terms have been combined
#' dict <- dictionary(dtm)
#' dtm2 <- combine_terms(dtm, dict)
#' ca2 <- corpus_ca(corpus, dtm2)
#' extreme_docs(corpus, ca2, 1)
#'
#' @export
extreme_docs <- function(corpus, ca, axis, ndocs=10, nterms=25) {
    if(!inherits(corpus, "Corpus"))
        stop(.gettext("`corpus` must be a `Corpus` object"))
    
    if(!inherits(ca, "CA"))
        stop(.gettext("`ca` must be a `CA` object"))
    
    if(!axis %in% 1:ncol(ca$row$coord))
        stop(.gettext("`axis` must be an integer between 1 and the number of axes"))
    
    actdocs <- names(corpus)[1] %in% rownames(ca$row$coord)
    if(actdocs) {
        docscontr <- head(order(abs(ca$row$coord[, axis]), decreasing=TRUE), ndocs)
        posdocs <- docscontr[ca$row$coord[docscontr, axis] > 0]
        negdocs <- docscontr[ca$row$coord[docscontr, axis] < 0]
        nms <- rownames(ca$row$coord)
    }
    else {
        docscontr <- head(order(abs(ca$row.sup$coord[, axis]), decreasing=TRUE), ndocs)
        posdocs <- docscontr[ca$row.sup$coord[docscontr, axis] > 0]
        negdocs <- docscontr[ca$row.sup$coord[docscontr, axis] < 0]
        nms <- rownames(ca$row.sup$coord)
    }
    
    msgpos <- .gettext("Most extreme documents on the positive side of axis %i:\n")
    msgneg <- .gettext("Most extreme documents on the negative side of axis %i:\n")
    termcontr <- head(order(ca$col$contrib[, axis], decreasing=TRUE), nterms)
    posterms <- termcontr[ca$col$coord[termcontr, axis] > 0]
    negterms <- termcontr[ca$col$coord[termcontr, axis] < 0]
    poswords <- rownames(ca$col$coord)[posterms]
    negwords <- rownames(ca$col$coord)[negterms]
    
    dict <- attr(ca, "dict")
    if(!is.null(dict)) {
        poswords <- rownames(dict)[dict[[.gettext("Term")]] %in% poswords]
        negwords <- rownames(dict)[dict[[.gettext("Term")]] %in% negwords]
    }
    
    # Used mostly to benefit from detection of terminal support
    redpat <- crayon::red(crayon::bold("X"))
    cat(crayon::red(crayon::bold(sprintf(msgpos, axis))))
    for(id in nms[posdocs]) {
        cat(crayon::bold(id), "\n")
        cat(gsub(sprintf("\\b(%s)\\b",
                         paste(poswords, collapse="|")),
                 sub("X", "\\\\1", redpat),
                 as.character(corpus[[id]]),
                 ignore.case=TRUE))
        cat("\n")
    }
    
    # Used mostly to benefit from detection of terminal support
    bluepat <- crayon::blue(crayon::bold("X"))
    cat("\n\n", crayon::blue(crayon::bold(sprintf(msgneg, axis))))
    for(id in nms[negdocs]) {
        cat(crayon::bold(id), "\n")
        cat(gsub(sprintf("\\b(%s)\\b",
                         paste(negwords, collapse="|")),
                 sub("X", "\\\\1", bluepat),
                 as.character(corpus[[id]]),
                 ignore.case=TRUE))
        cat("\n")
    }
}