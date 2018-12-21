#' dictionary
#'
#' Create a dictionary with information on all words in a corpus.
#'
#' @param dtm A `DocumentTermMatrix` object.
#' @param remove_stopwords Whether stopwords should be removed from the dictionary.
#'
#' @return A `data.frame` with row names indicating the terms, and columns giving the stem,
#'   the number of occurrences, and whether the term is a stopword.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' dictionary(dtm)
#'
#' @export
dictionary <- function(dtm, remove_stopwords=FALSE) {
  language <- attr(dtm, "language")
  stopwords <- colnames(dtm) %in% stopwords(language)
  if(remove_stopwords) {
    dtm <- dtm[, !stopwords]
    stopwords <- FALSE
  }
  dict <- data.frame(row.names=colnames(dtm),
                     SnowballC::wordStem(colnames(dtm), language),
                     col_sums(dtm),
                     ifelse(stopwords, .gettext("Stopword"), ""),
                     stringsAsFactors=FALSE)
  names(dict) <- c(.gettext("Term"), .gettext("Occurrences"), .gettext("Stopword"))
  dict
}

#' combine_terms
#'
#' Aggregate terms in a document-term matrix to according to groupings specified
#' by a dictionary.
#' 
#' If several terms use the same transformation, they will be aggregated together.
#' Terms missing from `dict` will be dropped.
#'
#' @param dtm A `DocumentTermMatrix` object.
#' @param dict A `data.frame` with one row per term in `dtm` that should be retained.
#'   The row names must match names of rows in `dtm`, and the first column must give the
#'   term into which it should be transformed.
#' @return An aggregated `DocumentTermMatrix` object.
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' dict <- dictionary(dtm)
#' combine_terms(dtm, dict)
#'
#' @export
combine_terms <- function(dtm, dict) {
  if(!all(rownames(dict) %in% colnames(dtm)))
    stop(.gettext("all rows of `dict` must correspond to terms in `dtm`"))
  else if(!.gettext("Term") %in% colnames(dict))
    stop(.gettext("`dict` must contain a \"Term\" column"))

  adtm <- rollup(dtm[, rownames(dict)], 2, dict[[.gettext("Term")]])
  attr(adtm, "weighting") <- attr(dtm, "weighting")
  attr(adtm, "language") <- attr(dtm, "language")
  attr(adtm, "dict") <- dict
  adtm
}