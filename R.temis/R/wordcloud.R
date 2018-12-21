#' word_cloud
#'
#' Plot a word cloud from a document-term matrix.
#'
#' @param dtm A `DocumentTermMatrix` object.
#' @param n The maximum number of words to plot.
#' @param remove_stopwords Whether to remove stopwords appearing in a language-specific list
#'   (see [`tm::stopwords`]).
#' @param ... Additional arguments passed to [`wordcloud`][wordcloud::wordcloud].
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' dtm <- build_dtm(corpus)
#' word_cloud(dtm)
#'
#' @export
word_cloud <- function(dtm, n=50, remove_stopwords=TRUE, ...) {
  terms <- colnames(dtm)
  freqs <- col_sums(dtm)

  if(remove_stopwords) {
    stopwords <- terms %in% stopwords(attr(dtm, "language"))
    terms <- terms[!stopwords]
    freqs <- freqs[!stopwords]
  }
      
  wordcloud::wordcloud(terms, freqs, max.words=n,
                       random.order=FALSE, fixed.asp=TRUE, rot.per=0, ...)
}