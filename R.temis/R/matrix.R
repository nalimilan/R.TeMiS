#' build_dtm
#'
#' Compute document-term matrix from a corpus.
#'
#' @param corpus A `Corpus` object.
#' @param sparsity Value between 0 and 1 indicating the proportion of documents
#'   with no occurrences of a term above which that term should be dropped. By default
#'   all terms are kept (`sparsity=1`).
#' @param dictionary A vector of terms. to which the matrix should be restricted.
#'   By default, all words with more than one character are considered.
#' @param remove_stopwords Whether to remove stopwords appearing in a language-specific list
#'   (see [`tm::stopwords`]).
#' @param tolower Whether to convert all text to lower case.
#' @param remove_punctuation Whether to remove all punctuation from text before
#'   tokenizing terms.
#' @param remove_numbers Whether to remove all numbers from text before
#'   tokenizing terms.
#'
#' @return A `DocumentTermMatrix` object.
#'
#'
#' @examples
#'
#' file <- system.file("texts", "reut21578-factiva.xml", package="tm.plugin.factiva")
#' corpus <- import_corpus(file, "factiva", language="en")
#' build_dtm(corpus)
#'
#' @export
build_dtm <- function(corpus, sparsity=1, dictionary=NULL,
                      remove_stopwords=FALSE,
                      tolower=TRUE, remove_punctuation=TRUE, remove_numbers=TRUE) {
  # The default tokenizer does not get rid of punctuation *and of line breaks!*, which
  # get concatenated with surrounding words
  # This also avoids French articles and dash-linked words from getting concatenated with their noun
  if(remove_punctuation)
    corpus <- tm_map(corpus, content_transformer(function(x)
      gsub("\\p{P}|\\p{S}|\\p{Z}|\\p{C}", " ", x, perl=TRUE)))

  dtm <- DocumentTermMatrix(corpus,
                            control=list(language=meta(corpus, "language", type="corpus"),
                                         dictionary=dictionary,
                                         stopwords=remove_stopwords,
                                         tolower=tolower, removeNumbers=remove_numbers,
                                         removePunctuation=remove_punctuation,
                                         wordLengths=c(2, Inf)))

  if(sparsity < 1)
    dtm <- removeSparseTerms(dtm, sparsity)

  attr(dtm, "language") <- meta(corpus, "language", type="corpus")

  dtm
}