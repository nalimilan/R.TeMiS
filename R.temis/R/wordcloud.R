#' CreateCloud
#'
#' Create wordcloud
#'
#' @param corpus  corpus.
#' @param language numeric argument that indicate the sparsity of your dtm.
#' @param max.words numeric argument that indicate the maximum of words used.
#' @param ... others arguments
#' @return a wordcloud
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' cloud <- CreateCloud(corpus,"fr")
#' @author Bouchet-Valat Milan
#' @export


CreateCloud <- function(corpus,language="fr",max.words=50,...){
  text_corpus <- tm_map(corpus, content_transformer(tolower))
  # The default tokenizer does not get rid of punctuation *and of line breaks!*, which
  # get concatenated with surrounding words
  # This also avoids French articles and dash-linked words from getting concatenated with their noun
  text_corpus <- tm_map(text_corpus, content_transformer(function(x)
    gsub("\\p{P}|\\p{S}|\\p{Z}|\\p{C}", " ", x, perl=TRUE)))

  text_corpus <- tm_map(text_corpus, function(x)removeWords(x,stopwords(kind = language)))
  wordcloud::wordcloud(text_corpus, max.words = as.numeric(max.words), random.order=F,fixed.asp =T,rot.per=0,...)
}

