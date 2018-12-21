#' Createdtm
#'
#' This function create a document-term matrix (dtm).
#'
#' @param corpus  corpus.
#' @param sparsity numeric argument that indicate the sparsity of your dtm.
#'
#' @return a matrix call dtm.
#'
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' @author Bouchet-Valat Milan
#' @export

Createdtm <- function(corpus,sparsity=1) {
  dtmCorpus <- corpus
  dtmCorpus <- tm_map(dtmCorpus, content_transformer(tolower)) #transforme des characteres en vecteur de chara
  dtmCorpus <- tm_map(dtmCorpus, content_transformer(function(x)
    gsub("\\p{P}|\\p{S}|\\p{Z}|\\p{C}", " ", x, perl=TRUE)))
  dtmCorpus <- tm_map(dtmCorpus, removeNumbers)
  dtm <- DocumentTermMatrix(dtmCorpus, control=list(tolower=FALSE, wordLengths=c(2,Inf)))
  if(sparsity < 1){
    dtm <- removeSparseTerms(dtm,sparsity)}
  return(dtm)

}



