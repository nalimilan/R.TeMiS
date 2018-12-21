
#' Freqterm
#'
#' This function return the freq of each term regarding variables.
#'
#' @param dtm a document-term matrix.
#' @param variable  variable.
#' @param nb number of term.
#'
#' @return an array of frequencies.
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' freq <- Freqterm(dtm,nb=20)
#' @author Bouchet-Valat Milan
#' @export

Freqterm <- function(dtm, variable=NA,nb=10){
  #NA pour les freq globales
  freqTerms <- frequentTerms(dtm,variable,10)
  return(freqTerms)
}




#' LexicalSummary
#'
#' This function sumarise lexical information from your corpus.
#'
#' @param corpus a corpus.
#' @param dtm  a document-term matrix.
#' @param unit vector.
#' @return summary of each document in the corpus.
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' Sumlex <- LexicalSummary(corpus,dtm)
#' @author Bouchet-Valat Milan
#' @export


LexicalSummary <- function(corpus,dtm,variable=NULL,unit=c("document", "global")){
  unit <- match.arg(unit)
  dtmCorpus <- corpus
  dtmCorpus <- tm_map(dtmCorpus, content_transformer(tolower))
  dtmCorpus <- tm_map(dtmCorpus, content_transformer(function(x) gsub("\\p{P}|\\p{S}|\\p{Z}|\\p{C}", " ", x, perl=TRUE)))
  dtmCorpus <- tm_map(dtmCorpus, removeNumbers)
  wordsDtm <- DocumentTermMatrix(dtmCorpus, control=list(wordLengths=c(2, Inf)))
  voc <- vocabularyTable(dtm, wordsDtm, variable=variable, unit)
  return(voc)
}


#' ConcordenceTerm
#'
#' This function print your concordence.
#'
#' @param corpus a corpus.
#' @param dtm  a document-term matrix.
#' @param term a word.
#' @return concordence of your term.
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' ConcordenceTerm(corpus,dtm,"sociologie")
#' @author test
#' @export


ConcordenceTerm <- function(corpus,dtm,term){
  keep <- row_sums(dtm[, term]) >= 1
  corpus2<-corpus[keep]
  for(i in corpus2$content){
    print(i$content)
  }
}
