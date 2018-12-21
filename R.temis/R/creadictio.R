
#' CreateDictionary
#'
#' This function create a dictionary with the words of your corpus.
#'
#' @param corpus  corpus.
#' @param dtm a document-term matrix.
#' @param language language of your corpus.
#'
#' @return a dictionary: matrix with frequencies , stems, meaningless words
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' dictionary <- CreateDictionary(corpus,dtm,"fr")
#' @author Bouchet-Valat Milan
#'
#' @export


CreateDictionary <- function(corpus,dtm,language="fr"){
dictionary <- data.frame(row.names=colnames(dtm), "Occurrences"=col_sums(dtm),
                         "Terme.Racine"=SnowballC::wordStem(colnames(dtm), language),
                         "Mot vide"=ifelse(colnames(dtm) %in%
                                             stopwords(language), "Mot vide", ""),
                         stringsAsFactors=FALSE)

dtm <- dtm[, Terms(dtm) != ""]
attr(dtm, "dictionary") <- dictionary
meta(corpus, type="corpus", tag="language") <- attr(dtm, "language") <- language
meta(corpus, type="corpus", tag="processing") <- attr(dtm, "processing") <-
  c(lowercase=TRUE, punctuation=TRUE, digits=TRUE, stopwords=FALSE, stemming=TRUE,
    customStemming=TRUE, twitter=FALSE, removeHashtags=NA, removeNames=NA)
return(dictionary)
}



#' SortDictionary
#'
#' This function order your dictionary.
#'
#' @param dictionary  corpus
#' @param by condition
#' @return a sorted dictionary
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' dictionary <- CreateDictionary(corpus,dtm,"fr")
#' dictionarysort <- SortDictionary(dictionary,"alphabetic")
#' @author Bouchet-Valat Milan
#' @export


SortDictionary<- function(dictionary,by="Occurrences"){
  if (by == "Occurrences"){
    sorted <- dictionary[order(-dictionary[,"Occurrences"]), ]
  }
  if (by == "Alphabetic"){
    sorted <- dictionary[order(dictionary[,"Terme.Racine"]), ]
  }
  return(sorted)
}


#' StemDtm
#'
#' This function change words by stems.
#'
#' @param dtm a document-term matrix.
#' @param dictionary dictionary.
#' @return a document-term matrix composed of stem.
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dtm <- Createdtm(corpus=corpus,sparsity=0.98)
#' dictionary <- CreateDictionary(corpus,dtm,"fr")
#' newdtm <- StemDtm(dtm,dictionary)
#' @author Bouchet-Valat Milan
#' @export

StemDtm <- function(dtm,dictionary) {
    dtm <- dtm[,dictionary["Mot.vide"]== ""]
    stemdtm <- rollup(dtm, 2, dictionary[dictionary["Mot.vide"]=="","Terme.Racine"])
    attr(stemdtm,"weighting")<- attr(dtm,"weighting")
  return(stemdtm)
    }
