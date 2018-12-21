

readinteger <- function()
{
  n <- readline(prompt="Enter positions of variables you want to keep (example : 1,2,3): ")
  n <-sort(as.integer(unlist(strsplit(n,","))))
}

#' ImportCorpus
#'
#' This function import your corpus.
#'
#' @param file file path you want to import.
#' @param format  file format.
#' @param language language of your corpus.
#' @param encoding encoding of the file.
#'
#' @return a corpus.
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' @author Bouchet-Valat Milan
#' @import NLP
#' @import methods
#' @import slam
#' @import tm
#' @import zoo
#' @import graphics
#' @import stats
#' @import utils
#' @export

ImportCorpus <- function(file, format, language="fr",encoding="UTF-8"){
  language <<- language
  if (format=="txt"){
    corpus <- VCorpus(DirSource(file, encoding=encoding),
                       readerControl=list(language))
    corpusVars <- data.frame(var1=factor(rep(" ", length(corpus))),
                              row.names=names(corpus))
    for(i in names(corpusVars)){
      meta(corpus,i) <- corpusVars[[i]]
    }
  }

  if (format=="csv"){
    corpusDataset <- read.csv2(file, fileEncoding=encoding)
    print("This is the variables : ")
    print(names(corpusDataset))
    choice <- as.integer(readline(prompt = "what is the position of the text variable ?  "))
    corpus <- VCorpus(DataframeSource(data.frame(doc_id=rownames(corpusDataset),
                                                 text=corpusDataset[[choice]])), readerControl=list(language))

    for(i in names(corpusDataset)[-choice]){
      meta(corpus,i) <- corpusDataset[[i]]
  }
  }
  if (format=="alceste"){
    corpus <- VCorpus(tm.plugin.alceste::AlcesteSource(file,encoding = encoding), readerControl=list(language))
    names(corpus) <- make.unique(names(corpus))
    corpusVars <- extractMetadata(corpus, date=FALSE)
    for(i in names(corpusVars)){
    meta(corpus,i) <- corpusVars[[i]]
  }
  }
  return(corpus)
}


#' SplitCorpus
#'
#' This function split your corpus.
#'
#' @param corpus your corpus.
#' @param nb number of paragraph you want.
#'
#' @return a splited corpus.
#'
#' @examples
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' splitedcorpus <- SplitCorpus(corpus,3)
#' @author Bouchet-Valat Milan
#' @export

SplitCorpus <- function(corpus,nb){
splitcorpus <- splitTexts(corpus, nb)
meta(splitcorpus, type="corpus", tag="split") <- TRUE
return(splitcorpus)
}

#' setCorpusVariables
#'
#' Set corpus meta-data variables from the active data set.
#'
#' @param corpus your corpus.
#' @param dset data set.
#'
#' @return a corpus with new variables
#'
#' @examples
#'
#' corpus <- ImportCorpus(system.file("texts/Corpus_final.txt",package = "functiontest"),"alceste","fr")
#' dset <- data.frame(x=c(1:length(corpus)))
#' corpus <- setCorpusVariables(corpus,dset)
#' @author Bouchet-Valat Milan
#' @export

setCorpusVariables <- function(corpus,dset) {

  # If corpus was split, we need to replicate variables
  split <- isTRUE(meta(corpus, type="corpus", tag="split"))

  len <- if (split) length(unique(meta(corpus, "Doc N")[[1]])) else length(corpus)

  # Remove dropped and empty variables
  for(var in colnames(meta(corpus))[!colnames(meta(corpus)) %in%
                                    c(colnames(dset), "Doc N", "Doc ID", "Cluster",
                                      sapply(corpusVars, function(x) all(is.na(x) | x == "")))])
    meta(corpus, var) <- NULL

  # Add new variables
  indices <- which(sapply(dset, function(x) !all(is.na(x) | x == "", na.rm=TRUE)))

  # Drop empty levels, which notably happen when changing values manually
  if(length(indices) > 0) {
    if(split) {
      for(i in indices)
        meta(corpus, colnames(dset)[i]) <- droplevels(factor(dset[meta(corpus, "Doc N")[[1]], i]))
    }
    else {
      for(i in indices)
        meta(corpus, colnames(dset)[i]) <- droplevels(factor(dset[[i]]))
    }
  }

  # Update names only if they changed
  oldDocNames <- if(split) unique(meta(corpus, "Doc ID")[[1]]) else names(corpus)
  corpusNames <- names(corpus)
  if(!identical(oldDocNames, row.names(dset))) {
    if(split) {
      # Note: does not work for SimpleCorpus
      names(corpus) <- make.unique(row.names(dset)[meta(corpus, "Doc N")[[1]]])
      meta(corpus, "Doc ID") <- row.names(dset)[meta(corpus, "Doc N")[[1]]]
    }
    else {
      # Note: does not work for SimpleCorpus
      names(corpus) <- row.names(dset)
    }


  }
  return(corpus)
}
